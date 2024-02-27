from __future__ import annotations

from abc import ABC
from dataclasses import dataclass
from functools import reduce
from typing import Dict, List, Optional, Tuple, Union

import xdsl.dialects.builtin
from xdsl.dialects.builtin import ModuleOp
from xdsl.ir import Attribute, MLContext, Operation
from xdsl.passes import ModulePass

from choco.ast_visitor import Visitor
from choco.dialects import choco_ast, choco_type
from choco.semantic_error import SemanticError


class Type(ABC):
    @staticmethod
    def from_op(op: Operation) -> "Type":
        if isinstance(op, choco_ast.TypeName):
            name = op.type_name.data  # type: ignore
            if name == "int":
                return int_type
            if name == "bool":
                return bool_type
            if name == "str":
                return str_type
            if name == "<None>":
                return none_type
            if name == "<Empty>":
                return empty_type
            if name == "object":
                return object_type
            raise Exception(f"Found type with unknown name `{name}'")
        if isinstance(op, choco_ast.ListType):
            elem_type = Type.from_op(op.elem_type.op)
            return ListType(elem_type)
        raise Exception(f"Found {op}, expected TypeName or ListType")

    @staticmethod
    def from_attribute(attr: Attribute) -> "Type":
        if isinstance(attr, choco_type.NamedType):
            name: str = attr.type_name.data  # type: ignore
            if name == "int":
                return int_type
            if name == "bool":
                return bool_type
            if name == "str":
                return str_type
            if name == "<None>":
                return none_type
            if name == "<Empty>":
                return empty_type
            if name == "object":
                return object_type
            raise Exception(f"Found type with unknown name `{name}'")
        if isinstance(attr, choco_type.ListType):
            elem_type = Type.from_attribute(attr.elem_type)  # type: ignore
            return ListType(elem_type)
        raise Exception(f"Found {attr}, expected TypeName or ListType")


@dataclass
class BasicType(Type):
    name: str


@dataclass
class ListType(Type):
    elem_type: Type


class BottomType(Type):  # "⊥"
    pass


class ObjectType(Type):  # "object"
    pass


@dataclass
class FunctionType(Type):  # input0 x ... x inputN -> output
    inputs: List[Type]
    output: Type


int_type = BasicType("int")
bool_type = BasicType("bool")
str_type = BasicType("str")
none_type = BasicType("<None>")
empty_type = BasicType("<Empty>")
bottom_type = BottomType()
object_type = ObjectType()


def to_attribute(t: Type) -> Attribute:
    if t == int_type:
        return choco_type.int_type
    elif t == bool_type:
        return choco_type.bool_type
    elif t == str_type:
        return choco_type.str_type
    elif t == none_type:
        return choco_type.none_type
    elif t == empty_type:
        return choco_type.empty_type
    elif t == object_type:
        return choco_type.object_type
    elif isinstance(t, ListType):
        elem_type = to_attribute(t.elem_type)
        return choco_type.ListType([elem_type])
    else:
        raise Exception(f"Can't translate {t} into an attribute")


def join(t1: Type, t2: Type) -> Type:
    if is_assignment_compatible(t1, t2):
        return t2
    if is_assignment_compatible(t2, t1):
        return t1
    else:
        return object_type


def is_subtype(t1: Type, t2: Type) -> bool:
    if t1 == t2:
        return True
    elif t2 == object_type and (
            t1 == int_type or t1 == bool_type or t1 == str_type or isinstance(t1, ListType)
    ):
        return True
    elif t1 == none_type and t2 == object_type:
        return True
    elif t1 == empty_type and t2 == object_type:
        return True
    elif t1 == bottom_type:
        return True
    else:
        return False


def is_assignment_compatible(t1: Type, t2: Type) -> bool:
    if is_subtype(t1, t2) and t1 != bottom_type:
        return True
    elif (
            (t1 == none_type)
            and t2 != int_type
            and t2 != bool_type
            and t2 != str_type
            and t2 != bottom_type
    ):
        return True
    elif isinstance(t2, ListType) and t1 == empty_type:
        return True
    elif (
            isinstance(t2, ListType)
            and isinstance(t1, ListType)
            and t1.elem_type == none_type
            and is_assignment_compatible(none_type, t2.elem_type)
    ):
        return True
    else:
        return False


def check_assignment_compatibility(t1: Type, t2: Type):
    if not is_assignment_compatible(t1, t2):
        raise SemanticError(f" Expected {t1} and {t2} to be assignment compatible")


def check_type(found: Type, expected: Type):
    if found != expected:
        raise SemanticError(f"Found `{found}' but expected {expected}")


def check_list_type(found: Type) -> Type:
    if not isinstance(found, ListType):
        raise SemanticError(f"Found `{found}' but expected a list type")
    else:
        return found.elem_type


@dataclass
class FunctionInfo:
    func_type: FunctionType
    params: List[str]
    nested_defs: List[Tuple[str, Type]]

    def __post_init__(self):
        if len(self.func_type.inputs) != len(self.params):
            raise Exception(f"Expected same number of input types and parameter names")


LocalEnvironment = Dict[str, Union[Type, FunctionInfo]]


class TypeChecking(ModulePass):
    name = "type-checking"

    def apply(self, ctx: MLContext, op: ModuleOp) -> None:
        o = build_env(op)
        r = bottom_type

        program = op.ops.first
        assert isinstance(program, choco_ast.Program)
        defs = list(program.defs.ops)
        if len(defs) >= 1:
            check_stmt_or_def_list(o, r, defs)
        stmts = list(program.stmts.ops)
        if len(stmts) >= 1:
            check_stmt_or_def_list(o, r, stmts)


# Build local environments
def build_env(module: ModuleOp) -> LocalEnvironment:
    o: LocalEnvironment = {
        "len": FunctionInfo(FunctionType([object_type], int_type), ["arg"], []),
        "print": FunctionInfo(FunctionType([object_type], none_type), ["arg"], []),
        "input": FunctionInfo(FunctionType([], str_type), [], []),
    }

    @dataclass
    class BuildEnvVisitor(Visitor):
        o: LocalEnvironment

        def visit_typed_var(self, typed_var: choco_ast.TypedVar):
            name, type = typed_var.var_name.data, Type.from_op(  # type: ignore
                typed_var.type.op
            )
            self.o.update({name: type})

        def traverse_func_def(self, func_def: choco_ast.FuncDef):
            f: str = func_def.func_name.data  # type: ignore
            # collect function parameter names and types
            xs: List[str] = []
            ts: List[Type] = []
            for op in func_def.params.ops:
                assert isinstance(op, choco_ast.TypedVar)
                name, type = op.var_name.data, Type.from_op(op.type.op)  # type: ignore
                xs.append(name)
                ts.append(type)
            # collect return type
            t = (
                Type.from_op(func_def.return_type.op)
                if (len(func_def.return_type.ops) == 1)
                else none_type
            )
            # collect nested variable definitions
            body_visitor = BuildEnvVisitor({})
            for op in func_def.func_body.ops:
                body_visitor.traverse(op)
            vs: List[Tuple[str, Type]] = []
            for var_name, var_type in body_visitor.o.items():
                assert isinstance(var_type, Type)
                vs.append((var_name, var_type))

            o.update({f: FunctionInfo(FunctionType(ts, t), xs, vs)})

    BuildEnvVisitor(o).traverse(module)

    return o


# Dispatch to typing rules to decide which rule to invoke


def check_stmt_or_def_list(o: LocalEnvironment, r: Type, ops: List[Operation]):
    return stmt_def_list_rule(o, r, ops)


def check_stmt_or_def(o: LocalEnvironment, r: Type, op: Operation):
    if isinstance(op, choco_ast.VarDef):
        typed_var = op.typed_var.op
        assert isinstance(typed_var, choco_ast.TypedVar)
        id = typed_var.var_name.data  # type: ignore
        e1 = op.literal.op
        return var_init_rule(o, r, id, e1)
    elif isinstance(op, choco_ast.Assign):
        if isinstance(op.target.op, choco_ast.ExprName):
            check_expr(o, r, op.target.op)
            id = op.target.op.id.data
            e1 = op.value.op
            return var_assign_stmt_rule(o, r, id, e1)
        elif isinstance(op.target.op, choco_ast.IndexExpr):
            e1 = op.target.op.value.op
            e2 = op.target.op.index.op
            e3 = op.value.op
            return list_assign_stmt_rule(o, r, e1, e2, e3)
        else:
            raise Exception("???")


    elif isinstance(op, choco_ast.Pass):
        return pass_rule(o, r)
    elif isinstance(op, choco_ast.Return):
        raise Exception("Support for choco_ast.Return not implemented yet")
    elif isinstance(op, choco_ast.If):
        raise Exception("Support for choco_ast.If not implemented yet")
    elif isinstance(op, choco_ast.While):
        raise Exception("Support for choco_ast.While not implemented yet")
    elif isinstance(op, choco_ast.For):
        raise Exception("Support for choco_ast.For not implemented yet")
    elif isinstance(op, choco_ast.GlobalDecl):
        raise Exception("Support for choco_ast.GlobalDecl not implemented yet")
    elif isinstance(op, choco_ast.NonLocalDecl):
        raise Exception(
            "Support for choco_ast.NonLocalDecl not implemented yet"
        )
    elif isinstance(op, choco_ast.FuncDef):
        raise Exception("Support for choco_ast.FuncDef not implemented yet")
    else:
        return expr_stmt_rule(o, r, op)


def check_expr(o: LocalEnvironment, r: Type, op: Operation) -> Type:
    t: Optional[Type] = None
    if isinstance(op, choco_ast.Literal):
        t = literal_rules(o, r, op)
    elif isinstance(op, choco_ast.UnaryExpr):
        op_name = op.op.data  # type: ignore
        e = op.value.op
        if op_name == "-":
            t = negate_rule(o, r, e)
        elif op_name == "not":
            t = not_rule(o, r, e)
        else:
            raise Exception(
                "Support for some choco_ast.UnaryExpr not implemented yet")
    elif isinstance(op, choco_ast.BinaryExpr):
        lhs = op.lhs.op
        binary_op = op.op.data
        rhs = op.rhs.op
        if binary_op == "and":
            t = and_rule(o, r, lhs, rhs)
        elif binary_op == "or":
            t = or_rule(o, r, lhs, rhs)
        elif binary_op == "is":
            t = is_rule(o, r, lhs, rhs)
        elif binary_op in ["<", ">", "<=", ">=", "==", "!="]:
            if binary_op in ["==", "!="]:
                if type(lhs.properties.get('value')) == choco_ast.BoolAttr:
                    t = bool_compare_rule(o, r, lhs, rhs)
                elif type(lhs.properties.get('value')) == choco_ast.IntegerAttr:
                    t = int_compare_rule(o, r, lhs, rhs)
                elif type(lhs.properties.get('value')) == choco_ast.StringAttr:
                    t = str_compare_rule(o, r, lhs, rhs)
            else:
                t = int_compare_rule(o, r, lhs, rhs)
        elif binary_op in ["+", "-", "*", "//", "%"]:
            if binary_op == "+":
                if type(lhs.properties.get('value')) == choco_ast.IntegerAttr:
                    t = arith_rule(o, r, lhs, rhs)
                elif lhs.name == "choco.ast.list_expr":
                    t = list_concat_rule(o, r, lhs, rhs)
                elif type(lhs.properties.get('value')) == choco_ast.StringAttr:
                    t = str_concat_rule(o, r, lhs, rhs)
            else:
                t = arith_rule(o, r, lhs, rhs)

    elif isinstance(op, choco_ast.ExprName):
        t = var_read_rule(o, r, op.id.data)  # type: ignore
    elif isinstance(op, choco_ast.IfExpr):
        e0 = op.cond.op
        e1 = op.then.op
        e2 = op.or_else.op
        t = cond_rule(o, r, e1, e0, e2)
    elif isinstance(op, choco_ast.IndexExpr):
        e1 = op.value.op
        e2 = op.index.op
        t = list_select_rule(o, r, e1, e2)
    elif isinstance(op, choco_ast.ListExpr):
        if op.elems.blocks[0].is_empty:
            t = nil_rule(o, r, op)
        elif not op.elems.blocks[0].is_empty:
            t = list_display_rule(o, r, op)
        else:
            raise Exception("???")

    elif isinstance(op, choco_ast.CallExpr):
        raise Exception("Support for choco_ast.CallExpr not implemented yet")

    assert isinstance(t, Type)

    op.properties["type_hint"] = to_attribute(t)

    return t


# Typing rules, each typing rule is implemented by a corresponding function


# [VAR-READ] rule
# O, R |- id: T
def var_read_rule(o: LocalEnvironment, r: Type, id: str) -> Type:
    # O(id) = T, where T is not a function type.
    t = o.get(id)
    if t is None:
        raise SemanticError(f"Unknown identifier {id} used")
    if isinstance(t, FunctionInfo):
        raise SemanticError(
            f"Function identifier `{id}' used where value identifier expected"
        )
    return t


# [VAR-INIT] rule
# O, R |- id: T = e1
def var_init_rule(o: LocalEnvironment, r: Type, id: str, e1: Operation):
    # O(id) = T
    t = o[id]
    if isinstance(t, FunctionInfo):
        raise SemanticError(
            f"Function identifier `{id}' used where value identifier expected"
        )
    # O, R |- e1: T1
    t1 = check_expr(o, r, e1)
    # T1 ≤a T
    check_assignment_compatibility(t1, t)


# [VAR-ASSIGN-STMT]
# O, R |- id = e1
def var_assign_stmt_rule(o: LocalEnvironment, r: Type, id: str, e1: Operation):
    t = o[id]

    t1 = check_expr(o, r, e1)

    check_assignment_compatibility(t1, t)


# [STMT-DEF-LIST] rule
# O, R |- s1 NEWLINE s2 NEWLINE . . . sn NEWLINE
def stmt_def_list_rule(o: LocalEnvironment, r: Type, sns: List[Operation]):
    # n >= 1
    assert len(sns) >= 1
    for sn in sns:
        # O, R |- sn
        check_stmt_or_def(o, r, sn)


# [PASS] rule
# O, R |- pass
def pass_rule(o: LocalEnvironment, r: Type):
    return


# [EXPR-STMT] rule
# O, R |- e
def expr_stmt_rule(o: LocalEnvironment, r: Type, e: Operation):
    # O, R |- e: T
    _t = check_expr(o, r, e)


# noinspection PySimplifyBooleanCheck
# O, R |- lit: T
def literal_rules(o: LocalEnvironment, r: Type, lit: choco_ast.Literal) -> Type:
    if isinstance(lit.value, choco_ast.BoolAttr):
        # [BOOL-FALSE] rule
        if lit.value.data == False:
            return bool_type
        # [BOOL-TRUE] rule
        if lit.value.data == True:
            return bool_type
    # [NONE] rule
    if isinstance(lit.value, choco_ast.NoneAttr):
        return none_type
    # [INT] rule
    if isinstance(lit.value, choco_ast.IntegerAttr):
        return int_type
    # [STR] rule
    if isinstance(lit.value, choco_ast.StringAttr):
        return str_type
    raise Exception(f"Could not type check literal `{lit}'")


# [NEGATE] rule
# O, R, |- - e: int
def negate_rule(o: LocalEnvironment, r: Type, e: Operation) -> Type:
    # O, R, |- e: int
    check_type(check_expr(o, r, e), expected=int_type)
    return int_type


# [ARITH] rule
# O, R |- e1 op e2 : int
def arith_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=int_type)
    check_type(check_expr(o, r, e2), expected=int_type)

    return int_type

# [INT-COMPARE] rule
# O, R |- e1 cmp_op e2 : bool
def int_compare_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=int_type)
    check_type(check_expr(o, r, e2), expected=int_type)

    return bool_type


# [BOOL-COMPARE] rule
# O, R |- e1 cmp_op e2 : bool
def bool_compare_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=bool_type)
    check_type(check_expr(o, r, e2), expected=bool_type)

    return bool_type

# [AND] rule
# O, R |- e1 and e2 : bool
def and_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=bool_type)
    check_type(check_expr(o, r, e2), expected=bool_type)

    return bool_type


# [OR] rule
# O, R |- e1 or e2 : bool
def or_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=bool_type)
    check_type(check_expr(o, r, e2), expected=bool_type)

    return bool_type


# [NOT] rule
# O, R |- not e : bool
def not_rule(o: LocalEnvironment, r: Type, e: Operation) -> Type:
    check_type(check_expr(o, r, e), expected=bool_type)
    return bool_type


# [COND] rule
# O, R |- e1 if e0 else e2 : T1 |_| T2
def cond_rule(
        o: LocalEnvironment, r: Type, e1: Operation, e0: Operation, e2: Operation
) -> Type:
    # O, R |- e0: bool
    check_type(check_expr(o, r, e0), expected=bool_type)
    # O, R |- e1: t1
    t1 = check_expr(o, r, e1)
    # O, R |- e2: t2
    t2 = check_expr(o, r, e2)

    return join(t1, t2)


# [STR-COMPARE] rule
# O, R |- e1 cmp_op e2 : bool
def str_compare_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=str_type)
    check_type(check_expr(o, r, e2), expected=str_type)

    return bool_type

# [STR-CONCAT]
# O, R |- e1 + e2 : str
def str_concat_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=str_type)
    check_type(check_expr(o, r, e2), expected=str_type)

    return str_type


# [STR-SELECT]
# O, R |- e1[e2] : str
def str_select_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    check_type(check_expr(o, r, e1), expected=str_type)
    check_type(check_expr(o, r, e2), expected=int_type)

    return str_type


# [IS]
# O, R |- e1 is e2 : bool
def is_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    t1 = check_expr(o, r, e1)
    t2 = check_expr(o, r, e2)
    if t1 not in [int_type, bool_type, str_type] and t2 not in [int_type, bool_type, str_type]:
        return bool_type
    else:
        print("Semantic error:")
        exit(0)


# [LIST-DISPLAY]
# O, R |- [e1, e2, ..., en] : [T]
def list_display_rule(o: LocalEnvironment, r: Type, e: Operation) -> Type:
    current = check_expr(o, r, e.elems.blocks[0].ops.first)

    for expr in e.elems.blocks[0].ops:
        temp = check_expr(o, r, expr)
        current = join(current, temp)


    res = ListType(current)
    return res


# [NIL]
# O, R |- [] : <Empty>
def nil_rule(o: LocalEnvironment, r: Type, e: Operation) -> Type:
    return empty_type


# [LIST-CONCAT]
# O, R |- e1 + e2 : [T]
def list_concat_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:

    list_t1 = check_expr(o, r, e1)
    list_t2 = check_expr(o, r, e2)

    t1 = list_t1.elem_type
    t2 = list_t2.elem_type

    t = join(t1, t2)

    list_t = ListType(t)
    return list_t

# [LIST-SELECT]
# O, R |- e1[e2] : T
def list_select_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation) -> Type:
    t = check_expr(o, r, e1)
    check_type(check_expr(o, r, e2), expected=int_type)
    return t.elem_type

# [LIST-ASSIGN-STMT]
# O, R |- e1[e2] = e3
def list_assign_stmt_rule(o: LocalEnvironment, r: Type, e1: Operation, e2: Operation, e3: Operation):
    list_t1 = check_expr(o, r, e1)

    t1 = list_t1.elem_type

    check_type(check_expr(o, r, e2), expected=int_type)
    t3 = check_expr(o, r, e3)

    check_assignment_compatibility(t3, t1)



# [MULTI-ASSIGN-STMT]
# O,R |- e1 = e2 = ... = en = e0
def multi_assign_stmt(o: LocalEnvironment, r: Type ):
    pass

# [INVOKE]
# O, R |- f(e1, e2, ..., en): T0
# def invoke_rule(o: LocalEnvironment, r: Type, ???) -> Type:
#     ???

# [RETURN-e] rule
# O, R |- return e
# def return_e_rule(o: LocalEnvironment, r: Type, ???):
#     ???

# [RETURN] rule
# O, R |- return
# def return_rule(o: LocalEnvironment, r: Type, ???):
#     ???

# [IF-ELSE]
# O, R |- if e: b1 else: b2
def if_else_rule(o: LocalEnvironment, r: Type, cond: Operation, then: Operation, or_else: Operation):
    check_type(check_expr(o, r, cond), expected=bool_type)
    check_expr(o, r, then)
    check_expr(o, r, or_else)

# [WHILE]
# O, R |- while e: b
def while_rule(o: LocalEnvironment, r: Type, cond: Operation, block: Operation):
    check_type(check_expr(o, r, cond), expected=bool_type)
    check_expr(o, r, block)

# [FOR-STR]
# O, R |- for id in e: b
def for_str_rule(o: LocalEnvironment, r: Type, id: str, e: Operation, b: Operation):
    t = o[id]
    check_type(check_expr(o, r, e), expected=str_type)
    check_assignment_compatibility(str_type, t)
    check_expr(o, r, b)

# [FOR-LIST]
# O, R |- for id in e: b
def for_list_rule(o: LocalEnvironment, r: Type, id: str, e: Operation, b: Operation):
    t = o[id]
    list_t1 = check_expr(o, r, e)
    check_type(list_t1, expected=ListType)
    check_assignment_compatibility(list_t1.elem_type, t)
    check_expr(o, r, b)



# [FUNC-DEF] rule
# O, R |- def f(x1:T1, ... , xn:Tn)  [[-> T0]]? :b
# def func_def_rule(o: LocalEnvironment, r: Type, ???):
#     ???
