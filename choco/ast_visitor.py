import re
from enum import Enum
from typing import Any, Callable

from choco.dialects.choco_ast import *


def camel_to_snake(name: str) -> str:
    pattern = re.compile(r"(?<!^)(?=[A-Z])")
    return pattern.sub("_", name).lower()


def get_method(instance: object, method: str) -> Optional[Callable[..., Any]]:
    if not hasattr(instance, method):
        return None
    else:
        f = getattr(instance, method)
        if callable(f):
            return f
        else:
            return None


class Visitor:
    def traverse(self, operation: Operation):
        class_name = camel_to_snake(type(operation).__name__)

        traverse = get_method(self, f"traverse_{class_name}")

        if traverse:
            traverse(operation)
        else:
            for r in operation.regions:
                for b in r.blocks:
                    for op in b.ops:
                        self.traverse(op)

        visit = get_method(self, f"visit_{class_name}")
        if visit:
            visit(operation)


class VisitorError:
    dictionaries = {}
    unreachable_return_or_pass = False

    func_dictionaries = {}

    def traverse(self, operation: Operation):
        class_name = camel_to_snake(type(operation).__name__)

        traverse = get_method(self, f"traverse_{class_name}")


        if traverse:
            traverse(operation)
        else:
            for r in operation.regions:
                for b in r.blocks:
                    for op in b.ops:
                        if isinstance(operation, Program):
                            if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                                    or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                                print("[Warning] Dead code found: The following expression is unused:")
                                print(op)
                                exit(0)
                        self.traverse(op)

        visit = get_method(self, f"visit_{class_name}")
        if visit:
            visit(operation)

    def traverse_func_def(self, operation):
        self.unreachable_return_or_pass = False
        self.dictionaries.update({operation.func_name.data: (operation, Status.FUNC_NOT_USED)})
        params = operation.params.ops
        params_dictionary = {}
        for param in params:
            params_dictionary.update({param.var_name.data: Status.PARAM_NOT_USED})



        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    # self.traverse(op)
                    if isinstance(op, FuncDef):
                        self.unreachable_return_or_pass = False
                    elif isinstance(op, Return) or isinstance(op, Pass):
                        if self.unreachable_return_or_pass:
                            print("[Warning] Dead code found: Program contains unreachable statements.")
                            exit(0)
                        else:
                            params_dictionary = self.traverse_func_def_helper(op, params_dictionary)
                            self.unreachable_return_or_pass = True
                    elif isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                            or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                        print("[Warning] Dead code found: The following expression is unused:")
                        print(op)
                        exit(0)
                    else:
                        params_dictionary = self.traverse_func_def_helper(op, params_dictionary)
                    self.traverse(op)


        params = params_dictionary.keys()

        for key in params:
            if params_dictionary.get(key) == Status.PARAM_NOT_USED:
                print("[Warning] Dead code found: The following function argument is unused: " + key + ".")
                exit(0)

    def traverse_func_def_helper(self, operation, params_dictionary):
        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    if isinstance(op, ExprName):
                        params_dictionary.update({op.id.data: Status.PARAM_USED})
                    else:
                        params_dictionary = self.traverse_func_def_helper(op, params_dictionary)
                    # self.traverse(op)
        return params_dictionary

    def traverse_if(self, operation):
        cond = operation.cond.op
        if isinstance(cond, Literal):
            if not cond.value.data:
                print("[Warning] Dead code found: Program contains unreachable statements.")
                exit(0)
            else:
                if operation.orelse.block.first_op:
                    print("[Warning] Dead code found: Program contains unreachable statements.")
                    exit(0)
        elif isinstance(cond, ExprName):
            id = cond.id.data
            status = self.dictionaries.get(id)
            variable_assign_or_init_op = status[0]
            if isinstance(variable_assign_or_init_op, VarDef):
                value = variable_assign_or_init_op.literal.op.value.data
                if not value:
                    print("[Warning] Dead code found: Program contains unreachable statements.")
                    exit(0)
                else:
                    if operation.orelse.block.first_op:
                        print("[Warning] Dead code found: Program contains unreachable statements.")
                        exit(0)
            elif isinstance(variable_assign_or_init_op, Assign):
                value = variable_assign_or_init_op.value.op.value.data
                if not value:
                    print("[Warning] Dead code found: Program contains unreachable statements.")
                    exit(0)
                else:
                    if operation.orelse.block.first_op:
                        print("[Warning] Dead code found: Program contains unreachable statements.")
                        exit(0)
        elif isinstance(cond, BinaryExpr):
            print(cond.op.data)
            exit(0)
        elif isinstance(cond, UnaryExpr):
            exit(0)

        orelse = operation.orelse.ops
        if operation.orelse.block.first_op:
            for op in orelse:
                if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                        or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                    print("[Warning] Dead code found: The following expression is unused:")
                    print(op)
                    exit(0)

        then = operation.then.ops
        for op in then:
            if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                    or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                print("[Warning] Dead code found: The following expression is unused:")
                print(op)
                exit(0)

        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def traverse_if_expr(self, operation):
        cond = operation.cond.op
        if isinstance(cond, Literal):
            print("[Warning] Dead code found: Program contains unreachable expressions.")
            exit(0)

        orelse = operation.orelse.ops
        if operation.orelse.block.first_op:
            for op in orelse:
                if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                        or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                    print("[Warning] Dead code found: The following expression is unused:")
                    print(op)
                    exit(0)

        then = operation.then.ops
        for op in then:
            if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                    or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                print("[Warning] Dead code found: The following expression is unused:")
                print(op)
                exit(0)

        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def traverse_while(self, operation):
        cond = operation.cond.op
        if isinstance(cond, Literal):
            print("[Warning] Dead code found: Program contains unreachable statements.")
            exit(0)

        body = operation.body.ops
        if operation.orelse.block.first_op:
            for op in body:
                if isinstance(op, BinaryExpr) or isinstance(op, UnaryExpr) \
                        or isinstance(op, IndexExpr) or isinstance(op, ListExpr) or isinstance(op, Literal):
                    print("[Warning] Dead code found: The following expression is unused:")
                    print(op)
                    exit(0)

        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def traverse_binary_expr(self, operation: BinaryExpr):
        binary_op = operation.op.data
        if binary_op == "or":
            lhs = operation.lhs.op
            if isinstance(lhs, Literal):
                if lhs.value.data:
                    print("[Warning] Dead code found: Program contains unreachable expressions.")
                    exit(0)
        elif binary_op == "and":
            lhs = operation.lhs.op
            if isinstance(lhs, Literal):
                if not lhs.value.data:
                    print("[Warning] Dead code found: Program contains unreachable expressions.")
                    exit(0)

        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def traverse_var_def(self, operation: VarDef):
        typed_var = operation.typed_var.op
        if not isinstance(typed_var.type.op, ListType):
            self.dictionaries.update({typed_var.var_name.data: (operation, Status.INIT_NOT_USED)})
        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    # def traverse_typed_var(self, operation: TypedVar):
    #     if not isinstance(operation.type.op, ListType):
    #         self.dictionaries.update({operation.var_name.data: (operation, Status.INIT_NOT_USED)})
    #     for r in operation.regions:
    #         for b in r.blocks:
    #             for op in b.ops:
    #                 self.traverse(op)

    def traverse_assign(self, operation: Assign):
        self.traverse(operation.value.op)
        target = operation.target.op
        if isinstance(target, ExprName):
            name = target.id.data
            status = self.dictionaries.get(name)
            if status[1] == Status.ASSIGN_NOT_USED:
                print("[Warning] Dead code found: The following store operation is unused:")
                print(status[0])
                exit(0)
            else:
                self.dictionaries.update({name: (operation, Status.ASSIGN_NOT_USED)})

    def traverse_expr_name(self, operation):
        if isinstance(operation, ExprName):
            status = self.dictionaries.get(operation.id.data)
            if status[1] == Status.INIT_NOT_USED:
                self.dictionaries.update({operation.id.data: (operation, Status.INIT_USED)})
            elif status[1] == Status.ASSIGN_NOT_USED:
                self.dictionaries.update({operation.id.data: (operation, Status.ASSIGN_USED)})
        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def traverse_call_expr(self, operation):
        name = operation.func.data
        status = (operation, Status.FUNC_USED)

        self.dictionaries.update({name: status})
        for r in operation.regions:
            for b in r.blocks:
                for op in b.ops:
                    self.traverse(op)

    def get_dictionaries(self):
        return self.dictionaries


class Status(Enum):
    INIT_NOT_USED = 1
    INIT_USED = 2
    ASSIGN_NOT_USED = 3
    ASSIGN_USED = 4

    FUNC_NOT_USED = 6
    FUNC_USED = 7

    PARAM_NOT_USED = 8
    PARAM_USED = 9
