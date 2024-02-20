// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// def foo() -> int:
//   return 0
//
// x: int = 0
// x = foo()
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.func_def"() <{"func_name" = "foo"}> ({
    ^0:
    }, {
      "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
    }, {
      "choco.ast.return"() ({
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "x"}> ({
        "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "x"}> : () -> ()
    }, {
      "choco.ast.call_expr"() <{"func" = "foo"}> ({
      ^1:
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.func_def"() <{"func_name" = "foo"}> ({
// CHECK-NEXT:     ^0:
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.return"() ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "x"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "x", "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.call_expr"() <{"func" = "foo", "type_hint" = !choco.ir.named_type<"int">}> ({
// CHECK-NEXT:       ^1:
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
