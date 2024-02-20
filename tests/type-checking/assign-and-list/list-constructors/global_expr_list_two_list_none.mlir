// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// [[], None]
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.list_expr"() ({
      "choco.ast.list_expr"() ({
      ^1:
      }) : () -> ()
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"<Empty>">>}> ({
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.named_type<"<Empty>">}> ({
// CHECK-NEXT:       ^1:
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.none, "type_hint" = !choco.ir.named_type<"<None>">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
