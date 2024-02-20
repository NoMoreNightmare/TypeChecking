// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// [True]
// [1]
// ["foo"]
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"bool">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"str">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "foo", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
