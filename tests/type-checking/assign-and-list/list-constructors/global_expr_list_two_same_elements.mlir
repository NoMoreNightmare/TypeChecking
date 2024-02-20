// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// [True, False]
// [2, 3]
// ["foo", "bar"]
// [[], []]
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = 2 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 3 : i32}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
      "choco.ast.literal"() <{"value" = "bar"}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.list_expr"() ({
      ^1:
      }) : () -> ()
      "choco.ast.list_expr"() ({
      ^2:
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"bool">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<False>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 2 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 3 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"str">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "foo", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "bar", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"<Empty>">>}> ({
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.named_type<"<Empty>">}> ({
// CHECK-NEXT:       ^1:
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.named_type<"<Empty>">}> ({
// CHECK-NEXT:       ^2:
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
