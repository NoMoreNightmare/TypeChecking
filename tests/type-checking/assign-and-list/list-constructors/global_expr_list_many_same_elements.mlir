// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// [True, False, True, True]
// [1, 2, 3, 4, 5, 6]
// ["f", "o", "o", "b", "a", "r"]
// [[0], [1], [2], [3], [4], [5], [6], [7]]
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 2 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 3 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 4 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 5 : i32}> : () -> ()
      "choco.ast.literal"() <{"value" = 6 : i32}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.literal"() <{"value" = "f"}> : () -> ()
      "choco.ast.literal"() <{"value" = "o"}> : () -> ()
      "choco.ast.literal"() <{"value" = "o"}> : () -> ()
      "choco.ast.literal"() <{"value" = "b"}> : () -> ()
      "choco.ast.literal"() <{"value" = "a"}> : () -> ()
      "choco.ast.literal"() <{"value" = "r"}> : () -> ()
    }) : () -> ()
    "choco.ast.list_expr"() ({
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 2 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 3 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 4 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 5 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 6 : i32}> : () -> ()
      }) : () -> ()
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 7 : i32}> : () -> ()
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
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 2 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 3 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 4 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 5 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = 6 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"str">>}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "f", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "o", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "o", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "b", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "a", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "r", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.list_type<!choco.ir.named_type<"int">>>}> ({
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 2 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 3 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 4 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 5 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 6 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 7 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
