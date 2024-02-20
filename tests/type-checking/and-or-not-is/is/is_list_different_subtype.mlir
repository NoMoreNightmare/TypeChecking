// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// b: bool = True
// b = [0] is ["0"]
// b = [True] is [1]
// b = [True] is [None]
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "b"}> ({
        "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "b"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "is"}> ({
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
        }) : () -> ()
      }, {
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = "0"}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "b"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "is"}> ({
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
        }) : () -> ()
      }, {
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "b"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "is"}> ({
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
        }) : () -> ()
      }, {
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "b"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "b", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "is", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"str">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = "0", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "b", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "is", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"bool">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "b", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "is", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"bool">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"<None>">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = #choco.ast.none, "type_hint" = !choco.ir.named_type<"<None>">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
