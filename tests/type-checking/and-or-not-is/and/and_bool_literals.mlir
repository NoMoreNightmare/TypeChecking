// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// a: bool = True
// a = True and True
// a = True and False
// a = False and True
// a = False and False
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "a"}> ({
        "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "a"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "and"}> ({
        "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "a"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "and"}> ({
        "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "a"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "and"}> ({
        "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "a"}> : () -> ()
    }, {
      "choco.ast.binary_expr"() <{"op" = "and"}> ({
        "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = #choco.ast.bool<False>}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "a"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "a", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "and", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "a", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "and", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<False>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "a", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "and", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<False>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "a", "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "and", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<False>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = #choco.ast.bool<False>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
