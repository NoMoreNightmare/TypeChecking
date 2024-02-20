// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// b: bool = True
// b = "foo" != "bar"
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
      "choco.ast.binary_expr"() <{"op" = "!="}> ({
        "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = "bar"}> : () -> ()
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
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "!=", "type_hint" = !choco.ir.named_type<"bool">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = "foo", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = "bar", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
