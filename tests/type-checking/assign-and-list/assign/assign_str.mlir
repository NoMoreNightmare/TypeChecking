// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// i: str = "foo"
// i = "bar"
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "i"}> ({
        "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "i"}> : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = "bar"}> : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "i"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "foo", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "i", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "bar", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
