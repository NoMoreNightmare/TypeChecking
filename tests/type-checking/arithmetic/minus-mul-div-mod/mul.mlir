// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// x: int = 0
// x = 0 * 1
//


builtin.module {
  "choco.ast.program"() ({
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
      "choco.ast.binary_expr"() <{"op" = "*"}> ({
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
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
// CHECK-NEXT:       "choco.ast.binary_expr"() <{"op" = "*", "type_hint" = !choco.ir.named_type<"int">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
