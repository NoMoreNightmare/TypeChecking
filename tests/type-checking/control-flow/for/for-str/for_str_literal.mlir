// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// x: str = ""
// for x in "a":
//   x = ""
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "x"}> ({
        "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = ""}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.for"() <{"iter_name" = "x"}> ({
      "choco.ast.literal"() <{"value" = "a"}> : () -> ()
    }, {
      "choco.ast.assign"() ({
        "choco.ast.id_expr"() <{"id" = "x"}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = ""}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "x"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.for"() <{"iter_name" = "x"}> ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "a", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.assign"() ({
// CHECK-NEXT:         "choco.ast.id_expr"() <{"id" = "x", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = "", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
