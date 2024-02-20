// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// res: str = ""
//
// res = "foo"[0]
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "res"}> ({
        "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = ""}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "res"}> : () -> ()
    }, {
      "choco.ast.index_expr"() ({
        "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "res"}> ({
// CHECK-NEXT:         "choco.ast.type_name"() <{"type_name" = "str"}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = "", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "res", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.index_expr"() <{"type_hint" = !choco.ir.named_type<"str">}> ({
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = "foo", "type_hint" = !choco.ir.named_type<"str">}> : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
