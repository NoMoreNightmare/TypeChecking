// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// i: str = "foo"
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
  ^0:
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
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
