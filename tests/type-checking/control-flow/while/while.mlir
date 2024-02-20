// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// while True:
//   pass
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.while"() ({
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }, {
      "choco.ast.pass"() : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.while"() ({
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.bool<True>, "type_hint" = !choco.ir.named_type<"bool">}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.pass"() : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
