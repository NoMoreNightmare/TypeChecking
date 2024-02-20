// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// None
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.literal"() <{"value" = #choco.ast.none, "type_hint" = !choco.ir.named_type<"<None>">}> : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
