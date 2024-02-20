// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// (6)
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.literal"() <{"value" = 6 : i32}> : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.literal"() <{"value" = 6 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
