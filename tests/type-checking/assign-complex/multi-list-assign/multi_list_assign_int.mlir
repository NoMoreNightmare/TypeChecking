// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// [0][0] = [1][0] = 0
//


builtin.module {
  "choco.ast.program"() ({
  ^0:
  }, {
    "choco.ast.assign"() ({
      "choco.ast.index_expr"() ({
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
        }) : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.assign"() ({
        "choco.ast.index_expr"() ({
          "choco.ast.list_expr"() ({
            "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
          }) : () -> ()
        }, {
          "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
        }) : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:   ^0:
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.index_expr"() <{"type_hint" = !choco.ir.named_type<"int">}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.assign"() ({
// CHECK-NEXT:         "choco.ast.index_expr"() <{"type_hint" = !choco.ir.named_type<"int">}> ({
// CHECK-NEXT:           "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:             "choco.ast.literal"() <{"value" = 1 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:           }) : () -> ()
// CHECK-NEXT:         }, {
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }, {
// CHECK-NEXT:         "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
