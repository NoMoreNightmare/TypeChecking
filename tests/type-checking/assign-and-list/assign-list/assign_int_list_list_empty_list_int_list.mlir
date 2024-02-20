// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// l: [[int]] = None
// l = [[], [0]]
// l = [[0], []]
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "l"}> ({
        "choco.ast.list_type"() ({
          "choco.ast.list_type"() ({
            "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
          }) : () -> ()
        }) : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "l"}> : () -> ()
    }, {
      "choco.ast.list_expr"() ({
        "choco.ast.list_expr"() ({
        ^0:
        }) : () -> ()
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "l"}> : () -> ()
    }, {
      "choco.ast.list_expr"() ({
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
        }) : () -> ()
        "choco.ast.list_expr"() ({
        ^1:
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK:      builtin.module {
// CHECK-NEXT:   "choco.ast.program"() ({
// CHECK-NEXT:     "choco.ast.var_def"() ({
// CHECK-NEXT:       "choco.ast.typed_var"() <{"var_name" = "l"}> ({
// CHECK-NEXT:         "choco.ast.list_type"() ({
// CHECK-NEXT:           "choco.ast.list_type"() ({
// CHECK-NEXT:             "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
// CHECK-NEXT:           }) : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.literal"() <{"value" = #choco.ast.none, "type_hint" = !choco.ir.named_type<"<None>">}> : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }, {
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "l", "type_hint" = !choco.ir.list_type<!choco.ir.list_type<!choco.ir.named_type<"int">>>}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.list_type<!choco.ir.named_type<"int">>>}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.named_type<"<Empty>">}> ({
// CHECK-NEXT:         ^0:
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:     "choco.ast.assign"() ({
// CHECK-NEXT:       "choco.ast.id_expr"() <{"id" = "l", "type_hint" = !choco.ir.list_type<!choco.ir.list_type<!choco.ir.named_type<"int">>>}> : () -> ()
// CHECK-NEXT:     }, {
// CHECK-NEXT:       "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.list_type<!choco.ir.named_type<"int">>>}> ({
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.list_type<!choco.ir.named_type<"int">>}> ({
// CHECK-NEXT:           "choco.ast.literal"() <{"value" = 0 : i32, "type_hint" = !choco.ir.named_type<"int">}> : () -> ()
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:         "choco.ast.list_expr"() <{"type_hint" = !choco.ir.named_type<"<Empty>">}> ({
// CHECK-NEXT:         ^1:
// CHECK-NEXT:         }) : () -> ()
// CHECK-NEXT:       }) : () -> ()
// CHECK-NEXT:     }) : () -> ()
// CHECK-NEXT:   }) : () -> ()
// CHECK-NEXT: }
