// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// a: [bool] = None
// a = [1, 2]
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "a"}> ({
        "choco.ast.list_type"() ({
          "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "a"}> : () -> ()
    }, {
      "choco.ast.list_expr"() ({
        "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
        "choco.ast.literal"() <{"value" = 2 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
