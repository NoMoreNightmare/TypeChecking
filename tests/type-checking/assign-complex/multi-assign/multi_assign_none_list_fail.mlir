// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// x: [int] = None
// y: [int] = None
// x = y = [None]
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "x"}> ({
        "choco.ast.list_type"() ({
          "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "y"}> ({
        "choco.ast.list_type"() ({
          "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "x"}> : () -> ()
    }, {
      "choco.ast.assign"() ({
        "choco.ast.id_expr"() <{"id" = "y"}> : () -> ()
      }, {
        "choco.ast.list_expr"() ({
          "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
        }) : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
