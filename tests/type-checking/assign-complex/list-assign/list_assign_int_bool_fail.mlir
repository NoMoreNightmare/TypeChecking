// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// x: [int] = None
// x[0] = True
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
  }, {
    "choco.ast.assign"() ({
      "choco.ast.index_expr"() ({
        "choco.ast.id_expr"() <{"id" = "x"}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
