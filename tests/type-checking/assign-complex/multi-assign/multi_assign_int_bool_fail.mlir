// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// x: int = None
// y: bool = None
// x = y = 1
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "x"}> ({
        "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "y"}> ({
        "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
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
        "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
