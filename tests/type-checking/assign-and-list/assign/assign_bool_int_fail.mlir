// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// i: bool = True
// i = 2
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "i"}> ({
        "choco.ast.type_name"() <{"type_name" = "bool"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.bool<True>}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.assign"() ({
      "choco.ast.id_expr"() <{"id" = "i"}> : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = 2 : i32}> : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
