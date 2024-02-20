// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// o: object = None
// for o in 1:
//   o = None
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "o"}> ({
        "choco.ast.type_name"() <{"type_name" = "object"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
    }) : () -> ()
  }, {
    "choco.ast.for"() <{"iter_name" = "o"}> ({
      "choco.ast.literal"() <{"value" = 1 : i32}> : () -> ()
    }, {
      "choco.ast.assign"() ({
        "choco.ast.id_expr"() <{"id" = "o"}> : () -> ()
      }, {
        "choco.ast.literal"() <{"value" = #choco.ast.none}> : () -> ()
      }) : () -> ()
    }) : () -> ()
  }) : () -> ()
}

// CHECK: Semantic error:
