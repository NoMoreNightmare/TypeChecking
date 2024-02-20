// RUN: choco-opt -p check-assign-target,name-analysis,type-checking %s | filecheck %s

//
// i: int = 0
// j: int = "foo"
// k: int = 0
//


builtin.module {
  "choco.ast.program"() ({
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "i"}> ({
        "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
    }) : () -> ()
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "j"}> ({
        "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = "foo"}> : () -> ()
    }) : () -> ()
    "choco.ast.var_def"() ({
      "choco.ast.typed_var"() <{"var_name" = "k"}> ({
        "choco.ast.type_name"() <{"type_name" = "int"}> : () -> ()
      }) : () -> ()
    }, {
      "choco.ast.literal"() <{"value" = 0 : i32}> : () -> ()
    }) : () -> ()
  }, {
  ^0:
  }) : () -> ()
}

// CHECK: Semantic error:
