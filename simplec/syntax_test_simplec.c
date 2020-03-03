// SYNTAX TEST "Packages/simplec/simplec.sublime-syntax"

  int a = 6;
//^^^ storage.type.simplec
//    ^ variable.simplec
//        ^ constant.numeric.simplec

  void foo() {
//^^^^ meta.function.simplec
//^^^^ storage.type.simplec
//     ^^^^^^^^ meta.function.simplec
//     ^^^ entity.name.function.simplec
//           ^ meta.block.simplec

    int b;
  //^^^^^^^ meta.function.simplec meta.block.simplec
  //^^^ storage.type.simplec
  //    ^ variable.simplec

    foo();
  //^^^^^^^ meta.function.simplec meta.block.simplec
  //^^^^^ meta.function-call.simplec
  //^^^ variable.function.simplec meta.path.simplec

    {
  //^^ meta.function.simplec meta.block.simplec meta.block.simplec

      foo();
    //^^^^^^^ meta.function.simplec meta.block.simplec meta.block.simplec
    //^^^^^ meta.function-call.simplec
    //^^^ variable.function.simplec meta.path.simplec

    }
  //^ meta.function.simplec meta.block.simplec meta.block.simplec

  }
//^ meta.function.simplec meta.block.simplec
