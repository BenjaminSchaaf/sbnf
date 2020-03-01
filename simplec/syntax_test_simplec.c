// SYNTAX TEST "Packages/simplec/simplec.sublime-syntax"

  int a;
//^^^ storage.type.simplec
//    ^ variable.simplec

  void foo() {

    int b;

    foo();

    {

        foo();

    }

}
