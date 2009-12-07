// -*- C -*-

prog p
{
  fn f(int a, int b) : lt(a,b) -> () {
  }

  pred lt(int a, int b) {
    ret a < b;
  }

  main {
    let int a = 10;
    let int b = 23;
    check lt(b,a);
    f(b,a);
  }
}
