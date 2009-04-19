// -*- C -*-

prog p
{
  pred lt(int a, int b) {
    ret a < b;
  }

  main {
    let int a = 10;
    let int b = 23;
    let int c = 77;
    check lt(a,b);
    check lt(b,c);
  }
}
