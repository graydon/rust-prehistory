// -*- C -*-

prog a
{
  type point = (int, int);

  fn f(point p, int x, int y) -> () {
    check (p.{0} == x);
    check (p.{1} == y);
  }

  main {
    let point p = (10, 20);
    check (p.{0} == 10);
    check (p.{1} == 20);
    let point p2 = p;
    let int x = p2.{0};
    check (x == 10);
    f(p, 10, 20);
    f(p2, 10, 20);
  }
}
