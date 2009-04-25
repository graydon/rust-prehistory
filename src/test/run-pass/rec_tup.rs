// -*- C -*-

prog a
{
  type point = rec(int x, int y);
  type rect = (point, point);

  fn f(rect r, int x1, int y1, int x2, int y2) -> () {
    check (r.{0}.x == x1);
    check (r.{0}.y == y1);
    check (r.{1}.x == x2);
    check (r.{1}.y == y2);
  }

  main {
    let rect r = ( rec(x=10, y=20),
                   rec(x=11, y=22) );
    check (r.{0}.x == 10);
    check (r.{0}.y == 20);
    check (r.{1}.x == 11);
    check (r.{1}.y == 22);
    let rect r2 = r;
    let int x = r2.{0}.x;
    check (x == 10);
    f(r, 10, 20, 11, 22);
    f(r2, 10, 20, 11, 22);
  }
}
