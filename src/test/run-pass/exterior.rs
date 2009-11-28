// -*- C -*-

prog a
{
  type point = rec(int x, int y, mutable int z);
  fn f(@point p) -> () {
    check (p.z == 12);
    p.z = 13;
    check (p.z == 13);
  }
  main {
    let point a = rec(x=10, y=11, z=12);
    let @point b = a;
    check (b.z == 12);
    f(b);
    check (b.z == 12);
  }
}
