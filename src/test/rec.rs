// -*- C -*-

prog a
{
  type rect = rec(int x, int y, int w, int h);
  fn f(rect r, int y) -> () {
    let int y2 = r.y;
    check (y2 == y);
  }

  main {
    let rect rectv = rec(x=10, y=20, w=100, h=200);
    let rect rect2 = rectv;
    let int x = rect2.x;
    check (x == 10);
    f(rectv, 20);
  }
}
