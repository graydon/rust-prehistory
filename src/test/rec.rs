// -*- C -*-

prog a
{
  main {
    type rect = rec(int x, int y, int w, int h);
    let rect rectv = rec(x=10, y=20, w=100, h=200);
    let rect rect2 = rectv;
    let int y = rect2.x;
    check (y == 10);
  }
}
