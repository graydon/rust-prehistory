// -*- C -*-

prog a
{
  main {
    type point = (int, int);
    let point pointv = (10, 20);
    let point point2 = pointv;
    let int y = point2.{1};
    log y;
  }
}
