// -*- C -*-

prog a
{
  init(int x) {
  }

  main {
    let port[int] p = port();
    let chan[int] c = chan(p);
    let int y;
    c <| 10;
    y <- p;
  }
}
