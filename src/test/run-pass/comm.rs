// -*- C -*-

prog root
{
  main {
    let port[int] p = port();
    spawn child(chan(p));
    let int y;
    y <- p;
    check (y == 10);
  }
}

prog child
{
  let chan[int] c;
  let int x;
  init (chan[int] c0) -> () {
    c = c0;
  }
  main {
    c <| 10;
  }
}

