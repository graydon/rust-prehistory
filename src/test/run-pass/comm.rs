// -*- C -*-

prog root
{
  main {
    let port[int] p = port();
    spawn child(10);
    //spawn child(chan(p));
    //let int y;
    //y <- p;
    //check (y == 10);
  }
}

prog child
{
  let chan[int] c;
  let int x;
  init (int c0) -> () {
    log "child got number";
    //log c0;
    x = c0;
    //  init (chan[int] c0) -> () {
    //c = c0;
  }
  main {
    log x;
    //log "in child";
    //c <| 10;
  }
}

