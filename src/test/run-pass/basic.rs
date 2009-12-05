// -*- C -*-

prog a
{
  let chan[int] c;
  init(port[int] p) -> (){
    c = chan(p);
  }
  main {
    if (true) {
      log "proc a";
      log "proc a";
      log "proc a";
      log "proc a";
      log "proc a";
    }
    c <| 10;
  }
}

prog root
{
  fn k(int x) -> int {
    ret 15;
  }
  fn g(int x, str y) -> int {
    log x;
    log y;
    let int z = k(1);
    ret z;
  }
  main {
    let int n = 2 + 3 * 7;
    let str s = "hello there";
    let port[int] p = port();
    spawn a(p);
    spawn b(p);
    let int x = 10;
    x = g(n,s);
    log x;
    n <- p;
    n <- p;
    // FIXME: use signal-channel for this.
    log "children finished, root finishing";
  }
}

prog b
{
  let chan[int] c;
  init(port[int] p) -> (){
    c = chan(p);
  }
  main {
    if (true) {
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
    }
    c <| 10;
  }
}
