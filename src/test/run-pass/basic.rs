// -*- C -*-

prog a
{
  main {
      log "proc a";
      log "proc a";
      log "proc a";
      log "proc a";
      log "proc a";
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
    spawn a();
    spawn b();
    let int x = 10;
    x = g(n,s);
    log x;
  }
}

prog b
{
  main {
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
      log "proc b";
  }
}
