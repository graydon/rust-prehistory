// -*- C -*-

prog a
{
  fn x(int n) -> () {
    log "hello from spawned function";
    log n;
  }
  main {
    spawn x(1);
    spawn x(2);
    spawn x(3);
    let int i = 10;
    while (i > 0) {
      i = i - 1;
      log "parent sleeping";
    }
  }
}
