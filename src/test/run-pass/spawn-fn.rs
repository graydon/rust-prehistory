// -*- C -*-

prog a
{
  fn x() -> () {
    log "hello from spawned function";
  }
  main {
    spawn x();
    let int i = 4;
    while (i > 0) {
      i = i - 1;
      log "parent sleeping";
    }
  }
}
