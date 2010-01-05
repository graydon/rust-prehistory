// -*- C -*-

prog a
{
  fn x() -> () {
    log "hello from spawned function";
  }
  main {
    spawn x();
  }
}
