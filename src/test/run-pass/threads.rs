// -*- C -*-

fn main() -> () {
  let port[int] p = port();
  spawn thread child(5);
  log "spawned new thread";
}

fn child(int x) -> () {
  log "I am the new thread";
  log x;
}

