// -*- C -*-

fn main() -> () {
  let port[int] p = port();
  spawn thread child();
  log "spawned new thread";
}

fn child() -> () {
  log "I am the new thread";
}

