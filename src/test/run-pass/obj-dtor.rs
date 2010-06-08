obj worker(chan[int] c) {
  drop {
    log "in dtor";
    c <| 10;
  }
}

fn do_work(chan[int] c) {
  log "in child task";
  let worker w = worker(c);
  log "constructed worker";
}

fn main() {
  let port[int] p = port();
  log "spawning worker";
  auto w = spawn do_work(chan(p));
  let int i;
  log "parent waiting for shutdown";
  i <- p;
  log "received int";
  check (i == 10);
  join w;
}