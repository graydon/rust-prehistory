// This checks that preemption works.

io fn starve_main(chan[int] alive) {
  log "signalling main";
  alive <| 1;
  log "starving main";
  let int i = 0;
  while (true) {
    i += 1;
  }
}

io fn main() {
  let port[int] alive = port();
  log "main started";
  let task s = spawn starve_main(chan(alive));
  let int i;
  log "main waiting for alive signal";
  i <- alive;
  log "main got alive signal";
  while (i < 1000) {
    log "main iterated";
    i += 1;
  }
  log "main completed";
}