fn main() {
  obj holder(@(mutable int) h) {
    drop {
      log "in dtor";
      h -= 1;
    }
  }

  let @(mutable int) i = 10;
  {
    log "allocating obj";
    let holder h = holder(i);
  }
  log "finished dtor";
  check (i == 9);
}