// -*- rust -*-

fn main() {
  for* (int i = first_ten()) {
    log "main";
    log i;
  }
}

fn* first_ten() -> int {
  let int i = 0;
  while (i < 10) {
    log "first_ten";
    log i;
    put i;
    i = i + 1;
  }
}
