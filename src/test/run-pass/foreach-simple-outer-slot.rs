// -*- rust -*-

fn main() {
  for each (int i in first_ten()) {
    log "main";
    log i;
  }
}

iter first_ten() -> int {
  let int i = 0;
  while (i < 10) {
    log "first_ten";
    put i;
    i = i + 1;
  }
}
