// -*- rust -*-

fn main() {
  //  for* (int i = first_ten()) {
  //    log i;
  //  }
}

fn* first_ten() -> int {
  let int i = 0;
  while (i < 10) {
    put i;
    i = i + 1;
  }
}
