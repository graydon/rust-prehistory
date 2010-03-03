// -*- C -*-

fn main() -> () {
  let int i = 0;
  while (i < 100) {
    i -= 1;
    yield;
  }
}
