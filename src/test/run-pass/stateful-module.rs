
// -*- C++ -*-

mod counter(mutable int x) {
  fn incr() -> () {
    x = x + 1;
  }
  fn get() -> int {
    ret x;
  }
}

fn main() -> () {
  auto y = counter(0);
  log y.get();
  y.incr();
  y.incr();
  log y.get();
}
