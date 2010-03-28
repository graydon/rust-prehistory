// -*- rust -*-

mod x {
  fn hello() {
    log "hello, first-class module world";
  }
}

fn main() {
  auto mx = x;
  mx.hello();
}
