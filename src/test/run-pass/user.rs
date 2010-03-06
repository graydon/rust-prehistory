// -*- C++ -*-

use std;

fn main() -> () {
  log std.hello.hello();
  log std.hello.goodbye("first");
  log std.hello.hello();
  log std.hello.goodbye("second");
  check (std.hello.hello() == 12345);
}
