// -*- rust -*-

use std;

fn main() {
  auto s = std._str.alloc(10);
  s += "hello ";
  log s;
  s += "there";
  log s;
  log std.hello.hello();
  log std.hello.goodbye("first");
  log std.hello.hello();
  log std.hello.goodbye("second");
  check (std.hello.hello() == 12345);
  auto z = std._vec.alloc[int](10);
}
