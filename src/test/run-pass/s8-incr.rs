// -*- C -*-

fn main() -> () {
  let s8 x = s8(-12);
  let s8 y = s8(-12);
  x = x + s8(1);
  x = x - s8(1);
  check(x == y);
}
