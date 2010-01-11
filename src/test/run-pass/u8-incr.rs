// -*- C -*-

fn main() -> () {
  let u8 x = u8(12);
  //let u8 y = u8(12);
  x = x + u8(1);
  check(x == x);
  //x = u8(14);
  //x = x + u8(1);
}

