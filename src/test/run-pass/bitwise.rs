// -*- rust -*-

fn main() {
  let int a = 1;
  let int b = 2;
  a xor= b;
  b xor= a;
  a = a xor b;
  log a;
  log b;
}

