// -*- rust -*-

fn main() {
  let int a = 1;
  let int b = 2;
  a ^= b;
  b ^= a;
  a = a ^ b;
  log a;
  log b;
  check (b == 1);
  check (a == 2);
}

