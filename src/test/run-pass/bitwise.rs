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

  check (~(0xf0) & 0xff == 0xf);
  check (0xf0 | 0xf == 0xff);
  check (0b1010_1010 | 0b0101_0101 == 0xff);
}

