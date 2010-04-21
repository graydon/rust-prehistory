// -*- rust -*-


fn main() {
  let int i = int('Q');
  check (i == 0x51);
  let u32 u = u32(i);
  check (u == u32(0x51));
  check (u == u32('Q'));
  check (s8(i) == s8('Q'));
  check (s8(u8(i)) == s8(u8('Q')));
  check (char(0x51) == 'Q');

  check (true == bool(1));
  check (u32(0) == u32(false));
}
