fn add(u8 x, u8 y) -> u8 { ret x + y; }
fn sub(u8 x, u8 y) -> u8 { ret x - y; }
fn mul(u8 x, u8 y) -> u8 { ret x * y; }
fn div(u8 x, u8 y) -> u8 { ret x / y; }
fn rem(u8 x, u8 y) -> u8 { ret x % y; }

pred lt(u8 x, u8 y) { ret x < y; }
pred le(u8 x, u8 y) { ret x <= y; }
pred eq(u8 x, u8 y) { ret x == y; }
pred ne(u8 x, u8 y) { ret x != y; }
pred ge(u8 x, u8 y) { ret x >= y; }
pred gt(u8 x, u8 y) { ret x > y; }
