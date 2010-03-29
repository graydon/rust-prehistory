fn add(int x, int y) -> int { ret x + y; }
fn sub(int x, int y) -> int { ret x - y; }
fn mul(int x, int y) -> int { ret x * y; }
fn div(int x, int y) -> int { ret x / y; }
fn rem(int x, int y) -> int { ret x % y; }

pred lt(int x, int y) { ret x < y; }
pred le(int x, int y) { ret x <= y; }
pred eq(int x, int y) { ret x == y; }
pred ne(int x, int y) { ret x != y; }
pred ge(int x, int y) { ret x >= y; }
pred gt(int x, int y) { ret x > y; }
