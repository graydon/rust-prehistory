fn main() {
  auto x = vec(1,2,3);
  auto y = 0;
  for (int i in x) {
    log i;
    y = y + i;
  }
  log y;
  check (y == 6);
}
