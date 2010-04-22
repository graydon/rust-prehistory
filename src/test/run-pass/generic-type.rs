fn main() {
  type pair[T] = (T,T);
  let pair[int] x = (10,12);
  check (x._0 == 10);
  check (x._1 == 12);
}
