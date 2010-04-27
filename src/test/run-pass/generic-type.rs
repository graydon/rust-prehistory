type pair[T] = (T,T);
fn main() {
  let pair[int] x = (10,12);
  check (x._0 == 10);
  check (x._1 == 12);
}
