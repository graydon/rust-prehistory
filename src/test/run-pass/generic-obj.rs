obj buf[T](vec[T] data) {
  fn get(int i) -> T {
    ret data.(i);
  }
}

fn main() {
  let vec[int] v = vec(1,2,3);
  let buf[int] b = buf[int](v);
  log b.get(0);
  log b.get(1);
  log b.get(2);
}
