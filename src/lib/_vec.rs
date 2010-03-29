native "rust" mod rustrt {
  fn vec_alloc[T](int n_elts) -> vec[T];
}

fn alloc[T](int n_elts) -> vec[T] {
  ret rustrt.vec_alloc[T](n_elts);
}
