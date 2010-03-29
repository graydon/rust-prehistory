native "rust" mod rustrt {
  fn str_alloc(int n_bytes) -> str;
}

fn alloc(int n_bytes) -> str {
  ret rustrt.str_alloc(n_bytes);
}
