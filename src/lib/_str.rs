native "rust" mod rustrt {
  type sbuf;
  fn str_alloc(int n_bytes) -> str;
  fn str_buf(str s) -> sbuf;
}

fn is_utf8(vec[u8] v) -> bool {
}

fn alloc(int n_bytes) -> str {
  ret rustrt.str_alloc(n_bytes);
}
