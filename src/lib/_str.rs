native "rust" mod rustrt {
  type sbuf;
  fn str_buf(str s) -> sbuf;
  fn str_len(str s) -> uint;
  fn str_alloc(int n_bytes) -> str;
}

pred is_utf8(vec[u8] v) {
}

fn alloc(int n_bytes) -> str {
  ret rustrt.str_alloc(n_bytes);
}

fn len(str s) -> uint {
  ret rustrt.str_len(s);
}

fn buf(str s) -> rustrt.sbuf {
  ret rustrt.str_buf(s);
}
