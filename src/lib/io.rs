type buf_reader = obj {
  fn read(vec[mutable u8] buf) -> uint;
};

type buf_writer = obj {
  fn writer(vec[u8] buf) -> uint;
};
