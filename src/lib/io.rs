type buf_reader = obj {
  fn read(vec[u8] buf) -> uint;
};

type buf_writer = obj {
  fn write(vec[u8] buf) -> uint;
};
