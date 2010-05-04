import _str.sbuf;
import _vec.vbuf;

native mod libc {
  fn open(sbuf s, int flags) -> int;
  fn read(int fd, vbuf buf, uint count) -> int;
  fn write(int fd, vbuf buf, uint count) -> int;
  fn close(int fd) -> int;
}

fn mk_buf_reader(str s) -> io.buf_reader {

  obj fd_reader(int fd) {
    fn read(vec[u8] v) -> uint {
      auto len = _vec.len[u8](v);
      auto buf = _vec.buf[u8](v);
      auto count = libc.read(fd, buf, len);
      if (count < 0) {
        fail;
      } else {
        ret uint(count);
      }
    }
  }

  auto fd = libc.open(_str.buf(s), 0);
  if (fd < 0) {
    fail;
  }
  ret fd_reader(fd);
}
