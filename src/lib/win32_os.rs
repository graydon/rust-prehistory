import _str.sbuf;
import _vec.vbuf;

native mod libc = "msvcrt.dll" {
  fn _open(sbuf s, int flags) -> int;
  fn _read(int fd, vbuf buf, uint count) -> int;
  fn _write(int fd, vbuf buf, uint count) -> int;
  fn _close(int fd) -> int;
}

fn mk_buf_reader(str s) -> io.buf_reader {

  obj fd_reader(int fd) {
    fn read(vec[u8] v) -> uint {
      auto len = _vec.len[u8](v);
      auto buf = _vec.buf[u8](v);
      auto count = libc._read(fd, buf, len);
      if (count < 0) {
        fail;
      } else {
        ret uint(count);
      }
    }
  }

  let int O_RDONLY = 0;

  auto fd = libc._open(_str.buf(s), O_RDONLY);
  if (fd < 0) {
    log "error opening file";
    log sys.rustrt.last_os_error();
    fail;
  }
  ret fd_reader(fd);
}
