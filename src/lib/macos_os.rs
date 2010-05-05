import _str.sbuf;
import _vec.vbuf;

native mod libc = "libc.dylib" {
  fn open(sbuf s, int flags) -> int;
  fn read(int fd, vbuf buf, uint count) -> int;
  fn write(int fd, vbuf buf, uint count) -> int;
  fn close(int fd) -> int;
}
