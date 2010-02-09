// -*- C++ -*-

fn main() -> () {
   auto f = "Makefile";
   auto s = str_buf(f);
   auto buf = libc.mem.malloc(1024);
   auto fd = libc.io.open(s, 0, 0);
   libc.io.read(fd, buf, 1024);
   libc.io.write(1, buf, 1024);
   libc.io.close(fd);
   libc.mem.free(buf);
}