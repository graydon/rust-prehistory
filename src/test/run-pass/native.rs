// -*- C -*-

fn main() -> () {
  libc.puts(librust.str_buf("hello, native world 1"));
  libc.puts(librust.str_buf("hello, native world 2"));
  libc.puts(librust.str_buf("hello, native world 3"));
}
