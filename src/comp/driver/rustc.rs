// -*- C -*-

fn main() -> () {
  let str filename = "comp/rustc.rc";
  libc.puts(librust.str_buf("beginnings of self-hosting"));
  let int file = libc.fopen(librust.str_buf(filename), librust.str_buf("r"));
  check (file != 0);
  log "opened file";
  log file;
  auto i = 0;
  while (i < 100) {
    auto ch = libc.fgetc(file);
    log ch;
    i = i + 1;
  }
  libc.fclose(file);
}
