// -*- C -*-

fn main() -> () {
  let str filename = "comp/rustc.rc";
  puts(str_buf("beginnings of self-hosting"));
  let int file = fopen(str_buf(filename), str_buf("r"));
  check (file != 0);
  log "opened file";
  log file;
  auto i = 0;
  while (i < 100) {
    auto ch = fgetc(file);
    log ch;
    i = i + 1;
  }
  fclose(file);
}
