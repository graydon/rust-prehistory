// -*- C -*-

prog rustc
{
  let str filename;
  init () -> () { 
    filename = "comp/rustc.rc";
  }
  main { 
    puts("beginnings of self-hosting");
    auto file = fopen(filename, "r");
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
}
