// -*- C -*-

prog rustc
{
  main { 
    puts("beginnings of self-hosting");
    let int file = fopen("comp/rustc.rc", "r");
    log "opened file";
    log file;
    let int i = 0;
    while (i < 100) {
      let int ch = fgetc(file);
      log ch;
      i = i + 1;
    }
    fclose(file);
  }
}
