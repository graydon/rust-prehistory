// -*- C -*-

prog fact
{
  fn f(int x) -> int {
	log "in f:";
	log x;
   	if (x == 1) {
	  log "bottoming out";
	  ret 1;
	} else {
	  log "recurring";
	  ret x * f(x-1);
	}
  }
  main
    {
	  log f(5);
    }
}

