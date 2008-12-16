// -*- C -*-

prog fact
{
  fn fact(int x) -> int {
	log "in fact:";
	log x;
   	if (x == 1) {
	  log "bottoming out";
	  ret 1;
	} else {
	  log "recurring";
	  ret x * fact(x-1);
	}
  }
  main
    {
	  log fact(5);
    }
}

