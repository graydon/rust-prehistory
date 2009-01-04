// -*- C -*-

prog basic
{
  fn k(int x) -> int {
	ret 15;
  }
  fn g(int x, str y) -> int {
	log x;
	log y;
	let int z = k(1);
	ret z;
  }
  main
    {
	  let int n = 2 + 3 * 7;
	  let str s = "hello there";
	  let int x = 10;
	  x = g(n,s);
	  log x;
    }
}

