// -*- C -*-

prog woohoo
{
  fn f(int x, str y) -> () {
	g(x,y);
  }
  fn g(int x, str y) -> () {
	log x;
	log y;
  }
  main
    {
	  let int n = 2 + 3 * 7;
	  let str s = "hello there";
	  f(n,s);
    }
}

