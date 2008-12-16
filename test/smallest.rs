// -*- C -*-

prog woohoo
{
  //  fn f(int x, str y) -> () {
  //g(x,y);
  //}
  fn g(int x, str y) -> () {
	log 3;
	let int z = k(1);
	log z;
	ret 1;
  }
  fn k(int x) -> int {
	ret 15;
  }
  main
    {
	  let int n = 2 + 3 * 7;
	  let str s = "hello there";
	  g(n,s);
    }
}

