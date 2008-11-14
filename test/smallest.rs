// -*- C -*-

prog woohoo
{
  pub type t = int;
  fn putstr(str s) -> () {}
  fn putint(int i) -> () {}
  fn zerg(int i) -> int { ret i; }
  lim fn foo(int x) -> int
  {
	let t y = x + 2;
    putstr("hello");
    while (y < 10) {
	  putint(y);
	  if (y - 3 == 4) {
		y = y + 2;
	  }
	}
	let t z;
	z = z + 0x55;
	foo(z);
  }
  
  main
    {
	  let int i = foo(0);
	  let int x = 2 + 2;
    }
}

