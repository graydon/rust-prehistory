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
    while (y+2 < 10) {
      let () j = putint(y);
	  if (y - 3 == 4) {
		y = y + 2;
	  }
	}
	let int x = y;
	ret y;
  }
  
  main
    {
	  let int i = foo(0);
	  let int x = 2 + 2;
    }
}

