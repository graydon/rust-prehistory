// -*- C -*-

prog woohoo
{
  pub type t = int;
  fn putstr(str s) -> () {}
  fn putint(int i) -> () {}
  fn zerg(int i) -> int { ret i; }
  lim fn foo(int x) -> ()
  {
	val t y = x + 2;
    //putstr("hello");
    while (y+2 < 10) {
      //val () j = putint(y);
	  y = y + 2;
	}
  }
  
  main
    {
	  //val int i = foo(0);
	  val int x = 2 + 2;
    }
}

