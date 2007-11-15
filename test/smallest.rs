prog woohoo
{
  
  native func putstr(str s) -> nil;
  native func putint(int i) -> nil;
  
  func foo(int x) -> nil
    {
      let int y = x + 2;
      putstr("hello from rust code");
      while (y < 10) {
	putint(y);
	y = y + 2;
      }
    }
  
  main
    {
      foo(0);
    }
}

// type proc = lim native; 
native func spawn(prog p) -> native;
main 
{
  spawn(woohoo);
  spawn(woohoo);
  spawn(woohoo);
}
