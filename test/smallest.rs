prog woohoo
{
  
  lim fn foo(int x) -> ()
    {
      val int y = x + 2;
      putstr("hello");
      while (zerg(y+2) < 10) {
        putint(y);
        y = y + 2;
      }
    }
  
  main
    {
      foo(0);
    }
}

