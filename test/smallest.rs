prog woohoo
{
  
  lim fn foo(int x) -> ()
    {
      val int y = x + 2;
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

