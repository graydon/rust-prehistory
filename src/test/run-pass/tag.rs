// -*- C -*-

prog a
{
  type colour = tag(red(int,int), green());

  fn f() -> () {
       auto x = red(1,2);
       auto y = green();
       check (x != y);
  }

  main {
       f();
  }
}
