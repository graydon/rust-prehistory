// -*- C -*-

prog a
{
  type mlist = tag(cons(int,@mlist), nil());

  main {
    auto x = nil();
    x = cons(10, x);
  }
}
