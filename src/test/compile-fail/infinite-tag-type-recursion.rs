// -*- C -*-

prog a
{
  type mlist = tag(cons(int,mlist), nil());

  main {
    auto a = cons(10, cons(11, nil()));
  }
}
