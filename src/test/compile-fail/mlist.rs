// -*- C -*-

prog a
{
  type mlist = tag(cons(int,mlist), nil());

  main {
       auto x = cons(10, cons(11, nil()));
  }
}
