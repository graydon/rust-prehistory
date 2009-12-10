// -*- C -*-

prog a
{
  type pair = rec(int head, mutable mlist tail);
  type mlist = tag(cons(@pair), nil());

  main {
    auto p = rec(head=10, tail=mutable nil());
    auto cycle = cons(p);
    p.tail = cycle;
  }
}
