// -*- C++ -*-

fn id[T](T x) -> T {
   ret x;
}

type triple = (int,int,int);

fn main() {
   auto x = 62;
   auto y = 63;
   auto a = 'a';
   auto b = 'b';

   let triple p = (65, 66, 67);
   let triple q = (68, 69, 70);

   y = id[int](x);
   log y;
   check (x == y);

   b = id[char](a);
   log b;
   check (a == b);

   q = id[triple](p);
   x = p._2;
   y = q._2;
   log y;
   check (x == y);

}
