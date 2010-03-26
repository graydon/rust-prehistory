// -*- C++ -*-

fn id[T](T x) -> T {
   ret x;
}

fn main() {
   auto x = 62;
   auto y = 63;
   auto a = 'a';
   auto b = 'b';

   y = id[int](x);
   log y;
   check (x == y);

   b = id[char](a);
   log b;
   check (a == b);

}
