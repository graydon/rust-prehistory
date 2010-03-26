// -*- C++ -*-

fn id[T](T x) -> T {
   ret x;
}

fn main() {
   auto x = 10;
   auto y = 11;
   y = id[int](x);
   log y;
   check (x == y);
}
