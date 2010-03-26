// -*- C++ -*-

fn id[T](T x) -> T {
   ret x;
}

fn main() {
   auto x = 62;
   auto y = 63;
   y = id[int](x);
   log y;
   check (x == y);
}
