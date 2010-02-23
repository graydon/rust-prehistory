// -*- C -*-

// error-pattern:predicate check

fn f(int a, int b) : lt(a,b) -> () {
}

pred lt(int a, int b) {
  ret a < b;
}

fn main() -> () {
  let int a = 10;
  let int b = 23;
  check lt(b,a);
  f(b,a);
}
