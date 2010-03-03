// -*- C -*-

fn checktrue(bool res) -> () {
  check(res);
}

fn main() -> () {
  evenk(42, checktrue);
  oddk(45, checktrue);
}

fn evenk(int n, fn(bool) -> () k) -> bool {
  if (n == 0) {
    be k(true);
  }
  else {
    be oddk(n - 1, k);
  }
}

fn oddk(int n, fn(bool) -> () k) -> bool {
  if (n == 0) {
    be k(false);
  }
  else {
    be evenk(n - 1, k);
  }
}
