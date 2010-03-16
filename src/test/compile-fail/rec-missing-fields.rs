// -*- C -*-

// https://bugzilla.mozilla.org/show_bug.cgi?id=552790

type point = rec(int x, int y);

fn main() {
  let point p = rec(x=10);
  log p.y;
}
