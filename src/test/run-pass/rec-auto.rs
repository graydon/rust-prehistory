// -*- rust -*-

// https://bugzilla.mozilla.org/show_bug.cgi?id=552706

fun main() {
  auto x = rec(foo = "hello", bar = "world");
  log x.foo;
  log x.bar;
}
