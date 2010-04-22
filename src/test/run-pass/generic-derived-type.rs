fn g[X](X x) -> X {
  ret x;
}

fn f[T](T t) -> (T,T) {
  type pair = (T,T);
  let pair x = (t,t);
  ret g[pair](x);
}

fn main() {
  auto b = f[int](10);
  check (b._0 == 10);
  check (b._1 == 10);
}
