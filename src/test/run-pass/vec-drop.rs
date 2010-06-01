fn main() {
  // This just tests whether the vec leaks its members.
  let vec[@(int,int)] pvec = vec((1,2),(3,4),(5,6));
}
