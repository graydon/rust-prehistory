// -*- C -*-

prog root
{
  prog sub
  {
    let int id;
    let chan[int] parent;

    init(chan[int] _parent, int _id) -> () {
      parent = _parent;
      id = _id;
    }
    main {
      if (id == 0) {
        parent <| 0;
      } else {
        let port[int] p = port();
        auto child = spawn sub(chan(p), id-1);
        let int y <- p;
        parent <| y + 1;
      }
    }
  }
  main {
    let port[int] p = port();
    auto child = spawn sub(chan(p), 5);
    let int y <- p;
    check (y == 5);
  }
}
