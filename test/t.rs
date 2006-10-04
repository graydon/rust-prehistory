type incrementer = -(pure func (-int i) : even(i) 
                                 -> int : odd(*), lt(i,*))^;

inline iter bar (zug[a,b,c] z, int i) 
         -> rat
{
        y.(5) = x.(3 + e.(12) >> 2).z;
        x.(10) <- p.q;
        yield 10;
}


type t = (alt { happy { int p; -a q; } sad });


pub prog entry
{
        sys.put.ty put;

        init(sys.rt^ rt, vec[str]^ args) -> nil
        {
                put = rt.put;
        }

        native func putstr(str x) -> ();
        native func putint(int x) -> ();

        func foo(int x) -> ()
	{	
		putstr("Hello, world\n");
		putint(x + 200 / 5);
	}

        main
        {
                foo(10);
        }

        fini
        {
        }
}

