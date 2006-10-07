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
        }

        native func putstr(str x) -> ();
        native func putint(int x) -> ();

        func foo(int x) -> ()
	{	
		int y;
		foo bleh;
		putstr("Hello, world\n");
		x = x + 200 / 5;
		while (x < 60)
		{
			putstr("counting: ");
			putint(x);
			putstr("\n");
			if ((x % 2) == 0)
			{
				putstr("ooh, an even number!\n");
			}
			x = x + 1;
		}
	}

        main
        {
                foo(10);
        }

        fini
        {
        }
}

