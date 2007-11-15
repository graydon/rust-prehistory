
new alt t.happy { p = 10; 
                  q = new rec k { x = 1; y = 2; }
                  q = new func(int x, int y) -> int { return y; } }



maybe we can be tighter with this, and omit the word 'new'. I hate it. users hate it.
it's a noise word.


new t.happy { p = 10,
	      q = 11,
              s = new func (int p, int q) -> int { return p + q; }   // switches to declarator form part way through
              x = new foo (1, 2, 3),                                 // vec or tup constructor
              y = new bar { a = b, c = d },                          // record or alt constructor
              z = new proc { main { print("hello"); } } }


a crucial rule here: func, prog, proc, tup, and vec types compare structurally.
alt and rec types compare nominally.


don't 