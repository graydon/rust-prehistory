module foo;
syntax rx, html;

use bar.baz.(a,b,c);

type nat = int : positive(*);
type natvec1 = vec[nat];
type natvec2 = vec[int : positive(*)];

pred bounded1(int a, int b, int c) = le(a,b), lt(b,c);
pred bounded2(int a, int b, int c)
{
  ret a <= b & b < c;
}

const str x = "hello";
const str y = replace(x, ~rx.pat{(el+)o}, ~rx.sub{$1foo});
const str z = ~html.doc(4.0, "xhtml") 
{
	<head><title>hello</title></head>
};

iter span(int a, int b) : le(a,b) 
  -> int                : le(a,*), lt(*,b)
{
    int i = a;
    while (i < b)
    {
        yield i;
        i += 1;
    }
}
