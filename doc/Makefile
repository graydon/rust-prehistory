
%.pdf: %.texi
	texi2pdf $<

%.html: %.texi
	makeinfo --html --force --no-split --output=$@ $<

clean:
	rm -f rust.aux rust.cp rust.fn rust.ky rust.log rust.pdf rust.pg rust.toc rust.tp rust.vr