all: css userguide changelog pdf utlist

userguide:	txt/userguide.txt
	asciidoc --unsafe --out-file=html/userguide.html -a linkcss=1 -a theme=tdh $<

utlist: txt/utlist.txt
	asciidoc --unsafe --out-file=html/utlist.html -a linkcss=1 -a theme=tdh $<

changelog:	txt/ChangeLog.txt
	asciidoc --out-file=html/ChangeLog.html txt/ChangeLog.txt 

.PHONY: pdf

pdf: txt/userguide.txt
	a2x -f pdf $<
	mv txt/userguide.pdf pdf/
	cd html && ln -sf ../pdf/userguide.pdf userguide.pdf
	rm txt/userguide.xml

css: html/toc.css
	cat /etc/asciidoc/stylesheets/xhtml11.css html/toc.css > html/tdh.css
	cp /etc/asciidoc/stylesheets/xhtml11-quirks.css html/tdh-quirks.css 

docbook: txt/userguide.txt
	asciidoc -b docbook --out-file=/tmp/userguide.xml $<
	xmlto -o html html-nochunks /tmp/userguide.xml 
