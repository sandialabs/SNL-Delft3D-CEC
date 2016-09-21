## Process this file with automake to produce Makefile.in

lib_LTLIBRARIES = libsigwatch.la
libsigwatch_la_SOURCES = sigwatch.c

ACLOCAL_AMFLAGS=-I m4

dist_man3_MANS = sigwatch.3

EXTRA_DIST = sigwatch.html style.css sigwatch.3.txt
MAINTAINERCLEANFILES = sigwatch.html

# Get a local copy of the CSS style sheet
# (snarf this from my web pages, for the mo)
style.css:
	rm -f style.css
	curl http://nxg.me.uk/style/base.css >style.css

sigwatch.3.txt: sigwatch.3
	rm -f sigwatch.3.txt
	nroff -man sigwatch.3 >sigwatch.3.txt

# Tarball for unpacking in the webpage distribution directory (maintainer only)
# (I think this is redundant, now that the distribution is happening from Bitbucket)
webpage-tarball.tar: $(distdir).tar.gz $(dist_man3_MANS) sigwatch.html
	rm -Rf webpage-tarball; mkdir webpage-tarball
	cp $(distdir).tar.gz webpage-tarball
	sed '1,10s,style.css,http://nxg.me.uk/style/base.css,' sigwatch.html >webpage-tarball/index.html
	cd webpage-tarball; tar cf ../$@ *
	rm -Rf webpage-tarball
