SHELL = /bin/sh

.SUFFIXES:

EMACS = emacs
BATCHFLAGS = -batch -q -no-site-file

ELFILES = delicioapi.el delicious-html.el delicious.el

MAKEINFO = makeinfo

INFOFILE = delicious.texi
INFOTARGETS = delicious-el.info README


all: elisp info

elisp: $(ELFILES)
	$(EMACS) $(BATCHFLAGS) -eval "(add-to-list 'load-path \".\")" \
	-f batch-byte-compile $(ELFILES)

info: $(INFOTARGETS)

delicious-el.info: $(INFOFILE)
	$(MAKEINFO) $<

README: $(INFOFILE)
	$(MAKEINFO) --plaintext -o $@ $<

clean:
	rm *.elc $(INFOTARGETS)
