HTML = index.html index.en.html

.SUFFIXES: .html .en.html .scm

.scm.html:
	gosh $< ja > $@

.scm.en.html:
	gosh $< en > $@

all: $(HTML)

clean:
	@rm -f $(HTML)
