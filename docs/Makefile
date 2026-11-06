# Build the Tempo manual from its Texinfo source.
#
#   make          # tempo.info
#   make html     # tempo.html (single page, custom CSS embedded)
#   make pdf      # tempo.pdf (requires a working TeX installation)
#   make check    # rebuild Info; fail on any texi2any warning
#   make clean
#
# The documentation coverage test lives in the tempo/tests system.

TEXI ?= tempo.texi
INFO ?= tempo.info
HTML ?= tempo.html
CSS  ?= texinfo-7.css

.PHONY: all info html pdf check clean

all: info
info: $(INFO)
html: $(HTML)

$(INFO): $(TEXI)
	texi2any --info -o $(INFO) $(TEXI)

$(HTML): $(TEXI) $(CSS)
	texi2any --html --no-split --css-include=$(CSS) -o $(HTML) $(TEXI)

pdf: tempo.pdf
tempo.pdf: $(TEXI)
	texi2any --pdf -o tempo.pdf $(TEXI)

check:
	@err=$$(mktemp); output=$$(mktemp); \
	texi2any --info -o "$$output" $(TEXI) 2>"$$err"; status=$$?; \
	if [ $$status -ne 0 ] || [ -s "$$err" ]; then \
	  cat "$$err"; rm -f "$$err" "$$output"; exit 1; \
	fi; \
	rm -f "$$err" "$$output"

clean:
	rm -f $(INFO) $(HTML) tempo.pdf
