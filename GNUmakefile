PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGDIR  := $(PWD)
TGZ     := $(PKGNAME)_$(PKGVERS).tar.gz
WINBIN  := $(PKGNAME)_$(PKGVERS).zip

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN ?= $(shell dirname "`which R`")

# Specify static documentation directories for subversion on r-forge
RFSVN ?= $(HOME)/svn/r-forge/kinfit
RFDIR ?= $(RFSVN)/pkg/gmkin
SDDIR ?= $(RFSVN)/www/gmkin_static

pkgfiles = NEWS.md \
	.Rbuildignore \
	data/* \
	DESCRIPTION \
	inst/GUI/* \
	inst/GUI/png* \
	man/* \
	NAMESPACE \
	R/* \
	man/* \
	README.html \
	vignettes/gmkin_manual.html

all: check clean

$(TGZ): $(pkgfiles)
	"$(RBIN)/R" CMD build .

roxygen: 
	"$(RBIN)/Rscript" -e 'devtools::document()'

build: roxygen $(TGZ)

$(WINBIN): build
	@echo "Building windows binary package..."
	"$(RBIN)/R" CMD INSTALL $(TGZ) --build
	@echo "DONE."

winbin: $(WINBIN)

install: build
	"$(RBIN)/R" CMD INSTALL $(TGZ)

check: build
	"$(RBIN)/R" CMD check --no-tests $(TGZ)

README.html: README.md
	"$(RBIN)/Rscript" -e "rmarkdown::render('README.md', output_format = 'html_document', output_options = list(self_contained = TRUE))"

vignettes/gmkin_manual.html: vignettes/gmkin_manual.Rmd vignettes/img/*
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/gmkin_manual.Rmd', dir = 'vignettes')"

manual: vignettes/gmkin_manual.html

vignettes: vignettes/gmkin_manual.html

pd:
	"$(RBIN)/Rscript" -e "pkgdown::build_site()"
	git add -A
	git commit -m 'Static documentation rebuilt by pkgdown' -e

drat: build
	"$(RBIN)/Rscript" -e "drat::insertPackage('$(TGZ)', commit = TRUE)"

dratwin: winbin
	"$(RBIN)/Rscript" -e "drat::insertPackage('$(WINBIN)', '~/git/drat/', commit = TRUE)"

r-forge:
	git archive main > $(PKGDIR)/gmkin.tar;\
	cd $(RFDIR) && rm -r `ls` && tar -xf $(PKGDIR)/gmkin.tar;\
	rm -r $(SDDIR)/*;\
	cp -a docs/* $(SDDIR);\
	svn add --force .; svn rm --force `svn status | grep "\!" | cut -d " " -f 8`; cd $(RFSVN) && svn commit -m 'sync with git'

clean: 
	$(RM) -r $(PKGNAME).Rcheck/
	$(RM) vignettes/*.R
