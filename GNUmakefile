PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename $(PWD))
TGZ     := ../$(PKGSRC)_$(PKGVERS).tar.gz
TGZVNR  := ../$(PKGSRC)_$(PKGVERS)-vignettes-not-rebuilt.tar.gz

# Specify the directory holding R binaries. To use an alternate R build (say a
# pre-prelease version) use `make RBIN=/path/to/other/R/` or `export RBIN=...`
# If no alternate bin folder is specified, the default is to use the folder
# containing the first instance of R on the PATH.
RBIN ?= $(shell dirname "`which R`")
#
# Specify static documentation directories for subversion on r-forge
RFSVN ?= $(HOME)/svn/kinfit.r-forge
RFDIR ?= $(RFSVN)/pkg/gmkin
SDDIR ?= $(RFSVN)/www/gmkin_static

pkgfiles = NEWS.md \
	data/* \
	DESCRIPTION \
	inst/GUI/gmkin.R \
	man/* \
	NAMESPACE \
	R/* \
	README.html \
	TODO \
	vignettes/gmkin_manual.html

all: check clean

$(TGZ): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC)

$(TGZVNR): $(pkgfiles)
	cd ..;\
		"$(RBIN)/R" CMD build $(PKGSRC) --no-build-vignettes;\
		cd $(PKGSRC);\
	mv $(TGZ) $(TGZVNR)
                
build: $(TGZ)

build-no-vignettes: $(TGZVNR)

install: build
	"$(RBIN)/R" CMD INSTALL $(TGZ)

install-no-vignettes: build-no-vignettes
	"$(RBIN)/R" CMD INSTALL $(TGZVNR)

check: build
	# Vignettes have been rebuilt by the build target
	"$(RBIN)/R" CMD check --no-tests --no-build-vignettes $(TGZ)

check-no-vignettes: build-no-vignettes
	mv $(TGZVNR) $(TGZ)
	"$(RBIN)/R" CMD check --no-tests $(TGZ)
	mv $(TGZ) $(TGZVNR)

README.html: README.md
	"$(RBIN)/Rscript" -e "rmarkdown::render('README.md', output_format = 'html_document')"

vignettes/gmkin_manual.html: vignettes/gmkin_manual.Rmd
	"$(RBIN)/Rscript" -e "tools::buildVignette(file = 'vignettes/gmkin_manual.Rmd', dir = 'vignettes')"

vignettes: vignettes/gmkin_manual.html

sd:
	rm -rf $(SDDIR)/*
	cp gmkin_screenshot.png Rprofile $(SDDIR)
	"$(RBIN)/Rscript" -e "library(staticdocs); build_site(site_path = '$(SDDIR)')"
	cd $(SDDIR) && svn add --force .
	git add -A
	git commit -m 'Vignettes rebuilt by staticdocs::build_site() for static documentation on r-forge' -e

r-forge: sd
	git archive master > $(HOME)/gmkin.tar;\
	cd $(RFDIR) && rm -r `ls` && tar -xf $(HOME)/gmkin.tar;\
	svn add --force .; cd $(RFSVN) && svn commit -m 'update gmkin from github repository'

clean: 
	$(RM) -r $(PKGNAME).Rcheck/
	$(RM) vignettes/*.R
