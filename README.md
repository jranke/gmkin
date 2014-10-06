# gmkin

The R package **gmkin** provides a browser based graphical user interface (GUI) for
fitting kinetic models to chemical degradation data based on R package
[mkin](http://github.com/jranke/mkin). The GUI is based on the 
[gWidgetsWWW2](http://github.com/jverzani/gWidgetsWWW2) package developed by
John Verzani. The GUI elements are created by the JavaScript library
ExtJS which is bundled with gWidgetsWWW2.

## Installation

For running gmkin you need a system running a recent version of R (version
3.0.0 or later should be OK), the gWidgesWWW2 package, the gmkin package and a
web browser (Firefox and Chrome work for me) with JavaScript enabled.

It should be possible to run gmkin on most laptop or desktop computers running
Linux, Mac OS X, Windows XP or Windows 7. It is frequently checked under Linux and
Windows 7.

### Notes on the gWidgetsWWW2 package

The R package gWidgetsWWW2 is not available on CRAN because it contains 
path names with more then 100 characters in the JavaScript files which limits
its portability.  Also, it attaches an R object called `app` to the global
environment in R, which is not allowed by the CRAN package policy. It is not
a widely used library for creating graphical user interfaces, is not supported 
by a commercial company and was used for gmkin simply because it makes it
possible to create a reasonably complex user interface by just writing R code.

### Installing R

Please refer to [CRAN](http://cran.r-project.org) for installation instructions
and binary packages. If you are on Windows and would like to upgrade your R 
installation, please refer to the respective [FAQ entry](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#What_0027s-the-best-way-to-upgrade_003f).

### Installing gmkin using an additional repository

Windows and Linux users running R 3.1.0 or later can make use of the gmkin
package repository on r-forge. If you would like to test gmkin just once, open
the R console and issue the commands

```s
setRepositories(addURLs = c(gmkin_repo = "http://kinfit.r-forge.r-project.org/repo"))
install.packages("gmkin")
```

This should pull the gmkin package and its dependencies, notably the
gWidgetsWWW2 package which is not available from the CRAN archive (see above).

### Keeping it current

If you would like to pull in new versions of gmkin from time to time, you could
add this repository to your startup options, e.g. by including a command like

```s
options(repos = c(CRAN = "http://cran.rstudio.com", 
                  gmkin_repo = "http://kinfit.r-forge.r-project.org/repo"))
```

to your startup options, e.g. to your `.Rprofile` file. Please consult the help
page for this, e.g. by typing

```s
?Startup
```

You can than update your packages including gmkin by using `update.packages()`,
please see its help files for details.

The latest changes to gmkin are recorded in the 
[NEWS](https://github.com/jranke/gmkin/blob/master/NEWS.md) file,
more details can be found in the 
[commit history](https://github.com/jranke/gmkin/commits/master).


### Installation using the devtools package

Users of the `devtools` package can also install gWidgetsWWW2 and gmkin directly from
the respective github repositories:

```s
require(devtools)
install_github("jverzani/gWidgetsWWW2", quick = TRUE)
install_github("jranke/gmkin", quick = TRUE)
```

Installing gWidgetsWWW2 in this way yields a lot of warnings concerning overly
long path names (see Notes on gWidgetsWWW2 above).  Using `quick = TRUE` skips
docs, multiple-architecture builds, demos, and vignettes, to make installation
as fast and painless as possible.

## Usage

You start the GUI from your R terminal with latest mkin installed as shown below. 
You may also want to adapt the browser that R starts (using
`options(browser="/usr/bin/firefox")` on linux, or setting the default browser
on Windows from the browser itself). Development was done with firefox. I also
did some testing with Chrome on Windows. Chrome sometimes hung when loading
the GUI and therefore ExtJS the first time, but when the GUI is loaded it appears
to work fine.

```s
require(gmkin)
gmkin()
```

The following screenshot is taken after loading the gmkin workspace with
an analysis of FOCUS dataset Z. It has to be saved as an .RData file 
first, and can then be loaded to the GUI.

```s
save(FOCUS_2006_Z_gmkin, file = "FOCUS_2006_gmkin_Z.RData")
```

![gmkin screenshot](gmkin_screenshot.png)

## Status and known issues

- gmkin was developed in the hope that it will be useful. However, no warranty can be 
  given that it will meet your expectations. There may be bugs, so please be
  careful, check your results for plausibility and use your own expertise to judge
  yourself.
- Please check the [issues](https://github.com/jranke/gmkin/issues) reported on github
- Starting the GUI takes some time. Once it is started, it is reasonably responsive.
- The fit list was not always updated when using Firefox version 28 on Windows. This
  works with Firefox starting from version 29 and with Chrome.
