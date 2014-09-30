# gmkin

The R package **gmkin** provides a browser based graphical user interface (GUI) for
fitting kinetic models to chemical degradation data based on R package
[mkin](http://github.com/jranke/mkin). The GUI is based on the 
[gWidgetsWWW2](http://github.com/jverzani/gWidgetsWWW2) package developed by
John Verzani. The GUI elements are created by the JavaScript library
ExtJS which is bundled with gWidgetsWWW2.

## Installation

For running gmkin you need a system running a recent version of R (version
3.0.0 or later should be OK), the gWidgesWWW2 package from github, the gmkin
package and a web browser (Firefox and Chrome work for me) with
JavaScript enabled.

It should be possible to run gmkin on most laptop or desktop computers running
Linux, Mac OS X, Windows XP or Windows 7. It is frequently checked under Linux and
Windows 7.

### Installing R

For Linux users, binary R packages should be available through the usual package repositories. 
For more information for the most common distributions, please refer to 
[CRAN](http://cran.r-project.org/bin/linux). 

[Windows](http://cran.r-project.org/bin/windows) and [Mac OS X](http://cran.r-project.org/bin/macosx)
users are also referred to the respective pages on [CRAN](http://cran.r-project.org) where
there is more background information, notably an 
[R for Windows FAQ](http://cran.r-project.org/bin/windows/base/rw-FAQ.html).

### Installing the devtools package

Installation of gWidgetsWWW2 and gmkin directly from
[github](http://github.com) is facilitated by installing the R package `devtools`
using either the Menu, or, more conveniently, using the R command

```s
install.packages("devtools")
```

### Installing gWidgetsWWW2

The gWidgetsWWW2 package provides the glue between R code defining the
graphical user interface (GUI) and the internal R help server which serves 
the GUI elements in the form of JavaScript. The JavaScript library ExtJS
is used for this, and it is bundled in the gWidgetsWWW2 package.

Therefore, the package is a bit large. It is not available on CRAN because it
contains very long path names in the JavaScript files which limits its portability.
Also, it attaches an R object called `app` to the global environment in R, which
is not allowed by the CRAN package policy.

Installation is easy using the devtools library in R, so make sure it is installed, and 
then run

```{r, eval = FALSE}
require(devtools)
install_github("gWidgetsWWW2", "jverzani", quick = TRUE)
```

Installing gWidgetsWWW2 yields a lot of warnings concerning overly long path
names.  Using `quick = TRUE` skips docs, multiple-architecture builds, demos,
and vignettes, to make installation as fast and painless as possible.

### Installing gmkin

Then you can install gmkin, also directly from github:

```s
require(devtools)
install_github("gmkin", "jranke", quick = TRUE)
```

The same command can be run if you want to update gmkin. See the 
[NEWS](https://github.com/jranke/gmkin/blob/master/NEWS.md) file 
for a list of recent changes, or the 
[commit history](https://github.com/jranke/gmkin/commits/master)
if you are interested in the details.

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
