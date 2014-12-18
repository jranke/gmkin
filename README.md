# gmkin

The R package [gmkin](http://kinfit.r-forge.r-project.org/gmkin_static) 
provides a browser based graphical user interface (GUI) for
fitting kinetic models to chemical degradation data based on R package
[mkin](http://github.com/jranke/mkin). The GUI is based on the 
[gWidgetsWWW2](http://github.com/jverzani/gWidgetsWWW2) package developed by
John Verzani. The GUI elements are created by the JavaScript library
ExtJS which is bundled with gWidgetsWWW2.

## Installation

For running gmkin you need a system running a recent version of R (version
3.1.0 or later should be OK), the gWidgesWWW2 package, the gmkin package and a
web browser (Firefox and Chrome work for me) with JavaScript enabled.

It should be possible to run gmkin on most laptop or desktop computers running
Linux, Mac OS X, Windows XP or Windows 7. It is frequently checked under Linux and
Windows 7.

To view the complete set of widgets in the browser window without resizing
anything, it needs a resolution of 1380x900 pixels.

### Notes on the gWidgetsWWW2 package

The R package gWidgetsWWW2 is not available on CRAN because it contains 
path names with more then 100 characters in the JavaScript files which limits
its portability.  Also, it attaches some R objects to the search path, which is, 
in its current form, not fully in line with the CRAN package policy. It is not
a widely used library for creating graphical user interfaces, is not supported 
by a commercial company and was used for gmkin simply because it makes it
possible to create a reasonably complex user interface by just writing R code.

### Installing R

Please refer to [CRAN](http://cran.r-project.org) for installation instructions
and binary packages. If you are on Windows, please consult the 
[FAQ for Windows](http://cran.r-project.org/bin/windows/base/rw-FAQ.html), especially
the entries 
"[How do I install R for Windows?](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-install-R-for-Windows_003f)", 
"[How do I run it?](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-run-it_003f)",
and 
"[How can I keep workspaces for different projects in different directories?](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-can-I-keep-workspaces-for-different-projects-in-different-directories_003f)".

If you would like to upgrade your R installation, please refer to the
respective 
[FAQ entry](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#What_0027s-the-best-way-to-upgrade_003f).

### Installing gmkin from R-Forge

Windows and Linux users running R 3.1.0 or later can make use of the 
package repository on R-Forge. For installing or upgrading to the latest released
version you can use the command

```s
install.packages("gmkin", repos = c("http://r-forge.r-project.org", getOption("repos")))
```

If you have not set your CRAN mirror yet, you may have to select one from the list that 
appears. 

The above command temporarily adds the R-Forge repository to your package
sources.
It should pull the gmkin package and its dependencies, notably the
gWidgetsWWW2 package which is not available from the CRAN archive (see above).
Mac users that have the necessary development files installed can probably 
install from the source files in this repository (not tested).

In a previous version of this README I have described how to permanently add 
the R-Forge repository to your options. However, I noticed this has unwanted
side effects, so I do not recommend it any longer. Therefore, you may want
to revert such changes to your R startup options.

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

You can also put these two commands into an `.Rprofile` file in the working directory
where you start R. For some reason, the `utils` package also needs to be loaded when 
you do this from an .Rprofile file. For your convenience, you can find such a
file [here](Rprofile?raw=true).  On Windows, you can save this file to the
directory where you would like to start gmkin
by right clicking on the link to this file, choose `save target as` or similar,
and choose `.Rprofile.` as the name. This will lead to the creation of a file
named `.Rprofile` which will be executed when you start R within this directory.

The following screenshot is taken after loading the gmkin workspace with
an analysis of FOCUS dataset Z. It has to be saved in R as an .RData file
first, and can then be loaded to the GUI.

```s
save(FOCUS_2006_Z_gmkin, file = "FOCUS_2006_gmkin_Z.RData")
```

![gmkin screenshot](gmkin_screenshot.png)

For a complete overview of the functionality of the gmkin graphical user
interface please refer to the 
[manual](http://kinfit.r-forge.r-project.org/gmkin_static/vignettes/gmkin_manual.html)
available at the gmkin [documentation website](http://kinfit.r-forge.r-project.org/gmkin_static).

## Status and known issues

- gmkin was developed in the hope that it will be useful. However, no warranty can be 
  given that it will meet your expectations. There may be bugs, so please be
  careful, check your results for plausibility and use your own expertise to judge
  yourself.
- Please check the [issues](https://github.com/jranke/gmkin/issues) reported on github
- The R console starting the graphical user interface is not secured against manipulations
  from local users on multiuser systems 
  (see [gWidgetsWWW2 issue](https://github.com/jverzani/gWidgetsWWW2/issues/22)).
- Starting the GUI takes some time. Once it is started, it is reasonably responsive.
- The fit list was not always updated when using Firefox version 28 on Windows. This
  works with Firefox starting from version 29 and with Chrome.
