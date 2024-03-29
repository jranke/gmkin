# gmkin

The R package [gmkin](https://pkgdown.jrwb.de/gmkin)
provides a browser based graphical user interface (GUI) for
fitting kinetic models to chemical degradation data based on R package
[mkin](https://pkgdown.jrwb.de/mkin). The GUI is based on the
[gWidgetsWWW2](http://github.com/jverzani/gWidgetsWWW2) package developed by
John Verzani. The GUI elements are created by the JavaScript library
ExtJS which is bundled with gWidgetsWWW2.

## System requirements

For running current gmkin you need a system running a recent version of
R (4.0.x, on Linux older versions may work as well), the gWidgesWWW2 package,
the gmkin package and a web browser (Firefox and Chrome work for me) with
JavaScript enabled.

It should be possible to run gmkin on most laptop or desktop computers running
Linux, Mac OS X, Windows 7 or Windows 10. 

To view the complete set of widgets in the browser window without resizing
anything, it needs a screen space of 1366x740 pixels.

### Installing R

Please refer to [CRAN](http://cran.r-project.org) for installation instructions
and binary packages. If you are on Windows, please consult the
[FAQ for Windows](http://cran.r-project.org/bin/windows/base/rw-FAQ.html), especially
the entries
"[How do I install R for Windows?](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-install-R-for-Windows_003f)",
"[How do I run it?](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#How-do-I-run-it_003f)".

If you would like to upgrade your R installation, please refer to the
respective
[FAQ entry](http://cran.r-project.org/bin/windows/base/rw-FAQ.html#What_0027s-the-best-way-to-upgrade_003f).

### Installing gmkin and gWidgetsWWW2

Windows and Linux users can make use of my drat
package repository on github. Before installing gmkin, you should update
your R packages already installed, e.g. by starting R, and pasting the
command

```s
update.packages()
```

or similar, depending on your R installation.

For installing gmkin or upgrading to the latest released version, please
install the drat package

```s
install.packages("drat")
```

add my repository

```s
drat::addRepo("jranke")
```

and install as usual

```s
install.packages("gmkin")
```

The latest changes to gmkin are recorded in the
[NEWS](https://github.com/jranke/gmkin/blob/main/NEWS.md) file,
more details can be found in the
[commit history](https://github.com/jranke/gmkin/commits/main).

![gmkin screenshot](gmkin_screenshot.png)

## Usage

Without further preparation, you can start the GUI by starting R and pasting the following
commands into the R terminal (R console on Windows):

```s
require(gmkin)
gmkin()
```

You may also want to adapt the browser that R starts (using e.g.
`options(browser="/usr/bin/firefox")` on linux, or setting the default browser
on Windows).

## Documentation

For a complete overview of the functionality of the gmkin graphical user
interface please refer to the
[manual](https://pkgdown.jrwb.de/gmkin/articles/gmkin_manual.html)
available at the gmkin [documentation website](https://pkgdown.jrwb.de/gmkin).

In case you would like to see the documentation of the underlying mkin package, please
refer to its
[documentation website](https://pkgdown.jrwb.de/mkin).

## Status and known issues

- gmkin was developed in the hope that it will be useful. However, no warranty can be
  given that it will meet your expectations. There may be bugs, so please be
  careful, check your results for plausibility and use your own expertise to judge
  yourself.
- Starting the GUI takes some time. If the GUI does not come up after 10-15
  seconds, close your browser and start gmkin again using the command 'gmkin()'.
  Once the GUI is started, it is reasonably responsive.
- Please check the [issues](https://github.com/jranke/gmkin/issues) reported on github
- The R console starting the graphical user interface is not secured against manipulations
  from local users on multiuser systems
  (see [gWidgetsWWW2 issue](https://github.com/jverzani/gWidgetsWWW2/issues/22)).

### Notes on the gWidgetsWWW2 package

The R package gWidgetsWWW2 is not available on CRAN because it contains
path names with more then 100 characters in the JavaScript files which limits
its portability.  Also, it attaches some R objects to the search path, which is,
in its current form, not fully in line with the CRAN package policy. It is not
a widely used library for creating graphical user interfaces and is not supported
by a commercial company. However it makes it possible to create a reasonably
complex user interface by just writing R code, and is therefore used by gmkin.

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

## Simplifying the start of gmkin under Windows

You can put the commands

```s
require(gmkin)
gmkin()
while (TRUE) {
  Sys.sleep(10)
}
```

in an R script (e.g. under `C:\Users\<your username>\AppData\Roaming\gmkin_start.R`),
and create a Windows shortcut, e.g. on the Desktop, with the target
`"C:\Program Files\R\R-4.x.y\bin\R.exe" --slave -f c:\Users\<your username>\AppData\Roaming\gmkin_start.R`,
where R-4.x.y is the version of R that you would like to use with gmkin.  This
will start a non-interactive R console that runs the gmkin GUI. In the
properties dialogue of the shortcut, you can also set the working directory in
which it starts gmkin.  Thanks to Jonas Klein of the Umweltbundesamt for the
trick with using 'Sys.sleep' in a while loop and for suggesting the AppData
directory for the start script.

Caveat: The last time I tested this (13 February 2021), starting gmkin with
such a shortcut slowed down the loading of the GUI elements significantly. This
was under Windows 10 64 bit. Therefore, for Windows I would currently recommend
to start the R GUI application and start gmkin from there as described above
under 'Usage'.

## Acknowledgements

Financial support, feedback and suggestions by the German Federal Environmental
Agency ([Umweltbundesamt](http://www.umweltbundesamt.de)) in two projects in
2014 and 2015 was crucial for reaching version 0.6.3 in November 2015 and is
gratefully acknowledged. In particular, Stefan Meinecke contributed with a lot
of user feedback and suggestions for improvement in that time.

The adaptation to mkin versions > 0.9.49.6 that can do fits using a two-component
error model was supported by another project by the Umweltbundesamt in 2018/2019.
