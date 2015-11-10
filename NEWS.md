# NEWS for package 'gmkin'

## gmkin 0.6-2 (2015-11-10)

- Make it possible to export model predictions as CSV

- Bugfix on windows: Start a windows graphics device for plotting during the fit if none is active
  to avoid an error

- Only tell the user that the fit can be aborted in the R console when gmkin is running in an interactive
  R session

## gmkin 0.6-1 (2015-11-10)

- Completely rewritten user interface with a three column layout 

- Introduce project management with the possibility to move datasets and models

- Tabbed viewing area on the right, with workflow graph, data editor, the manual and changes in gmkin (this file)

- Introduction of a workspace object as an R6 class 'gmkinws'

- Simplify model specification by using a combobox with multiple selection for specifiying target variables (metabolites, target compartments)

- Introduce a model gallery (loaded when the tab is selected the first time, as it loads slowly)

## gmkin 0.5-10 (2015-05-08)

### Minor changes

- Updates to DESCRIPTION and README.md

- man/gmkin.Rd: Do not mark gmkin as being experimental any more

## gmkin 0.5-8 (2014-11-25)

### Minor changes

- Adapt fit configuration to the fact that the default optimisation algorithm in mkin has been changed to `Port`

## gmkin 0.5-7

### New features

- Installation is further simplified, as both gmkin and gWidgetsWWW2 are now available from R-Forge.

## gmkin 0.5-6

### New features

- The plot of the current fit and the residuals can be saved as pdf or png file, on windows also as a windows metafile (wmf)

## gmkin 0.5-5

### Bug fixes

- Prevent loading old gmkin workspace files created with mkin < 0.9-32 as they do not load properly

- Fix saving summary files. They were all saved under the same name without a warning

## gmkin 0.5-4

### Bug fixes

- A heading in the manual was fixed

## gmkin 0.5-3

### New features

- First version installable from the package repository on r-forge

- New vignette gmkin_manual

- Small improvements in the model editor

## gmkin 0.5-2

### New features

- Make `Port` and `SANN` optimisation algorithms from FME available to gmkin, in addition to the default algorithm `Marq`

- Make it possible to specify the maximum number of iterations for these algorithms

- Provide the possibility to save summaries as text file

- Option to show the fitting process in a separate plot device, default is not to

- Add an inital message explaining the use of the target input box of the model editor 

### Bug fixes

- Sorting in the fit table now works correctly also for more than 9 fits

- The fit table was not always updated (e.g. after deleting a fit) due to a bug introduced while fixing the bug above

### Minor changes

- The statusbar message was improved

- This NEWS file was added, in markdown format for viewing on github

## gmkin 0.5-1

- This is the gmkin version presented at SETAC in Basel in May 2014
