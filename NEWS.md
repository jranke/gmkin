# CHANGES in gmkin VERSION 0.5-6

## NEW FEATURES

- The plot of the current fit and the residuals can be saved as pdf or png file, on windows also as a windows metafile (wmf)

# CHANGES in gmkin VERSION 0.5-5

## BUG FIXES

- Prevent loading old gmkin workspace files created with mkin < 0.9-32 as they do not load properly

- Fix saving summary files. They were all saved under the same name without a warning

# CHANGES in gmkin VERSION 0.5-4

## BUG FIXES

- A heading in the manual was fixed

# CHANGES in gmkin VERSION 0.5-3

## NEW FEATURES

- First version installable from the package repository on r-forge

- New vignette gmkin_manual

- Small improvements in the model editor

# CHANGES in gmkin VERSION 0.5-2

## NEW FEATURES

- Make `Port` and `SANN` optimisation algorithms from FME available to gmkin, in addition to the default algorithm `Marq`

- Make it possible to specify the maximum number of iterations for these algorithms

- Provide the possibility to save summaries as text file

- Option to show the fitting process in a separate plot device, default is not to

- Add an inital message explaining the use of the target input box of the model editor 

## BUG FIXES

- Sorting in the fit table now works correctly also for more than 9 fits

- The fit table was not always updated (e.g. after deleting a fit) due to a bug introduced while fixing the bug above

## MINOR CHANGES

- The statusbar message was improved

- This NEWS file was added, in markdown format for viewing on github

# CHANGES in gmkin VERSION 0.5-1

- This is the gmkin version presented at SETAC in Basel in May 2014
