# NEWS for package 'gmkin'

## gmkin 0.6-11 (2021-02-21)

- Adapt built-in 'FOCUS_2006' workspace to mkin 1.0.x as we have now have the 'deg_func' element in 'mkinmod' objects

- The three HOWTO tests in the manual pass

## gmkin 0.6-10 (2019-07-09)

- Adapt to mkin 0.9.49.6, making the new way to fit variance by variable and the two-component error model available to gmkin

- Use devEMF for better vector graphics export for Windows

- Manual weighting is not possible as this functionality is currently not present in mkin versions > 0.9.48.1

## gmkin 0.6-9 (2018-09-13)

- Enable fits with the two-component error model during iterative reweighting (IRLS)

## gmkin 0.6-8 (2016-01-25)

- Formal improvements mainly in the docs

## gmkin 0.6-7 (2017-03-06)

### Bug fixes

- The compiled versions of the models were removed from the model gallery in order to avoid invalid pointers.

### Major changes

- A howto section was added to the manual. The howtos are also useful as functionality tests.

## gmkin 0.6-6 (2016-01-08)

### Bug fixes

- Activate the button "Keep changes" as soon as a dataset title is entered in the dataset editor so entered data can be kept.

- Entering "x" into an input field triggered saving the file due to a peculiarity of the GAction implementation in gWidgetsWWW2. The keybinding was changed to "Shift-F12" (see below).

- When data was entered manually, sometimes it could not be fitted as the override column was of type "character" instead of "numeric".

### Major changes

- Changed the keybinding for saving the current workspace to "Shift-F12"

### Minor changes

- The project file is now immediately shown with its full path in the project editor window after saving a file.

- Do not show widgets for fit options in 'Configuration' tab when not fit is configured. Also inactivate the run button and show a message telling the user what to do to configure a fit.

## gmkin 0.6-5 (2015-12-11)

### Bug fixes

- When configuring a fit using a model with use_of_ff = 'max', this was ignored when the model was loaded from a project file

- The configure button was not disabled when switching a project (which clears model and dataset selections)

## gmkin 0.6-4 (2015-12-09)

- Various small corrections of unexpected or incorrect GUI behaviour. See git commit logs for details.

### Bug fixes

- Unchecking the pathway to sink in the model editor was ignored.

## gmkin 0.6-3 (2015-11-28)

- Various small layout and GUI logic improvements based on the suggestions by Stefan Meinecke (Umweltbundesamt Germany). Thanks, also for the financial support from the Umweltbundesamt!

### Minor changes

- Removed the 'Add' and 'Remove' buttons from the data editor, as adding rows leads to a corrupt state of the GUI

- The button 'Keep changes' now asks if the currently selected model should be overwritten if the name was not changed

### Bug fixes

- In the model editor, the combobox for the 'to' field did not accept to be empty after holding a selection

- Some graphical errors in the model gallery were fixed

- Importing data in wide format corrupted the array of observed variables as `mkin_wide_to_long` returns 'name' as a factor

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
