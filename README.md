# Data_Analysis

### Files 
The application is based on Shiny Library, and thus composed of a "ui" (graphical interpreter) and a "server"(computations) part.
This application has been designed to analyse results of electrophysiological recordings.
Files to analyse must be in .csv formats, organized in the following way:
- Each files represents recordings at a specific time
- the first row contains the variable names
- the following rows represents different cell-recordings
- the  columns contains the values of the different variables recorded

The population_Classes.csv file must be organized in the following way:
- The first column represents the different cell-recordings (same as in File to analyses)
- the next columns represent the different factors for each recordings (Species, Firing-type...)

The Data_files, the Source_functions.R  and the application code do not have to be in the same directory.

### Launching the application
To launch the app, click on "Run App " button on the top right corner of R script.
You are invited to upload the Source_functions.R file, the population_classes.csv file as well as the number of factors possible (default = 2)
You then select which time point you want to analyse :5ms, 10ms, 25...500ms, and you then have to select the corresponding files, then click in "Proceed" button to launch computations.
Once launched, the application will automatically check if you have the required libraries, and if not, it will download and install them.

To start data analysis, go into the "Data Analysis panel"
You can then select the factor analysis, the variable and the variable to analyse.

###Over Time Study
The first sub-panel ("Over Time") presents the evolution of the selected variable, according to the factor of interest, along the time you selected earlier. You can modify plot parameters by clicking on "Modify plot parameters" button (dot size, log scale, Standard deviation...)
You can save the plot by clicking on the "Save plot" button
The following tables represent the values of t-test and Standard Deviations and Mean computations

###Single Time Point Study
To study a specific time point (among those you selected at the beginning), you can click on the "single time point " sub-panel.
In the first tab "Stats", you will have a table representing the results of parametric test performed to know which test must be applied to compare variable means among groups.

The corresponding results per Variable can be found in the "Plots" tab.
This tab will present you a first table with the number of observation per category.
Then you will find a first boxplot for the selected variable. If statistical difference between the categories' mean is detected, corresponding marks will be displayed.
Plot bellow represents the same data but in an interactive way,  in which you can display all the data points

### Saving Parameters
When you click on a saving button (for plots, or table), you are invited to enter a directory in which to save the file. **Be careful**, on macOS, the path you enter must en by "/", in windows it must end in \\\.
You then have to enter a name and click on the save button

