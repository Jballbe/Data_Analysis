# Data_Analysis (to be updated)

The application is based on Shiny Library, and thus composed of a "ui" and a "server" part.
Before lauchingg the app, make sure the files to analyse, and the "Source function.R" file are in the same folder than "Data_analysis_app.R"

To lauch the app, click on "Run App " button on the top right corner of R script.

Once lauched, tha application will automatically check if you have the required libraries, and if not, it will download and install them.
Therefore, the first time you run the app, it may take some time before the app Interface appears.

To launch the computation, select "Source_functions.R", and the files you want to analyze.
The data_file and Population_file must be in csv format, with factor of analysis (Species, Firing_type...) in the first rows

When the files will be uploaded, the app will automatically lauch computation and display 2 tables in the tab "General information"
The first one contains results of variance homogeneity test, normal distribution test, the variance test to be perfomed, and the general results of the variance test for each variable.
The second table shows the number of data points for each variable.

In the tab "Current Variable" you will find:
A table containing the number of data points per category for the observed variable
A box plot, static, containing if applicable the significance level for variance test
An interactive boxplot, in which you can zoom in/out, display the value of some metrics (mean, quartiles...). If you check the button "Display points in plotly" on the left panel, the data points will be displayed over the boxplot

If you want to save plots:
you can select the plots to save, either "All", "Some", "Current one". The plot you select, will be done for the factor currently selected.
If you select "Some", a list will appear in which you can check all the variable plots you want to save
Then, you have to enter the name of the file you want, knowing that the "name of the variable" a,d ".pdf" will be add after what you enter.
The saving will be done when you click on "Save as pdf" button
