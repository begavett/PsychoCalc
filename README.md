# Reliable Change Index Shiny app

This is an application for Shiny that can be used to calculate classification accuracy statistics, reliable change indices, and other classical test theory statistics (e.g., 95% confidence intervals for observed and estimated scores).
To run this app, the user must download and install the free R software package from http://www.r-project.org.
Once R is installed, the shiny package must be installed. In R, simply type the code below to install shiny.

```R
install.packages("shiny", dependencies = TRUE)
```

The easiest way to run the PsychoCalc Shiny app is by typing the code below into R.

```R
library(shiny)

runGitHub("PsychoCalc", "begavett")
```

See http://www.uccs.edu/bgavett/psychocalc.html for more details, or to download standalone applications for Mac OS X, Linux, and Windows.
