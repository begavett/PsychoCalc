# The MIT License (MIT)
# 
# Copyright (c) 2014 Brandon Gavett
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#   
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

library(shiny)

shinyUI(fluidPage(
  # Application Title
  title= "PsychoCalc - the Psychometrics Calculator",
  fluidRow(
    column(8,
           h2("PsychoCalc - the Psychometrics Calculator")),
    column(4,
           img(src = "http://www.uccs.edu/Images/brand/uccs-logo.png", width=400, height=58))),
  fluidRow(HTML("See <a href = 'http://www.uccs.edu/bgavett/psychocalc.html'> http://www.uccs.edu/bgavett/psychocalc.html</a> for more details or to download standlone applications for Mac OS X, Linux, or Windows. Based on Hinton-Bayre, A. D. (2010). Deriving reliable change statistics from test–retest normative data: Comparison of models and mathematical expressions. <i>Archives of Clinical Neuropsychology</i>, <i>25</i>, 244-256. <a href = 'http://dx.doi.org/10.1093%2Farclin%2Facq008'><img src = 'http://upload.wikimedia.org/wikipedia/commons/thumb/2/25/External.svg/200px-External.svg.png', width=10, height=10</img></a>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<a href = 'mailto:bgavett@uccs.edu?Subject=PsychoCalc%20Bug%20Report'>Submit Bug Report</a>")),
  tags$hr(),
  fluidRow(
    column(5,
           tabsetPanel(
             id = "tabs1",
             tabPanel("Classification Accuracy",
                      h4("Classification Accuracy Statistics"),
                      tabsetPanel(
                        id = "tabs2",
                        tabPanel("Frequencies",
                                 numericInput("TP", "True Positives (n):",0,min=0,step=.001),
                                 numericInput("FP", "False Positives (n):",0,min=0,step=.001),
                                 numericInput("FN", "False Negatives (n):",0,min=0,step=.001),
                                 numericInput("TN", "True Negatives (n):",0,min=0,step=.001),
                                 numericInput("BR1", "Base Rate (%)",0,min=0,max=100,step=.001),
                                 submitButton("Calculate")
                        ),
                        tabPanel("Rate Stats",
                                 numericInput("Sn", "Sensitivity:",0,min=0,max=1,step=.001),
                                 numericInput("Sp", "Specificity:",0,min=0,max=1,step=.001),
                                 numericInput("BR2", "Base Rate (%)",0,min=0,max=100,step=.001),
                                 submitButton("Calculate")
                        ))
             ),
             tabPanel("Ability Estimates",
                      h4("Estimate Ability Levels Based on Observed Scores"),
                      numericInput("CI1", "Confidence Interval (%):",95,min=0,max=100),
                      numericInput("OS", "Observed Score:",0,step=.001),
                      numericInput("SM", "Score Mean:",0,step=.001),
                      numericInput("SSD", "Score SD:",0,min=0,step=.001),
                      numericInput("ICR", "Internal Consistency Reliability:",0,min=0,step=.001),
                      submitButton("Calculate")
             ),
             tabPanel("Reliable Change",
                      h4("Reliable Change Index"),
                      selectInput("RCI","Reliable Change Model:",
                                  choices=list("Jacobson & Truax (1991)" = "JT",
                                               "Speer (1992)" = "Speer",
                                               "Chelune et al. (1993)" = "Chelune",
                                               "McSweeny et al. (1993)" = "McSweeny",
                                               "Charter (1996)" = "Charter",
                                               "Crawford & Howell (1998)" = "CH",
                                               "Temkin et al. (1999)" = "Temkin",
                                               "Iverson et al. (2003)" = "Iverson",
                                               "Maassen et al. (2006)" = "Maassen"),multiple=FALSE,
                                  selected = "CH"),
                      numericInput("CI2", "Confidence Interval (%):",95,min=0,max=100),
                      numericInput("EITS", "Examinee's Initial Test Score:",0,step=.001),
                      numericInput("CGITM", "Control Group Initial Test Mean:",0,step=.001),
                      numericInput("CGITS", "Control Group Initial Test SD:",0,step=.001),
                      numericInput("CGRM", "Control Group Retest Mean:",0,step=.001),
                      numericInput("CGRS", "Control Group Retest SD:",0,step=.001),
                      numericInput("TRR", "Test-Retest Reliability:",0,min=0,max=1,step=.001),
                      numericInput("CGSS", "Control Sample Size (N):",0,min=0,step=.001),
                      numericInput("OT2S", "Observed Time 2 Score:",0,step=.001),
                      submitButton("Calculate")
             )
           )
    ),
    column(5,
      uiOutput("disp1"),
      uiOutput("disp2"),
      uiOutput("disp3"),
      uiOutput("disp4")
    )
)))