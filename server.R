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

# Define server logic required to calculate various reliable change indices
shinyServer(function(input, output) {
  SN1 <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    return(round(sn1,4))
  })
  
  SP1 <- reactive({
    sp1 <- input$TN/(input$TN+input$FP)
    return(round(sp1,3))
  })
  
  BR1c <- reactive({
    br1c <- (input$TP+input$FN)/(input$TP+input$FN+input$FP+input$TN)
    return(round(br1c,3))
  })
  
  PPP1a <- reactive({
    ppp1a <- input$TP/(input$TP+input$FP)
    return(round(ppp1a,3))
  })
  
  NPP1a <- reactive({
    npp1a <- input$TN/(input$TN+input$FN)
    return(round(npp1a,3))
  })
  
  PPP1b <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    sp1 <- input$TN/(input$TN+input$FP)
    br1i <- input$BR1/100
    ppp1b <- (br1i*sn1)/((br1i*sn1)+((1-br1i)*(1-sp1)))
    return(round(ppp1b,3))
  })
  
  NPP1b <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    sp1 <- input$TN/(input$TN+input$FP)
    br1i <- input$BR1/100
    npp1b <- ((1-br1i)*sp1)/(((1-br1i)*sp1)+(br1i*(1-sn1)))
    return(round(npp1b,3))
  })
  
  PLR1 <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    sp1 <- input$TN/(input$TN+input$FP)
    plr1 <- sn1/(1-sp1)
    return(round(plr1,3))
  })
  
  NLR1 <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    sp1 <- input$TN/(input$TN+input$FP)
    nlr1 <- (1-sn1)/sp1
    return(round(nlr1,3))
  })
  
  OR1 <- reactive({
    sn1 <- input$TP/(input$TP+input$FN)
    sp1 <- input$TN/(input$TN+input$FP)
    plr1 <- sn1/(1-sp1)
    nlr1 <- (1-sn1)/sp1
    or1 <- plr1/nlr1
    return(round(or1,3))
  })
  
  PPP2 <- reactive({
    ppp2 <- ((input$BR2/100)*input$Sn)/(((input$BR1/100)*input$Sn)+((1-(input$BR1/100))*(1-input$Sp)))
    return(round(ppp2,3))
  })
  
  NPP2 <- reactive({
    npp2 <- ((1-(input$BR2/100))*input$Sp)/(((1-(input$BR2/100))*input$Sp)+((input$BR2/100)*(1-input$Sn)))
    return(round(npp2,3))
  })
  
  PLR2 <- reactive({
    plr2 <- input$Sn/(1-input$Sp)
    return(round(plr2,3))
  })
  
  NLR2 <- reactive({
    nlr2 <- (1-input$Sn)/input$Sp
    return(round(nlr2,3))
  })
  
  OR2 <- reactive({
    plr2 <- input$Sn/(1-input$Sp)
    nlr2 <- (1-input$Sn)/input$Sp
    or2 <- plr2/nlr2
    return(round(or2,3))
  })
  
  SEM <- reactive({
    sem <- input$SSD*sqrt(1-input$ICR)
    return(round(sem,3))
  })
  
  OSCIL <- reactive({
    sem <- input$SSD*sqrt(1-input$ICR)
    oscil <- input$OS-(sem*qnorm(1-((1-(input$CI1/100))/2)))
    return(round(oscil,3))
  })
  
  OSCIU <- reactive({
    sem <- input$SSD*sqrt(1-input$ICR)
    osciu <- input$OS+(sem*qnorm(1-((1-(input$CI1/100))/2)))
    return(round(osciu,3))
  })
  
  ETS <- reactive ({
    ets <- input$SM + (input$ICR*(input$OS-input$SM))
    return(round(ets,3))
  })
  
  SEE <- reactive({
    see <- input$SSD*sqrt((input$ICR*(1-input$ICR)))
    return(round(see,3))
  })
  
  ESCIL <- reactive({
    ets <- input$SM + (input$ICR*(input$OS-input$SM))
    see <- input$SSD*sqrt((input$ICR*(1-input$ICR)))
    escil <- ets-(see*qnorm(1-((1-(input$CI1/100))/2)))
    return(round(escil,3))
  })
  
  ESCIU <- reactive({
    ets <- input$SM + (input$ICR*(input$OS-input$SM))
    see <- input$SSD*sqrt((input$ICR*(1-input$ICR)))
    esciu <- ets+(see*qnorm(1-((1-(input$CI1/100))/2)))
    return(round(esciu,3))
  })
  
  Yprime <- reactive({
    if (input$RCI == "JT") {
      return(round(input$EITS,2))
    }
    if (input$RCI == "Speer") { 
      return(round(input$CGRM + input$TRR*(input$EITS - input$CGITM),2))
    }
    if (input$RCI == "Chelune") {
      return(round(input$EITS + (input$CGRM - input$CGITM),2))
    }
    if (input$RCI == "McSweeny") {
      b <- input$TRR * (input$CGRS/input$CGITS)
      a <- input$CGRM - b*input$CGITM
      return(round(b*input$EITS + a,2))
    }
    if (input$RCI == "Charter") {
      return(round(input$CGRM + input$TRR * (input$EITS - input$CGITM),2))
    }
    if (input$RCI == "CH") {
      b <- input$TRR * (input$CGRS/input$CGITS)
      a <- input$CGRM - b*input$CGITM
      return(round(b*input$EITS + a,2))
    }
    if (input$RCI == "Temkin") {
      return(round(input$EITS + (input$CGRM - input$CGITM),2))
    }
    if (input$RCI == "Iverson") {
      return(round(input$EITS + (input$CGRM - input$CGITM),2))
    }
    if (input$RCI == "Maassen") {
      badj <- input$CGRS/input$CGITS
      aadj <- input$CGRM - badj*input$CGITM
      return(round(badj*input$EITS + aadj,2))
    }})
  
  
  SE <- reactive({
    if (input$RCI == "JT") {
      return(round(sqrt(2*input$CGITS^2*(1-input$TRR)),2))
    }
    if (input$RCI == "Speer") { 
      return(round(sqrt(2*input$CGITS^2*(1-input$TRR)),2))
    }
    if (input$RCI == "Chelune") {
      return(round(sqrt(2*input$CGITS^2*(1-input$TRR)),2))
    }
    if (input$RCI == "McSweeny") {
      return(round(input$CGRS*sqrt(1-input$TRR^2),2))
    }
    if (input$RCI == "Charter") {
      return(round(input$CGRS*sqrt(1-input$TRR^2),2))
    }
    if (input$RCI == "CH") {
      SE4 <- input$CGRS*sqrt(1-input$TRR^2)
      return(round(SE4*sqrt(1+(1/input$CGSS)+(((input$EITS-input$CGITM)^2)/((input$CGITS^2)*(input$CGSS-1)))),2))
    }
    if (input$RCI == "Temkin") {
      return(round(sqrt(input$CGITS^2 + input$CGRS^2 - 2*input$CGITS*input$CGRS*input$TRR),2))
    }
    if (input$RCI == "Iverson") {
      return(round(sqrt((input$CGITS^2 + input$CGRS^2)*(1-input$TRR)),2))
    }
    if (input$RCI == "Maassen") {
      return(round(sqrt((input$CGITS^2 + input$CGRS^2)*(1-input$TRR)),2))
    }
  })
  LowerCI <- reactive({round(Yprime()+(qnorm((100-input$CI2)/200)*SE()),2)})
  UpperCI <- reactive({round(Yprime()+(qnorm((100-input$CI2)/200,lower.tail=FALSE)*SE()),2)})
  zscore <- reactive({round((input$OT2S - Yprime())/SE(),2)})
  
  output$Table1 <- renderTable(data.frame(Value = c(SN1(), SP1(), BR1c(), PPP1a(), NPP1a(), PPP1b(), 
                                            NPP1b(), PLR1(), NLR1(), OR1()), row.names = 
                                            c("Sensitivity", "Specificity", "Base Rate (calculated)", 
                                             "PPP (based on frequencies)", "NPP (based on frequencies)",
                                             "PPP (based on user-entered base rate)",
                                             "NPP (based on user-entered base rate)",
                                             "Positive Likelihood Ratio", "Negative Likelihood Ratio",
                                             "Odds Ratio")),digits=3)
  output$Table2 <- renderTable(data.frame(Value = c(as.numeric(input$Sn), as.numeric(input$Sp),
                                                    as.numeric(input$BR2/100), PPP2(), NPP2(), 
                                                    PLR2(), NLR2(), OR2()), row.names = 
                                            c("Sensitivity", "Specificity", "Base Rate", 
                                              "Positive Predictive Power", "Negative Predictive Power", 
                                              "Positive Likelihood Ratio", "Negative Likelihood Ratio",
                                              "Odds Ratio")),digits=3)
  output$Table3 <- renderTable(data.frame(Value = c(SEM(), paste0(OSCIL(), " - ", OSCIU()), ETS(), SEE(), 
                                                    paste0(ESCIL(), " - ", ESCIU())), row.names = 
                                            c("Standard Error of Measurement", 
                                              paste0("Observed Score ", as.numeric(input$CI1), "% Confidence Interval"), 
                                              "Estimated True Score", "Standard Error of the Estimate",
                                              paste0("Estimated True Score ", as.numeric(input$CI1), "% Confidence Interval"))),digits=3)
  output$Table4 <- renderTable(data.frame(Value = c(Yprime(), SE(), 
                                                    paste0(LowerCI(), " - ", UpperCI()), 
                                                    zscore()), row.names = 
                                            c("Predicted Time 2 Score", 
                                              "Standard Error",
                                              paste0("Predicted Score ", as.numeric(input$CI2), "% Confidence Interval"),
                                              "z-score")),digits=3)
  output$plot <- renderPlot({
    plot(input$OT2S, 1, type = "p", xlab = "Score", axes = FALSE, ylab = "", 
         pch = 16, xlim = c(input$CGITM-3*input$CGITS, input$CGITM+3*input$CGITS),
         ylim=c(.5, 1.5))
    axis(1)
    points(Yprime(), 1, pch = 17, col = "red")
    arrows(Yprime(), 1, LowerCI(), 1, col = "red", angle = 90)
    arrows(Yprime(), 1, UpperCI(), 1, col = "red", angle = 90)
    legend("top", pch = c(16, 17), col = c("black","red"), 
           c("Observed Time 2 Score", "Predicted Time 2 Score"))
  })
  
  output$disp1 <- renderUI({
      conditionalPanel(
        condition = "input.tabs1=='Classification Accuracy' && input.tabs2=='Frequencies'",
        tagList(
          h4("Classification Accuracy"),
          br(),
          tableOutput("Table1")
          )
        )
  })
    output$disp2 <- renderUI({
      conditionalPanel(
        condition = "input.tabs1=='Classification Accuracy' && input.tabs2=='Rate Stats'",
        tagList(
          h4("Classification Accuracy"),
          br(),
          tableOutput("Table2")
          ))
    })
  output$disp3 <- renderUI({
      conditionalPanel(
      condition = "input.tabs1=='Ability Estimates'",
      tagList(
        h4("Ability Estimates"),
        br(),
        tableOutput("Table3")
        ))
  })
  output$disp4 <- renderUI({
    conditionalPanel(
      condition = "input.tabs1=='Reliable Change'",
      tagList(
        h4("Reliable Change Values"),
        br(),
        tableOutput("Table4"),
        hr(),
        plotOutput("plot")))
  })
})