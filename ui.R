#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)



shinyUI(fluidPage(

  
  tags$style(type="text/css",
             ".recalculating {opacity: 1.0;}"
  ),
  
  
  
  fluidRow(
    column(4, offset = 1,
           sliderInput('month', 'Month', min = 1, max = 12,
                       value = 1, step = 1, round = 0)
    ),
   
    column(4,
           sliderInput("hour", "Hour",
                       min =0, max = 23,
                       value = 0, animate = animationOptions(interval = 3000))
    )
  ),
  hr(),
  
 

  fluidRow(
    plotOutput("chart",height=500,width = 1200,
               click = "plot_click"),
    column(width = 4,offset = 1,
         plotOutput("hist"))
   
  )
   
  
))
