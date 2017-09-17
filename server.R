#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(ggmap)
# Define server logic required to draw a histogram
library(dplyr)
library(sp)
shinyServer(function(input, output, session) {
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/

  
  hourcount <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    
    df <- carolina_w_cord %>%
      filter(hour == as.integer(input$hour) & month == input$month)%>%
      group_by(ADDR_PCT_CD) %>%
      summarise(count=n())
    shapemerge <- merge(shapefile_df,df,by.x ='intgroup', by.y = 'ADDR_PCT_CD',sort=FALSE)
    shapemerge
  
  })
  
  
  output$chart <- renderPlot({
        ggmap(nymap)+
          geom_polygon(data = hourcount(), 
                       aes(x = long, y = lat, fill= count,group = group),
                       alpha=0.6, colour = "black", size = .2)+
          scale_fill_gradient(low = "yellow", high = "blue", na.value=NA) +
          
          labs(x="", y="", title="New York City Precincts")+ #labels
          theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
                axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
                plot.title = element_text(lineheight=.8, face="bold", vjust=1))+ # make title bold and add space
          
          coord_equal(ratio=1) 
        
        
        
     })
  data_extra <- reactive({
    for(i in 1:77){
      if(point.in.polygon(point.x = input$plot_click[['x']],
                          point.y=input$plot_click[['y']],
                          pol.x = precinctsList[[i]]['long']$long,
                          pol.y = precinctsList[[i]]['lat']$lat)==1){
        
        data <- carolina_w_cord %>%
          filter(hour == as.integer(input$hour) & month == input$month &ADDR_PCT_CD == Lst[i])

        
      }
    }
      

    data
  })
  
  output$plot_clickinfo <- renderPrint({
    x <- point.in.polygon(point.x = input$plot_click[['x']],
                     point.y=input$plot_click[['y']],
                     pol.x = precinctsList[[77]]['long']$long,
                     pol.y = precinctsList[[77]]['lat']$lat)
   
    cat("Click:\n")
    str(input$plot_click[['x']])
    str(input$plot_click[['y']])
    str(data_extra()$LAW_CAT_CD)
  })
  
  output$hist <-
    
    renderPlot(
    # law_cat_cd
      
      try(ggplot(data=data_extra(), aes(LAW_CAT_CD))
          + geom_histogram(stat = "count") + ggtitle('Crime Counts by Category')+ scale_color_hue(l=40, c=35) )
    
   
    
  )
    output$hist1 <- renderPlot(
      # law_cat_cd
      
        try(ggplot(data=data_extra(), aes(HADEVELOPT))
            + geom_histogram(stat = "count") + ggtitle('Location by Category') + scale_color_hue(l=40, c=35))
    )
      
      
      
      

    
  
  
})