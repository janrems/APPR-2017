library(shiny)
source("../analiza/analiza.r")

shinyServer(function(input, output) {
  
  output$Graf <- renderPlot({
  
    kategorija <- switch(input$var1, 
                         "Pred" = 0,
                         "Po" = 1)
    
    faktor <- switch(input$var2,
                     "Ginijev koeficient" = 1,
                     "Indeks BDP na prebivalca" = 2,
                     "Delež BDP-ja namenjen socialnim transferjem" = 3)
    
    y_os <- switch(input$var1,
                   "Pred" = "Delež socialno ogorženih pred transferji",
                   "Po" = "Delež socialno ogroženih po transferjih")
    
    
    graf <- grafi[[faktor + 3*kategorija]]
    graf <- graf + xlab(input$var2) + ylab(y_os)
    
    print(graf)
                     
  })
  
  output$tabela <- renderText({
    kategorija <- switch(input$var1, 
                         "Pred" = 0,
                         "Po" = 1)
    
    faktor <- switch(input$var2,
                     "Ginijev koeficient" = 1,
                     "Indeks BDP na prebivalca" = 2,
                     "Delež BDP-ja namenjen socialnim transferjem" = 3)
    paste("Vsota kvadratov residualov je",matrika_odnosov$Residual[[faktor + 3*kategorija]]," ")
  })
})
