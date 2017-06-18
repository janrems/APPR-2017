

shinyServer(function(input, output) {
  
  output$Graf <- renderPlot({
  
    kategorija <- switch(input$var1, 
                         "Pred" = 0,
                         "Po" = 1)
    
    faktor <- switch(input$var2,
                     "Koeficient" = 1,
                     "Indeks" = 2,
                     "Delez_bdp" = 3)
    
    y_os <- switch(input$var1,
                   "Pred" = "Delež socialno ogorženih pred transferji",
                   "Po" = "Delež socialno ogroženih po transferjih")
    
    x_os <- switch(input$var2,
                   "Koeficient" = "Ginijev koeficient",
                   "Indeks" = "Indeks BDP na prebivalca",
                   "Delez_bdp" = "Delež BDP-ja namenjen socialnim transferjem")
    
    graf <- grafi[[faktor + 3*kategorija]]
    graf <- graf + xlab(x_os) + ylab(y_os)
    
    print(graf)
                     
  })
  
  output$tabela <- renderText({
    
    
    paste("Vsota kvadratov residualov je",
          matrika_odnosov %>% filter(`Delez ogrozenih` == input$var1,
                                     Spremenljivka == input$var2) %>% .$Residual, " ")
  })
  
  output$kor <- renderText({
    kategorija <- switch(input$var1, 
                         "Pred" = 0,
                         "Po" = 1)
    
    faktor <- switch(input$var2,
                     "Koeficient" = 1,
                     "Indeks" = 2,
                     "Delez_bdp" = 3)
    
    paste("Korelacijski koeficient spremenljivk je ",round(korelacija[[faktor + 3*kategorija]],3)," ")
  })
})
