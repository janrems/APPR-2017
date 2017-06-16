library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Odvisnost socialne ogroženosti od različnih faktorjev"),
  
  withMathJax(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Izberi delež ogroženih pred ali po socialnih transferjih", 
                  choices = c("Pred", "Po")),
      
      selectInput("var2", "Izberi faktor odvisnosti",
                  choices = c("Ginijev koeficient", "Indeks BDP na prebivalca", "Delež BDP-ja namenjen socialnim transferjem"))
    ),
               mainPanel(h3("Graf"),
                         plotOutput("Graf"),
                         h3("Odstopanje od regresijske funcije"),
                         textOutput("tabela"))
    
    )
))
