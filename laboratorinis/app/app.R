#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(readr)
duomenys = read_csv("https://raw.githubusercontent.com/JauniusI/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")
duomenys = duomenys %>% filter(ecoActCode == 702200)
duomenys$month = as.Date(paste0(as.character(duomenys$month), '01'), format = "%Y%m%d")
# Define UI

ui <- fluidPage(theme = bs_theme(bootswatch = "sandstone"),
                navbarPage(
                
                  "Konsultacine verslo ir kito valdymo veikla",
                  tabPanel("Pirmas puslapis",
                           sidebarPanel(
                             textInput("txt1", "Iveskite kompanijos koda"),
                             
                             
                           ),
                           mainPanel(
                             h1("Vidutinio atlyginimo grafikas : "),
                             
                             plotOutput("distPlot"),
                             
                           )
                           
                  ),
                  tabPanel("Antras puslapis",
                           sidebarPanel(
                             selectizeInput(inputId = "imone", label = "Pasirinkite kompanijos pavadinima",choices = unique(duomenys$name)),
                             
                           ),
                           mainPanel(
                             h1("Vidutinio atlyginimo grafikas : "),
                             
                             plotOutput("distPlot2"),
                             
                           )
                           
                  ),
                  
                  
                ) 
)
              

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  
  output$distPlot <- renderPlot({
    duomenys %>%
      filter(code == as.numeric(input$txt1)) %>%
      ggplot(aes(x=month,y=avgWage))+
      labs(x="Month", y= "Average wage") +
      geom_line(color = "black") +
      theme_bw()
  })
  
  output$distPlot2 <- renderPlot({
    updateSelectInput(session,"kompanija",choices = unique(duomenys$name))
    duomenys %>%
      filter(name== input$imone) %>%
      ggplot(aes(x=month,y=avgWage))+
      labs(x="Month", y= "Average wage") +
      geom_line(color = "black") +
      theme_bw()
      
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
