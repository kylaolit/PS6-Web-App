library(shiny)
library(tidyverse)
library(stringr)
library(readr)
library(plotly)
library(dplyr)

t <- read_delim("species_data.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

  titlePanel("Threatened Species in the Past Years"),
  
  tabsetPanel(
    tabPanel("About",
             mainPanel(
               em("This app exhibits all data of threatened animals from ", strong("IUCN"), "in the recent years.."),
               br(),
               p("There are a total of ", n_distinct(t$Year), "years of data for threatened animals."),
               p("Here is a small (random) sample of data:"),
               tableOutput('table')
             )
    ),
  
    tabPanel("Plots",
           sidebarLayout(
             sidebarPanel(
               p("Here you can analyze different kinds of threatened species. 
          Select the specie, and you will see a plot."),
               selectInput("choose",
                           "Choose a specie",
                           choices = c("Mammals","Birds","Reptiles","Amphibians","Fishes",
                                       "Subtotal_Vertebrates","Insects","Molluscs","Crustaceans","Corals","Arachnids","Velvet_worms",
                                       "Horseshoe_crabs","Other_invertebrates","Subtotal_Invertebrates","Mosses","Ferns_and_allies",
                                       "Gymnosperms","Flowering_plants","Green_algae","Red_algae","Subtotal_Plants","Lichens",
                                       "Mushrooms","Brown_algae","Subtotal_Fungi_and_protists","Total")),
               selectInput("palette", "Select a colors:",
                           choices = c("1" = "Dark2",
                                       "2" = "Set1", "3" = "Pastel1")),
             ),
             mainPanel(
               plotOutput('plots'),
               textOutput('message')
             )
           )
  ),
  
  tabPanel(
    "Tables",
    sidebarLayout(
      sidebarPanel(
        p("his tab displays the data of threatened species 
          over different groups of organisms from the options."),
        radioButtons("choice","Choose an option:",
                     c("Vertebrates" = "v",
                       "Invertebrates" = "i",
                       "Plants" = "p",
                       "Fungi & Protists" = "f")
        )
      ), 
      mainPanel(
        textOutput("message2"),
        tableOutput("table2")
        
      )
    )
  )
)
)
# Define server logic required to draw a histogram
server <- function(input, output) {

  output$table <- renderTable({
    t %>% 
      filter(Year != "") %>% 
      sample_n(5)
  })
  
  output$plots <- renderPlot({
    k <- input$choose
    p <- input$palette
    
    ggplot(data = t,(aes(Year, t[[k]], color = Category)))+
      geom_point()+
      scale_color_brewer(palette = p)+
      labs(x = "Year", y = "Numbers")+
      theme(
        axis.line = element_line(size = 0.6),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
  })
  
  output$message <- renderText({
    paste("The average number of threatened ", input$kind, "from 2000 to 2022 is: ", 
          mean(t[[input$choose]][t$Category == "Total threatened"]))
  })
  
  output$table2 <- renderTable({
    c <- input$choice
    if(c == "v"){
      t %>% 
        select(Year, Category, Mammals, Birds, Reptiles, Amphibians, Fishes, Subtotal_Vertebrates)
    }
    else if(c == "i"){
      t %>% 
        select(Year, Category, Insects, Molluscs, Crustaceans, Corals, Arachnids, Velvet_worms,
               Horseshoe_crabs, Other_invertebrates, Subtotal_Invertebrates)
    }
    else if(c == "p"){
      t %>% 
        select(Year, Category, Mosses, Ferns_allies, Gymnosperms, Flowering_plants,
               Green_algae, Red_algae, Subtotal_Plants)
    }
    else if(c == "f"){
      t %>% 
        select(Year, Category, Lichens, Mushrooms, Brown_algae, Subtotal_Fungi_protists)
    }
  })
  
  output$message2 <- renderText({
    g <- input$choice
    if(g == "v"){
      paste("Increase of the total threatened (2000-2022): 10739-3507 = ", 10739-3507)
    }
    else if(g == "i"){
      paste("Increase of the total threatened (2000-2022): 6161-1928 = ", 6161-1928)
    }
    else if(g == "p"){
      paste("Increase of the total threatened (2000-2022): 24914-5611 = ", 24914-5611)
    }
    else if(g == "f"){
      paste("Increase of the total threatened (2000-2022): 294-0 = ", 294)
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
