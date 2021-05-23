#loading the data 

mappdata <- readRDS("Shiny App Creation/Map_Data.rds")

temppdata <- read.csv("Shiny App Creation/mean_temp_longformatdata.csv")

rainndata <- read.csv("Shiny App Creation/mean_rain_longformatdata.csv")

carbonndata <- read.csv("Shiny App Creation/mean_co2conc_longformatdata.csv")



#loading the libraries 
library(graph)
library(rsconnect)
library(tidyverse)
library(ggplot2)
library(knitr)
library(ggrepel)
library(ggspatial)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(mdsr)
library(osmdata) # package for working with streets
library(osmplotr)
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(sysfonts)
library(shiny)
library(rnaturalearthhires)
library(plotly)



ui <- fluidPage(
  titlePanel(h3("TANZANIA CLIMATE DATA FROM 2000 - 2016", align ="center")),
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = 'Year',
                  label = 'Choose a year:',
                  choices = c("2000", "2001", "2002", "2003" , "2004" ,"2005" , "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015","2016")),
      
      selectInput(inputId = 'Region',
                  label = 'Choose a region:',
                  choices = c("Arusha", "Dar es Salaam"      ,      "Dodoma"       ,            "Geita"       ,             "Iringa" ,                 
                              "Kagera" ,                  "Katavi"    ,               "Kigoma"   ,                "Kilimanjaro"      ,        "Lindi"  ,                
                              "Manyara"   ,               "Mara"    ,                 "Mbeya"        ,            "Morogoro"       ,          "Mtwara" ,                 
                              "Mwanza"   ,                "Njombe" ,                  "North Pemba",              "Pwani" ,                   "Rukwa" ,                  
                              "Ruvuma" ,                  "Shinyanga" ,               "Simiyu"   ,                "Singida"  ,                "South Pemba" ,            
                              "Tabora",                   "Tanga",                    "Zanzibar North" ,          "Zanzibar South & Central", "Zanzibar Urban/West" )),
      
      selectInput(inputId = 'weather',
                  label = 'Choose the weather condition:',
                  choices = c("Rainfall","Temperature"))
    ),
    
    mainPanel(
      tabPanel("Plot",
               fluidRow(
                 column(12, plotlyOutput("plot1")),
                 
               ),
               
               
               fluidRow(
                 
                 splitLayout(cellWidths = c("45%", "45%"), plotlyOutput("plot2"), plotlyOutput("plot3")),
               ),
               hr(),
               print("All data used obtained from http://geo.aiddata.org/ which sourced temperature data from the Climate Research Unit, rainfall data from University of Delaware and carbondioxide concentration from NASA Jet Propulsion laboratory "),
               
               
      )
      
    )
    
  )
)


server <- function(input, output) {
  output$plot1  <- renderPlotly({
    
    
    if (input$weather == "Temperature"){
      
      Tanzania <- ne_states(country = "united republic of tanzania", returnclass = c("sf"))
      
      whoa <- temppdata[temppdata$year == input$Year  , ]
      
      whoaa1 <- left_join(Tanzania, whoa, by = "name")
      
      col3 <- colorRamp(c( "white", "red")) 
      
     themap <-  plot_ly(whoaa1) %>% 
        add_sf(
          color = ~temparature, 
          colors = col3, 
          split = ~name, 
          span =  I(1), #this is the 
          text = ~paste(name, temparature),
          hoverinfo = "text",
          hoveron = "fills"
        ) %>%
        layout(title = paste("Map showing Mean Annual", input$weather, "(°C)", "in Tanzania", "in", input$Year)) %>%
        colorbar(title = paste("Temparature \n", input$Year))%>%
        layout(showlegend = FALSE, margin=list(t = 75), font=list(size = 10))
      
    }
    
    else  { 
      Tanzania <- ne_states(country = "united republic of tanzania", returnclass = c("sf"))
      
      whoa <- rainndata[rainndata$year == input$Year  , ]
      
      whoaa1 <- left_join(Tanzania, whoa, by = "name")
      
      col3 <- colorRamp(c( "white", "blue")) 
      
      themap <-  plot_ly(whoaa1) %>% 
        add_sf(
          color = ~rainfall, 
          colors = col3, 
          split = ~name, 
          span =  I(1), #this is the size
          text = ~paste(name, rainfall), #this is what appears when someone hovers  
          hoverinfo = "text", #this shows what will appear when someone hovers 
          hoveron = "fills" #this means it hovers on the filled area displaying text 
        ) %>%
        layout(showlegend = FALSE, margin=list(t = 75), font=list(size = 10)) %>%
        layout(title = paste("Map showing Mean Annual",input$weather,"(mm)", "in Tanzania", "in", input$Year)) %>%
        colorbar(title = paste("Rainfall \n", input$Year))%>% 
        style(hoverlabel = list(bgcolor = "white")) #changes the background color of your displayed text
      
    }
    
    print(themap)
    
  })
  
  
  output$plot2  <- renderPlotly({
    #check for weather condition
    if (input$weather == "Temperature"){
      
      TREND <- temppdata[temppdata$name== input$Region, ]
      
      yes <-  ggplot(TREND,aes(year, temparature)) +
        geom_point()+ 
        theme_bw() +
        ggtitle(paste("Mean Annual",input$weather,"in", input$Region,"from 2000 to 2016")) +
        geom_smooth() + 
        theme(plot.title = element_text(size = 10))
      
      trend <- ggplotly(yes) %>% 
        layout(margin=list(t = 75))
      
    }
    
    
    else  { 
      
      
      TREND <- rainndata[rainndata$name== input$Region, ]
      
      yes <-  ggplot(TREND, aes(x = year, y = rainfall)) +
        geom_point(show.legend=FALSE) +
        theme_bw()+
        ggtitle(paste("Mean Annual",input$weather, "in", input$Region,"from 2000 to 2016")) +
        geom_smooth()+ 
        theme(plot.title = element_text(size = 10))
      
      trend <- ggplotly(yes) %>% 
        layout(margin=list(t = 75))
      
    }
    
    print(trend)
    
  })
  
  
  output$plot3  <- renderPlotly({
    
    carbonnn <- carbonndata[carbonndata$name== input$Region, ]
    
    pollution2 <-  ggplot(carbonnn, aes(year, carbonconc)) +
      geom_point(show.legend=FALSE) +
      theme_bw() +
      ggtitle(paste("Average Carbondioxide Concentration in",input$Region)) +
      geom_smooth()+ 
      theme(plot.title = element_text(size = 10))
    
    pollution <- ggplotly(pollution2) %>% 
      layout(margin=list(t = 75)) #this removes the overlap between the two of them by adding a top margin which your plotly menu will occupy 
    
    print(pollution)
    
  })
  
}      
shinyApp(ui = ui, server = server)      

