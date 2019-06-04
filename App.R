#System Viewer - Quaterly BSS Data 

#########################################################################################################################################################
library(shiny)
library(ggplot2)
library(plotly)
library(reshape2)
library(MASS)
library(ggthemes)
library(shinydashboard)

#########################################################################################################################################################

QuarterlyBSSData <- read.csv("QuarterlyBSSData.csv")
QuarterlyBSSData$Q <- substr(QuarterlyBSSData$YrQtr, 6,7)
QuarterlyBSSData$Year <- substr(QuarterlyBSSData$YrQtr, 1,4)

Names <- as.data.frame(unique(QuarterlyBSSData[,c(2)]))
names(Names)[1]<-paste("Names")

Missing_Area <- QuarterlyBSSData[is.na(QuarterlyBSSData$Area),]

#########################################################################################################################################################

ui <- fluidPage(titlePanel(title = "Bike Sharing System Evolution"),
                  sidebarPanel(selectInput(inputId ="City", "Select BSS:", choices = Names, selected = "London", multiple = F)),
                mainPanel(fluidRow(plotOutput("Weekday")), br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),br(), br(),
                          fluidRow(plotOutput("Weekend"))))

server <- function(input, output){
  output$Weekday <- renderPlot({
    ggplot(data = QuarterlyBSSData, aes(x = MaxBikes, y = TDB)) + 
                                  geom_point(data = subset(QuarterlyBSSData, City == input$City & Weekday == 1), aes(x = MaxBikes, y = TDB, size = Area, col = Q)) + 
                                  geom_segment(data = subset(QuarterlyBSSData, City == input$City & Weekday == 1), aes(xend=c(tail(MaxBikes, n=-1), NA), yend=c(tail(TDB, n=-1), NA)),
                                               arrow=arrow(length=unit(0.3,"cm")), alpha = 0.7) + 
                                  labs(title = "Bike Sharing System Evolution (Weekday Averages)", color = "Quarter", size = expression(paste("Area (Km"^"2", ")"))) +
                                  scale_colour_manual(values = c("Q1"="#fb8072",  "Q2" = "#1f78b4", "Q3" = "#bebada", "Q4" = "#fdb462"))  + 
                                  geom_text(data = subset(QuarterlyBSSData, City == input$City & Q == "Q1" & Weekday == 1 | City == input$City & Q == "Q3" & Weekday == 1), aes(label = Year), hjust = -0.2, vjust = 0.5, size = 5) + 
                                  scale_size_continuous(range = c(3, 8)) + 
                                  theme_hc() + theme(legend.title = element_text(size=12, color = "Black", face="bold"),
                                                     legend.justification=c(0,1), 
                                                     legend.position=c(0.05, 0.95),
                                                     legend.background = element_blank(),
                                                     legend.key = element_blank(),
                                                     plot.title = element_text(color = "black", size = 20, face = "bold"),
                                                     axis.title = element_text(color = "black", size = 18, face = "bold"), 
                                                     panel.grid.major = element_blank(),
                                                     panel.grid.minor = element_blank(), 
                                                     strip.background = element_rect(fill = "white", colour = NA))}, height = 700, width = 1200)
  
  output$Weekend <- renderPlot({
    ggplot(data = QuarterlyBSSData, aes(x = MaxBikes, y = TDB)) + 
      geom_point(data = subset(QuarterlyBSSData, City == input$City & Weekday == 0), aes(x = MaxBikes, y = TDB, size = Area, col = Q)) + 
      geom_segment(data = subset(QuarterlyBSSData, City == input$City & Weekday == 0), aes(xend=c(tail(MaxBikes, n=-1), NA), yend=c(tail(TDB, n=-1), NA)),
                   arrow=arrow(length=unit(0.3,"cm")), alpha = 0.7) + 
      labs(title = "Bike Sharing System Evolution (Weekend Averages)", color = "Quarter", size = expression(paste("Area (Km"^"2", ")"))) +
      scale_colour_manual(values = c("Q1"="#fb8072",  "Q2" = "#1f78b4", "Q3" = "#bebada", "Q4" = "#fdb462"))  + 
      geom_text(data = subset(QuarterlyBSSData, City == input$City & Q == "Q1" & Weekday == 0 | City == input$City & Q == "Q3" & Weekday == 0), aes(label = Year), hjust = -0.2, vjust = 0.5, size = 5) + 
      scale_size_continuous(range = c(3, 8)) + 
      theme_hc() + theme(legend.title = element_text(size=12, color = "Black", face="bold"),
                         legend.justification=c(0,1), 
                         legend.position=c(0.05, 0.95),
                         legend.background = element_blank(),
                         legend.key = element_blank(),
                         plot.title = element_text(color = "black", size = 20, face = "bold"),
                         axis.title = element_text(color = "black", size = 18, face = "bold"), 
                         panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), 
                         strip.background = element_rect(fill = "white", colour = NA))}, height = 700, width = 1200)
  
    }




shinyApp(ui, server)

