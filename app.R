library(shiny)
library(bslib)
library(scales)
library(plotly)
#"canadacovid" is the name of the dataset I imported from the Government of Canada website
on <- dplyr::filter(canada_covid, prname == "Ontario")
ab <- dplyr::filter(canada_covid, prname == "Alberta")
bc <- dplyr::filter(canada_covid, prname == "British Columbia")
sk <- dplyr::filter(canada_covid, prname == "Saskatechewan")
mn <- dplyr::filter(canada_covid, prname == "Manitoba")
qc <- dplyr::filter(canada_covid, prname == "Quebec")
pei <- dplyr::filter(canada_covid, prname == "Prince Edward Island")
nl <- dplyr::filter(canada_covid, prname == "Newfoundland and Labrador")
ns <- dplyr::filter(canada_covid, prname == "Nova Scotia")
nb <- dplyr::filter(canada_covid, prname == "New Brunswick")
yk <- dplyr::filter(canada_covid, prname == "Yukon")
nv <- dplyr::filter(canada_covid, prname == "Nunavut")
nw <- dplyr::filter(canada_covid, prname == "Northwest Territories")



ui <- page_sidebar(
  sidebar = sidebar(checkboxGroupInput("pro", label = "Toggle Provinces", choices = c("Ontario" = "on", "Alberta"= "ab" ,"British Columbia" =  "bc" , "Quebec" = "qc"), selected = c("on", "ab", "bc", "qc")  ), "Data from https://health-infobase.canada.ca/covid-19/"),
  fluidRow(
    
    plotlyOutput("myplot")
  ))


server = function(input, output){
  al <- NULL
  br <- NULL
  qu <- NULL
  ot <- NULL
  observe({ if ("on" %in% input$pro) {ot <- geom_line(data = on, aes(x = date, y = ratecases_last7, color = "Ontario")) }
    if ("ab" %in% input$pro) {al <- geom_line(data = ab, aes(x = date, y = ratecases_last7, color = "Alberta")) }
    if ("bc" %in% input$pro) {br <- geom_line(data = bc, aes(x = date, y = ratecases_last7, color = "British Columbia")) }
    if ("qc" %in% input$pro) {qu <- geom_line(data = qc, aes(x = date, y = ratecases_last7, color = "Québec")) }
    
    output$myplot <- renderPlotly({
    p <- ggplot() + ot  +
      al + br + qu + xlab("Date") +
      ylab("New Cases per week per capita") + scale_color_manual("Provinces", values=c('blue', 'green', 'red', "black")) +
      theme(legend.background = element_rect(fill="orange",
                                             size=0.5, linetype="solid", 
                                             colour ="black")) + 
      theme(legend.title = element_text(family = "Helvetica", size  = 11, face = "italics")) +
      theme(legend.text = element_text(family = "Helvetica")) + 
      theme(axis.title.x = element_text(family = "Helvetica", color = "#4C0099")) + 
        theme(axis.title.y = element_text(family = "Helvetica", color = "#4C0099")) +
       labs(title = "COVID-19 Pandemic: Comparing 4 Canadian Provinces", 
            caption = "Data from https://health-infobase.canada.ca/covid-19/") +
      theme(plot.title = element_text(family = "Helvetica", face = "bold"))+
     theme(
      plot.caption = element_text(hjust = 1)
    )

    
    ggplotly(p, tooltip = "text")
      
  })})
}

#Run the app
shinyApp(ui, server)
