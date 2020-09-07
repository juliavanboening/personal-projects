library(shiny)

ui <- fluidPage (
  
  titlePanel(title = "Predict Mean Ozone"),
  
  h4("Instruction:"),
  
  h5("- Select a prediction model from the radio button (either Temperature or Wind Speed)."),
  
  h5("- For Temperture or Wind Speed, choose your desired value from the sliders."),
  
  h5("- The app will show an appropriate plot with the corresponding linear model."),
  
  h5("- Moreover, it will predict the Ozone quality based on your choice."),
  
  sidebarLayout(
    
    sidebarPanel(
      
      radioButtons("radio", label = "Choose a prediction model based on:", 
                   
                   choices = c("Temprature (Ozone ~ Temp)" = 1, "Wind Speed (Ozone ~ Wind)" = 2),
                   
                   selected = "1"
                   
      ),
      
      sliderInput("sliderTemp", "What is the Temperature (fahrenheit)?", 50, 110, value = 59),
      
      sliderInput("sliderWind", "What is the Wind Speed (miles/hour)?", 0, 15, value = 5)
      
    ), 
    
    mainPanel(
      
      h4("Predicted Mean Ozone from Model 1:"), 
      
      textOutput("pred1"),
      
      h4("Predicted Mean Ozone from Model 2:"), 
      
      textOutput("pred2"),
      
      plotOutput("plot1")
      
    )
    
  )
  
)

data("airquality")

airquality <- airquality[complete.cases(airquality),]

server <- function(input, output) {
  
  model1 <- lm(Ozone ~ Temp, data = airquality)
  
  model2 <- lm(Ozone ~ Wind, data = airquality)
  
  model1pred <- reactive({
    
    tempInput <- input$sliderTemp
    
    predict(model1, newdata = data.frame(Temp = tempInput))
    
  })
  
  model2pred <- reactive({
    
    tempInput <- input$sliderTemp
    
    windInput <- input$sliderWind
    
    predict(model2, newdata = data.frame(Temp = tempInput, Wind = windInput))
    
  })
  
  output$plot1 <- renderPlot({
    
    tempInput <- input$sliderTemp
    
    windInput <- input$sliderWind
    
    if(input$radio == "1"){
      
      plot(airquality$Temp, airquality$Ozone, xlab = "Temperature", 
           
           ylab = "Ozone", bty = "n", pch = 16, 
           
           xlim = c(50, 110), ylim = c(-25, 180))
      
      abline(model1, col = "red", lwd = 2)
      
      points(tempInput, model1pred(), col = "red", pch = 16, cex = 2)
      
    }
    
    else{
      
      plot(airquality$Wind, airquality$Ozone, xlab = "Wind", 
           
           ylab = "Ozone", bty = "n", pch = 16, 
           
           xlim = c(0, 15), ylim = c(-25, 180))
      
      abline(model2, col = "blue", lwd = 2)
      
      points(windInput, model2pred(), col = "blue", pch = 16, cex = 2)
      
    }
    
    #if(input$showModel1){
    
    #       abline(model1, col = "red", lwd = 2)
    
    #      points(tempInput, model1pred(), col = "red", pch = 16, cex = 2)
    
    #}
    
    #if(input$showModel2){
    
    #       model2lines <- predict(model2)
    
    #      lines(model2lines, col = "blue", lwd = 2)
    
    #      points(tempInput, model2pred(), col = "blue", pch = 16, cex = 2)
    
    #}
    
  })
  
  output$pred1 <- renderText({
    
    if(input$radio == "1"){
      
      model1pred()
      
    }
    
    else{return("NA")}
    
  })
  
  output$pred2 <- renderText({
    
    if(input$radio == "2"){
      
      model2pred()
      
    }
    
    else{return("NA")}
    
  })
  
}

shinyApp(ui = ui, server = server)




