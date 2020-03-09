library(shiny)
library(shinydashboard)
library(plotly)
library(xgboost)

body <- dashboardBody(
  
  h1("Let’s get started listing your space."),
  fluidRow(
    box(width = 12,status = "primary", 
        h3("Step 1: Start with the basics",style = "color:grey"),
        #br(), 
        h2("What kind of place do you have?"),
        fluidRow(
          column(6,selectInput("roomtype", label = "",
                               choices = list("Entire Place" = 1, "Private Room" = 2,
                                              "Shared Room" = 3), selected = 1)),
          column(6,selectInput("accommodates", label = "",
                               choices = list("for 1 guest" = 1, "for 2 guests" = 2,
                                              "for 3 guests" = 3,"for 4 guests" = 4,
                                              "for 5 guests" = 5, "for 6 guests" = 6), 
                               selected = 1))
        ),
        selectInput("city", label = "",
                    choices = list("New York, NY, USA" = 1), 
                    selected = 1),
        h2("How many guests can your place accommodate?"),
        h4("Check that you have enough beds to accommodate all your guests comfortably."),
        h3("How many bedrooms can guests use?",style = "color:grey"),
        fluidRow(
          column(6,selectInput("bedrooms", label = "",
                               choices = list("1 bedroom" = 1, "2 bedrooms" = 2,
                                              "3 bedrooms" = 3,"4 bedrooms" = 4), 
                               selected = 1))),
        h3("How many beds can guests use?",style = "color:grey"),
        fluidRow(
          column(3, h3("Beds")),
          column(6,selectInput("Beds", label = "",
                               choices = list("1" = 1, "2" = 2,
                                              "3" = 3,"4" = 4), 
                               selected = 1))),
        h2("How many bathrooms?"),
        fluidRow(
          column(3, h3("Bathrooms")),
          column(6,selectInput("Bathrooms", label = "",
                               choices = list("1" = 1, "2" = 2,
                                              "3" = 3,"4" = 4), 
                               selected = 1))),
        h2("Where’s your place located?"),
        h4("Guests will only get your exact address once they’ve booked a reservation."),
        textInput("Neighbourhood","", value = "Chelsea"),
        h2("What amenities do you offer?"),
        h4("These are just the amenities guests usually expect, but you can add even more after you publish."),
        checkboxGroupInput("amenities", "", 
                           choices = list("Essentials" = 1, 
                                          "Air conditioning" = 2, 
                                          "Heat" = 3,
                                          "TV" = 4,
                                          "Wifi" = 5)),
        h3("Safety amenities",style = "color:grey"),
        checkboxGroupInput("safeamenities", "", 
                           choices = list("Fire extinguisher" = 1, 
                                          "Carbon monoxide detector" = 2, 
                                          "Smoke detector" = 3,
                                          "First aid kit" = 4)),
        h2("What spaces can guests use?"),
        h4("Include common areas, but don’t add spaces that aren’t on your property."),
        checkboxGroupInput("spaceamenities", "", 
                           choices = list("Kitchen" = 1, 
                                          "Parking" = 2, 
                                          "Gym" = 3,
                                          "Hot tub" = 4))
        
    )
  ),
  fluidRow(
    box(width = 12,status = "warning",
        h3("Step 2: Set the scene",style = "color:grey"),
        #br(), 
        h2("Add photos to your listing"),
        radioButtons("photo", "",
                     choices = list("Yes, I did!" = 1, "No, skip!" = 0),selected = 1),
        
        h2("Name your place"),
        h4("Attract guests with a listing title that highlights what makes your place special."),
        textInput("Name","", value = "Listing title..."),
        h4("50 characters remaining",style = "color:grey")
    )
    
  ),
  
  fluidRow(
    box(width = 12,status = "primary", 
        h3("Step 3: Get ready for guests",style = "color:grey"),
        h2("Review Airbnb’s guest requirements"),
        h4("Airbnb has requirements that all guests must meet before they book."),
        h3("All Airbnb guests must provide: ",style = "color:grey"),
        checkboxGroupInput("guestverification", "", 
                           choices = list("Email address" = 1, 
                                          "Confirmed phone number" = 2, 
                                          "Payment information" = 3)),
        h2("Set house rules for your guests"),
        h4("Guests must agree to your house rules before they book."),
        checkboxGroupInput("houserules", "", 
                           choices = list("Suitable for children" = 1, 
                                          "Smoking allowed" = 2, 
                                          "Events or parties allowed" = 3))
        
    )
  ),
  actionButton("go", "Click me to price!"),
  br(),
  fluidRow(
    #valueBox(width = 12,
    #"Lovely Apt", "Your listing", icon = icon("dollar"))
    
    infoBoxOutput(width = 12, "listing"),
  ),
  fluidRow(
    valueBoxOutput("price"),
    valueBoxOutput("occupacy"),
    valueBoxOutput("income"),
    plotlyOutput("demandcurve")
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Airbnb Pricing Tips"),
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input, output) { 
  
  plotdata <- reactive({
  load("/Users/keke/Documents/Job/Healthpeak/Final Project/shinydashboard/data_col.Rdata")
  data_col <- t(data_col)
  data <- as.matrix(data_col)
  roomtype = c("room_typeHotel room","room_typePrivate room","room_typeShared room")
  data[1,roomtype[as.numeric(input$roomtype)]] = 1
  data[1,"accommodates"] = input$accommodates
  data[1,"bedrooms"] = input$bedrooms
  data[1,"beds"] = input$Beds
  data[1,"bathrooms"] = input$Bathrooms
  ##
  amenities = c("Essentials","`Air conditioning`","Heating","TV","Wifi")
  for ( i in as.numeric(input$amenities)){
    data[1,amenities[i]] = 1
  }
  ##
  safe = c("`Fire extinguisher`","`Carbon monoxide detector`","`Smoke detector`",
           "`First aid kit`")
  for (i in as.numeric(input$safeamenities)){
    data[1,safe[i]] = 1
  }
  ##
  space = c("Kitchen","`Free parking on premises`","Gym","`Hot tub`")
  for (i in as.numeric(input$spaceamenities)){
    data[1,space[i]] = 1
  }
  data[1,"host_has_profile_pict"] = input$photo
  neighbour = paste("neighbourhood_cleansed",input$Neighbourhood,sep = "")
  data[1,neighbour] = 1
  data[1,"require_guest_phone_verificationt"] = ifelse(2 %in% as.numeric(input$guestverification), 1, 0)
  ##
  houserules = c("`Family/kid friendly`","`Smoking allowed`", "`Suitable for events`")
  for ( i in as.numeric(input$houserules)){
    data[1,houserules[i]] = 1
  }
  
  data = as.numeric(data)
  
  ts_data = data.frame()
  ts_data = rbind(ts_data,data)
  #colnames(ts_data) = colnames(data)
  len = 301
  ts_data = ts_data[rep(seq_len(nrow(ts_data)), each = len),]
  ts_data$Occupancy = seq(0,30,0.1)
  ts_data = ts_data[, - ncol(ts_data)]
  #colnames(ts_data) = colnames(data_col)
  ts_data = as.matrix(ts_data)
  ts_label = rep(0,len)
  dts <- xgb.DMatrix(data = ts_data)
  
  
  
  load("/Users/keke/Documents/Job/Healthpeak/Final Project/shinydashboard/xgb.rda")
  colnames(dts) <- xgb3$feature_names
  prediction = predict(xgb3,dts)
  data.frame("Price" = prediction, "Occupancy" = seq(0,30,0.1),"Income" =prediction*seq(0,30,0.1))
  
  
  
  })
  
  output$listing = renderInfoBox({
    #input$go
    infoBox(width = 12,
            "Your Listing",h2(input$Name) , icon = icon("credit-card"),
            color = "blue", fill = TRUE
    )})
  
  
  output$price = renderValueBox({
    
    input$go
    
    show = plotdata()
    
    valueBox(width = 4,
            paste(round(show[which.max(show$Income),"Price"],0),"$"),"Daily Price" , icon = icon("dollar"),
             color = "red")
    
  })
  
  output$occupacy <- renderValueBox({
    show = plotdata()
    valueBox(width = 4,
             paste(round(show[which.max(show$Income),"Occupancy"],0),"Days"),"Monthly Occupancy" , icon = icon("list"),
             color = "purple")
  })
  
  output$income <- renderValueBox({
    
    show = plotdata()
    valueBox(width = 4,
             paste(round(show[which.max(show$Income),"Income"],0),"$"),"Monthly Income" , icon = icon("thumbs-up", lib = "glyphicon"),
             color = "yellow")
  })
  
  
  
}






shinyApp(ui, server)
