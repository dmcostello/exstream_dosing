#Packages needed
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Calculate nutrient dosing for KSU ExStream"),
    
    #Nutrients
    h2("Nutrient for spike"),
    fluidRow(
      
      column(4,
           selectInput("nut",label="Nutrient",choices=list("Nitrogen","Phosphorus","Iron"))  
           ),
      
      column(4,
             numericInput("a.mass","Molar mass of salt",value=101)
             ),
      
      column(4,
             numericInput("bump","Concentration increase (ug/L)",value=250)
             )
    ),
    
    br(),
    
    #ExStream
    h2("ExStream settings"),
    fluidRow(
      
      column(4,
             sliderInput("meso.no",label="# of mesocosms dosed",min = 4,max=32,value=16)  
             ),
      
      column(4,
             numericInput("flow","Mesocosm flow rate (L/min)",value=0.8,min=0.2,max=2)
             ),
      
      column(4,
             numericInput("duration","Experiment duration (d)",value=21)
             )
    ),
    
    fluidRow(
      
      column(4,
             numericInput("drip",label="Emitter drip rate (mL/min)",value=24)  
      ),
      
      column(4,
             numericInput("pump","Pump flow rate (mL/min)",value=60)
      )
    ),
    
    br(),
    
    h2("Experimental parameters"),
    h4("Nutrient concentrations"),
    textOutput("emit.conc"),
    textOutput("barrel.conc"),
    br(),
    h4("Daily totals"),
    textOutput("vol.waste"),
    textOutput("sump.vol"),
    textOutput("daily.salt"),
    br(),
    h4("Experiment totals"),
    textOutput("salt.tot"),
 
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #Define element symbols
  symbol <- reactive({
    if(input$nut=="Nitrogen"){"N"}
    else
      if(input$nut=="Phosphorus"){"P"}
    else
      if(input$nut=="Iron"){"Fe"}
  })
 
  #Define atomic mass
  atomic <- reactive({
    if(input$nut=="Nitrogen"){14}
    else
      if(input$nut=="Phosphorus"){31}
    else
      if(input$nut=="Iron"){55.845}
  })
  
  #Total waste volume per day
  output$vol.waste <- renderText({
    paste("Total daily volume of waste =",
           input$meso.no*input$flow*60*24,"L"
      
    )
  })
  
  #Reactives for values used in multiple calculations
  drip.con <- reactive({
    (input$flow*input$bump)/input$drip
  })
  
  bar.conc <- reactive({
    (drip.con()*input$meso.no*input$drip)/input$pump
  })
  
  dsalt <- reactive({
    (bar.conc()/1000)*(input$pump/1000)*24*60
  })
  
  #Concentration from drip emitters
  output$emit.conc <- renderText({
    paste("Conc. from drip emitter =",
          round(drip.con(),2),"mg",symbol(),"/L"
          
    )
  })
  
  #Concentration in the barrel
  output$barrel.conc <- renderText({
    paste("Nutrient conc. in barrel =",
          round(bar.conc(),2),"mg",symbol(),"/L")
  })
  
  #Total salt used daily
  output$daily.salt <- renderText({
    paste("Daily salt used =",
          round(dsalt()*input$a.mass/atomic(),2),
    "g/d")
  })
  
  output$salt.tot <- renderText({
    paste("Total salt used =",
          round(dsalt()*input$duration*input$a.mass/atomic(),2),"g")
  })
  
  output$sump.vol <- renderText({
    paste0("Daily volume used = ",
          round(input$pump*60*24/1000,0)," L ",
          "(",
          round(input$pump*60*24/1000*0.264,1)," gal)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
