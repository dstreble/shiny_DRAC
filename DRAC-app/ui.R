require(shiny)

shinyUI(fluidPage(
  titlePanel("DRAC"),
  
  tabsetPanel(
    tabPanel("Data",
             fluidRow(
               column(width = 2,
                      h4("General parameters"),
                      selectInput(inputId = "material",
                                  label = "Context",
                                  choices = c("sediment", 
                                              "flint", 
                                              "ceramic",
                                              "cave sediment",
                                              "cave flint"),
                                  selected = "sediment"),
                      textInput(inputId = "project",
                                label = "Project",
                                placeholder = "required"),
                      textInput(inputId = "sample",
                                label = "Sample",
                                placeholder = "required"),
                      dateInput(inputId = "date",
                                label = "Sampling date"),
                      selectInput(inputId = "mineral",
                                  label = "Mineral",
                                  choices = c("Q","F","PM"),
                                  selected = "Q"),
                      selectInput(inputId = "conversionFactor",
                                  label = "Conversion factor",
                                  choices = c("AdamiecAitken1998",
                                              "Guerinetal2011",
                                              "Liritzisetal2013",
                                              "X"),
                                  selected = "Liritzisetal2013"),
                      selectInput(inputId = "alphaSizeFactor",
                                  label = "Alpha size attenuation factor",
                                  choices = c("Bell1980",
                                              "Brennanetal1991"),
                                  selected = "Brennanetal1991"),
                      uiOutput(outputId="betaSizeFactor"),

                      selectInput(inputId = "betaEtchFactor",
                                  label = "Beta etch attenuation factor",
                                  choices = c("Bell1979",
                                              "Brennan2003"),
                                  selected = "Brennan2003")
               ),
               
               uiOutput(outputId= "m1_column"),
               
               uiOutput(outputId= "m2_column"),

               uiOutput(outputId= "m3_column"),
               
               
               
               column(width = 2,
                      h4("Dc information"),
                      
                      checkboxInput(inputId = "geoDcBox",
                                    label = " Use geographical position",
                                    value = FALSE),     
                      
                      uiOutput(outputId = "DcLoc"),
                      
                      checkboxInput(inputId = "directDcBox",
                                    label = " direct Dc measurement",
                                    value = FALSE),
                      
                      uiOutput(outputId="directDc")
                      
               ),
               
               column(width = 2,
                      
                      h4("De information"),
                      
                      h5("Equivalent dose [Gy]"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "de",
                                         label = "De")),
                        column(width = 6,
                               textInput(inputId = "de_err",
                                         label = "\u03B4De"))
                      ),
                      
                      br(),
                      br(),
                      
                      actionButton(inputId = "ageButton",
                                   label = "Age estimation"),
                      
                      uiOutput(outputId= "ageText")
                 
               )
             )
    ),
    
    
    tabPanel("Result",
             
             DT::dataTableOutput("resultTab"))
  )

))
