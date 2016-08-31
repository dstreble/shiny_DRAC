require(shiny)

shinyUI(fluidPage(
  titlePanel("DRAC"),
  
  tabsetPanel(
    tabPanel("Data",
             fluidRow(
               column(width = 2,
                      helpText("General parameters"),
                      selectInput(inputId = "material",
                                  label = "Material",
                                  choices = c("sediment", 
                                              "flint", 
                                              "ceramic",
                                              "cave sediment",
                                              "cave flint"),
                                  selected = "sediment"),
                      textInput(inputId = "project",
                                label = "Project"),
                      textInput(inputId = "sample",
                                label = "Sample"),
                      dateInput(inputId = "date",
                                label = "Sampling date"),
                      selectInput(inputId = "mineral",
                                  label = "Mineral",
                                  choices = c("Q","F","PM"),
                                  selected = "Q"),
                      selectInput(inputId = "conversionFactor",
                                  label = "Conversion factor",
                                  choices = c("Liritzisetal2013"),
                                  selected = "Liritzisetal2013"),
                      selectInput(inputId = "alphaSizeFactor",
                                  label = "Alpha size attenuation factor",
                                  choices = c("Brennanetal1991"),
                                  selected = "Brennanetal1991"),
                      selectInput(inputId = "betaSizeFactor",
                                  label = "Beta size attenuation factor",
                                  choices = c("Guerinetal2012-Q"),
                                  selected = "Guerinetal2012-Q"),
                      selectInput(inputId = "betaEtchFactor",
                                  label = "Beta etch attenuation factor",
                                  choices = c("Brennan2003"),
                                  selected = "Brennan2003")
               ),
               
               column(width = 2,
                      helpText("Grain information"),
                      
                      checkboxInput(inputId = "m1_concentration",
                                    label = "Radioelement concentration",
                                    value = TRUE),                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_U",
                                         label = "U [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m1_U_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_Th",
                                         label = "Th [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m1_Th_err",
                                         label = "error"))
                      ),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_K",
                                         label = "K [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m1_K_err",
                                         label = "error"))
                      ),
                      
                      
                      checkboxInput(inputId = "m1_K2Rb",
                                    label = "Rb from K",
                                    value = TRUE),
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_Rb",
                                         label = "Rb [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m1_Rb_err",
                                         label = "error"))
                      ),
                      
                      checkboxInput(inputId = "m1_doseRateBox",
                                    label = "Direct dose rate measurement",
                                    value = FALSE),                        
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_alpha",
                                         label = "Alpha [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m1_alpha_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_beta",
                                         label = "Beta [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m1_beta_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_gamma",
                                         label = "Gamma [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m1_gamma_err",
                                         label = "error"))
                      ),
                      
                      helpText("Grain size"),
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_size_min",
                                         label = "Min [um]")),
                        column(width = 6,
                               textInput(inputId = "m1_size_max",
                                         label = "Max [um]"))
                      ),
                      
                      helpText("Grain etch"),
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_etch_min",
                                         label = "Min [um]")),
                        column(width = 6,
                               textInput(inputId = "m1_etch_max",
                                         label = "Max [um]"))
                      ),
                      
                      helpText("a value"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_aValue",
                                         label = "a value")),
                        column(width = 6,
                               textInput(inputId = "m1_aValue_err",
                                         label = "error"))
                      ),
                      
                      helpText("density"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_density",
                                         label = "density")),
                        column(width = 6,
                               textInput(inputId = "m1_density_err",
                                         label = "error"))
                      ),
                      
                      helpText("Water content"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m1_water",
                                         label = "water [%]")),
                        column(width = 6,
                               textInput(inputId = "m1_water_err",
                                         label = "error"))
                      )
               ),
               
               column(width = 2,
                      helpText("Sediment information"),
                      
                      checkboxInput(inputId = "m2_concentration",
                                    label = "Radioelement concentration",
                                    value = TRUE),  
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_U",
                                         label = "U [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m2_U_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_Th",
                                         label = "Th [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m2_Th_err",
                                         label = "error"))
                      ),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_K",
                                         label = "K [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m2_K_err",
                                         label = "error"))
                      ),
                      
                      checkboxInput(inputId = "m2_K2Rb",
                                    label = "Rb from K",
                                    value = TRUE),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_Rb",
                                         label = "Rb [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m2_Rb_err",
                                         label = "error"))
                      ),
                      
                      checkboxInput(inputId = "m2_doseRateBox",
                                    label = "Direct dose rate measurement",
                                    value = FALSE),                       
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_alpha",
                                         label = "Alpha [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m2_alpha_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_beta",
                                         label = "Beta [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m2_beta_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_gamma",
                                         label = "Gamma [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m2_gamma_err",
                                         label = "error"))
                      ),
                      
                      helpText("density"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_density",
                                         label = "density")),
                        column(width = 6,
                               textInput(inputId = "m2_density_err",
                                         label = "error"))
                      ),
                      
                      helpText("Water content"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m2_water",
                                         label = "water [%]")),
                        column(width = 6,
                               textInput(inputId = "m2_water_err",
                                         label = "error"))
                      )
               ),
               
               column(width = 2,
                      helpText("Ceramic information"),
                      
                      checkboxInput(inputId = "m3_concentration",
                                    label = "Radioelement concentration",
                                    value = TRUE),                        
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_U",
                                         label = "U [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m3_U_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_Th",
                                         label = "Th [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m3_Th_err",
                                         label = "error"))
                      ),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_K",
                                         label = "K [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m3_K_err",
                                         label = "error"))
                      ),
                      
                      checkboxInput(inputId = "m3_K2Rb",
                                    label = "Rb from K",
                                    value = TRUE),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_Rb",
                                         label = "Rb [ppm]")),
                        column(width = 6,
                               textInput(inputId = "m3_Rb_err",
                                         label = "error"))
                      ),
                      
                      checkboxInput(inputId = "m3_doseRateBox",
                                    label = "Direct dose rate measurement",
                                    value = FALSE),                       
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_alpha",
                                         label = "Alpha [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m3_alpha_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_beta",
                                         label = "Beta [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m3_beta_err",
                                         label = "error"))
                      ),
                      
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_gamma",
                                         label = "Gamma [Gy]")),
                        column(width = 6,
                               textInput(inputId = "m3_gamma_err",
                                         label = "error"))
                      ),
                      
                      helpText("density"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_density",
                                         label = "density")),
                        column(width = 6,
                               textInput(inputId = "m3_density_err",
                                         label = "error"))
                      ),
                      
                      helpText("Water content"),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "m3_water",
                                         label = "water [%]")),
                        column(width = 6,
                               textInput(inputId = "m3_water_err",
                                         label = "error"))
                      )
               ),
               
               column(width = 2,
                      helpText("Depth information"),
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "depth",
                                         label = "Depth")),
                        column(width = 6,
                               textInput(inputId = "depthErr",
                                         label = "Depth err"))
                      ),
                      
                      checkboxInput(inputId = "shallowDepthBox",
                                    label = "Scale for shallow depth",
                                    value = FALSE),
                      
                      checkboxInput(inputId = "geoDcBox",
                                    label = " cosmic dose rate based on geographical position",
                                    value = TRUE),                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "altitude",
                                         label = "altitude"),
                               textInput(inputId = "latitude",
                                         label = "latitude"),
                               textInput(inputId = "longitude",
                                         label = "longitude"))
                      ),
                      
                      checkboxInput(inputId = "fieldChangeBox",
                                    label = " field change correction",
                                    value = FALSE),
                      
                      checkboxInput(inputId = "directDcBox",
                                    label = " direct cosmic dose rate measurement",
                                    value = FALSE),
                      
                      fluidRow(
                        column(width = 6,
                               textInput(inputId = "dc",
                                         label = "Gamma [Gy]")),
                        column(width = 6,
                               textInput(inputId = "dc_err",
                                         label = "error"))
                      ),
                      
                      br(),
                      
                      helpText("Equivalent dose"),
                      
                      fluidRow(
                        column(width = 6,
                               
                               textInput(inputId = "De",
                                         label = "De"),
                               textInput(inputId = "De_err",
                                         label = "De err")
                               
                               
                        )
                      )
               ),
               column(width = 2,
                      
                      br(),
                      br(),
                      
                      actionButton(inputId = "generateButton",
                                   label = "Estimate dose rate")
                      
               )
             )
    ),
    
    tabPanel("Result",
             tableOutput("result"))
  )
  
))
