require(Luminescence)
require(TLdating)
require(shiny)
require(shinyjs)
require(DT)
require(xtable)

shinyServer(function(input, output, session){

  ##############################################################################################
  # Sample information
  ##############################################################################################

  output$infoPage <- renderUI({
    sidebarLayout(
      sidebarPanel(h4("Sample information"),
                   textInput(inputId = "sa_project",
                             label = "Project",
                             placeholder = "required"),
                   textInput(inputId = "sa_site",
                             label = "Site",
                             placeholder = "required"),
                   dateInput(inputId = "sa_date",
                             label = "Sampling year",
                             format = "yyyy",
                             startview = "decade"),
                   textInput(inputId = "sa_sample",
                             label = "Sample name",
                             placeholder = "required")

      ),
      mainPanel(div(class="welcomeText",
                    list(h4("Welcome in ShinyDRAC")
                         ))
      )
    )
  })

  ##############################################################################################
  # Input data
  ##############################################################################################

  output$inPage <- renderUI({

    sidebarLayout(
      uiOutput("inputSidePanel"),
      uiOutput("inputMainPanel")
    )
  })

  output$inputSidePanel <- renderUI({

    sidebarPanel(width = 3,
                 uiOutput(outputId = "deValue"),
                 h4("General parameters"),
                 selectInput(inputId = "material",
                             label = "Context",
                             choices = c("sediment",
                                         "flint",
                                         "ceramic",
                                         "cave sediment",
                                         "cave flint"),
                             selected = "sediment"),
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
                             selected = "Brennan2003"),

                 br(),
                 actionButton(inputId = "dracButton",
                              label = "Age estimation"),

                 uiOutput(outputId= "dracText")
    )
  })

  output$deValue <- renderUI({
    fluidRow(
      h4("Equivalent dose"),
      column(width = 6,
             textInput(inputId = "de",
                       label = "D\u2091 [Gy]",
                       placeholder = "required")),
      column(width = 6,
             textInput(inputId = "de_err",
                       label = "\u03B4D\u2091",
                       placeholder = "required"))
    )
  })

  output$inputMainPanel <- renderUI({
    mainPanel(width = 9,
              fluidRow(
                uiOutput(outputId= "m1_column"),
                uiOutput(outputId= "m2_column"),
                uiOutput(outputId= "m3_column"),
                uiOutput(outputId = "dc_column")
              ))
  })

  output$m1_column <- renderUI({
    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){
      column(width = 3,
             uiOutput(outputId="m1_text"),

             uiOutput(outputId = "m1_doseRateBox"),

             uiOutput(outputId="m1_U"),
             uiOutput(outputId="m1_Th"),
             uiOutput(outputId="m1_K"),
             uiOutput(outputId="m1_K2RbBox"),
             uiOutput(outputId="m1_Rb"),


             uiOutput(outputId="m1_alpha"),
             uiOutput(outputId="m1_beta"),
             uiOutput(outputId="m1_gamma"),

             uiOutput(outputId="m1_sizeText"),
             uiOutput(outputId="m1_size"),

             uiOutput(outputId="m1_etchText"),
             uiOutput(outputId="m1_etch"),

             uiOutput(outputId="m1_aValueText"),
             uiOutput(outputId="m1_aValue"),

             uiOutput(outputId = "m1_densityText"),
             uiOutput(outputId = "m1_density"),

             uiOutput(outputId = "m1_waterText"),
             uiOutput(outputId = "m1_water")
      )
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,

             uiOutput(outputId="m1_text"),

             uiOutput(outputId = "m1_doseRateBox"),

             uiOutput(outputId="m1_U"),
             uiOutput(outputId="m1_Th"),
             uiOutput(outputId="m1_K"),
             uiOutput(outputId="m1_K2RbBox"),
             uiOutput(outputId="m1_Rb"),

             uiOutput(outputId="m1_alpha"),
             uiOutput(outputId="m1_beta"),
             uiOutput(outputId="m1_gamma"),

             uiOutput(outputId="m1_sizeText"),
             uiOutput(outputId="m1_size"),

             uiOutput(outputId="m1_etchText"),
             uiOutput(outputId="m1_etch"),

             uiOutput(outputId="m1_aValueText"),
             uiOutput(outputId="m1_aValue"),

             uiOutput(outputId = "m1_densityText"),
             uiOutput(outputId = "m1_density"),

             uiOutput(outputId = "m1_waterText"),
             uiOutput(outputId = "m1_water")
      )
    }
  })

  output$m2_column <- renderUI({
    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,

             uiOutput(outputId="m2_text"),

             uiOutput(outputId = "m2_doseRateBox"),


             uiOutput(outputId="m2_U"),
             uiOutput(outputId="m2_Th"),
             uiOutput(outputId="m2_K"),
             uiOutput(outputId="m2_K2RbBox"),
             uiOutput(outputId="m2_Rb"),

             uiOutput(outputId="m2_alpha"),
             uiOutput(outputId="m2_beta"),
             uiOutput(outputId="m2_gamma"),

             uiOutput(outputId="m2_densityText"),
             uiOutput(outputId="m2_density"),

             uiOutput(outputId="m2_waterText"),
             uiOutput(outputId="m2_water"),
             uiOutput(outputId = "m2_proportion")
      )
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,

             uiOutput(outputId="m2_text"),

             uiOutput(outputId = "m2_doseRateBox"),

             uiOutput(outputId="m2_U"),
             uiOutput(outputId="m2_Th"),
             uiOutput(outputId="m2_K"),
             uiOutput(outputId="m2_K2RbBox"),
             uiOutput(outputId="m2_Rb"),

             uiOutput(outputId="m2_alpha"),
             uiOutput(outputId="m2_beta"),
             uiOutput(outputId="m2_gamma"),

             uiOutput(outputId="m2_densityText"),
             uiOutput(outputId="m2_density"),

             uiOutput(outputId="m2_waterText"),
             uiOutput(outputId="m2_water"),


             uiOutput(outputId = "m2_proportion")
      )
    }
  })

  output$m3_column <- renderUI({

    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,

             uiOutput(outputId="m3_text"),

             uiOutput(outputId = "m3_doseRateBox"),

             uiOutput(outputId="m3_U"),
             uiOutput(outputId="m3_K"),
             uiOutput(outputId="m3_Th"),
             uiOutput(outputId="m3_K2RbBox"),
             uiOutput(outputId="m3_Rb"),


             uiOutput(outputId="m3_alpha"),
             uiOutput(outputId="m3_beta"),
             uiOutput(outputId="m3_gamma"),

             uiOutput(outputId="m3_densityText"),
             uiOutput(outputId="m3_density"),

             uiOutput(outputId="m3_waterText"),
             uiOutput(outputId="m3_water"),

             uiOutput(outputId = "m3_proportion")
      )
    }
  })

  output$dc_column <- renderUI({

    material <- input$material

    if(is.null(material)){
      return(NULL)
    }

    if(material %in% c("ceramic", "cave sediment", "cave flint")){

      column(width = 3,
             h4("Dc information"),

             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = "geographical position"),

             uiOutput(outputId = "DcLoc"),


             uiOutput(outputId="directDc"),

             h5(tags$b("Depth [m]")),

             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                placeholder = "required"))
             )
      )
    }else if(material %in% c("sediment", "flint")){
      column(width = 4,
             h4("Dc information"),

             radioButtons(inputId = "DcRadioButton",
                          label = "D\u0309 based on:",
                          choices = c("geographical position", "in-situ measurement"),
                          selected = "geographical position"),

             uiOutput(outputId = "DcLoc"),


             uiOutput(outputId="directDc"),

             h5(tags$b("Depth [m]")),

             fluidRow(
               column(width = 6,
                      textInput(inputId = "depth",
                                label = "\u21a7",
                                placeholder = "required")),
               column(width = 6,
                      textInput(inputId = "depth_err",
                                label = "\u03B4\u21a7",
                                placeholder = "required"))
             )
      )
    }
  })

  output$betaSizeFactor <- renderUI({
    if(input$mineral == "Q"){
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-Q"),
                  selected = "Guerinetal2012-Q")

    }else if(input$mineral == "F"){
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-F"),
                  selected = "Guerinetal2012-F")

    }else{
      selectInput(inputId = "betaSizeFactor",
                  label = "Beta size attenuation factor",
                  choices = c("Mejdahl1979",
                              "Brennan2003",
                              "Guerinetal2012-Q",
                              "Guerinetal2012-F"),
                  selected = "Brennan2003")
    }
  })

  # m1
  # Text
  output$m1_text <- renderUI({

    if(input$material %in% c("flint","cave flint")){
      h4("Flint information")

    }else if(input$material %in% c("ceramic", "cave sediment", "sediment") ){

      h4("Grain information")
    }
  })

  output$m1_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m1_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  # Concentration
  output$m1_U <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m1_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m1_Th <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m1_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m1_K <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m1_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m1_K2RbBox <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){
      checkboxInput(inputId = "m1_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m1_Rb <- renderUI({
    if("radioelement concentration" %in% input$m1_doseRateBox){

      if(!is.null(input$m1_K2RbBox) && !input$m1_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m1_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m1_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m1_alpha <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_alpha_err",
                         label = "\u03B4\u03b1"))
      )
    }
  })

  output$m1_beta <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m1_gamma <- renderUI({
    if("direct measurement" %in% input$m1_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m1_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other

  output$m1_sizeText <- renderUI({
    h5(tags$b("Grain size [\u03bcm]"))
  })

  output$m1_size <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_size_min",
                       label = "min",
                       placeholder = "required",
                       value = 100)),
      column(width = 6,
             textInput(inputId = "m1_size_max",
                       label = "max",
                       placeholder = "required",
                       value = 200))
    )
  })

  output$m1_etchText <- renderUI({
    h5(tags$b("Etch depth [\u03bcm]"))
  })

  output$m1_etch <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_etch_min",
                       label = "min",
                       placeholder = "required",
                       value = 0)),
      column(width = 6,
             textInput(inputId = "m1_etch_max",
                       label = "max",
                       placeholder = "required",
                       value = 0))
    )
  })

  output$m1_aValueText <- renderUI({
    h5(tags$b("a-value"))
  })

  output$m1_aValue <- renderUI({

    fluidRow(
      column(width = 6,
             textInput(inputId = "m1_aValue",
                       label = "a")),
      column(width = 6,
             textInput(inputId = "m1_aValue_err",
                       label = "\u03B4a"))
    )
  })

  output$m1_densityText <- renderUI({
    if(input$material %in% c("flint", "cave flint")){
      h5("density")
    }
  })

  output$m1_density <- renderUI({

    if(input$material %in% c("flint", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_density",
                         label = "\u03C1",
                         value = 2.65)),
        column(width = 6,
               textInput(inputId = "m1_density_err",
                         label = "\u03B4\u03C1",
                         value = 0.2))
      )
    }
  })

  output$m1_waterText <- renderUI({
    if(input$material %in% c("")){
      h5("Water content m= (W-D)/D [%]")
    }
  })

  output$m1_water <- renderUI({
    if(input$material %in% c("")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m1_water",
                         label = "m",
                         value = 0)),
        column(width = 6,
               textInput(inputId = "m1_water_err",
                         label = "\u03B4m",
                         value = 0))
      )

    }
  })



  # m2
  output$m2_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m2_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  # concentration
  output$m2_text <- renderUI({

    if(input$material %in% c("ceramic")){
      h4("Ceramic information")

    }else if(input$material %in% c("flint",  "sediment", "cave sediment", "cave flint") ){

      h4("Sediment information")
    }
  })

  output$m2_U <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m2_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m2_Th <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m2_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m2_K <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m2_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m2_K2RbBox <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      checkboxInput(inputId = "m2_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m2_Rb <- renderUI({
    if("radioelement concentration" %in% input$m2_doseRateBox){
      if(!is.null(input$m2_K2RbBox) && !input$m2_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m2_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m2_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m2_alpha <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_alpha_err",
                         label = "\u03B4\u03B1"))
      )
    }
  })

  output$m2_beta <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m2_gamma <- renderUI({
    if("direct measurement" %in% input$m2_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m2_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m2_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other
  output$m2_densityText <- renderUI({
    h5(tags$b("Density [mg/mm3]"))
  })

  output$m2_density <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m2_density",
                       label = "\u03C1",
                       value = 1.8)),
      column(width = 6,
             textInput(inputId = "m2_density_err",
                       label = "\u03B4\u03C1",
                       value = 0.1))
    )
  })

  output$m2_waterText <- renderUI({
    h5(tags$b("Water content m = (W-D)/D [%]"))
  })

  output$m2_water <- renderUI({
    fluidRow(
      column(width = 6,
             textInput(inputId = "m2_water",
                       label = "m",
                       placeholder = "required",
                       value = 5)),
      column(width = 6,
             textInput(inputId = "m2_water_err",
                       label = "\u03B4m",
                       placeholder = "required",
                       value = 2))
    )
  })

  output$m2_proportion <- renderUI({

    P <- input$m3_proportion
    P_err <- input$m3_proportion_err

    if(is.null(P)){
      P <- 0
    }

    if(is.null(P_err)){
      P_err <- 0
    }

    if(input$material %in% c("cave sediment", "cave flint")){

      fluidRow(
        column(width = 6,
               p(strong("p [%]"),
                 br(),
                 p(100-P))
        ),
        column(width = 6,
               p(strong("\u03B4p"),
                 br(),
                 div(P_err))
        )
      )
    }
  })

  # m3
  # concentration
  output$m3_text <- renderUI({

    if(input$material %in% c("ceramic")){
      h4("Sediment information")

    }else if(input$material %in% c("cave sediment", "cave flint")){
      h4("Rock information")

    }
  })

  output$m3_doseRateBox <- renderUI({
    checkboxGroupInput(inputId = "m3_doseRateBox",
                       label = "D\u0309 based on:",
                       choices = c("radioelement concentration", "direct measurement"),
                       selected = "radioelement concentration")
  })

  output$m3_U <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_U",
                         label = "U [ppm]")),
        column(width = 6,
               textInput(inputId = "m3_U_err",
                         label = "\u03B4U"))
      )
    }
  })

  output$m3_Th <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_Th",
                         label = "Th [ppm]")),
        column(width = 6,
               textInput(inputId = "m3_Th_err",
                         label = "\u03B4Th"))
      )
    }
  })

  output$m3_K <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_K",
                         label = "K [%]")),
        column(width = 6,
               textInput(inputId = "m3_K_err",
                         label = "\u03B4K"))
      )
    }
  })

  output$m3_K2RbBox <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      checkboxInput(inputId = "m3_K2RbBox",
                    label = "Rb from K",
                    value = TRUE)
    }
  })

  output$m3_Rb <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "radioelement concentration" %in% input$m3_doseRateBox){
      if(!is.null(input$m3_K2RbBox) && !input$m3_K2RbBox){
        fluidRow(
          column(width = 6,
                 textInput(inputId = "m3_Rb",
                           label = "Rb [ppm]")),
          column(width = 6,
                 textInput(inputId = "m3_Rb_err",
                           label = "\u03B4Rb"))
        )
      }
    }
  })

  #dose rate

  output$m3_alpha <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_alpha",
                         label = "\u03B1 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_alpha_err",
                         label = "\u03B4\u03B1"))
      )
    }
  })

  output$m3_beta <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_beta",
                         label = "\u03B2 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_beta_err",
                         label = "\u03B4\u03B2"))
      )
    }
  })

  output$m3_gamma <- renderUI({
    if(!is.null(input$m3_doseRateBox) && "direct measurement" %in% input$m3_doseRateBox){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_gamma",
                         label = "\u03B3 [Gy]")),
        column(width = 6,
               textInput(inputId = "m3_gamma_err",
                         label = "\u03B4\u03B3"))
      )
    }
  })

  #other

  output$m3_densityText <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Density [mg/mm3]"))
    }
  })

  output$m3_density <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_density",
                         label = "\u03C1",
                         value = 1.8)),
        column(width = 6,
               textInput(inputId = "m3_density_err",
                         label = "\u03B4\u03C1",
                         value = 0.1))
      )
    }
  })

  output$m3_waterText <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      h5(tags$b("Water content m = (W-D)/D [%]"))
    }
  })

  output$m3_water <- renderUI({
    if(input$material %in% c("ceramic","cave sediment", "cave flint")){
      fluidRow(
        column(width = 6,
               textInput(inputId = "m3_water",
                         label = "m",
                         placeholder = "required",
                         value = 5)),
        column(width = 6,
               textInput(inputId = "m3_water_err",
                         label = "\u03B4m",
                         placeholder = "required",
                         value = 2))
      )
    }
  })

  output$m3_proportion <- renderUI({

    P <- input$m2_proportion
    P_err <- input$m2_proportion_err

    if(is.null(P)){
      P <- 100
    }
    if(is.null(P_err)){
      P_err <- 0
    }

    if(input$material %in% c("cave sediment", "cave flint")){
      shinyjs::disable("m2_proportion")
      shinyjs::disable("m2_proportion_err")

      fluidRow(
        shinyjs::useShinyjs(),
        column(width = 6,
               numericInput(inputId = "m3_proportion",
                            label = "p [%]",
                            value = 100-P,
                            min = 0,
                            max = 100,
                            step = 5)),
        column(width = 6,
               textInput(inputId = "m3_proportion_err",
                         label = "\u03B4p",
                         value = P_err))
      )
    }
  })

  #Dc
  output$DcLoc <- renderUI({

    #if(input$geoDcBox){
    if(input$DcRadioButton == "geographical position"){
      fluidRow(
        column(width = 12,
               fluidRow(column(width = 6,
                               textInput(inputId = "latitude",
                                         label = "Latitude")),
                        column(width = 6,
                               textInput(inputId = "longitude",
                                         label = "Longitude"))),

               textInput(inputId = "altitude",
                         label = "Altitude [m]"),

               checkboxInput(inputId = "fieldChangeBox",
                             label = " field change correction",
                             value = TRUE),

               checkboxInput(inputId = "shallowDepthBox",
                             label = "Scale for shallow depth",
                             value = TRUE)
        )
      )
    }
  })

  output$directDc <- renderUI({

    #if(input$directDcBox){
    if(input$DcRadioButton == "in-situ measurement"){

      fluidRow(
        column(width = 6,
               textInput(inputId = "dc",
                         label = "Dc [Gy]")),
        column(width = 6,
               textInput(inputId = "dc_err",
                         label = "\u03B4Dc"))
      )
    }
  })


  # Age estimation
  dr_DATA.Dr <- reactive({

    temp <- input$dracButton

    if(is.null(temp) || temp == 0){
      data <- NULL
    }else{
      data <- dr_generate.Dr()
    }

    return(data)
  })

  dr_generate.Dr <- eventReactive(input$dracButton,{


    de <- input$de
    de <- gsub(",",".", de, fixed = TRUE)
    de <- as.numeric(de)
    if(length(de)==0 || !is.finite(de) ){
      de <- "X"
    }

    de_err <- input$de_err
    de_err <- gsub(",",".", de_err, fixed = TRUE)
    de_err <- as.numeric(de_err)
    if(length(de_err)==0 || !is.finite(de_err) ){
      de_err <- "X"
    }


    material <- input$material

    if(input$DcRadioButton == "in-situ measurement"){
      directDc <- TRUE
    }else{
      directDc <- FALSE
    }

    project <- input$sa_project
    if(project == ""){
      project <- "unknown"
    }else{
      project <- gsub(" ", "", project, fixed = TRUE)
    }
    sample <- input$sa_sample
    if(sample==""){
      sample <- "unknown"
    }else{
      sample <- gsub(" ", "", sample, fixed = TRUE)

    }

    date <- input$sa_date
    date <- as.numeric(format(date,"%Y"))

    mineral <- input$mineral

    conversionFactor <- input$conversionFactor
    alphaSizeFactor <- input$alphaSizeFactor
    betaSizeFactor <- input$betaSizeFactor
    betaEtchFactor <- input$betaEtchFactor

    m1_U <- input$m1_U
    m1_U <- gsub(",",".", m1_U, fixed = TRUE)
    m1_U <- as.numeric(m1_U)
    if(length(m1_U)==0 || !is.finite(m1_U) ){
      m1_U <- "X"
    }
    m1_U_err <- input$m1_U_err
    m1_U_err <- gsub(",",".", m1_U_err, fixed = TRUE)
    m1_U_err <- as.numeric(m1_U_err)
    if(length(m1_U_err)==0 ||!is.finite(m1_U_err)){
      m1_U_err <- "X"
    }
    m1_Th <- input$m1_Th
    m1_Th <- gsub(",",".", m1_Th, fixed = TRUE)
    m1_Th <- as.numeric(m1_Th)
    if(length(m1_Th)==0 ||!is.finite(m1_Th)){
      m1_Th <- "X"
    }
    m1_Th_err <- input$m1_Th_err
    m1_Th_err <- gsub(",",".", m1_Th_err, fixed = TRUE)
    m1_Th_err <- as.numeric(m1_Th_err)
    if(length(m1_Th_err)==0 ||!is.finite(m1_Th_err)){
      m1_Th_err <- "X"
    }
    m1_K <- input$m1_K
    m1_K <- gsub(",",".", m1_K, fixed = TRUE)
    m1_K <- as.numeric(m1_K)
    if(length(m1_K)==0 ||!is.finite(m1_K)){
      m1_K <- "X"
    }
    m1_K_err <- input$m1_K_err
    m1_K_err <- gsub(",",".", m1_K_err, fixed = TRUE)
    m1_K_err <- as.numeric(m1_K_err)
    if(length(m1_K_err)==0 ||!is.finite(m1_K_err)){
      m1_K_err <- "X"
    }
    m1_Rb <- input$m1_Rb
    m1_Rb <- gsub(",",".", m1_Rb, fixed = TRUE)
    m1_Rb <- as.numeric(m1_Rb)
    if(length(m1_Rb)==0 ||!is.finite(m1_Rb)){
      m1_Rb <- "X"
    }
    m1_Rb_err <- input$m1_Rb_err
    m1_Rb_err <- gsub(",",".", m1_Rb_err, fixed = TRUE)
    m1_Rb_err <- as.numeric(m1_Rb_err)
    if(length(m1_Rb_err)==0 ||!is.finite(m1_Rb_err)){
      m1_Rb_err <- "X"
    }

    if(is.logical(input$m1_K2RbBox) && input$m1_K2RbBox){
      m1_K2Rb <- TRUE
    }else{
      m1_K2Rb <-  FALSE
    }

    m1_alpha <- input$m1_alpha
    m1_alpha <- gsub(",",".", m1_alpha, fixed = TRUE)
    m1_alpha <- as.numeric(m1_alpha)
    if(length(m1_alpha)==0 ||!is.finite(m1_alpha)){
      m1_alpha <- "X"
    }
    m1_alpha_err <- input$m1_alpha_err
    m1_alpha_err <- gsub(",",".", m1_alpha_err, fixed = TRUE)
    m1_alpha_err <- as.numeric(m1_alpha_err)
    if(length(m1_alpha_err)==0 ||!is.finite(m1_alpha_err)){
      m1_alpha_err <- "X"
    }
    m1_beta <- input$m1_beta
    m1_beta <- gsub(",",".", m1_beta, fixed = TRUE)
    m1_beta <- as.numeric(m1_beta)
    if(length(m1_beta)==0 ||!is.finite(m1_beta)){
      m1_beta <- "X"
    }
    m1_beta_err <- input$m1_beta_err
    m1_beta_err <- gsub(",",".", m1_beta_err, fixed = TRUE)
    m1_beta_err <- as.numeric(m1_beta_err)
    if(length(m1_beta_err)==0 ||!is.finite(m1_beta_err)){
      m1_beta_err <- "X"
    }
    m1_gamma <- input$m1_gamma
    m1_gamma <- gsub(",",".", m1_gamma, fixed = TRUE)
    m1_gamma <- as.numeric(m1_gamma)
    if(length(m1_gamma)==0 ||!is.finite(m1_gamma)){
      m1_gamma <- "X"
    }
    m1_gamma_err <- input$m1_gamma_err
    m1_gamma_err <- gsub(",",".", m1_gamma_err, fixed = TRUE)
    m1_gamma_err <- as.numeric(m1_gamma_err)
    if(length(m1_gamma_err)==0 ||!is.finite(m1_gamma_err)){
      m1_gamma_err <- "X"
    }

    m2_U <- input$m2_U
    m2_U <- gsub(",",".", m2_U, fixed = TRUE)
    m2_U <- as.numeric(m2_U)
    if(length(m2_U)==0 ||!is.finite(m2_U)){
      m2_U <- "X"
    }
    m2_U_err <- input$m2_U_err
    m2_U_err <- gsub(",",".", m2_U_err, fixed = TRUE)
    m2_U_err <- as.numeric(m2_U_err)
    if(length(m2_U_err)==0 ||!is.finite(m2_U_err)){
      m2_U_err <- "X"
    }
    m2_Th <- input$m2_Th
    m2_Th <- gsub(",",".", m2_Th, fixed = TRUE)
    m2_Th <- as.numeric(m2_Th)
    if(length(m2_Th)==0 ||!is.finite(m2_Th)){
      m2_Th <- "X"
    }
    m2_Th_err <- input$m2_Th_err
    m2_Th_err <- gsub(",",".", m2_Th_err, fixed = TRUE)
    m2_Th_err <- as.numeric(m2_Th_err)
    if(length(m2_Th_err)==0 ||!is.finite(m2_Th_err)){
      m2_Th_err <- "X"
    }
    m2_K <- input$m2_K
    m2_K <- gsub(",",".", m2_K, fixed = TRUE)
    m2_K <- as.numeric(m2_K)
    if(length(m2_K)==0 ||!is.finite(m2_K)){
      m2_K <- "X"
    }
    m2_K_err <- input$m2_K_err
    m2_K_err <- gsub(",",".", m2_K_err, fixed = TRUE)
    m2_K_err <- as.numeric(m2_K_err)
    if(length(m2_K_err)==0 ||!is.finite(m2_K_err)){
      m2_K_err <- "X"
    }
    m2_Rb <- input$m2_Rb
    m2_Rb <- gsub(",",".", m2_Rb, fixed = TRUE)
    m2_Rb <- as.numeric(m2_Rb)
    if(length(m2_Rb)==0 ||!is.finite(m2_Rb)){
      m2_Rb <- "X"
    }
    m2_Rb_err <- input$m2_Rb_err
    m2_Rb_err <- gsub(",",".", m2_Rb_err, fixed = TRUE)
    m2_Rb_err <- as.numeric(m2_Rb_err)
    if(length(m2_Rb_err)==0 ||!is.finite(m2_Rb_err)){
      m2_Rb_err <- "X"
    }

    if(is.logical(input$m2_K2RbBox) && input$m2_K2RbBox){
      m2_K2Rb <- TRUE
    }else{
      m2_K2Rb <- FALSE
    }

    m2_alpha <- input$m2_alpha
    m2_alpha <- gsub(",",".", m2_alpha, fixed = TRUE)
    m2_alpha <- as.numeric(m2_alpha)
    if(length(m2_alpha)==0 ||!is.finite(m2_alpha)){
      m2_alpha <- "X"
    }
    m2_alpha_err <- input$m2_alpha_err
    m2_alpha_err <- gsub(",",".", m2_alpha_err, fixed = TRUE)
    m2_alpha_err <- as.numeric(m2_alpha_err)
    if(length(m2_alpha_err)==0 ||!is.finite(m2_alpha_err)){
      m2_alpha_err <- "X"
    }
    m2_beta <- input$m2_beta
    m2_beta <- gsub(",",".", m2_beta, fixed = TRUE)
    m2_beta <- as.numeric(m2_beta)
    if(length(m2_beta)==0 ||!is.finite(m2_beta)){
      m2_beta <- "X"
    }
    m2_beta_err <- input$m2_beta_err
    m2_beta_err <- gsub(",",".", m2_beta_err, fixed = TRUE)
    m2_beta_err <- as.numeric(m2_beta_err)
    if(length(m2_beta_err)==0 ||!is.finite(m2_beta_err)){
      m2_beta_err <- "X"
    }
    m2_gamma <- input$m2_gamma
    m2_gamma <- gsub(",",".", m2_gamma, fixed = TRUE)
    m2_gamma <- as.numeric(m2_gamma)
    if(length(m2_gamma)==0 ||!is.finite(m2_gamma)){
      m2_gamma <- "X"
    }
    m2_gamma_err <- input$m2_gamma_err
    m2_gamma_err <- gsub(",",".", m2_gamma_err, fixed = TRUE)
    m2_gamma_err <- as.numeric(m2_gamma_err)
    if(length(m2_gamma_err)==0 ||!is.finite(m2_gamma_err)){
      m2_gamma_err <- "X"
    }

    m3_U <- input$m3_U
    m3_U <- gsub(",",".", m3_U, fixed = TRUE)
    m3_U <- as.numeric(m3_U)
    if(length(m3_U)==0 ||!is.finite(m3_U)){
      m3_U <- "X"
    }
    m3_U_err <- input$m3_U_err
    m3_U_err <- gsub(",",".", m3_U_err, fixed = TRUE)
    m3_U_err <- as.numeric(m3_U_err)
    if(length(m3_U_err)==0 ||!is.finite(m3_U_err)){
      m3_U_err <- "X"
    }
    m3_Th <- input$m3_Th
    m3_Th <- gsub(",",".", m3_Th, fixed = TRUE)
    m3_Th <- as.numeric(m3_Th)
    if(length(m3_Th)==0 ||!is.finite(m3_Th)){
      m3_Th <- "X"
    }
    m3_Th_err <- input$m3_Th_err
    m3_Th_err <- gsub(",",".", m3_Th_err, fixed = TRUE)
    m3_Th_err <- as.numeric(m3_Th_err)
    if(length(m3_Th_err)==0 ||!is.finite(m3_Th_err)){
      m3_Th_err <- "X"
    }
    m3_K <- input$m3_K
    m3_K <- gsub(",",".", m3_K, fixed = TRUE)
    m3_K <- as.numeric(m3_K)
    if(length(m3_K)==0 ||!is.finite(m3_K)){
      m3_K <- "X"
    }
    m3_K_err <- input$m3_K_err
    m3_K_err <- gsub(",",".", m3_K_err, fixed = TRUE)
    m3_K_err <- as.numeric(m3_K_err)
    if(length(m3_K_err)==0 ||!is.finite(m3_K_err)){
      m3_K_err <- "X"
    }
    m3_Rb <- input$m3_Rb
    m3_Rb <- gsub(",",".", m3_Rb, fixed = TRUE)
    m3_Rb <- as.numeric(m3_Rb)
    if(length(m3_Rb)==0 ||!is.finite(m3_Rb)){
      m3_Rb <- "X"
    }
    m3_Rb_err <- input$m3_Rb_err
    m3_Rb_err <- gsub(",",".", m3_Rb_err, fixed = TRUE)
    m3_Rb_err <- as.numeric(m3_Rb_err)
    if(length(m3_Rb_err)==0 ||!is.finite(m3_Rb_err)){
      m3_Rb_err <- "X"
    }

    if(is.logical(input$m3_K2RbBox) && input$m3_K2RbBox){
      m3_K2Rb <- TRUE
    }else{
      m3_K2Rb <- FALSE
    }

    m3_alpha <- input$m3_alpha
    m3_alpha <- gsub(",",".", m3_alpha, fixed = TRUE)
    m3_alpha <- as.numeric(m3_alpha)
    if(length(m3_alpha)==0 ||!is.finite(m3_alpha)){
      m3_alpha <- "X"
    }
    m3_alpha_err <- input$m3_alpha_err
    m3_alpha_err <- gsub(",",".", m3_alpha_err, fixed = TRUE)
    m3_alpha_err <- as.numeric(m3_alpha_err)
    if(length(m3_alpha_err)==0 ||!is.finite(m3_alpha_err)){
      m3_alpha_err <- "X"
    }
    m3_beta <- input$m3_beta
    m3_beta <- gsub(",",".", m3_beta, fixed = TRUE)
    m3_beta <- as.numeric(m3_beta)
    if(length(m3_beta)==0 ||!is.finite(m3_beta)){
      m3_beta <- "X"
    }
    m3_beta_err <- input$m3_beta_err
    m3_beta_err <- gsub(",",".", m3_beta_err, fixed = TRUE)
    m3_beta_err <- as.numeric(m3_beta_err)
    if(length(m3_beta_err)==0 ||!is.finite(m3_beta_err)){
      m3_beta_err <- "X"
    }
    m3_gamma <- input$m3_gamma
    m3_gamma <- gsub(",",".", m3_gamma, fixed = TRUE)
    m3_gamma <- as.numeric(m3_gamma)
    if(length(m3_gamma)==0 ||!is.finite(m3_gamma)){
      m3_gamma <- "X"
    }
    m3_gamma_err <- input$m3_gamma_err
    m3_gamma_err <- gsub(",",".", m3_gamma_err, fixed = TRUE)
    m3_gamma_err <- as.numeric(m3_gamma_err)
    if(length(m3_gamma_err)==0 ||!is.finite(m3_gamma_err)){
      m3_gamma_err <- "X"
    }

    if(is.logical(input$shallowDepthBox) && input$shallowDepthBox){
      shallowDepth <- TRUE

    }else{
      shallowDepth <- FALSE
    }

    m1_size_min <- input$m1_size_min
    m1_size_min <- gsub(",",".", m1_size_min, fixed = TRUE)
    m1_size_min <- as.numeric(m1_size_min)
    if(length(m1_size_min)==0 || !is.finite(m1_size_min)){
      m1_size_min <- 1
    }

    m1_size_max <- input$m1_size_max
    m1_size_max <- gsub(",",".", m1_size_max, fixed = TRUE)
    m1_size_max <- as.numeric(m1_size_max)
    if(length(m1_size_max)==0 || !is.finite(m1_size_min) ){
      m1_size_max <- 1000
    }

    m1_etch_min <- input$m1_etch_min
    m1_etch_min <- gsub(",",".", m1_etch_min, fixed = TRUE)
    m1_etch_min <- as.numeric(m1_etch_min)
    if(length(m1_etch_min)==0 || !is.finite(m1_etch_min)){
      m1_etch_min <- 0
    }
    m1_etch_max <- input$m1_etch_max
    m1_etch_max <- gsub(",",".", m1_etch_max, fixed = TRUE)
    m1_etch_max <- as.numeric(m1_etch_max)
    if(length(m1_etch_max)==0 || !is.finite(m1_etch_max) ){
      m1_etch_max <- 30
    }

    m1_aValue <- input$m1_aValue
    m1_aValue <- gsub(",",".", m1_aValue, fixed = TRUE)
    m1_aValue <- as.numeric(m1_aValue)
    if(length(m1_aValue)==0 || !is.finite(m1_aValue)){
      m1_aValue <- "X"
    }
    m1_aValue_err <- input$m1_aValue_err
    m1_aValue_err <- gsub(",",".", m1_aValue_err, fixed = TRUE)
    m1_aValue_err <- as.numeric(m1_aValue_err)
    if(length(m1_aValue_err)==0 || !is.finite(m1_aValue_err) ){
      m1_aValue_err <- "X"
    }

    m1_water <- input$m1_water
    m1_water <- gsub(",",".", m1_water, fixed = TRUE)
    m1_water <- as.numeric(m1_water)
    if(length(m1_water)==0 || !is.finite(m1_water)){
      m1_water <- 0
    }
    m1_water_err <- input$m1_water_err
    m1_water_err <- gsub(",",".", m1_water_err, fixed = TRUE)
    m1_water_err <- as.numeric(m1_water_err)
    if(length(m1_water_err)==0 || !is.finite(m1_water_err)){
      m1_water_err <- 0
    }

    m1_density <- input$m1_density
    m1_density <- gsub(",",".", m1_density, fixed = TRUE)
    m1_density <- as.numeric(m1_density)
    m1_density <- as.numeric(input$m1_density)
    if(length(m1_density)==0  || !is.finite(m1_density)){
      m1_density <- "X"
    }
    m1_density_err <- input$m1_density_err
    m1_density_err <- gsub(",",".", m1_density_err, fixed = TRUE)
    m1_density_err <- as.numeric(m1_density_err)
    if(length(m1_density_err)==0 || !is.finite(m1_density_err)){
      m1_density_err <- "X"
    }

    m2_water <- input$m2_water
    m2_water <- gsub(",",".", m2_water, fixed = TRUE)
    m2_water <- as.numeric(m2_water)
    if(length(m2_water)==0 || !is.finite(m2_water)){
      m2_water <- 0
    }
    m2_water_err <- input$m2_water_err
    m2_water_err <- gsub(",",".", m2_water_err, fixed = TRUE)
    m2_water_err <- as.numeric(m2_water_err)
    if(length(m2_water_err)==0 || !is.finite(m2_water_err)){
      m2_water_err <- 0
    }

    m2_density <- input$m2_density
    m2_density <- gsub(",",".", m2_density, fixed = TRUE)
    m2_density <- as.numeric(m2_density)
    if(length(m2_density)==0 || !is.finite(m2_density)){
      m2_density <- "X"
    }
    m2_density_err <- input$m2_density_err
    m2_density_err <- gsub(",",".", m2_density_err, fixed = TRUE)
    m2_density_err <- as.numeric(m2_density_err)
    if(length(m2_density_err)==0 || !is.finite(m2_density_err)){
      m2_density_err <- "X"
    }

    m3_water <- input$m3_water
    m3_water <- gsub(",",".", m3_water, fixed = TRUE)
    m3_water <- as.numeric(m3_water)
    if(length(m3_water)==0 || !is.finite(m3_water)){
      m3_water <- 0
    }
    m3_water_err <- input$m3_water_err
    m3_water_err <- gsub(",",".", m3_water_err, fixed = TRUE)
    m3_water_err <- as.numeric(m3_water_err)
    if(length(m3_water_err)==0 || !is.finite(m3_water_err)){
      m3_water_err <- 0
    }

    m3_density <- input$m3_density
    m3_density <- gsub(",",".", m3_density, fixed = TRUE)
    m3_density <- as.numeric(m3_density)
    if(length(m3_density)==0 || !is.finite(m3_density) ){
      m3_density <- "X"
    }
    m3_density_err <- input$m3_density_err
    m3_density_err <- gsub(",",".", m3_density_err, fixed = TRUE)
    m3_density_err <- as.numeric(m3_density_err)
    if(length(m3_density_err)==0 || !is.finite(m3_density_err) ){
      m3_density_err <- "X"
    }

    m2_proportion <- input$m2_proportion
    m2_proportion <- gsub(",",".", m2_proportion, fixed = TRUE)
    m2_proportion <- as.numeric(m2_proportion)
    m2_proportion <- m2_proportion/100
    m2_proportion_err <- input$m2_proportion_err
    m2_proportion_err <- gsub(",",".", m2_proportion_err, fixed = TRUE)
    m2_proportion_err <- as.numeric(m2_proportion_err)
    m2_proportion_err <- m2_proportion_err/100

    m3_proportion <- input$m3_proportion
    m3_proportion <- gsub(",",".", m3_proportion, fixed = TRUE)
    m3_proportion <- as.numeric(m3_proportion)
    m3_proportion <- m3_proportion/100
    m3_proportion_err <- input$m3_proportion_err
    m3_proportion_err <- gsub(",",".", m3_proportion_err, fixed = TRUE)
    m3_proportion_err <- as.numeric(m3_proportion_err)
    m3_proportion_err <- m3_proportion_err/100

    depth <- input$depth
    depth <- gsub(",",".", depth, fixed = TRUE)
    depth <- as.numeric(depth)
    if(length(depth)==0|| !is.finite(depth)){
      depth <- "X"
    }
    depth_err <- input$depth_err
    depth_err <- gsub(",",".", depth_err, fixed = TRUE)
    depth_err <- as.numeric(depth_err)
    if(length(depth_err)==0 || !is.finite(depth_err)){
      depth_err <- "X"
    }

    if(is.logical(input$fieldChangeBox) && input$fieldChangeBox){
      fieldChange <- TRUE
    }else{
      fieldChange <- FALSE
    }


    latitude <- input$latitude
    latitude <- gsub(",",".", latitude, fixed = TRUE)
    latitude <- as.numeric(latitude)
    longitude <- input$longitude
    longitude <- gsub(",",".", longitude, fixed = TRUE)
    longitude <- as.numeric(longitude)
    if(length(latitude)==0 || length(longitude)==0 || !is.finite(latitude) || !is.finite(longitude) || directDc){
      latitude <- "X"
      longitude <- "X"
    }
    altitude <- input$altitude
    altitude <- gsub(",",".", altitude, fixed = TRUE)
    altitude <- as.numeric(altitude)
    if(length(altitude)==0 || !is.finite(altitude) || directDc){
      altitude <- "X"
    }

    dc <- input$dc
    dc <- gsub(",",".", dc, fixed = TRUE)
    dc <- as.numeric(dc)
    if(length(dc)==0 || !is.finite(dc) || !directDc){
      dc <- "X"
    }
    dc_err <- input$dc_err
    dc_err <- gsub(",",".", dc_err, fixed = TRUE)
    dc_err <- as.numeric(dc_err)
    if(length(dc_err)==0 || !is.finite(dc_err) || !directDc){
      dc_err <- "X"
    }

    if(material == "sediment"){
      data <- template_DRAC(notification = FALSE)

      data$`Project ID` <- project
      data$`Sample ID` <- sample
      data$Mineral <- mineral
      data$`Conversion factors` <- conversionFactor

      data$`ExternalU (ppm)` <- m2_U
      data$`errExternal U (ppm)` <- m2_U_err
      data$`External Th (ppm)`  <- m2_Th
      data$`errExternal Th (ppm)` <- m2_Th_err
      data$`External K (%)` <- m2_K
      data$`errExternal K (%)` <- m2_K_err
      data$`External Rb (ppm)` <- m2_Rb
      data$`errExternal Rb (ppm)` <- m2_Rb_err

      if(m2_K2Rb){
        m2_K2Rb <- "Y"
      }else{
        m2_K2Rb <- "N"
      }
      data$`Calculate external Rb from K conc?` <- m2_K2Rb

      data$`Internal U (ppm)` <- m1_U
      data$`errInternal U (ppm)` <- m1_U_err
      data$`Internal Th (ppm)` <- m1_Th
      data$`errInternal Th (ppm)` <- m1_Th_err
      data$`Internal K (%)` <- m1_K
      data$`errInternal K (%)` <- m1_K_err
      data$`Rb (ppm)` <- m1_Rb
      data$`errRb (ppm)` <- m1_Rb_err

      if(m1_K2Rb){
        m1_K2Rb <- "Y"
      }else{
        m1_K2Rb <- "N"
      }
      data$`Calculate internal Rb from K conc?` <- m1_K2Rb

      data$`User external alphadoserate (Gy.ka-1)` <- m2_alpha
      data$`errUser external alphadoserate (Gy.ka-1)` <- m2_alpha_err
      data$`User external betadoserate (Gy.ka-1)` <- m2_beta
      data$`errUser external betadoserate (Gy.ka-1)` <- m2_beta_err
      data$`User external gamma doserate (Gy.ka-1)` <- m2_gamma
      data$`errUser external gammadoserate (Gy.ka-1)` <- m2_gamma_err

      if(!("X" %in% c(m1_alpha, m1_alpha_err, m1_beta, m1_beta_err))){
        data$`User internal doserate (Gy.ka-1)` <- sum(m1_alpha,m1_beta,na.rm = TRUE)
        data$`errUser internal doserate (Gy.ka-1)` <- sqrt(sum(m1_alpha_err^2,m1_beta_err^2,na.rm = TRUE))

      }else if("X" %in% c(m1_alpha, m1_alpha_err)){
        data$`User internal doserate (Gy.ka-1)` <- m1_beta
        data$`errUser internal doserate (Gy.ka-1)` <- m1_beta_err

      }else if("X" %in% c(m1_beta, m1_beta_err)){
        data$`User internal doserate (Gy.ka-1)` <- m1_alpha
        data$`errUser internal doserate (Gy.ka-1)` <- m1_alpha_err

      }else{
        data$`User internal doserate (Gy.ka-1)` <- "X"
        data$`errUser internal doserate (Gy.ka-1)` <- "X"
      }


      if(shallowDepth){
        shallowDepth <- "Y"
      }else{
        shallowDepth <- "N"
      }
      data$`Scale gammadoserate at shallow depths?` <- shallowDepth

      data$`Grain size min (microns)` <- m1_size_min
      data$`Grain size max (microns)`  <- m1_size_max

      data$`alpha-Grain size attenuation` <- alphaSizeFactor
      data$`beta-Grain size attenuation ` <- betaSizeFactor

      data$`Etch depth min (microns)` <- m1_etch_min
      data$`Etch depth max (microns)` <- m1_etch_max

      data$`beta-Etch depth attenuation factor` <- betaEtchFactor

      data$`a-value` <- m1_aValue
      data$`erra-value` <- m1_aValue_err

      data$`Water content ((wet weight - dry weight)/dry weight) %` <- m2_water
      data$`errWater content %` <- m2_water_err

      data$`Depth (m)` <- depth
      data$`errDepth (m)` <- depth_err

      data$`Overburden density (g cm-3)` <- m2_density
      data$`errOverburden density (g cm-3)`<- m2_density_err

      data$`Latitude (decimal degrees)` <- latitude
      data$`Longitude (decimal degrees)` <- longitude
      data$`Altitude (m)` <- altitude

      data$`User cosmicdoserate (Gy.ka-1)` <- dc
      data$`errUser cosmicdoserate (Gy.ka-1)` <- dc_err

      data$`De (Gy)` <- de
      data$`errDe (Gy)`<- de_err

      # Use_DRAC
      res <- try(use_DRAC(file = data,
                          name= "shinyDRAC",
                          verbose=FALSE),
                 silent = TRUE)

      if(class(res) == "try-error"){
        result <- NULL

      }else{
        DRAC.age <- as.numeric(res$DRAC$highlights$`Age (ka)`[1])
        DRAC.age.err <- as.numeric(res$DRAC$highlights$`errAge (ka)`[1])

        int.alpha <- as.numeric(res$DRAC$highlights$`Internal Dry alphadoserate (Gy.ka-1)`[1])
        int.alpha.err <- as.numeric(res$DRAC$highlights$`Internal Dry erralphadoserate (Gy.ka-1)`[1])

        int.beta <- as.numeric(res$DRAC$highlights$`Internal Dry betadoserate (Gy.ka-1)`[1])
        int.beta.err <- as.numeric(res$DRAC$highlights$`Internal Dry errbetadoserate (Gy.ka-1)`[1])

        ext.alpha <- as.numeric(res$DRAC$highlights$`Water corrected alphadoserate`[1])
        ext.alpha.err <- as.numeric(res$DRAC$highlights$`Water corrected erralphadoserate`[1])

        ext.beta <- as.numeric(res$DRAC$highlights$`Water corrected betadoserate`[1])
        ext.beta.err <- as.numeric(res$DRAC$highlights$`Water corrected errbetadoserate`[1])

        ext.gamma <- as.numeric(res$DRAC$highlights$`Water corrected gammadoserate (Gy.ka-1)`[1])
        ext.gamma.err <- as.numeric(res$DRAC$highlights$`Water corrected errgammadoserate (Gy.ka-1)`[1])

        cosmic <- as.numeric(res$DRAC$highlights$`Cosmicdoserate (Gy.ka-1)`[1])
        cosmic.err <- as.numeric(res$DRAC$highlights$`errCosmicdoserate (Gy.ka-1)`[1])

        DRAC.int.Dr <- as.numeric(res$DRAC$highlights$`Internal doserate (Gy.ka-1)`[1])
        DRAC.int.Dr.err <- as.numeric(res$DRAC$highlights$`Internal errdoserate (Gy.ka-1)`[1])

        DRAC.ext.Dr <- ext.alpha+ext.beta+ext.gamma
        DRAC.ext.Dr.err <- sqrt(sum(ext.alpha.err^2, ext.beta.err^2, ext.gamma.err^2) )

        DRAC.env.Dr <- 0
        DRAC.env.Dr.err <- 0

        DRAC.alpha.Dr <- int.alpha+ext.alpha
        DRAC.alpha.Dr.err <- sqrt(sum(int.alpha.err^2, ext.alpha.err^2))

        DRAC.beta.Dr <- int.beta+ext.beta
        DRAC.beta.Dr.err <- sqrt(sum(int.beta.err^2, ext.beta.err^2))

        DRAC.gamma.Dr <- ext.gamma
        DRAC.gamma.Dr.err <- ext.gamma.err

        DRAC.cosmic.Dr <- cosmic
        DRAC.cosmic.Dr.err <- cosmic.err

        DRAC.Dr <- sum(DRAC.alpha.Dr,
                       DRAC.beta.Dr,
                       DRAC.gamma.Dr,
                       DRAC.cosmic.Dr)


        DRAC.Dr.err <- sqrt(sum(DRAC.alpha.Dr.err^2,
                                DRAC.beta.Dr.err^2,
                                DRAC.gamma.Dr.err^2,
                                DRAC.cosmic.Dr.err^2))

        DRAC.result <- list(Age = DRAC.age,
                            Age.err =DRAC.age.err,
                            De=de,
                            De.err = de_err,
                            Dr = DRAC.Dr,
                            Dr.err = DRAC.Dr.err,
                            int.Dr = DRAC.int.Dr,
                            int.Dr.err = DRAC.int.Dr.err,
                            ext.Dr = DRAC.ext.Dr,
                            ext.Dr.err = DRAC.ext.Dr.err,
                            env.Dr = DRAC.env.Dr,
                            env.Dr.err = DRAC.env.Dr.err,
                            alpha.Dr = DRAC.alpha.Dr,
                            alpha.Dr.err = DRAC.alpha.Dr.err,
                            beta.Dr = DRAC.beta.Dr,
                            beta.Dr.err = DRAC.beta.Dr.err,
                            gamma.Dr = DRAC.gamma.Dr,
                            gamma.Dr.err = DRAC.gamma.Dr.err,
                            cosmic.Dr = DRAC.cosmic.Dr,
                            cosmic.Dr.err = DRAC.cosmic.Dr.err)

        comment <- ""

        temp.result <- list(age=DRAC.age,
                            age.err=DRAC.age.err,
                            Dr=DRAC.Dr,
                            Dr.err=DRAC.Dr.err,
                            DRAC = DRAC.result,
                            R = DRAC.result,
                            comment=comment)

        result <- set_TLum.Results(data = temp.result)
      }

    }else if(material == "flint"){

      data <- template_DRAC4flint()

      data$info$project <- project
      data$info$sample <- sample
      data$info$date <- date
      data$info$mineral <- mineral
      data$info$conversion.factors <- conversionFactor
      data$info$alpha.size.attenuation <- alphaSizeFactor
      data$info$beta.size.attenuation <- betaSizeFactor
      data$info$beta.etch.attenuation <- betaEtchFactor

      data$De$De <- de
      data$De$De.err <- de_err

      data$flint$Dr$U <- m1_U
      data$flint$Dr$U.err <- m1_U_err
      data$flint$Dr$Th <- m1_Th
      data$flint$Dr$Th.err <- m1_Th_err
      data$flint$Dr$K <- m1_K
      data$flint$Dr$K.err <- m1_K_err
      data$flint$Dr$Rb <- m1_Rb
      data$flint$Dr$Rb.err <- m1_Rb_err
      data$flint$Dr$K2Rb <- m1_K2Rb

      data$flint$Dr$alpha <- m1_alpha
      data$flint$Dr$alpha.err <- m1_alpha_err
      data$flint$Dr$beta <- m1_beta
      data$flint$Dr$gamma <- m1_gamma
      data$flint$Dr$gamma.err <- m1_gamma_err

      data$flint$info$grain.size.min <- m1_size_min
      data$flint$info$grain.size.max <- m1_size_max

      data$flint$info$grain.etch.min <- m1_etch_min
      data$flint$info$grain.etch.max <- m1_etch_max

      data$flint$info$a.value <- m1_aValue
      data$flint$info$a.value.err <- m1_aValue_err
      data$flint$info$water.content <- m1_water
      data$flint$info$water.content.err <- m1_water_err
      data$flint$info$density <- m1_density
      data$flint$info$density.err <- m1_density_err

      data$sediment$Dr$U <- m2_U
      data$sediment$Dr$U.err <- m2_U_err
      data$sediment$Dr$Th <- m2_Th
      data$sediment$Dr$Th.err <- m2_Th_err
      data$sediment$Dr$K <- m2_K
      data$sediment$Dr$K.err <- m2_K_err
      data$sediment$Dr$Rb <- m2_Rb
      data$sediment$Dr$Rb.err <- m2_Rb_err
      data$sediment$Dr$K2Rb <- m2_K2Rb

      data$sediment$Dr$alpha <- m2_alpha
      data$sediment$Dr$alpha.err <- m2_alpha_err
      data$sediment$Dr$beta <- m2_beta
      data$sediment$Dr$gamma <- m2_gamma
      data$sediment$Dr$gamma.err <- m2_gamma_err

      data$sediment$info$water.content <- m1_water
      data$sediment$info$water.content.err <- m1_water_err

      data$sediment$info$density <- m1_density
      data$sediment$info$density.err <- m1_density_err

      data$sediment$info$scale4shallow.depth <- shallowDepth

      data$cosmic$depth <- depth
      data$cosmic$depth.err <- depth_err

      data$cosmic$latitude <- latitude
      data$cosmic$longitude <- longitude
      data$cosmic$altitude <- altitude

      data$cosmic$Dr <- dc
      data$cosmic$Dr.err <- dc_err

      data$cosmic$corr.fieldChanges <- fieldChange

      res <- try(use_DRAC4flint(data))

      if(class(res) == "try-error"){
        result <- NULL

      }else{
        result <- res
      }

    }else if(material == "ceramic"){
      data <- template_DRAC4ceramic()

      data$info$project <- project
      data$info$sample <- sample
      data$info$date <- date
      data$info$mineral <- mineral
      data$info$conversion.factors <- conversionFactor
      data$info$alpha.size.attenuation <- alphaSizeFactor
      data$info$beta.size.attenuation <- betaSizeFactor
      data$info$beta.etch.attenuation <- betaEtchFactor

      data$De$De <- de
      data$De$De.err <- de_err

      data$grain$Dr$U <- m1_U
      data$grain$Dr$U.err <- m1_U_err
      data$grain$Dr$Th <- m1_Th
      data$grain$Dr$Th.err <- m1_Th_err
      data$grain$Dr$K <- m1_K
      data$grain$Dr$K.err <- m1_K_err
      data$grain$Dr$Rb <- m1_Rb
      data$grain$Dr$Rb.err <- m1_Rb_err
      data$grain$Dr$K2Rb <- m1_K2Rb

      data$grain$Dr$alpha <- m1_alpha
      data$grain$Dr$alpha.err <- m1_alpha_err
      data$grain$Dr$beta <- m1_beta
      data$grain$Dr$gamma <- m1_gamma
      data$grain$Dr$gamma.err <- m1_gamma_err

      data$grain$info$grain.size.min <- m1_size_min
      data$grain$info$grain.size.max <- m1_size_max

      data$grain$info$grain.etch.min <- m1_etch_min
      data$grain$info$grain.etch.max <- m1_etch_max

      data$grain$info$a.value <- m1_aValue
      data$grain$info$a.value.err <- m1_aValue_err

      data$ceramic$Dr$U <- m2_U
      data$ceramic$Dr$U.err <- m2_U_err
      data$ceramic$Dr$Th <- m2_Th
      data$ceramic$Dr$Th.err <- m2_Th_err
      data$ceramic$Dr$K <- m2_K
      data$ceramic$Dr$K.err <- m2_K_err
      data$ceramic$Dr$Rb <- m2_Rb
      data$ceramic$Dr$Rb.err <- m2_Rb_err
      data$ceramic$Dr$K2Rb <- m2_K2Rb

      data$ceramic$Dr$alpha <- m2_alpha
      data$ceramic$Dr$alpha.err <- m2_alpha_err
      data$ceramic$Dr$beta <- m2_beta
      data$ceramic$Dr$gamma <- m2_gamma
      data$ceramic$Dr$gamma.err <- m2_gamma_err

      data$ceramic$info$water.content <- m2_water
      data$ceramic$info$water.content.err <- m2_water_err
      data$ceramic$info$density <- m2_density
      data$ceramic$info$density.err <- m2_density_err

      data$sediment$Dr$U <- m3_U
      data$sediment$Dr$U.err <- m3_U_err
      data$sediment$Dr$Th <- m3_Th
      data$sediment$Dr$Th.err <- m3_Th_err
      data$sediment$Dr$K <- m3_K
      data$sediment$Dr$K.err <- m3_K_err
      data$sediment$Dr$Rb <- m3_Rb
      data$sediment$Dr$Rb.err <- m3_Rb_err
      data$sediment$Dr$K2Rb <- m3_K2Rb

      data$sediment$Dr$alpha <- m3_alpha
      data$sediment$Dr$alpha.err <- m3_alpha_err
      data$sediment$Dr$beta <- m3_beta
      data$sediment$Dr$gamma <- m3_gamma
      data$sediment$Dr$gamma.err <- m3_gamma_err

      data$sediment$info$water.content <- m3_water
      data$sediment$info$water.content.err <- m3_water_err
      data$sediment$info$density <- m3_density
      data$sediment$info$density.err <- m3_density_err
      data$sediment$info$scale4shallow.depth <- shallowDepth

      data$cosmic$depth <- depth
      data$cosmic$depth.err <- depth_err

      data$cosmic$latitude <- latitude
      data$cosmic$longitude <- longitude
      data$cosmic$altitude <- altitude

      data$cosmic$Dr <- dc
      data$cosmic$Dr.err <- dc_err

      data$cosmic$corr.fieldChanges <- fieldChange

      res <- try(use_DRAC4ceramic(data))

      if(class(res) == "try-error"){
        result <- NULL
      }else{
        result <- res
      }

    }else if(material == "cave sediment"){
      data <- template_DRAC4cave()

      data$info$project <- project
      data$info$sample <- sample
      data$info$date <- date
      data$info$mineral <- mineral
      data$info$conversion.factors <- conversionFactor
      data$info$alpha.size.attenuation <- alphaSizeFactor
      data$info$beta.size.attenuation <- betaSizeFactor
      data$info$beta.etch.attenuation <- betaEtchFactor

      data$De$De <- de
      data$De$De.err <- de_err

      data$grain$Dr$U <- m1_U
      data$grain$Dr$U.err <- m1_U_err
      data$grain$Dr$Th <- m1_Th
      data$grain$Dr$Th.err <- m1_Th_err
      data$grain$Dr$K <- m1_K
      data$grain$Dr$K.err <- m1_K_err
      data$grain$Dr$Rb <- m1_Rb
      data$grain$Dr$Rb.err <- m1_Rb_err
      data$grain$Dr$K2Rb <- m1_K2Rb

      data$grain$Dr$alpha <- m1_alpha
      data$grain$Dr$alpha.err <- m1_alpha_err
      data$grain$Dr$beta <- m1_beta
      data$grain$Dr$gamma <- m1_gamma
      data$grain$Dr$gamma.err <- m1_gamma_err

      data$grain$info$grain.size.min <- m1_size_min
      data$grain$info$grain.size.max <- m1_size_max

      data$grain$info$grain.etch.min <- m1_etch_min
      data$grain$info$grain.etch.max <- m1_etch_max

      data$grain$info$a.value <- m1_aValue
      data$grain$info$a.value.err <- m1_aValue_err

      data$sediment$Dr$U <- m2_U
      data$sediment$Dr$U.err <- m2_U_err
      data$sediment$Dr$Th <- m2_Th
      data$sediment$Dr$Th.err <- m2_Th_err
      data$sediment$Dr$K <- m2_K
      data$sediment$Dr$K.err <- m2_K_err
      data$sediment$Dr$Rb <- m2_Rb
      data$sediment$Dr$Rb.err <- m2_Rb_err
      data$sediment$Dr$K2Rb <- m2_K2Rb

      data$sediment$Dr$alpha <- m2_alpha
      data$sediment$Dr$alpha.err <- m2_alpha_err
      data$sediment$Dr$beta <- m2_beta
      data$sediment$Dr$gamma <- m2_gamma
      data$sediment$Dr$gamma.err <- m2_gamma_err

      data$sediment$info$water.content <- m2_water
      data$sediment$info$water.content.err <- m2_water_err
      data$sediment$info$density <- m2_density
      data$sediment$info$density.err <- m2_density_err
      data$sediment$info$scale4shallow.depth <- shallowDepth


      data$rock$Dr$U <- m3_U
      data$rock$Dr$U.err <- m3_U_err
      data$rock$Dr$Th <- m3_Th
      data$rock$Dr$Th.err <- m3_Th_err
      data$rock$Dr$K <- m3_K
      data$rock$Dr$K.err <- m3_K_err
      data$rock$Dr$Rb <- m3_Rb
      data$rock$Dr$Rb.err <- m3_Rb_err
      data$rock$Dr$K2Rb <- m3_K2Rb

      data$rock$Dr$alpha <- m3_alpha
      data$rock$Dr$alpha.err <- m3_alpha_err
      data$rock$Dr$beta <- m3_beta
      data$rock$Dr$gamma <- m3_gamma
      data$rock$Dr$gamma.err <- m3_gamma_err

      data$rock$info$water.content <- m3_water
      data$rock$info$water.content.err <- m3_water_err
      data$rock$info$density <- m3_density
      data$rock$info$density.err <- m3_density_err
      data$rock$info$ratio <- m3_proportion
      data$rock$info$ratio.err <- m3_proportion_err



      data$cosmic$depth <- depth
      data$cosmic$depth.err <- depth_err

      data$cosmic$latitude <- latitude
      data$cosmic$longitude <- longitude
      data$cosmic$altitude <- altitude

      data$cosmic$Dr <- dc
      data$cosmic$Dr.err <- dc_err

      data$cosmic$corr.fieldChanges <- fieldChange

      res <- try(use_DRAC4cave(data))

      if(class(res) == "try-error"){
        result <- NULL

      }else{
        result <- res
      }

    }else if(material == "cave flint"){
      result <- NULL
    }else{
      return(NULL)
    }

    if(!is.null(result)){
      updateNavbarPage(session, "shinyDRAC", "Output")
    }

    return(result)

  })

  ##############################################################################################
  # Output
  ##############################################################################################


  output$outPage <- renderUI({
    fluidRow(column(width = 12,
                    uiOutput("inputTab"),
                    uiOutput("resultTab"))
    )
  })

  output$dracText <- renderUI({

    material <- input$material

    if(!(material %in% c("sediment", "flint",  "ceramic", "cave sediment"))){
      helpText("This context is still under development.")

    }else{

      dr <- dr_DATA.Dr()

      if(is.null(dr)){
        if(input$dracButton > 0){
          helpText("Missing data")
        }else{
          helpText("Waiting for age calculation")
        }
      }else{
        helpText("age calculated")
      }
    }
  })

  # Results

  output$inputTab <- renderUI({
    fluidRow(column(width = 12,
                    h4("Input table"),
                    DT::dataTableOutput(outputId = "concentrationTable"),
                    checkboxInput(inputId = "concentrationTexBox",label = "LaTeX source",
                                  value = FALSE),
                    verbatimTextOutput(outputId = "concentrationTex")
    ))
  })

  output$resultTab <- renderUI({
    fluidRow(column(width = 12,
                    h4("Result table"),
                    DT::dataTableOutput(outputId = "doseRateTable"),
                    checkboxInput(inputId = "doseRateTexBox",label = "LaTeX source",
                                  value = FALSE),
                    verbatimTextOutput(outputId = "doseRateTex")
    ))
  })

  output$concentrationTable <- DT::renderDataTable({
    TABLE.concentration()

  })

  output$concentrationTex <- renderText({
    if(input$concentrationTexBox){
      concentrationTex()
    }else{
      return(NULL)
    }
  })

  TABLE.concentration <- reactive({
    site <- input$sa_site
    sample <- input$sa_sample

    material <- input$material
    if(is.null(material)){
      material <- "sediment"
    }

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    m1_U <- as.numeric(input$m1_U)
    m1_U_err <- as.numeric(input$m1_U_err)
    m1_Th <- as.numeric(input$m1_Th)
    m1_Th_err <- as.numeric(input$m1_Th_err)
    m1_K <- as.numeric(input$m1_K)
    m1_K_err <- as.numeric(input$m1_K_err)

    m1_aValue <- as.numeric(input$m1_aValue)
    m1_aValue_err <- as.numeric(input$m1_aValue_err)

    m2_U <- as.numeric(input$m2_U)
    m2_U_err <- as.numeric(input$m2_U_err)
    m2_Th <- as.numeric(input$m2_Th)
    m2_Th_err <- as.numeric(input$m2_Th_err)
    m2_K <- as.numeric(input$m2_K)
    m2_K_err <- as.numeric(input$m2_K_err)

    m2_water <- as.numeric(input$m2_water)
    m2_water_err <- as.numeric(input$m2_water_err)

    m3_U <- as.numeric(input$m3_U)
    m3_U_err <- as.numeric(input$m3_U_err)
    m3_Th <- as.numeric(input$m3_Th)
    m3_Th_err <- as.numeric(input$m3_Th_err)
    m3_K <- as.numeric(input$m3_K)
    m3_K_err <- as.numeric(input$m3_K_err)

    m3_water <- as.numeric(input$m3_water)
    m3_water_err <- as.numeric(input$m3_water_err)

    if(material == "sediment"){
      table <- data.frame(Site = site,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Site'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "flint"){
      table <- data.frame(Site = site,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Site'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Flint'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "ceramic"){
      table <- data.frame(Site = site,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                          Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                          K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                          a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                          Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                          K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                          water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                          Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                          K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                          water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Site'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Ceramic'),
            tags$th(colspan = 4, 'Sediment')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )

    }else if(material == "cave sediment"){
      table <- data.frame(Site = site,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m_3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                           Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                           K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                           water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Site'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Grain'),
            tags$th(colspan = 4, 'Sediment'),
            tags$th(colspan = 4, 'Rock')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )
    }else if(material == "cave flint"){
      table <- data.frame(Site = site,
                          Sample = sample,
                          Depth = paste(depth, "\u00B1", depth_err),
                          m_1 = data.frame(U = paste(round(m1_U,2), "\u00B1", round(m1_U_err,2)),
                                           Th = paste(round(m1_Th,2), "\u00B1", round(m1_Th_err,2)),
                                           K = paste(round(m1_K,2), "\u00B1", round(m1_K_err,2)),
                                           a = paste(round(m1_aValue,2), "\u00B1", round(m1_aValue_err,2))),
                          m_2 = data.frame(U = paste(round(m2_U,2), "\u00B1", round(m2_U_err,2)),
                                           Th = paste(round(m2_Th,2), "\u00B1", round(m2_Th_err,2)),
                                           K = paste(round(m2_K,2), "\u00B1", round(m2_K_err,2)),
                                           water = paste(round(m2_water,2), "\u00B1", round(m2_water_err,2))),
                          m_3 = data.frame(U = paste(round(m3_U,2), "\u00B1", round(m3_U_err,2)),
                                           Th = paste(round(m3_Th,2), "\u00B1", round(m3_Th_err,2)),
                                           K = paste(round(m3_K,2), "\u00B1", round(m3_K_err,2)),
                                           water = paste(round(m3_water,2), "\u00B1", round(m3_water_err,2)))
      )

      container <- tags$table(
        class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, 'Site'),
            tags$th(rowspan = 2, 'Sample'),
            tags$th(rowspan = 2, 'Depth [m]'),
            tags$th(colspan = 4, 'Flint'),
            tags$th(colspan = 4, 'Sediment'),
            tags$th(colspan = 4, 'Rock')
          ),
          tags$tr(
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "a"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th),
            lapply(c('U [ppm]', 'Th [ppm]', "K [%]", "W [%]"), tags$th)
          )
        )
      )
    }else{

    }

    datatable <- datatable(data = table,
                           container = container,
                           rownames = FALSE
                           , options = list(dom = "t"))
    return(datatable)
  })

  concentrationTex <- reactive({
    site <- input$sa_site
    sample <- input$sa_sample

    material <- input$material
    if(is.null(material)){
      material <- "sediment"
    }

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    m1_U <- as.numeric(input$m1_U)
    m1_U_err <- as.numeric(input$m1_U_err)
    m1_Th <- as.numeric(input$m1_Th)
    m1_Th_err <- as.numeric(input$m1_Th_err)
    m1_K <- as.numeric(input$m1_K)
    m1_K_err <- as.numeric(input$m1_K_err)

    m1_aValue <- as.numeric(input$m1_aValue)
    m1_aValue_err <- as.numeric(input$m1_aValue_err)

    m2_U <- as.numeric(input$m2_U)
    m2_U_err <- as.numeric(input$m2_U_err)
    m2_Th <- as.numeric(input$m2_Th)
    m2_Th_err <- as.numeric(input$m2_Th_err)
    m2_K <- as.numeric(input$m2_K)
    m2_K_err <- as.numeric(input$m2_K_err)

    m2_water <- as.numeric(input$m2_water)
    m2_water_err <- as.numeric(input$m2_water_err)

    m3_U <- as.numeric(input$m3_U)
    m3_U_err <- as.numeric(input$m3_U_err)
    m3_Th <- as.numeric(input$m3_Th)
    m3_Th_err <- as.numeric(input$m3_Th_err)
    m3_K <- as.numeric(input$m3_K)
    m3_K_err <- as.numeric(input$m3_K_err)

    m3_water <- as.numeric(input$m3_water)
    m3_water_err <- as.numeric(input$m3_water_err)


    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    if(material == "sediment"){

      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(material =="flint"){
      table <- c(table,
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Flint} &  \\multicolumn{4}{c|}{Sediment}  \\\\", "\n",
                 "\\cline{4-11}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "ceramic"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Ceramic} & \\multicolumn{4}{c|}{Sediment} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "cave sediment"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{Grain} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")

    }else if(material == "cave flint"){
      table <- c(table,
                 "\\tiny", "\n",
                 "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|c|c|c|c|c|}", "\n",
                 "\\hline", "\n",
                 "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & ","\n",
                 "\\multicolumn{4}{c|}{flint} &  \\multicolumn{4}{c|}{Sediment} & \\multicolumn{4}{c|}{Rock} \\\\", "\n",
                 "\\cline{4-15}", "\n",
                 "& & & U [ppm] & Th [ppm] & K [\\%] & a & U [ppm] & Th [ppm] & K [\\%] & W[\\%] & U [ppm] & Th [ppm] & K [\\%] & W[\\%] \\\\", "\n",
                 "\\hline", "\n")

      table <- c(table,
                 paste(site, "&", sample, "&" ,
                       "$",depth, "\\pm", depth_err, "$", "&",
                       "$",round(m1_U,3) , "\\pm", round(m1_U_err,3), "$", "&",
                       "$",round(m1_Th,3) , "\\pm", round(m1_Th_err,3), "$", "&",
                       "$",round(m1_K,3) , "\\pm", round(m1_K_err,3), "$", "&",
                       "$",round(m1_aValue,3) , "\\pm", round(m1_aValue_err,3), "$", "&",
                       "$",round(m2_U,3) , "\\pm", round(m2_U_err,3), "$", "&",
                       "$",round(m2_Th,3) , "\\pm", round(m2_Th_err,3), "$", "&",
                       "$",round(m2_K,3) , "\\pm", round(m2_K_err,3), "$", "&",
                       "$",round(m2_water,3) , "\\pm", round(m2_water_err,3), "$", "&",
                       "$",round(m3_U,3) , "\\pm", round(m3_U_err,3), "$", "&",
                       "$",round(m3_Th,3) , "\\pm", round(m3_Th_err,3), "$", "&",
                       "$",round(m3_K,3) , "\\pm", round(m3_K_err,3), "$", "&",
                       "$",round(m3_water,3) , "\\pm", round(m3_water_err,3), "$", "\\\\"), "\n")
    }


    table <- c(table,
               "\\hline", "\n",
               "\\end{tabular}", "\n",
               "\\end{table}", "\n",
               "\\end{landscape}", "\n"
    )



    return(table)
  })


  output$doseRateTable <- DT::renderDataTable({
    TABLE.doseRate()
  })

  output$doseRateTex <- renderText({
    if(input$doseRateTexBox){
      doseRateTex()
    }else{
      return(NULL)
    }
  })

  TABLE.doseRate <- reactive({

    material <- input$material

    site <- input$sa_site
    sample <- input$sa_sample

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- numeric()
      alpha.Dr_err <- numeric()
      beta.Dr <- numeric()
      beta.Dr_err <- numeric()
      gamma.Dr <- numeric()
      gamma.Dr_err <- numeric()
      cosmic.Dr <- numeric()
      cosmic.Dr_err <- numeric()

      tot.Dr <- numeric()
      tot.Dr_err <- numeric()

      De <- numeric()
      De_err <- numeric()

      age <- numeric()
      age_err <- numeric()

    }else{
      data <- Dr.values@data

      alpha.Dr <- as.numeric(data$R$alpha.Dr)
      alpha.Dr_err <- as.numeric(data$R$alpha.Dr.err)
      beta.Dr <- as.numeric(data$R$beta.Dr)
      beta.Dr_err <- as.numeric(data$R$beta.Dr.err)
      gamma.Dr <- as.numeric(data$R$gamma.Dr)
      gamma.Dr_err <- as.numeric(data$R$gamma.Dr.err)
      cosmic.Dr <- as.numeric(data$R$cosmic.Dr)
      cosmic.Dr_err <- as.numeric(data$R$cosmic.Dr.err)

      tot.Dr <- as.numeric(data$Dr)
      tot.Dr_err <- as.numeric(data$Dr.err)

      De <- as.numeric(data$R$De)
      De_err <- as.numeric(data$R$De.err)

      age <- as.numeric(data$age)
      age_err <- as.numeric(data$age.err)
    }

    table <- data.frame(site = site,
                        Sample = sample,
                        Depth = paste(depth, "\u00B1", depth_err),
                        Dr = data.frame(alpha = paste(round(alpha.Dr), "\u00B1", round(alpha.Dr_err,3)),
                                        beta = paste(round(beta.Dr,3), "\u00B1", round(beta.Dr_err,3)),
                                        gamma = paste(round(gamma.Dr,3), "\u00B1", round(gamma.Dr_err,3)),
                                        cosmic = paste(round(cosmic.Dr,3), "\u00B1", round(cosmic.Dr_err,3)),
                                        total = paste(round(tot.Dr,3), "\u00B1", round(tot.Dr_err,3)),
                                        De = paste(round(De,3), "\u00B1", round(De_err,3)),
                                        Age = paste(round(age,3), "\u00B1", round(age_err,3))
                                        )
                        )

    container <- tags$table(
      class = 'display',
      tags$thead(
        tags$tr(
          tags$th(rowspan = 2, 'Site'),
          tags$th(rowspan = 2, 'Sample'),
          tags$th(rowspan = 2, 'Depth [m]'),
          tags$th(colspan = 5, 'D\u0309 [Gy/ka]'),
          tags$th(rowspan = 2, 'D\u2091 [Gy]'),
          tags$th(rowspan = 2, 'Age [ka]')
        ),
        tags$tr(
          lapply(c('\u03b1 [Gy/ka]', '\u03b2 [Gy/ka]', "\u03b3 [Gy/ka]", "cosmic [Gy/ka]", "Tot. [Gy/ka]"), tags$th)
        )
      )
    )

    datatable <- datatable(data = table,
                           container = container,
                           rownames = FALSE
                           , options = list(dom = "t"))
    return(datatable)
  })

  doseRateTex <- reactive({

    material <- input$material

    site <- input$sa_site
    sample <- input$sa_sample

    depth <- as.numeric(input$depth)
    depth_err <- as.numeric(input$depth_err)

    Dr.values <- dr_DATA.Dr()

    if(is.null(Dr.values)){
      alpha.Dr <- numeric()
      alpha.Dr_err <- numeric()

      beta.Dr <- numeric()
      beta.Dr_err <- numeric()

      gamma.Dr <- numeric()
      gamma.Dr_err <- numeric()

      cosmic.Dr <- numeric()
      cosmic.Dr_err <- numeric()

      tot.Dr <- numeric()
      tot.Dr_err <- numeric()


      De <- numeric()
      De_err <- numeric()

      age <- numeric()
      age_err <- numeric()

    }else{
      data <- Dr.values@data

      alpha.Dr <- as.numeric(data$R$alpha.Dr)
      alpha.Dr_err <- as.numeric(data$R$alpha.Dr.err)

      beta.Dr <- as.numeric(data$R$beta.Dr)
      beta.Dr_err <- as.numeric(data$R$beta.Dr.err)

      gamma.Dr <- as.numeric(data$R$gamma.Dr)
      gamma.Dr_err <- as.numeric(data$R$gamma.Dr.err)

      cosmic.Dr <- as.numeric(data$R$cosmic.Dr)
      cosmic.Dr_err <- as.numeric(data$R$cosmic.Dr.err)

      tot.Dr <- as.numeric(data$Dr)
      tot.Dr_err <- as.numeric(data$Dr.err)

      De <- as.numeric(data$R$De)
      De_err <- as.numeric(data$R$De.err)

      age <- as.numeric(data$age)
      age_err <- as.numeric(data$age.err)
    }

    table <- c("\\usepackage{multirow}", "\n",
               "\\usepackage{pdflscape}", "\n", "\n",
               "\\begin{landscape}", "\n",
               "\\begin{table}", "\n",
               "\\renewcommand{\\arraystretch}{1.5}", "\n",
               "\\centering", "\n")

    table <- c(table,
               "\\begin{tabular}{|c|c|c|c|c|c|c|c|c|c|}", "\n",
               "\\hline", "\n",
               "\\multirow{2}{*}{Site} & \\multirow{2}{*}{Sample} & \\multirow{2}{*}{Depth} & \\multicolumn{5}{c|}{$\\dot{D}$ [Gy/ka]} & \\multirow{2}{*}{$D_{e}$ [Gy]}& \\multirow{2}{*}{Age [ka]} \\\\", "\n",
               "\\cline{4-8}", "\n",
               "& & & $\\alpha$ [Gy/ka] & $\\beta$ [Gy/ka] & $\\gamma$ [Gy/ka] & $D_c$ [Gy/ka] & Tot. [Gy/ka] & & \\\\", "\n",
               "\\hline", "\n")

    table <- c(table,
               paste(site, "&", sample, "&" ,
                     "$",depth, "\\pm", depth_err, "$", "&",
                     "$",round(alpha.Dr,3) , "\\pm", round(alpha.Dr_err,3), "$", "&",
                     "$",round(beta.Dr,3) , "\\pm", round(beta.Dr_err,3), "$", "&",
                     "$",round(gamma.Dr,3) , "\\pm", round(gamma.Dr_err,3), "$", "&",
                     "$",round(cosmic.Dr,3) , "\\pm", round(cosmic.Dr_err,3), "$", "&",
                     "$",round(tot.Dr,3) , "\\pm", round(tot.Dr_err,3), "$", "&",
                     "$",round(De,3) , "\\pm", round(De_err,3), "$", "&",
                     "$",round(age,3) , "\\pm", round(age_err,3), "$", "\\\\"),
               "\n")

    table <- c(table,
               "\\hline", "\n",
               "\\end{tabular}", "\n",
               "\\end{table}", "\n",
               "\\end{landscape}", "\n"
    )

    return(table)
  })

  ##############################################################################################
  # Help
  ##############################################################################################

  output$helpPage<- renderUI({
    fluidRow(column(width = 12,
                    div(class="helptext",
                        list(
                          h4("Generality"),
                          p("This shiny app rely on different fonction from the R packages 'Luminescence' and 'TLdating'
                                        to produce an estimation of the annual dose rate (D\u0307).
                                        All these functions rely on the web-application DRAC (https://www.aber.ac.uk/en/iges/research-groups/quaternary/luminescence-research-laboratory/dose-rate-calculator/) to produce their results.
                                       "),
                          h4("Input"),
                          p("First, the user have to select the context of the sampling, by default, the app consider that we are dating sediment.
                                       Second, the user have to select the references values that will be used as conversion factor. See the DRAC webpage for more information about the available reference.
                                       Third, the user have to select the kind of measurment he use to determine de different dose rate by ticking the corresponding check boxes.
                                       Forth, the user have to fill in all the visible input box.
                                       Finally the user have to press on the 'Age estimation' button."),
                          h4("Output"),
                          p("Once you press the 'Age estimation' button, a html table containing the results and the mains input is produced.
                                       This table is available in the 'Result' tab.
                                       This tab also contain the TeX code to add these results to a LaTeX manuscript.
                                       This app cannot be use to date multiple sample simultaniously, you have to generate the age of each sample separatly."),
                          h4("Support"),
                          p("This app is currently maintained by David Strebler (david.strebler(at)uni(dash)koeln(dot)de).")
                        ))
                    ))
  })
})
