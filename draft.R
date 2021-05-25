library(shiny)




ui <- fluidPage(
  actionButton("Ready", "Files selected"),
  actionButton('Go','Run it'),
  fileInput("Source_functions","Choose the source function file"),
  fileInput("My_data","Choose data file (csv) to analyse"),
  fileInput("Pop_class_file","Choose Population Class file"),
  numericInput("nbfactors","How many possible factors are they?",2),
  selectInput("factor","Based on which factor do the analysis",choices=""),
  selectInput("Variable","Select the variable to observe",choices=""),
  plotOutput("plot")
)

server <- function(session,input, output) {
  
  dwlFiles <- observeEvent(input$Ready, {
    source(file=input$Source_functions$name)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr")
    have_library(required_packages = required_packages)
    
    data_file=read.csv(input$My_data$name,header=T)
    population_class=read.csv(file=input$Pop_class_file$name,header=T)
    print('libraries and files ready')
    nbfactors=input$nbfactors
    factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
    variable_list=create_fulldataset(population_class,data_file,nbfactors)$variable_list
    full_dataset=create_fulldataset(population_class,data_file,nbfactors)$full_dataset
    updateSelectInput(session,"factor","factor",choices=factor_list)
    
    updateSelectInput(session,"Variable","Variable",choices=variable_list)
    factor=input$factor
    Hypothesis_table=parametric_test(full_dataset,nbfactors,factor)
  })
  
  Analysis <- observeEvent(input$Go, {
    variable=input$Variable
    
    variable_plot=generate_plot(Hypothesis_table,full_dataset,variable,factor)
  })
  output$plot <- renderPlot({
    Analysis()
  })
}

shinyApp(ui, server)
