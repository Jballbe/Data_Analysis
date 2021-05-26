library(shiny)




ui <- fluidPage(
  titlePanel("Application Mean variance analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Source_functions","Choose the source function file"),
      fileInput("My_data","Choose data file (csv) to analyse"),
      fileInput("Pop_class_file","Choose Population Class file"),
      numericInput("nbfactors","How many possible factors are they?",2),
      selectInput("factor","Based on which factor do the analysis",choices=""),
      selectInput("Variable","Select the variable to observe",choices=""),
      actionButton("save", "Save Figures"),
    ),
    mainPanel(
      textOutput("files"),
      tableOutput("Hypothesis"),
      plotOutput("plot"),
      plotlyOutput("plotly")
    )
  )
  
)

server <- function(session,input, output) {
    myenv=new.env()
  #dwlFiles <- observeEvent(input$Ready, {
  output$files <- renderText({
    req(input$Source_functions$name,input$My_data$name,input$Pop_class_file$name)
    source(file=input$Source_functions$name)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr")
    have_library(required_packages = required_packages)
    
    data_file=read.csv(input$My_data$name,header=T)
    population_class=read.csv(file=input$Pop_class_file$name,header=T)
    
    nbfactors=input$nbfactors
    factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
    variable_list=create_fulldataset(population_class,data_file,nbfactors)$variable_list
    full_dataset=create_fulldataset(population_class,data_file,nbfactors)$full_dataset
    

    myenv$full_dataset=full_dataset
    
    
    updateSelectInput(session,"factor","factor",choices=factor_list)
    
    updateSelectInput(session,"Variable","Variable",choices=variable_list)
    print('libraries and files ready')
    
  })
  
  output$Hypothesis <- renderTable( {
    req(input$factor,myenv$full_dataset,input$nbfactors)
    factor=input$factor
    full_dataset=myenv$full_dataset
    
    nbfactors=input$nbfactors
    
    Hypothesis_table=parametric_test(full_dataset,nbfactors,factor)
    myenv$Hypothesis_table=Hypothesis_table
    data.frame(Hypothesis_table)
    
  })
  output$plot <- renderPlot({
    req(input$factor,input$Variable,myenv$full_dataset,input$nbfactors,myenv$Hypothesis_table)
    factor=input$factor
    variable=input$Variable
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    
    formula=as.formula(paste0(variable," ~ ",factor))
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    
    if (variable_test$p<0.05){
      current_dunn_test=dunn_test(full_dataset,formula=formula,p.adjust.method = "bonferroni")
      current_dunn_test=add_xy_position(current_dunn_test,x=factor)
      variable_plot=ggboxplot(full_dataset,x=factor,y=colnames(full_dataset[variable]))+
        stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
    }
    else{
      variable_plot=ggboxplot(full_dataset,x=factor,y=colnames(full_dataset[variable]))+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    }
    variable_plot
    
  })
  
  output$plotly <- renderPlotly({
    req(input$factor,input$Variable,myenv$full_dataset,input$nbfactors,myenv$Hypothesis_table)
    factor=input$factor
    variable=input$Variable
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    formula=as.formula(paste0(variable," ~ ",factor))
    
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    variable_plotly=ggboxplot(full_dataset,x=factor,y=colnames(full_dataset[variable]))+
      labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    
    variable_plotly
  })
}

shinyApp(ui, server)
