library(shiny)
required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
install.packages(setdiff(required_packages,rownames(installed.packages())))
print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")

ui <- fluidPage(
  titlePanel("Application Mean variance analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("Source_functions","Choose the source function file"),
      fileInput("My_data","Choose data file (csv) to analyse"),
      fileInput("Pop_class_file","Choose Population Class file"),
      textOutput("files"),
      numericInput("nbfactors","How many possible factors are they?",2),
      selectInput("myfactor","Factor of analysis",choices=""),
      selectInput("Variable","Variable to observe",choices=""),
      checkboxInput("points","Display points in plotly"),
      selectInput("save","Select saving mode",choices=c("All","Some","Current one")),
      checkboxGroupInput("Select_variable","Select_variable",choices=""),
      
      
      
      textInput("file_name", label= "Enter file name (without variable_name.pdf)"),
      
      actionButton("execute_saving","Save as pdf"),
      
      textOutput("dwlFigures")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("General informations",tableOutput("Hypothesis"),tableOutput("counterglobal")),
        tabPanel("Current variable",tableOutput("countervariable"),plotOutput("plot"),plotlyOutput("plotly"))
      ),
      
      
      
      
    )
  )
  
)

server <- function(session,input, output) {
  #Create a local environment to pass variable across different functions
  myenv=new.env()
  myenv$previous_value=0
  
  output$files <- renderText({
    #These lines only execute when the files are selected
    req(input$Source_functions$name,input$My_data$name,input$Pop_class_file$name)
    source(file=input$Source_functions$name)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
    #Check if the user have all required libraries and if not, install them
    have_library(required_packages = required_packages)
    # Prepare the full data table
    data_file=read.csv(input$My_data$name,header=T)
    population_class=read.csv(file=input$Pop_class_file$name,header=T)
    
    nbfactors=input$nbfactors
    factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
    variable_list=create_fulldataset(population_class,data_file,nbfactors)$variable_list
    full_dataset=create_fulldataset(population_class,data_file,nbfactors)$full_dataset
    
    myenv$factor_list=factor_list
    myenv$full_dataset=full_dataset
    myenv$variable_list=variable_list
    myenv$nbvariable=length(variable_list)
    
    #Update the choice selection for factor and variable according to input files
    updateSelectInput(session,"myfactor","Factor of analysis",choices=factor_list)
    updateSelectInput(session,"Variable","Variable to observe",choices=variable_list)
    
    
    # If you want to save only some variable plot, display a checkbox list to select the variables
    if (input$save == 'Some'){
      
      updateCheckboxGroupInput(session,"Select_variable","Select_variable",choices=variable_list)}
    print('Libraries and files successfully loaded')
    
  })
  
  output$Hypothesis <- renderTable( {
    #only begin when the full data table is created
    req(input$myfactor,myenv$full_dataset,input$nbfactors)
    myfactor=input$myfactor
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    #Perform the test to know which parametric test has to be perfomed for each variable
    Hypothesis_table=parametric_test(full_dataset,nbfactors,myfactor)
    myenv$Hypothesis_table=Hypothesis_table
    
    #Display the table
    
    data.frame(Hypothesis_table)
    
  },rownames=TRUE)
  
  output$counterglobal <- renderTable({
    req(input$myfactor,input$nbfactors)
    full_dataset=myenv$full_dataset
    table_count=count_samples(full_dataset = full_dataset,nbfactor=input$nbfactors,myfactor=input$myfactor,nbvariable = myenv$nbvariable)
    data.frame(table_count)
  },rownames=TRUE)
  
  output$countervariable <- renderTable({
    req(input$myfactor,input$nbfactors,input$Variable)
    full_dataset=myenv$full_dataset
    variable_table=count_samples(full_dataset = full_dataset,nbfactor=input$nbfactors,myfactor=input$myfactor,nbvariable = myenv$nbvariable)[input$Variable]
    
    data.frame(variable_table)
  },rownames=TRUE)
  
  output$plot <- renderPlot({
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(input$myfactor,myenv$full_dataset,input$nbfactors,myenv$Hypothesis_table)
    myfactor=input$myfactor
    variable=input$Variable
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    
    formula=as.formula(paste0(variable," ~ ",myfactor))
    
    #Perform the required variance test; Kruskal-Wallis or ANOVA
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    
    #If the test result is significant, perform a pair-wise comparison to know which means are different and create a plot
    if (variable_test$p<0.05){
      current_dunn_test=dunn_test(full_dataset,formula=formula,p.adjust.method = "bonferroni")
      current_dunn_test=add_xy_position(current_dunn_test,x=myfactor)
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable]))+
        stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
    }
    else{
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable]))+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    }
    myenv$current_plot=variable_plot
    #Display the plot
    variable_plot
    
  })
  
  output$plotly <- renderPlotly({
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(input$myfactor,input$Variable,myenv$full_dataset,input$nbfactors,myenv$Hypothesis_table)
    myfactor=input$myfactor
    variable=input$Variable
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    formula=as.formula(paste0(variable," ~ ",myfactor))
    
    #Perform the required variance test; Kruskal-Wallis or ANOVA
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    
    variable_plotly=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable]))+
      labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    
    if (input$points == TRUE){
      variable_plotly=variable_plotly+geom_jitter(shape=16,position=position_jitter(0.2))
    }
    #Display an interactive plot
    variable_plotly
  })
  
  
  
  output$dwlFigures <- renderText({
    #Only begin when the user have entered the file name and pushed the button
    req(input$execute_saving,input$file_name)
    saving_path=input$folder
    file_name=input$file_name
    myfactor=input$myfactor
    full_dataset=myenv$full_dataset
    Hypothesis_table=myenv$Hypothesis_table
    nbfactors=input$nbfactors
    #Select the variable to be saved according to the user's choice
    if(input$save == 'All'){
      variable_to_save=myenv$variable_list
    }
    
    if(input$save == 'Some'){
      variable_to_save=input$Select_variable
    }
    
    if(input$save == "Current one"){
      variable_to_save=input$Variable
    }
    
    which_plot=input$save
    
    #Save the selected plots only when the user clicks the button
    if(input$execute_saving !=0 && input$execute_saving != myenv$previous_value){
      
      saveallfigures(Hypothesis_table = Hypothesis_table, full_dataset = full_dataset, saving_path=saving_path ,file_name = file_name, nbfactors = nbfactors,myfactor = myfactor,variable_to_save=variable_to_save, which_plot)
      myenv$previous_value=input$execute_saving
      print(paste0("Files succesfully saved"))
    }
    
    
  })
}
shinyApp(ui, server)
