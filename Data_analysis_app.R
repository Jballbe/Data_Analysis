library(shiny)
required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets')
install.packages(setdiff(required_packages,rownames(installed.packages())))
print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")

#UI

ui <- fluidPage(
  titlePanel("Application Data Analysis"),
 
  sidebarLayout(
    sidebarPanel(
      fileInput("Source_functions","Choose the source function file"),
      fileInput("Pop_class_file","Choose Population Class file"),
      selectInput("Which_Analysis","Single or Multpile Files analysis",choices=c("","Single File","Multiple Files")),
      conditionalPanel(condition = "input.Which_Analysis == 'Single File'",
                       fileInput("My_data","Choose data file (csv) to analyse")
                       ),
      
      conditionalPanel(condition = "input.Which_Analysis == 'Multiple Files'",
                       numericInput("Number_of_Files",label = 'Number of files to upload',value = 2,min=2,max=7),
                       fileInput("5ms","5ms data file (csv)"),
                       fileInput("10ms","10ms data file (csv)"),
                       fileInput("25ms","25ms data file (csv)"),
                       fileInput("50ms","50ms data file (csv)"),
                       fileInput("100ms","100ms data file (csv)"),
                       fileInput("250ms","250ms data file (csv)"),
                       fileInput("500ms","500ms data file (csv)"),
                       actionButton("proceed_to_multiple_analysis","Proceed")
                       ),
      
      
      
      textOutput("files"),
      numericInput("nbfactors","How many possible factors are they?",2),
      selectInput("myfactor","Factor of analysis",choices=""),
      selectInput("Variable","Select variable to observe",choices=""),

      textInput("stat_name", label= "Enter file name (without _analysis.csv)"),
      actionButton("save_table","Save stat Table"),
      textOutput("dwlstat_table"),
      checkboxInput("points","Display points in plotly"),
      checkboxInput("want_to_save","Save"),
      conditionalPanel(condition = "input.want_to_save == true",
                       selectInput("save","Select saving mode",choices=c("All","Some","Current one")),
                       checkboxGroupInput("Select_variable","Select_variable",choices=""),
                       textInput("file_name", label= "Enter file name (without variable_name_by_factor.pdf)"),
                       actionButton("execute_saving","Save as pdf"),
                       textOutput("dwlFigures"),),
      
      
      
      
      
      
      fileInput("threeDarray","Choose 3D array file"),
      
      selectInput("Variabletoshow","Select Variable to display",choices=""),
      sliderTextInput("whichtime","Time response:",choices="",animate=TRUE)
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("General informations",tableOutput("Hypothesis"),tableOutput("counterglobal"), tableOutput("basic_stats")),
        tabPanel("Current variable",tableOutput("countervariable"),plotOutput("plot"),plotlyOutput("plotly")),
        tabPanel("3D plot",plotlyOutput("plotthreeD"))
      ),
      
      
      
      
    )
  )
  
)

server <- function(session,input, output) {
  #Create a local environment (myenv) to pass variable across different functions
  myenv=new.env()
  myenv$previous_value=0
  myenv$previous_stat_value=0
  myarray=readRDS("Full_response_array")
  myenv$myarray=myarray
  
  output$files <- renderText({
    #The following lines only execute when the files are selected
    req(input$Source_functions$datapath,input$My_data$datapath,input$Pop_class_file$datapath)
    
    source(file=input$Source_functions$datapath)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
    #Check if the user have all required libraries and if not, install them
    have_library(required_packages = required_packages)
    # Prepare the full data table
    
    data_file=read.csv(input$My_data$datapath,header=T)
    population_class=read.csv(file=input$Pop_class_file$datapath,header=T)
    
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
    updateSelectInput(session,"Variable","Select variable to observe",choices=variable_list)
    
    
    # If you want to save only some variable plot, display a checkbox list to select the variables
    if (input$save == 'Some'){
      
      updateCheckboxGroupInput(session,"Select_variable","Select_variable",choices=variable_list)}
    print('Libraries and files successfully loaded')
    
  })
  
  output$plotthreeD <- renderPlotly({
    req(input$threeDarray$name)
    print('here')
    
    myarray=myenv$myarray
    variable=dimnames(myarray)[[2]]
    time=c("5","10","25", "50","100","250","500")
    dimnames(myarray)[[3]]=time
    
    updateSliderTextInput(session,"whichtime","whichtime",choices=time)
    updateSelectInput(session,"Variabletoshow","Variabletoshow",choices=variable)
    
    
    # Current_hypothesis_table=parametric_test(myarray[,,input$whichtime],nbfactors = 1, myfactor="Firing_Type")
    # 
    # formula=as.formula(paste0(input$Variabletoshow," ~Firing_Type"))
    # 
    # if (Current_hypothesis_table["Variance_test",input$Variabletoshow]=="KW"){
    #   variable_test=kruskal_test(myarray[,,input$whichtime],formula = formula)
    # }
    # else{
    #   variable_test=anova_test(myarray[,,input$whichtime],formula = formula)
    # }
    # 
    # #If the test result is significant, perform a pair-wise comparison to know which means are different and create a plot
    # if (variable_test$p<0.05){
    #   current_dunn_test=dunn_test(myarray[,,input$whichtime],formula=formula,p.adjust.method = "bonferroni")
    #   current_dunn_test=add_xy_position(current_dunn_test,x="Firing_Type")
    #   variable_plot=ggboxplot(myarray[,,input$whichtime],x="Firing_Type",y=colnames(myarray[,,input$whichtime][input$Variabletoshow]))+
    #     stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
    #     labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
    # }
    # else{
    #   variable_plot=ggboxplot(myarray[,,input$whichtime],x="Firing_Type",y=colnames(myarray[,,input$whichtime][input$Variabletoshow]))+
    #     labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    # }
    
    #variable_plot
    
    current_time_data=data.frame((myarray[,,as.character(input$whichtime)]))
    
    
      #myarray[,1,elt]=as.factor(myarray[,1,elt])
      for (col in seq(2,length(colnames(current_time_data)))){
        current_time_data[,col]=as.numeric(current_time_data[,col])
      }
    
    variable=input$Variabletoshow
    print(typeof(current_time_data[12,2]))
    print(variable)
    variable_plotly=ggboxplot(current_time_data,x="Firing_Type",y="G_Input_Gain_Slope")
      
    
    if (input$points == TRUE){
      variable_plotly=variable_plotly+geom_jitter(shape=16,position=position_jitter(0.2))
    }
    #Display an interactive plot
    variable_plotly
    
    
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
  
  output$basic_stats <- renderTable({
    req(input$myfactor,myenv$full_dataset,input$nbfactors)
    myfactor=input$myfactor
    full_dataset=myenv$full_dataset
    nbfactors=input$nbfactors
    basic_stats=get_basic_stat(full_dataset = full_dataset, nbfactors = nbfactors, myfactor = myfactor)
    stat_table=basic_stats$stat_table
    myenv$stat_table=basic_stats$stat_table
    myenv$mean_table=basic_stats$mean_table
    myenv$sd_table=basic_stats$sd_table
    data.frame(stat_table)
    
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
  
  output$dwlstat_table<- renderText({
    #Only begin when the user have entered the file name and pushed the button
    req(input$save_table,input$stat_name)
    
    stat_name=input$stat_name
    
    
    mean_table=myenv$mean_table
    sd_table=myenv$sd_table
    
    
    
    
    which_plot=input$save
    
    #Save the selected plots only when the user clicks the button
    if(input$save_table !=0 && input$save_table != myenv$previous_stat_value){
      write.csv(mean_table,file=paste0(stat_name,"_mean.csv"),row.names = TRUE)
      write.csv(sd_table,file=paste0(stat_name,"_sd.csv"),row.names = TRUE)
      myenv$previous_stat_value=input$save_table
      print(paste0("Tables succesfully saved"))
    }
    
    
  })
}
shinyApp(ui, server)
