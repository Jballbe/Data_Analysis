library(shiny)
required_packages=c("abind","plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets')
install.packages(setdiff(required_packages,rownames(installed.packages())))
print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")

#UI

ui <- fluidPage(
  titlePanel("Application Data Analysis"),
  navbarPage(title = "Files",
             ##1
    tabPanel("Files",
      sidebarLayout(
       sidebarPanel(
        fileInput("Source_functions","Choose the source function file"),
        fileInput("Pop_class_file","Choose Population Class file"),
        selectInput("Which_Analysis","Single or Multpile Files analysis",choices=c("","Single File","Multiple Files")),
        numericInput("nbfactors","How many possible factors are they?",2),
        conditionalPanel(condition = "input.Which_Analysis == 'Single File'",
                         fileInput("My_data","Choose data file (csv) to analyse")
        ),
        
        conditionalPanel(condition = "input.Which_Analysis == 'Multiple Files'",
                        
                         checkboxInput("isfive",'5ms'),
                         checkboxInput("isten",'10ms'),
                         checkboxInput("istwentyfive",'25ms'),
                         checkboxInput("isfifty",'50ms'),
                         checkboxInput("ishundred",'100ms'),
                         checkboxInput("istwohundredfifty",'250ms'),
                         checkboxInput("isfivehundred",'500ms'),
                         
                         fileInput("fivems","5ms data file (csv)"),
                         fileInput("tenms","10ms data file (csv)"),
                         fileInput("twentyfivems","25ms data file (csv)"),
                         fileInput("fiftyms","50ms data file (csv)"),
                         fileInput("hundredms","100ms data file (csv)"),
                         fileInput("twohundredfiftyms","250ms data file (csv)"),
                         fileInput("fivehundredms","500ms data file (csv)"),
                         
                         actionButton("proceed_to_multiple_analysis","Proceed")
        ),
        ),
      mainPanel(textOutput("files"),textOutput("multiplefiles"))
  ),
  
    ),##1
  ##2
      tabPanel(title = "Single File",
               sidebarLayout(sidebarPanel(
                 selectInput("myfactor","Factor of analysis",choices=""),
                 selectInput("Variable","Select variable to observe",choices=""),
                 actionButton("save_table","Save stat Table"),
                 textOutput("dwlstat_table"),
                 conditionalPanel(condition = "input.save_table == true",
                                  textInput("stat_name", label= "Enter file name (without _analysis.csv)")),
                 checkboxInput("points","Display points in plotly"),
                 checkboxInput("want_to_save","Save"),
                 conditionalPanel(condition = "input.want_to_save == true",
                                  selectInput("save","Select saving mode",choices=c("All","Some","Current one")),
                                  checkboxGroupInput("Select_variable","Select_variable",choices=""),
                                  textInput("file_name", label= "Enter file name (without variable_name_by_factor.pdf)"),
                                  actionButton("execute_saving","Save as pdf"),
                                  textOutput("dwlFigures"),),
               ),
               mainPanel(tabsetPanel(tabPanel(title = "General",tableOutput("Hypothesis"),tableOutput("counterglobal"), tableOutput("basic_stats")),
                                     tabPanel(title = "Current variable",tableOutput("countervariable"),plotOutput("plot"),plotlyOutput("plotly")),
                 )),
               ),
      
      ),
      ##2
      
      ##3
      tabPanel(title = "Multiple Files",
               sidebarLayout(
                 sidebarPanel(selectInput("Variabletoshow","Select Variable to display",choices=""),
                              selectInput("multiple_file_factor","Factor of analysis",choices=""),
                             
                              
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(title = "Plots",plotlyOutput("time_evol")),
                   tabPanel(title = "Stats")  
                 )
                   
                 ),
               )
        
      ),
      ##3
     
      
      
      
      
      
      
      fileInput("threeDarray","Choose 3D array file"),
      
      
      sliderTextInput("whichtime","Time response:",choices="",animate=TRUE),
      
   
    # mainPanel(
    #   tabsetPanel(
    #     tabPanel("General informations",tableOutput("Hypothesis"),tableOutput("counterglobal"), tableOutput("basic_stats")),
    #     tabPanel("Current variable",tableOutput("countervariable"),plotOutput("plot"),plotlyOutput("plotly")),
    #     tabPanel("3D plot",plotlyOutput("plotthreeD"))
      ),
      
      
      
      
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
    Species=population_class[,2]
    FT=population_class[,3]
    nbfactors=input$nbfactors
    factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
    variable_list=create_fulldataset(population_class,data_file,nbfactors)$variable_list
    full_dataset=create_fulldataset(population_class,data_file,nbfactors)$full_dataset
    
    myenv$factor_list=factor_list
    myenv$full_dataset=full_dataset
    myenv$variable_list=variable_list
    myenv$nbvariable=length(variable_list)
    myenv$Species=Species
    myenv$FT=FT
    
    #Update the choice selection for factor and variable according to input files
    updateSelectInput(session,"myfactor","Factor of analysis",choices=factor_list)
    updateSelectInput(session,"Variable","Select variable to observe",choices=variable_list)
    
    
    # If you want to save only some variable plot, display a checkbox list to select the variables
    if (input$save == 'Some'){
      
      updateCheckboxGroupInput(session,"Select_variable","Select_variable",choices=variable_list)}
    print('Libraries and files successfully loaded')
    
  })
  
  output$multiplefiles <- renderText({
    req(input$proceed_to_multiple_analysis)
    source(file=input$Source_functions$datapath)
    time_list=c()
    population_class=read.csv(file=input$Pop_class_file$datapath,header=T)
    nb_of_files=0
    file_list=list(NA)
    
    #fullarray=c()
    Species_MF=population_class[,2]
    FT_MF=population_class[,3]
    nbfactors=input$nbfactors
    isfactor_variable_ok=0
    
    if (input$isfive == TRUE){
      nb_of_files=nb_of_files+1
      FR_5ms=read.csv(file = input$fivems$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_5ms,nbfactors=nbfactors,population_class = population_class)
      FR_5ms=results_from_createfulldataset$full_dataset
      factor_list_MF=results_from_createfulldataset$factor_list
      variable_list_MF=results_from_createfulldataset$variable_list
      isfactor_variable_ok=1
      FR_5ms=FR_5ms[,1:length(colnames(FR_5ms))]
      time_list=c(time_list,"5ms")
      file_list[[nb_of_files]] <- FR_5ms
      Species_MF=results_from_createfulldataset$Species
      FT_MF=results_from_createfulldataset$Firing_Type
      
    }
    
    if (input$isten == TRUE){
      nb_of_files=nb_of_files+1
      FR_10ms=read.csv(file = input$tenms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_10ms,nbfactors=nbfactors,population_class = population_class)
      FR_10ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_10ms=FR_10ms[,1:length(colnames(FR_10ms))]
      time_list=c(time_list,"10ms")
      file_list[[nb_of_files]] <- FR_10ms
      
      
    }
    
    if (input$istwentyfive == TRUE){
      nb_of_files=nb_of_files+1
      FR_25ms=read.csv(file = input$twentyfivems$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_25ms,nbfactors=nbfactors,population_class = population_class)
      FR_25ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_25ms=FR_25ms[,1:length(colnames(FR_25ms))]
      time_list=c(time_list,"25ms")
      file_list[[nb_of_files]] <- FR_25ms
    }
    
    if (input$isfifty == TRUE){
      nb_of_files=nb_of_files+1
      FR_50ms=read.csv(file = input$fiftyms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_50ms,nbfactors=nbfactors,population_class = population_class)
      FR_50ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_50ms=FR_50ms[,1:length(colnames(FR_50ms))]
      time_list=c(time_list,"50ms")
      file_list[[nb_of_files]] <- FR_50ms
    }
    
    if (input$ishundred == TRUE){
      nb_of_files=nb_of_files+1
      FR_100ms=read.csv(file = input$hundredms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_100ms,nbfactors=nbfactors,population_class = population_class)
      FR_100ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_100ms=FR_100ms[,1:length(colnames(FR_100ms))]
      time_list=c(time_list,"100ms")
      file_list[[nb_of_files]] <- FR_100ms
    }
    
    if (input$istwohundredfifty == TRUE){
      nb_of_files=nb_of_files+1
      FR_250ms=read.csv(file = input$twohundredfiftyms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_250ms,nbfactors=nbfactors,population_class = population_class)
      FR_250ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_250ms=FR_250ms[,1:length(colnames(FR_250ms))]
      time_list=c(time_list,"250ms")
      file_list[[nb_of_files]] <- FR_250ms
    }
    
    if (input$isfivehundred == TRUE){
      nb_of_files=nb_of_files+1
      FR_500ms=read.csv(file = input$fivehundredms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_500ms,nbfactors=nbfactors,population_class = population_class)
      FR_500ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list_MF=results_from_createfulldataset$factor_list
        variable_list_MF=results_from_createfulldataset$variable_list
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
      }
      FR_500ms=FR_500ms[,1:length(colnames(FR_500ms))]
      time_list=c(time_list,"500ms")
      file_list[[nb_of_files]] <- FR_500ms
    }
    
    threeDarray=abind(file_list,along=3)
    
    myenv$threeDarray=threeDarray
    myenv$time_list_MF=time_list
    myenv$factor_list_MF=factor_list_MF
    myenv$variable_list_MF=variable_list_MF
    updateSelectInput(session,"Variabletoshow","Variabletoshow",choices=variable_list_MF)
    updateSelectInput(session,"multiple_file_factor","Factor of analysis",choices=factor_list_MF)
    
    
    print("multiple file analysis ready")
  })
  
  output$time_evol <- renderPlotly({
    req(input$proceed_to_multiple_analysis)
    variable_list_MF=myenv$variable_list_MF
    factor_list_MF=myenv$factor_list_MF
   
    time_list_MF=myenv$time_list_MF
    threeDarray=myenv$threeDarray
    variable=dimnames(threeDarray)[[2]]
    
    dimnames(threeDarray)[[3]]=time_list_MF
     
    #updateSliderTextInput(session,"whichtime","whichtime",choices=time_list)
    threeDarray
    print(input$Variabletoshow)
    print(typeof(input$Variabletoshow))
    current_data=threeDarray[,as.character(input$Variabletoshow),]
    if (input$multiple_file_factor == "Species"){
      factor_col=myenv$Species
    }
    if (input$multiple_file_factor == "Firing_Type"){
      factor_col=myenv$FT
    }
    time_list=myenv$time_list
    variable_to_analyse=input$VariabletoShow
    prepare_for_ggplot(current_data,factor,time_list,variable_to_analyse)
    
    
    
    
    current_time_data=data.frame((threeDarray[,,as.character(input$whichtime)]))
    
    
    
    
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
