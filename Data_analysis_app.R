library(shiny)
required_packages=c("Cairo","plyr","stringr","abind","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets')
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
        
        numericInput("nbfactors","How many possible factors are they?",2),
        
       
                        
         checkboxInput("isfive",'5ms'),
         checkboxInput("isten",'10ms'),
         checkboxInput("istwentyfive",'25ms'),
         checkboxInput("isfifty",'50ms'),
         checkboxInput("ishundred",'100ms'),
         checkboxInput("istwohundredfifty",'250ms'),
         checkboxInput("isfivehundred",'500ms'),
         
         conditionalPanel("input.isfive == true",fileInput("fivems","5ms data file (csv)")),
         conditionalPanel("input.isten == true",fileInput("tenms","10ms data file (csv)")),
         conditionalPanel("input.istwentyfive == true",fileInput("twentyfivems","25ms data file (csv)")),
         conditionalPanel("input.isfifty == true",fileInput("fiftyms","50ms data file (csv)")),
         conditionalPanel("input.ishundred == true",fileInput("hundredms","100ms data file (csv)")),
         conditionalPanel("input.istwohundredfifty == true",fileInput("twohundredfiftyms","250ms data file (csv)")),
         conditionalPanel("input.isfivehundred == true",fileInput("fivehundredms","500ms data file (csv)")),
         
         
         actionButton("proceed_to_multiple_analysis","Proceed")
        
        ),
      mainPanel(textOutput("checklibraries"),textOutput("multiplefiles"))
  ),
  
    ),##1
 
      
      ##3
      tabPanel(title = "Data Analysis",
               sidebarLayout(
                 sidebarPanel(selectInput("multiple_file_factor","Factor of analysis",choices=""),
                              selectInput("Variabletoshow","Select Variable to display",choices=""),
                              selectInput("Which_time_file","Select time to show",choices="")
                              
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(title = "Over Time",
                            plotlyOutput("time_evol"),
                            actionButton("update_parameters","Modify plot parameters"),actionButton("time_plot_saving","Save plot"),
                            textOutput("t_test_name"),
                            tableOutput("t_test"),
                            actionButton("save_t_test_table","Save t-test table"),
                            textOutput("function_to_save_t_test_table"),
                            tableOutput("basic_stats"),
                            actionButton("save_stat_table","Save stat table"),
                            textOutput("save_time_plot")),
                   
                   tabPanel(title = "Single Time Point",
                            tabsetPanel(
                              tabPanel(title="Stats",
                                       tableOutput("Hypothesis"),
                                       actionButton("save_hypo_table","Save Hypothesis table"),
                                       textOutput("dwlhypo_table"),
                                       textOutput("dwlstat_table")),
                              tabPanel(title="Plots",
                                       tableOutput("countervariable"),
                                       plotOutput("plot"),
                                       plotlyOutput("plotly"),
                                       checkboxInput("points","Display points in plotly"),
                                       actionButton("save_variable_plot","Save plot")),
                                       textOutput("save_var_plot"),)
                                )
                 )
                   
                 ),
               )
               )
        
      )
      ##3
  
      )
      
      
      
      
    
  

server <- function(session,input, output) {
  #Create a local environment (myenv) to pass variable across different functions
  myenv=new.env()
  myenv$previous_value=0
  myenv$previous_value_MF=0
  myenv$previous_stat_value=0

  output$checklibraries <- renderText({
    #The following lines only execute when the files are selected
    req(input$Source_functions$datapath,input$proceed_to_multiple_analysis)
    
    source(file=input$Source_functions$datapath)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
    #Check if the user have all required libraries and if not, install them
    have_library(required_packages = required_packages)
    nbfactors=input$nbfactors
    print('Libraries and files successfully loaded')
  })
  
  
  
  output$multiplefiles <- renderText({
    req(input$proceed_to_multiple_analysis)
    source(file=input$Source_functions$datapath)
    time_list=c()
    population_class=read.csv(file=input$Pop_class_file$datapath,header=T)
    nb_of_files=0
    file_list=list()
    Species_MF=population_class[,2]
    FT_MF=population_class[,3]
    nbfactors=input$nbfactors
    isfactor_variable_ok=0
    
    if (input$isfive == TRUE){
      nb_of_files=nb_of_files+1
      FR_5ms=read.csv(file = input$fivems$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_5ms,nbfactors=nbfactors,population_class = population_class)
      FR_5ms=results_from_createfulldataset$full_dataset
      factor_list=results_from_createfulldataset$factor_list
      variable_list=results_from_createfulldataset$variable_list
      myenv$nbvariable=length(variable_list)
      isfactor_variable_ok=1
     
      
      time_list=c(time_list,"5ms")
      current_list=list("FR_5ms"=FR_5ms)
      file_list=append(file_list,current_list)
      
      Species_MF=results_from_createfulldataset$Species
      FT_MF=results_from_createfulldataset$Firing_Type
      unit_dict=results_from_createfulldataset$unit_dict
      
    }
    
    if (input$isten == TRUE){
      nb_of_files=nb_of_files+1
      FR_10ms=read.csv(file = input$tenms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_10ms,nbfactors=nbfactors,population_class = population_class)
      FR_10ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
     
      time_list=c(time_list,"10ms")
      current_list=list("FR_10ms"=FR_10ms)
      file_list=append(file_list,current_list)
     
      
      
    }
    
    if (input$istwentyfive == TRUE){
      nb_of_files=nb_of_files+1
      FR_25ms=read.csv(file = input$twentyfivems$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_25ms,nbfactors=nbfactors,population_class = population_class)
      FR_25ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
     
      time_list=c(time_list,"25ms")
      
      current_list=list("FR_25ms"=FR_25ms)
      file_list=append(file_list,current_list)
    }
    
    if (input$isfifty == TRUE){
      nb_of_files=nb_of_files+1
      FR_50ms=read.csv(file = input$fiftyms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_50ms,nbfactors=nbfactors,population_class = population_class)
      FR_50ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
     
      time_list=c(time_list,"50ms")
      
      current_list=list("FR_50ms"=FR_50ms)
      file_list=append(file_list,current_list)
    }
    
    if (input$ishundred == TRUE){
      nb_of_files=nb_of_files+1
      FR_100ms=read.csv(file = input$hundredms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_100ms,nbfactors=nbfactors,population_class = population_class)
      FR_100ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
    
      time_list=c(time_list,"100ms")
      
      current_list=list("FR_100ms"=FR_100ms)
      file_list=append(file_list,current_list)
    }
    
    if (input$istwohundredfifty == TRUE){
      nb_of_files=nb_of_files+1
      FR_250ms=read.csv(file = input$twohundredfiftyms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_250ms,nbfactors=nbfactors,population_class = population_class)
      FR_250ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
    
      time_list=c(time_list,"250ms")
      
      current_list=list("FR_250ms"=FR_250ms)
      file_list=append(file_list,current_list)
      
    }
    
    if (input$isfivehundred == TRUE){
      nb_of_files=nb_of_files+1
      FR_500ms=read.csv(file = input$fivehundredms$datapath,header=T)
      results_from_createfulldataset=create_fulldataset(data_file=FR_500ms,nbfactors=nbfactors,population_class = population_class)
      FR_500ms=results_from_createfulldataset$full_dataset
      if (isfactor_variable_ok == 0){
        factor_list=results_from_createfulldataset$factor_list
        variable_list=results_from_createfulldataset$variable_list
        myenv$nbvariable=length(variable_list)
        isfactor_variable_ok=1
        Species_MF=results_from_createfulldataset$Species
        FT_MF=results_from_createfulldataset$Firing_Type
        unit_dict=results_from_createfulldataset$unit_dict
      }
    
      time_list=c(time_list,"500ms")
      
      current_list=list("FR_500ms"=FR_500ms)
      file_list=append(file_list,current_list)
    }
    
    threeDarray=abind(file_list,along=3)
    factor_columns=data.frame(cbind(data.frame(Species_MF),data.frame(FT_MF)))
    colnames(factor_columns)=c("Species","Firing_Type")
    
    updateSelectInput(session,"Variabletoshow","Variable to show",choices=variable_list,selected = variable_list[1])
    updateSelectInput(session,"multiple_file_factor","Factor of analysis",choices=factor_list,selected=factor_list[1])
    
    myenv$factor_columns=factor_columns
    myenv$unit_dict=unit_dict
    myenv$threeDarray=threeDarray
    myenv$time_list_MF=time_list
    myenv$factor_list=factor_list
    myenv$variable_list=variable_list
    myenv$file_list=file_list
    file_list_name=list()
    for (elt in seq((length(file_list)))){
      file_list_name=append(file_list_name,names(file_list)[elt])
    }
    updateSelectInput(session,"Which_time_file","Select time to show",choices=file_list_name,selected=file_list_name[1])
    
    print("Multiple file analysis ready")
  })
 
  output$t_test_name <- renderPrint({
    print("Statistical mean difference from 0 (one-sample t-test, p.val<0.05 = significantly different; X= not enough observation to compute t-test")
  })
  t_test_table_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  observeEvent(input$save_t_test_table,{
    
    showModal(save_t_test_table())
  })
  
  save_t_test_table <- function(failed=FALSE){
    modalDialog(
      
      textInput("folder_to_save_t_test_table","Saving folder (ending with / or \ "),
      
      textInput("file_name_t_test_table", label= "File name (without .csv)"),
      
      span('Please select a directory,',' and file name for saving'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_t_test_table_saving","Save table as csv"),
      )
    )
  }
  
  
  output$function_to_save_t_test_table <- renderText({
    req(t_test_table_vals$saving_path,t_test_table_vals$saving_name)
    my_t_test_table=myenv$my_t_test_table
    write.csv(my_t_test_table,file=paste0(t_test_table_vals$saving_path,t_test_table_vals$saving_name,".csv"),row.names = TRUE)
    print(paste0(t_test_table_vals$saving_name,".csv"," successfully saved!"))
  })
  observeEvent(input$execute_t_test_table_saving,{
    
    if (input$folder_to_save_t_test_table != "" && input$file_name_t_test_table != "" ){
      
      t_test_table_vals$saving_path <- input$folder_to_save_t_test_table
      t_test_table_vals$saving_name <- input$file_name_t_test_table
      
      removeModal()
      
    }
    else{
      showModal(save_t_test_table(failed=TRUE))
    }
  })
  output$t_test <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    t_test_dataset=threeDarray[,as.character(input$Variabletoshow),]
    t_test_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(t_test_dataset)))
    
    t_test_dataset[,"Firing_Type"]=droplevels(t_test_dataset[,"Firing_Type"])
    my_t_test_table=perform_t_test(t_test_dataset,myfactor,my_time_list)
    myenv$my_t_test_table=my_t_test_table
    my_t_test_table
  },rownames = TRUE,
  digits=-4,align = 'c')
   
  output$time_evol <- renderPlotly({
    req(input$proceed_to_multiple_analysis)
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list_MF=myenv$time_list_MF
    threeDarray=myenv$threeDarray
    variable=dimnames(threeDarray)[[2]]
    dimnames(threeDarray)[[3]]=time_list_MF
    updateSliderTextInput(session,"whichtime","whichtime",choices=time_list_MF)
    current_data=threeDarray[,as.character(input$Variabletoshow),]
    factor_columns=myenv$factor_columns
    current_data=cbind(factor_columns,current_data)
    if (input$multiple_file_factor == "Species"){
      factor_col=myenv$Species
    }
    if (input$multiple_file_factor == "Firing_Type"){
      factor_col=myenv$FT
    }
    
    variable_to_analyse=input$Variabletoshow
   
    nbfactors=input$nbfactors
    ggdatatable=prepare_for_ggplot(current_data,time_list_MF,variable_to_analyse,nbfactors)$ggdatatable
    unit_dict=myenv$unit_dict
    
    if (parameters_ggplot$is.perTimeonly == TRUE){
      myplot=ggplot(data=ggdatatable,aes(x=Time,y=.data[[input$Variabletoshow]]))+
             geom_point(alpha=parameters_ggplot$alpha.geompoint,
                        size=parameters_ggplot$size.geompoint)
    }
    else{
      myplot=ggplot(data=ggdatatable,aes(x=Time,y=.data[[input$Variabletoshow]],color=.data[[input$multiple_file_factor]]))+
        geom_point(alpha=parameters_ggplot$alpha.geompoint,
                   size=parameters_ggplot$size.geompoint)
        
    }
    if (parameters_ggplot$is.sd == TRUE){
        sd_table=getsd(ggdatatable,input$multiple_file_factor,variable_to_analyse,perTimeonly=parameters_ggplot$is.perTimeonly)$sd_table
        myplot <- myplot+geom_line(data = sd_table,aes(x=Time,y=SD),
                                   linetype= parameters_ggplot$line_type.sd,
                                   alpha=parameters_ggplot$alpha.geomlinesd,
                                   size=parameters_ggplot$size.geomlinesd)
      }
      
    if (parameters_ggplot$is.mean == TRUE){
        mean_table=getmean(ggdatatable,input$multiple_file_factor,variable_to_analyse,perTimeonly=parameters_ggplot$is.perTimeonly)$mean_table
        myplot=myplot+geom_line(data = mean_table,aes(x=Time,y=Mean),
                                linetype= parameters_ggplot$line_type.mean,
                                alpha=parameters_ggplot$alpha.geomlinemean,
                                size=parameters_ggplot$size.geomlinemean)
      }
  
    myplot=myplot+
      labs(y=as.character(unit_dict[variable_to_analyse]),x='Time(ms)')
    if (parameters_ggplot$is.logscale==TRUE){
      myplot=myplot+scale_x_continuous(trans="log10")
    }
      
    
    myenv$plot_MF=myplot
    myplot
  })
  
  
  time_plot_saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  save_time_plot <- function(failed=FALSE){
    modalDialog(
      
      textInput("folder_to_save_time_plot","Saving folder (ending with / or \ "),
      
      textInput("file_name_time_plot", label= "File name (without .pdf)"),
      
      span('Please select a directory,',' and file name for saving'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_time_plot_saving","Save plot as pdf"),
      )
    )
  }
  
  
  observeEvent(input$time_plot_saving,{
    
    showModal(save_time_plot())
  })
  observeEvent(input$execute_time_plot_saving,{
    
    if (input$folder_to_save_time_plot != "" && input$file_name_time_plot != "" ){
      
      time_plot_saving_vals$saving_path <- input$folder_to_save_time_plot
      time_plot_saving_vals$saving_name <- input$file_name_time_plot
      
      removeModal()
      
    }
    else{
      showModal(save_time_plot(failed=TRUE))
    }
  })
  
  output$save_time_plot <- renderText({
    
    req(time_plot_saving_vals$saving_path,time_plot_saving_vals$saving_name)
    
    ggsave(filename = paste0(time_plot_saving_vals$saving_name,".pdf"),plot=myenv$plot_MF,path=time_plot_saving_vals$saving_path,device = cairo_pdf,width=200,height = 100,units="mm")
    
    print(paste0(time_plot_saving_vals$saving_name,".pdf ","succesfully saved!"))
  })
  
  output$Hypothesis <- renderTable( {
    #only begin when the full data table is created
    #req(input$myfactor,myenv$full_dataset,input$nbfactors)
    myfactor=input$multiple_file_factor
    file_list=myenv$file_list
    
    nbfactors=input$nbfactors
    current_time_dataset=file_list[[input$Which_time_file]]
    myenv$current_time_dataset=current_time_dataset
    #Perform the test to know which parametric test has to be performed for each variable
    Hypothesis_table=parametric_test(current_time_dataset,nbfactors,myfactor)
    myenv$Hypothesis_table=Hypothesis_table
    
    #Display the table
    
    data.frame(Hypothesis_table)
    
  },rownames=TRUE)
  
  output$basic_stats <- renderTable({
    
    myfactor=input$multiple_file_factor
    file_list=myenv$file_list
    current_time_dataset=file_list[[input$Which_time_file]]
    nbfactors=input$nbfactors
    basic_stats=get_basic_stat(current_time_dataset, nbfactors = nbfactors, myfactor = myfactor)
    stat_table=basic_stats$stat_table
    myenv$stat_table=basic_stats$stat_table
    myenv$mean_table=basic_stats$mean_table
    myenv$sd_table=basic_stats$sd_table
    data.frame(stat_table)
    
  },rownames=TRUE)
  
  output$counterglobal <- renderTable({
    req(input$multiple_file_factor,input$nbfactors)
    file_list=myenv$file_list
    current_time_dataset=file_list[[input$Which_time_file]]
    table_count=count_samples(current_time_dataset,nbfactor=input$nbfactors,myfactor=input$multiple_file_factor,nbvariable = myenv$nbvariable)
    data.frame(table_count)
  },rownames=TRUE)
  
  output$countervariable <- renderTable({
    req(input$multiple_file_factor,input$nbfactors,input$Variabletoshow)
    file_list=myenv$file_list
    current_time_dataset=file_list[[input$Which_time_file]]
    variable_table=count_samples(current_time_dataset,nbfactor=input$nbfactors,myfactor=input$multiple_file_factor,nbvariable = myenv$nbvariable)[input$Variabletoshow]
    
    data.frame(variable_table)
  },rownames=TRUE)
  
  output$plot <- renderPlot({
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(input$multiple_file_factor,input$Which_time_file,input$nbfactors,myenv$Hypothesis_table)
    myfactor=input$multiple_file_factor
    variable=input$Variabletoshow
    file_list=myenv$file_list
    current_time_dataset=file_list[[input$Which_time_file]]
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    
    formula=as.formula(paste0(variable," ~ ",myfactor))
    
    
    current_time_dataset[,2]=droplevels(current_time_dataset[,2])
   
    
    #Perform the required variance test; Kruskal-Wallis or ANOVA
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(current_time_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(current_time_dataset,formula = formula)
    }
    
    #If the test result is significant, perform a pair-wise comparison to know which means are different and create a plot
    if (variable_test$p<0.05){
      current_dunn_test=dunn_test(current_time_dataset,formula=formula,p.adjust.method = "bonferroni")
      
      
      current_dunn_test=add_xy_position(current_dunn_test,x=myfactor)
     
      variable_plot=ggboxplot(current_time_dataset,x=myfactor,y=colnames(current_time_dataset[variable]))+
        stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      
    }
    else{
      variable_plot=ggboxplot(current_time_dataset,x=myfactor,y=colnames(current_time_dataset[variable]))+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    }
    myenv$current_plot=variable_plot
    #Display the plot
    variable_plot
    
  })
  
  output$plotly <- renderPlotly({
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(input$multiple_file_factor,input$Variabletoshow,input$Which_time_file,input$nbfactors,myenv$Hypothesis_table)
    myfactor=input$multiple_file_factor
    variable=input$Variabletoshow
    file_list=myenv$file_list
    current_time_dataset=file_list[[input$Which_time_file]]
    nbfactors=input$nbfactors
    Hypothesis_table=myenv$Hypothesis_table
    formula=as.formula(paste0(variable," ~ ",myfactor))
    
    #Perform the required variance test; Kruskal-Wallis or ANOVA
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(current_time_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(current_time_dataset,formula = formula)
    }
    
    variable_plotly=ggboxplot(current_time_dataset,x=myfactor,y=colnames(current_time_dataset[variable]))+
      labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    
    if (input$points == TRUE){
      variable_plotly=variable_plotly+geom_jitter(shape=16,position=position_jitter(0.2))
    }
    #Display an interactive plot
    variable_plotly
  })
  
  observeEvent(input$save_variable_plot,{
    
    showModal(save_variable_plot())
  })
  
  
  variable_plot_saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  save_variable_plot <- function(failed=FALSE){
    modalDialog(
      textInput("folder_to_save_var_plot","Saving folder (ending with / or \ "),
      textInput("file_name_var", label= "File name (without .pdf)"),
      
      span('Please select a directory and file name for saving'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_variable_plot_saving","Save plot as pdf")
      )
    )
  }
  observeEvent(input$execute_variable_plot_saving,{
    if (input$folder_to_save_var_plot != "" && input$file_name_var != "" ){
      
      variable_plot_saving_vals$saving_path <- input$folder_to_save_var_plot
      variable_plot_saving_vals$saving_name <- input$file_name_var
      removeModal()
    }
    else{
      showModal(save_variable_plot(failed=TRUE))
    }
  })
  
  output$save_var_plot <- renderText({
    req(variable_plot_saving_vals$saving_path,
        variable_plot_saving_vals$saving_name,
        myenv$Hypothesis_table,
        input$multiple_file_factor,
        myenv$current_time_dataset,
        input$Variabletoshow
    )
    saving_path=variable_plot_saving_vals$saving_path
    file_name=variable_plot_saving_vals$saving_name
    
    saveallfigures(Hypothesis_table = myenv$Hypothesis_table, full_dataset = myenv$current_time_dataset, saving_path=saving_path ,file_name = file_name, nbfactors = nbfactors,myfactor = input$multiple_file_factor,variable_to_save=input$Variabletoshow)
    print(paste0(variable_plot_saving_vals$saving_name,".pdf succesfully saved!"))
  })
 
  
  
  observeEvent(input$save_stat_table,{
    
    showModal(save_stat_table())
  })
  
  stat_table_saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  save_stat_table <- function(failed=FALSE){
    modalDialog(
      textInput("folder_to_save_stat_table","Saving folder (ending with / or \ "),
      textInput("file_name_stat_table", label= "File name (without _mean/_sd.csv)"),
      
      span('Please select a directory and file name for saving'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_stat_table_saving","Save table")
      )
    )
  }
  observeEvent(input$execute_stat_table_saving,{
    if (input$folder_to_save_stat_table != "" && input$file_name_stat_table != "" ){
      
      stat_table_saving_vals$saving_path <- input$folder_to_save_stat_table
      stat_table_saving_vals$saving_name <- input$file_name_stat_table
      removeModal()
    }
    else{
      showModal(save_stat_table(failed=TRUE))
    }
  })
  
  output$dwlstat_table<- renderText({
    #Only begin when the user have entered the file name and pushed the button
    req(stat_table_saving_vals$saving_path,stat_table_saving_vals$saving_name)
    
    stat_name=stat_table_saving_vals$saving_name
    
    
    mean_table=myenv$mean_table
    sd_table=myenv$sd_table
    
    
    
      write.csv(mean_table,file=paste0(stat_table_saving_vals$saving_path,stat_name,"_mean.csv"),row.names = TRUE)
      write.csv(sd_table,file=paste0(stat_table_saving_vals$saving_path,stat_name,"_sd.csv"),row.names = TRUE)
      print(paste0("Stats tables succesfully saved"))
  })
  
  
  
  observeEvent(input$save_hypo_table,{
    
    showModal(save_hypo_table())
  })
  hypo_table_saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  save_hypo_table <- function(failed=FALSE){
    modalDialog(
      textInput("folder_to_save_hypo_table","Saving folder (ending with / or \ "),
      textInput("file_name_hypo_table", label= "File name (without .csv)"),
      
      span('Please select a directory and file name for saving'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_hypo_table_saving","Save table")
      )
    )
  }
  observeEvent(input$execute_hypo_table_saving,{
    if (input$folder_to_save_hypo_table != "" && input$file_name_hypo_table != "" ){
      
      hypo_table_saving_vals$saving_path <- input$folder_to_save_hypo_table
      hypo_table_saving_vals$saving_name <- input$file_name_hypo_table
      removeModal()
    }
    else{
      showModal(save_hypo_table(failed=TRUE))
    }
  })
  
  output$dwlhypo_table<- renderText({
    #Only begin when the user have entered the file name and pushed the button
    req(hypo_table_saving_vals$saving_path,hypo_table_saving_vals$saving_name)
    
    hypo_name=hypo_table_saving_vals$saving_name
    
    Hypothesis_table=myenv$Hypothesis_table
    
    write.csv(Hypothesis_table,file=paste0(hypo_table_saving_vals$saving_path,hypo_name,".csv"),row.names = TRUE)
   
    print(paste0("Hypothesis tables succesfully saved"))
  })
  
  
  parameters_ggplot <- reactiveValues(
    is.logscale=FALSE,
    alpha.geompoint=0.3,
    size.geompoint=1,
    is.perTimeonly=FALSE,
    is.sd=FALSE,
    size.geomlinesd=1,
    alpha.geomlinesd=1,
    line_type.sd="solid",
    
    is.mean=FALSE,
    size.geomlinemean=1,
    alpha.geomlinemean=1,
    line_type.mean="dashed"
  )
  
  observeEvent(input$update_parameters,{
    showModal(update_parameters_ggplot())})
  
  update_parameters_ggplot <- function(failed=FALSE){
    modalDialog(
      checkboxInput("is.logscale","Display x axis in log scale",value = parameters_ggplot$is.logscale),
      checkboxInput('is.perTimeonly',"Group only per time",value=parameters_ggplot$is.perTimeonly),
      sliderInput("size.geompoint","Point size",min=0,max=1,value=parameters_ggplot$size.geompoint,step=0.05),
      sliderInput("alpha.geompoint","Point opacity",min=0,max=1,step=0.05,value=parameters_ggplot$alpha.geompoint),
      
      checkboxInput("is.sd","Display Standard Deviation",value=parameters_ggplot$is.sd),
      selectInput("line_type.sd","Type of line for SD",choices=c("solid","twodash","longdash","dotted","dotdash","dashed"),selected=parameters_ggplot$line_type.sd),
      sliderInput("size.geomlinesd","Standard deviation line size",min=0,max=1,step=0.05,value=parameters_ggplot$size.geomlinesd),
      sliderInput("alpha.geomlinesd","Standard deviation line opacity",min=0,max=1,value=parameters_ggplot$alpha.geomlinesd,step=0.05),
      
      checkboxInput("is.mean",'Display Mean',value=parameters_ggplot$is.mean),
      selectInput("line_type.mean","Type of line for mean",choices=c("solid","twodash","longdash","dotted","dotdash","dashed"),selected=parameters_ggplot$line_type.mean),
      sliderInput("size.geomlinemean","Mean line size",min=0,max=1,value=parameters_ggplot$size.geomlinemean,step=0.05),
      sliderInput("alpha.geomlinemean","Mean line opacity",min=0,max=1,value=parameters_ggplot$alpha.geomlinemean,step=0.05),
      
      
      span('Modify parameters for ggplot'),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_update_of_ggplot_param","Apply")
      )
    )
  }
  
  observeEvent(input$execute_update_of_ggplot_param,{
    parameters_ggplot$is.logscale <- input$is.logscale
    parameters_ggplot$size.geompoint <- input$size.geompoint
    parameters_ggplot$alpha.geompoint <- input$alpha.geompoint
    parameters_ggplot$line_type.sd <- input$line_type.sd
    parameters_ggplot$size.geomlinesd <- input$size.geomlinesd
    parameters_ggplot$alpha.geomlinesd <- input$alpha.geomlinesd
    parameters_ggplot$line_type.mean <- input$line_type.mean
    parameters_ggplot$size.geomlinemean <- input$size.geomlinemean
    parameters_ggplot$alpha.geomlinemean <- input$alpha.geomlinemean
    parameters_ggplot$is.mean <- input$is.mean
    parameters_ggplot$is.sd <- input$is.sd
    parameters_ggplot$is.perTimeonly <- input$is.perTimeonly
    
    removeModal()
  })
  
  
  
}
shinyApp(ui, server)
