library(shiny)
required_packages=c("Cairo","Skillings.Mack","plyr","stringr","abind","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets')
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
        checkboxInput("usual_files","Automatic file upload"),
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
                 sidebarPanel(
                              textOutput("function_to_save"),
                              selectInput("multiple_file_factor","Factor of analysis",choices=""),
                              selectInput("Variabletoshow","Select Variable to display",choices=""),
                              selectInput("Which_time_file","Select time to show",choices=""),
                              
                                               
                                              numericInput('which.degree','degree of the polynomial to fit',value=1,step=1),
                                               checkboxInput("is.lm","Linear regression"),
                                              
                                               checkboxInput('is.loess','Locally weighted regression'),
                                               
                                               checkboxInput('is.confidencebands',"Show confidence bands"),
                                               
                                    
                              
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(title = "Over Time",
                            tabsetPanel(
                              tabPanel(title="General stats",
                                       plotlyOutput("time_evol"),
                                       actionButton("update_parameters","Modify plot parameters"),actionButton("time_plot_saving","Save plot"),
                                       textOutput("t_test_name"),
                                       tableOutput("t_test"),
                                       actionButton("save_t_test_table","Save t-test table"),
                                       
                                       tableOutput("overtime_stat_mean"),actionButton("save_mean_overtime_table","Save Mean Table"),
                                       tableOutput("overtime_stat_sd"),actionButton("save_sd_overtime_table","Save SD Table")
                              ),
                              tabPanel(title="Time differences",
                                       plotOutput("mean_difference_over_time"),
                                       actionButton("save_mean_diff_plot","Save plot"),
                                       textOutput("function_to_save_mean_diff_plot"))
                            )
                            
                            
                            ),
                   
                   tabPanel(title = "Single Time Point",
                            tabsetPanel(
                              tabPanel(title="Stats",
                                       tableOutput("Hypothesis"),
                                       actionButton("save_hypo_table","Save Hypothesis table"),
                                       tableOutput("basic_stats"),
                                       actionButton("save_stat_table","Save stat table")),
                              tabPanel(title="Plots",
                                       tableOutput("countervariable"),
                                       plotOutput("plot"),
                                       plotlyOutput("plotly"),
                                       checkboxInput("points","Display points in plotly"),
                                       actionButton("save_variable_plot","Save plot"),
                                       textOutput("save_var_plot")),
                                       )
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
  myenv$is.stat.table=FALSE

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
    if (input$usual_files == TRUE){
      #select all files directly
      populationclass=read.csv("/Users/julienballbe/Documents/Script/Data_Files/Population_Classes.csv",header=T)
      source(file = "/Users/julienballbe/My_Work/Data_Analysis/Source_functions.R")
      FR_5ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_5ms.csv",header=T)
      FR_5ms=create_fulldataset(FR_5ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      
      FR_10ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_10ms.csv",header=T)
      FR_10ms=create_fulldataset(FR_10ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      
      FR_25ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_25ms.csv",header=T)
      FR_25ms=create_fulldataset(FR_25ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      FR_50ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_50ms.csv",header=T)
      FR_50ms=create_fulldataset(FR_50ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      
      FR_100ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_100ms.csv",header=T)
      FR_100ms=create_fulldataset(FR_100ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      
      FR_250ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_250ms.csv",header=T)
      FR_250ms=create_fulldataset(FR_250ms,nbfactors=2,population_class = populationclass)$full_dataset
      
      
      FR_500ms=read.csv("/Users/julienballbe/Documents/Script/Data_Files/FR_Hz_500ms.csv",header=T)
      results_from_createfulldataset=create_fulldataset(FR_500ms,nbfactors=2,population_class = populationclass)
      
      FR_500ms=create_fulldataset(FR_500ms,nbfactors=2,population_class = populationclass)$full_dataset
    
      factor_list=results_from_createfulldataset$factor_list
      variable_list=results_from_createfulldataset$variable_list
      file_list=list("FR_5ms"=FR_5ms,
                     "FR_10ms"=FR_10ms,
                     "FR_25ms"=FR_25ms,
                     "FR_50ms"=FR_50ms,
                     "FR_100ms"=FR_100ms,
                     "FR_250ms"=FR_250ms,
                     "FR_500ms"=FR_500ms)
      time_list=c("5ms","10ms","25ms","50ms","100ms","250ms","500ms")
      Species_MF=results_from_createfulldataset$Species
      FT_MF=results_from_createfulldataset$Firing_Type
      unit_dict=results_from_createfulldataset$unit_dict
      myenv$nbvariable=length(variable_list)
    }
    if (input$usual_files == FALSE){
      
    
    
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
  
  saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL,
    saving_file_kind=NULL,
    saving_y_max=NULL,
    saving_y_min=NULL,
    is.custom_y_range=FALSE,
    proceed=FALSE
  )
  observeEvent(input$execute_saving,{
    if (input$folder_to_save != "" && input$file_name_save != ""){
      saving_vals$saving_path <- input$folder_to_save
      saving_vals$saving_name <- input$file_name_save
      saving_vals$saving_file_kind <- input$Kind_of_file
      saving_vals$is.custom_y_range <- input$is.custom_y_range
      saving_vals$saving_y_max <- input$saving_y_max
      saving_vals$saving_y_min <- input$saving_y_min
      saving_vals$proceed = TRUE
      removeModal()
    }
    else{
      showModal(save_modal(failed = TRUE))
    }
  })
  observeEvent(input$save_t_test_table,{
    myenv$table_to_save=myenv$my_t_test_table
    myenv$table_or_plot="table"
    showModal(save_modal())
  })
  observeEvent(input$save_mean_diff_plot,{
    myenv$plot_to_save=myenv$mean_diff_plot
    myenv$table_or_plot="plot"
    showModal(save_modal())
  })
  observeEvent(input$time_plot_saving,{
    myenv$plot_to_save=myenv$plot_MF
    myenv$table_or_plot="plot"
    showModal(save_modal())
  })

  observeEvent(input$save_stat_table,{
    myenv$is.stat.table=TRUE
    myenv$table_or_plot="table"
    showModal(save_modal())
  })
  observeEvent(input$save_hypo_table,{
    myenv$table_to_save=myenv$Hypothesis_table
    myenv$table_or_plot="table"
    showModal(save_modal())
  })
  observeEvent(input$save_mean_overtime_table,{
    myenv$table_to_save=myenv$overtime_mean_table
    myenv$table_or_plot="table"
    showModal(save_modal())
  })
  observeEvent(input$save_sd_overtime_table,{
    myenv$table_to_save=myenv$overtime_sd_table
    showModal(save_modal())
  })
  
  output$function_to_save <- renderText({
    if (saving_vals$proceed==TRUE){
    req(saving_vals$saving_path,saving_vals$saving_name)
    if(myenv$table_or_plot=="table"){
      if (myenv$is.stat.table==TRUE){
        write.csv(myenv$mean_table,file=paste0(saving_vals$saving_path,saving_vals$saving_name,"_mean.csv"),row.names = TRUE,col.names = TRUE)
        write.csv(myenv$sd_table,file=paste0(saving_vals$saving_path,saving_vals$saving_name,"_sd.csv"),row.names = TRUE,col.names = TRUE)
        print(paste0("Stats tables successfully saved!"))
        myenv$is.stat.table=FALSE
      }
      else{
      write.csv(myenv$table_to_save,file=paste0(saving_vals$saving_path,saving_vals$saving_name,".csv"),row.names = TRUE,col.names = TRUE)
      print(paste0(saving_vals$saving_name,".csv"," successfully saved!"))
      }
      saving_vals$proceed=FALSE
      myenv$table_to_save=NULL
      
    }
    
    if(myenv$table_or_plot=="plot"){
      plot_to_save=myenv$plot_to_save
      plot_to_save=plot_to_save+ylim(saving_vals$saving_y_min,saving_vals$saving_y_max)
      ggsave(filename = paste0(saving_vals$saving_name,".pdf"),plot=plot_to_save,path=saving_vals$saving_path,device = cairo_pdf,width=200,height = 100,units="mm")
      print(paste0(saving_vals$saving_name,".pdf ","succesfully saved!"))
      saving_vals$proceed=FALSE
      myenv$plot_to_save=NULL
    }
    
    }
  })
  save_modal <- function(failed=FALSE){
    modalDialog(
      
      textInput("folder_to_save",label="Saving folder (ending with / or \ "),
      
      textInput("file_name_save", label= "File name (without .csv or .pdf)"),
      
      span('In case of plot saving'),
      checkboxInput("is.custom_y_range","Do you want custom y range?"),
      
      numericInput("saving_y_max","Select maximum y axis value",1),
      numericInput("saving_y_min","Select minimum y axis value",0),
      span('Please select a directory,',' and file name for saving','Plot are save as .pdf, and tabke as .csv'),
      if (failed)
        div(tags$b("Please enter all required information")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("execute_saving","Save file"),
      )
    )
  }

  
  
  
  
  
  output$mean_difference_over_time <- renderPlot({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    unit_dict=myenv$unit_dict
    
    my_time_list=names(myenv$file_list)
    current_data=threeDarray[,as.character(input$Variabletoshow),]
    current_data=data.frame(current_data)
    for (elt in seq(ncol(current_data))){
      current_data[,elt]=as.numeric(current_data[,elt])
    }
    id=c(seq(nrow(current_data)))
    id=data.frame(id)
    colnames(id)="id"
    current_data=data.frame(cbind(data.frame(id),data.frame(current_data)))
    
    my_current_data <- current_data %>%
      gather(key="Time",value=Variable,FR_5ms,FR_10ms,FR_25ms,FR_50ms,FR_100ms,FR_250ms,FR_500ms)%>%
      convert_as_factor(id)
    
    my_current_data$Time <- factor(my_current_data$Time,levels=c("FR_5ms","FR_10ms","FR_25ms","FR_50ms","FR_100ms","FR_250ms","FR_500ms"))
    my_current_data[,"Variable"]=as.numeric(my_current_data[,"Variable"])
    outlier=my_current_data %>% 
      group_by(Time) %>%
      identify_outliers(Variable)
    
    extreme_outlier=outlier[which(outlier[,"is.extreme"]==TRUE),"Variable"]
    for (elt in seq(nrow(my_current_data))){
      if (my_current_data[elt,"Variable"] %in% extreme_outlier$Variable){
        my_current_data[elt,"Variable"] = NA
      }
    }
    
    is_normally_distributed=my_current_data %>%
      group_by(Time) %>%
      shapiro_test(Variable)
   
    my_current_data %>%
      group_by(Time)
    for (elt in nrow(is_normally_distributed)){
      if (is_normally_distributed[elt,"p"] < 0.05){
        all.normal=FALSE
      }
      else{
        all.normal=TRUE
      }
        
    }
    bxp=ggboxplot(my_current_data,x='Time', y="Variable",add='jitter',size=0.3)
    
    if (all.normal == TRUE){
      my.res = anova_test(data=my_current_data,dv="Variable",wid=id,within=Time)
      pwc=pairwise_t_test(my_current_data, formula = Variable ~ Time,p.adjust.method = "bonferroni")
    }
    
    if (all.normal ==FALSE){
      
      pwc=wilcox_test(data=my_current_data, formula=Variable ~ Time, paired = TRUE, p.adjust.method = "bonferroni")
    }
    
   
    
    pwc=add_xy_position(pwc,x="Time")
    
    bxp=bxp + 
      stat_pvalue_manual(pwc,hide.ns = TRUE,step.increase = 0.06) +
      labs(
           caption = get_pwc_label(pwc),
           y=as.character(unit_dict[input$Variabletoshow]),x='Time(ms)')
    
    myenv$mean_diff_plot=bxp
    bxp
    
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
   
  output$overtime_stat_mean <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    stat_dataset=threeDarray[,as.character(input$Variabletoshow),]
    stat_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(stat_dataset)))
    stat_dataset[,"Firing_Type"]=droplevels(stat_dataset[,"Firing_Type"])
    
    overtime_mean_table=overtime_basic_stat(stat_dataset,myfactor,my_time_list)$mean_table
    
   myenv$overtime_mean_table=overtime_mean_table
    overtime_mean_table
  },rownames = TRUE,
  digits=4,align = 'c')
  
  output$overtime_stat_sd <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    stat_dataset=threeDarray[,as.character(input$Variabletoshow),]
    stat_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(stat_dataset)))
    stat_dataset[,"Firing_Type"]=droplevels(stat_dataset[,"Firing_Type"])
    overtime_sd_table=overtime_basic_stat(stat_dataset,myfactor,my_time_list)$sd_table
    myenv$overtime_sd_table=overtime_sd_table
  },rownames = TRUE,
  digits=4,align = 'c')
  
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
    
    if (parameters_ggplot$is_overall_mean ==TRUE){
      overall_mean_table=getmean(ggdatatable,input$multiple_file_factor,variable_to_analyse,perTimeonly=parameters_ggplot$is_overall_mean)$mean_table
      myplot=myplot+geom_line(data = overall_mean_table,aes(x=Time,y=Mean),
                              linetype= parameters_ggplot$line_type.mean,
                              alpha=parameters_ggplot$alpha.geomlinemean,
                              size=parameters_ggplot$size.geomlinemean,
                              color="black")
    }
    
    if (parameters_ggplot$is_overall_sd ==TRUE){
      overall_sd_table=getsd(ggdatatable,input$multiple_file_factor,variable_to_analyse,perTimeonly=parameters_ggplot$is_overall_sd)$sd_table
      myplot=myplot+geom_line(data = overall_sd_table,aes(x=Time,y=SD),
                              linetype= parameters_ggplot$line_type.sd,
                              alpha=parameters_ggplot$alpha.geomlinesd,
                              size=parameters_ggplot$size.geomlinesd,
                              color="black")
    }
    if (parameters_ggplot$is_smooth==TRUE){
      if(parameters_ggplot$is.perTimeonly ==TRUE){
       
        if(input$is.lm==TRUE){
         
          myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(color = "Linear Model ^2") )
            #:stat_regline_equation(label.y = 1000,label.x = 100,formula = y ~ poly(x,input$which.degree),output.type = "latex")+
          
        }
        if(input$is.loess==TRUE){
          myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(color = "LOESS"))
        }
      }
      
    if (parameters_ggplot$is.perTimeonly==FALSE){
      
      if(input$is.lm==TRUE){
        #To facet by group
        myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(group=.data[[input$multiple_file_factor]],color = .data[[input$multiple_file_factor]]))
       
        
          #stat_cor(formula = y ~ poly(x,input$which.degree),output.type = "latex")
      }
      if(input$is.loess==TRUE){
        myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(group=.data[[input$multiple_file_factor]],color = .data[[input$multiple_file_factor]]))
      }
    }
     
    }
    myplot=myplot+
      labs(y=as.character(unit_dict[variable_to_analyse]),x='Time(ms)')
    if (parameters_ggplot$is.logscale==TRUE){
      myplot=myplot+scale_x_continuous(trans="log10")
    }
      
    
    myenv$plot_MF=myplot
    myplot
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
    
    print(stat_table)
    
    
  },rownames=TRUE,digits=4,align = 'c')
  
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
    variable_plot=variable_plot+theme(axis.text.x = element_text(angle = 90))
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

  
  
  parameters_ggplot <- reactiveValues(
    is.logscale=FALSE,
    smooth.interval=0.95,
    
    alpha.geompoint=0.3,
    size.geompoint=1,
    is.perTimeonly=FALSE,
    is.sd=FALSE,
    size.geomlinesd=1,
    alpha.geomlinesd=1,
    line_type.sd="solid",
    is_overall_mean=FALSE,
    is_overall_sd=FALSE,
    is.mean=FALSE,
    size.geomlinemean=1,
    alpha.geomlinemean=1,
    is_smooth=FALSE,
    line_type.mean="dashed"
  )
  
  observeEvent(input$update_parameters,{
    showModal(update_parameters_ggplot())})
  
  update_parameters_ggplot <- function(failed=FALSE){
    modalDialog(
      checkboxInput("is.logscale","Display x axis in log scale",value = parameters_ggplot$is.logscale),
      checkboxInput('is.perTimeonly',"Group only per time",value=parameters_ggplot$is.perTimeonly),
      checkboxInput("is_smooth","Display estimated trend",value=parameters_ggplot$is_smooth),
      
      sliderInput("smooth.interval","Confidence interval (0 to 1)", min=0,max=1,value=parameters_ggplot$smooth.interval,step=0.01),
      sliderInput("size.geompoint","Point size",min=0,max=1,value=parameters_ggplot$size.geompoint,step=0.05),
      sliderInput("alpha.geompoint","Point opacity",min=0,max=1,step=0.05,value=parameters_ggplot$alpha.geompoint),
      
      checkboxInput("is.sd","Display Standard Deviation",value=parameters_ggplot$is.sd),
      checkboxInput("is_overall_sd","Display general Standard Deviation",value=parameters_ggplot$is_overall_sd),
      selectInput("line_type.sd","Type of line for SD",choices=c("solid","twodash","longdash","dotted","dotdash","dashed"),selected=parameters_ggplot$line_type.sd),
      sliderInput("size.geomlinesd","Standard deviation line size",min=0,max=1,step=0.05,value=parameters_ggplot$size.geomlinesd),
      sliderInput("alpha.geomlinesd","Standard deviation line opacity",min=0,max=1,value=parameters_ggplot$alpha.geomlinesd,step=0.05),
      
      
      checkboxInput("is.mean",'Display Mean',value=parameters_ggplot$is.mean),
      checkboxInput("is_overall_mean","Display general Mean",value=parameters_ggplot$is_overall_mean),
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
    parameters_ggplot$is_smooth <- input$is_smooth
    parameters_ggplot$smooth.interval <- input$smooth.interval
    parameters_ggplot$is_overall_sd <- input$is_overall_sd
    parameters_ggplot$is_overall_mean <- input$is_overall_mean
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
