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
        
        selectInput("factor_of_analysis","Factor of analysis",choices=""),
        selectInput("Feature_to_study","Feature to study",choices=""),
        #numericInput("nbfactors","How many possible factors are they?",2),
        
        checkboxInput("pertime","Analysis per time"),
        checkboxInput("perspike","Analysis per spike"),
         
         conditionalPanel("input.pertime==true",checkboxInput("isfive",'5ms')),
         conditionalPanel("input.pertime==true",checkboxInput("isten",'10ms')),
         conditionalPanel("input.pertime==true",checkboxInput("istwentyfive",'25ms')),
         conditionalPanel("input.pertime==true",checkboxInput("isfifty",'50ms')),
         conditionalPanel("input.pertime==true",checkboxInput("ishundred",'100ms')),
         conditionalPanel("input.pertime==true",checkboxInput("istwohundredfifty",'250ms')),
         conditionalPanel("input.pertime==true",checkboxInput("isfivehundred",'500ms')),
         conditionalPanel("input.pertime==true",checkboxInput("isthousand","1000ms")),
         
         conditionalPanel("input.isfive == true",fileInput("fivems","5ms data file (csv)")),
         conditionalPanel("input.isten == true",fileInput("tenms","10ms data file (csv)")),
         conditionalPanel("input.istwentyfive == true",fileInput("twentyfivems","25ms data file (csv)")),
         conditionalPanel("input.isfifty == true",fileInput("fiftyms","50ms data file (csv)")),
         conditionalPanel("input.ishundred == true",fileInput("hundredms","100ms data file (csv)")),
         conditionalPanel("input.istwohundredfifty == true",fileInput("twohundredfiftyms","250ms data file (csv)")),
         conditionalPanel("input.isfivehundred == true",fileInput("fivehundredms","500ms data file (csv)")),
         conditionalPanel("input.isthousand ==true",fileInput('thousandms','1000ms data file (csv)')),
         
         conditionalPanel("input.perspike==true",checkboxInput("isfourspikes",'4 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("isfivespikes",'5 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("issixspikes",'6 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("issevenspikes",'7 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("iseightspikes",'8 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("isninespikes",'9 spikes')),
         conditionalPanel("input.perspike==true",checkboxInput("istenspikes",'10 spikes')),
        
         conditionalPanel("input.isfourspikes == true",fileInput("fourspikes","4 spikes data file (csv)")),
         conditionalPanel("input.isfivespikes == true",fileInput("fivespikes","5 spikes data file (csv)")),
         conditionalPanel("input.issixspikes == true",fileInput("sixspikes","6 spikes data file (csv)")),
         conditionalPanel("input.issevenspikes == true",fileInput("sevenspikes","7 spikes data file (csv)")),
         conditionalPanel("input.iseightspikes == true",fileInput("eightspikes","8 spikes data file (csv)")),
         conditionalPanel("input.isninespikes == true",fileInput("ninespikes","9 spikes data file (csv)")),
         conditionalPanel("input.istenspikes == true",fileInput("tenspikes","10 spikes data file (csv)")),
         actionButton("import_files","Import_files"),
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
                              selectInput("Variabletoshow","Variable to show",choices=""),
                              selectInput("Which_time_file","Select time to show",choices=""),
                              
                                               
                                              numericInput('which.degree','degree of the polynomial to fit',value=1,step=1),
                                               checkboxInput("is.lm","Linear regression"),
                                              
                                               checkboxInput('is.loess','Locally weighted regression'),
                                               
                                               checkboxInput('is.confidencebands',"Show confidence bands"),
                                               
                                    
                              
                 ),
                 mainPanel(tabsetPanel(
                   tabPanel(title = "Over Time/Over Spikes",
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
                                       textOutput("function_to_save_mean_diff_plot")
                                       ),
                              tabPanel(title="Data repartition",
                                       plotlyOutput("plotly_data_evolution"))
                            )
                            
                            
                            ),
                   
                   tabPanel(title = "Single Time Point/Spike",
                            tabsetPanel(
                              tabPanel(title="Stats",
                                       tableOutput("Hypothesis"),
                                       actionButton("save_hypo_table","Save Hypothesis table"),
                                       tableOutput("basic_stats"),
                                       actionButton("save_stat_table","Save stat table")),
                              tabPanel(title="Plots",
                                       plotlyOutput("countervariable"),
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
  myenv$features_names_download=0
  myenv$factor_list_download=0
  myenv$previous_value=0
  myenv$previous_value_MF=0
  myenv$previous_stat_value=0
  myenv$is.stat.table=FALSE

  
  
  output$multiplefiles <- renderText({
    req(input$import_files)
    options(digits=2)
    source(file=input$Source_functions$datapath)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
    #Check if the user have all required libraries and if not, install them
    have_library(required_packages = required_packages)
    if (myenv$factor_list_download==0){
      factor_list=c(colnames(read.csv(input$Pop_class_file$datapath,header=T)[1,]))
      factor_list=factor_list[2:length(factor_list)]
      updateSelectInput(session,"factor_of_analysis","Factor of analysis",choices=factor_list,selected = factor_list[1])
      myenv$factor_list_download==1
      print("Please select a factor and a variable to analyze")
    }
    
    
    
    factor_table=data.frame(read.csv(file=input$Pop_class_file$datapath,header=T)[,c("Cell_id",as.character(input$factor_of_analysis))])
    full_table=factor_table
    full_table[,1]=as.character(full_table[,1])
    full_table[,2]=as.factor(full_table[,2])
    
    time_list=c()

    if (input$pertime ==TRUE){
      if (input$isfive == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$fivems$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_5ms_file=data.frame(read.csv(file = input$fivems$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_5ms_file[1,2]
        FR_5ms_file=FR_5ms_file[2:nrow(FR_5ms_file),]
        colnames(FR_5ms_file)=c("Cell_id","5ms")
        full_table=merge(full_table,FR_5ms_file,by=c("Cell_id"))
        time_list=c(time_list,"5ms")
        
      
        
      }
      
      if (input$isten == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$tenms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_10ms_file=data.frame(read.csv(file = input$tenms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_10ms_file[1,2]
        FR_10ms_file=FR_10ms_file[2:nrow(FR_10ms_file),]
        colnames(FR_10ms_file)=c("Cell_id","10ms")
        full_table=merge(full_table,FR_10ms_file,by=c("Cell_id"))
        time_list=c(time_list,"10ms")
        
        
        
        
        
      }
      
      if (input$istwentyfive == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$twentyfivems$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_25ms_file=data.frame(read.csv(file = input$twentyfivems$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_25ms_file[1,2]
        FR_25ms_file=FR_25ms_file[2:nrow(FR_25ms_file),]
        colnames(FR_25ms_file)=c("Cell_id","25ms")
        full_table=merge(full_table,FR_25ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"25ms")
        
      }
      
      if (input$isfifty == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$fiftyms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_50ms_file=data.frame(read.csv(file = input$fiftyms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_50ms_file[1,2]
        FR_50ms_file=FR_50ms_file[2:nrow(FR_50ms_file),]
        colnames(FR_50ms_file)=c("Cell_id","50ms")
        full_table=merge(full_table,FR_50ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"50ms")
        
      }
      
      if (input$ishundred == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$hundredms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_100ms_file=data.frame(read.csv(file = input$hundredms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_100ms_file[1,2]
        FR_100ms_file=FR_100ms_file[2:nrow(FR_100ms_file),]
        colnames(FR_100ms_file)=c("Cell_id","100ms")
        full_table=merge(full_table,FR_100ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"100ms")
        
        
      }
      
      if (input$istwohundredfifty == TRUE){
       
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$twohundredfiftyms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_250ms_file=data.frame(read.csv(file = input$twohundredfiftyms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_250ms_file[1,2]
        FR_250ms_file=FR_250ms_file[2:nrow(FR_250ms_file),]
        colnames(FR_250ms_file)=c("Cell_id","250ms")
        full_table=merge(full_table,FR_250ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"250ms")
        
        
      }
      
      if (input$isfivehundred == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$fivehundredms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_500ms_file=data.frame(read.csv(file = input$fivehundredms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_500ms_file[1,2]
        FR_500ms_file=FR_500ms_file[2:nrow(FR_500ms_file),]
        colnames(FR_500ms_file)=c("Cell_id","500ms")
        full_table=merge(full_table,FR_500ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"500ms")
        
        
      }
      
      if (input$isthousand == TRUE){
        if (myenv$features_names_download==0){
          features_list=c(colnames(read.csv(input$thousandms$datapath,header=T)[1,]))
          features_list=features_list[2:length(features_list)]
          updateSelectInput(session,"Feature_to_study","Feature to study",choices=features_list,selected = features_list[1])
          myenv$features_names_download=1
        }
        
        FR_1000ms_file=data.frame(read.csv(file = input$thousandms$datapath,header=T)[,c("Cell_id",as.character(input$Feature_to_study))])
        current_unit=FR_1000ms_file[1,2]
        FR_1000ms_file=FR_1000ms_file[2:nrow(FR_1000ms_file),]
        colnames(FR_1000ms_file)=c("Cell_id","1000ms")
        full_table=merge(full_table,FR_1000ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"1000ms")
        
        
      }
    }
    colnames(full_table)=c("Cell_id",as.character(input$factor_of_analysis),time_list)
    for (elt in seq(3,ncol(full_table))){
      
      full_table[,elt]=as.numeric(full_table[,elt])}
    myenv$full_table=full_table
    
    myenv$current_unit=current_unit
    print('Libraries and files successfully loaded')
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
    #perform multiple measure anova to compare mean evolution along time
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    
    for (time in seq(3,ncol(full_table))){
      print("oko")
      print(colnames(full_table)[time])
      current_outlier=full_table %>% identify_outliers(colnames(full_table)[time])
      print("pkpo")
      extreme_outlier_id=current_outlier[which(current_outlier[,"is.extreme"]==TRUE),"Cell_id"]
      print('kzkz')
      full_table[which(full_table[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(full_table)[time])]=NA
    }
    
    symbol_val=sym(input$Feature_to_study)
    
    RMA_full_table=gather(full_table,key="Time",value=symbol_val,3:ncol(full_table))
    
    
    RMA_full_table$Time <- factor(RMA_full_table$Time,levels=colnames(full_table[3:ncol(full_table)]))
  
    print("kjnkjn")
    View(RMA_full_table)
    is_normally_distributed=RMA_full_table %>%
      group_by(Time) %>%
      shapiro_test(symbol_val)
    View(is_normally_distributed)
    p_val_shapiro=is_normally_distributed$p>0.05
    if (FALSE %in% p_val_shapiro){all.normal=FALSE}
    else{all.normal=TRUE}
    print("jiij")
    colnames(RMA_full_table)[ncol(RMA_full_table)]="my_current_variable"
    View(RMA_full_table)
    bxp=ggboxplot(RMA_full_table,x='Time', y='my_current_variable',size=0.3)+geom_jitter(alpha=0.1)
    print(bxp)
    if (all.normal == TRUE){
      my.res = anova_test(data=RMA_full_table,dv='my_current_variable',wid=id,within=Time)
      pwc=pairwise_t_test(RMA_full_table, formula = as.formula(my_current_variable ~ Time),p.adjust.method = "bonferroni")
    }
    
    if (all.normal ==FALSE){
      
      pwc=wilcox_test(data=RMA_full_table, formula=as.formula(my_current_variable ~ Time), paired = TRUE, p.adjust.method = "bonferroni")
    }
    
    
    
    pwc=add_xy_position(pwc,x="Time")
    
    bxp=bxp + 
      stat_pvalue_manual(pwc,hide.ns = TRUE,step.increase = 0.06) +
      labs(
           caption = get_pwc_label(pwc),
           y=as.character(myenv$current_unit),x='Time(ms)')
    
    myenv$mean_diff_plot=bxp
    print('jijij')
    
    bxp
    
  })

  output$t_test <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    t_test_dataset=threeDarray[,as.character(input$Variabletoshow),]
    t_test_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(t_test_dataset)))
    
   
    my_t_test_table=perform_t_test(t_test_dataset,myfactor,my_time_list)
    is.num <- sapply(my_t_test_table, is.numeric)
    my_t_test_table[is.num] <- lapply(my_t_test_table[is.num], round, 8)
    myenv$my_t_test_table=my_t_test_table
    
    my_t_test_table
  },rownames = TRUE,
  digits=-2,align = 'c')
   
  output$overtime_stat_mean <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    stat_dataset=threeDarray[,as.character(input$Variabletoshow),]
    
    stat_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(stat_dataset)))
   
    
    overtime_mean_table=overtime_basic_stat(stat_dataset,myfactor,my_time_list)$mean_table
    
   myenv$overtime_mean_table=overtime_mean_table
    overtime_mean_table
  },rownames = TRUE,
  digits=-2,align = 'c')
  
  output$overtime_stat_sd <- renderTable({
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    my_time_list=names(myenv$file_list)
    stat_dataset=threeDarray[,as.character(input$Variabletoshow),]
    stat_dataset=data.frame(cbind(data.frame(myenv$factor_columns),data.frame(stat_dataset)))
    
    overtime_sd_table=overtime_basic_stat(stat_dataset,myfactor,my_time_list)$sd_table
    myenv$overtime_sd_table=overtime_sd_table
  },rownames = TRUE,
  digits=-2,align = 'c')
  
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
    if (input$pertime==TRUE){
      myplot=myplot+
        labs(y=as.character(unit_dict[variable_to_analyse]),x='Time(ms)')
    }
    if (input$perspike==TRUE){
      myplot=myplot+
        labs(y=as.character(unit_dict[variable_to_analyse]),x='Spike number')
    }
    
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
    
  },rownames=TRUE,digits=2)
  
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
  },rownames=TRUE,digits = 2)
  
  output$countervariable <- renderPlotly({
    req(input$multiple_file_factor)
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    unit_dict=myenv$unit_dict
    
    my_time_list=names(myenv$file_list)
    factor_columns=data.frame(myenv$factor_columns)
    
    factor_columns=factor_columns[myfactor]
    
    current_data=threeDarray[,as.character(input$Variabletoshow),]
    current_data=cbind(data.frame(factor_columns),data.frame(current_data))
    
    positions=levels(factor_columns)
    my_current_data =gather(current_data,key="Time",value="Variable",2:(length(my_time_list)+1))
    my_current_data =my_current_data[my_current_data$Time==input$Which_time_file,]
   
    my_current_data=na.omit(my_current_data)
    colnames(my_current_data)=c(myfactor,"Time","Variable")
    my_plot=ggplot(data=my_current_data,aes_string(myfactor))+geom_bar(alpha=0.7)+scale_x_discrete(limits=positions)
    myenv$plot_data_evol=my_plot
    my_plot
  })
  
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

  output$plotly_data_evolution <- renderPlotly({ 
    req(input$multiple_file_factor)
    threeDarray=myenv$threeDarray
    myfactor=input$multiple_file_factor
    unit_dict=myenv$unit_dict
    
    my_time_list=names(myenv$file_list)
    factor_columns=data.frame(myenv$factor_columns)
    
    factor_columns=factor_columns[myfactor]
    
    current_data=threeDarray[,as.character(input$Variabletoshow),]
    current_data=cbind(data.frame(factor_columns),data.frame(current_data))
    
    positions=my_time_list
    my_current_data =gather(current_data,key="Time",value="Variable",2:(length(my_time_list)+1))
    
    my_current_data=na.omit(my_current_data)
    colnames(my_current_data)=c(myfactor,"Time","Variable")
    my_plot=ggplot(data=my_current_data,aes(Time))+geom_bar(aes_string(fill=myfactor),alpha=0.7)+scale_x_discrete(limits=positions)
    myenv$plot_data_evol=my_plot
    my_plot
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
