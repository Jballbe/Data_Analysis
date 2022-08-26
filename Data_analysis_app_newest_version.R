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
                    
                          fileInput("Pop_class_file","Choose Population Class file"),
                          checkboxInput("input_resistance","Import input resistance"),
                          conditionalPanel('input.input_resistance==true',fileInput("input_resistance_file","Choose the input resistance file")),
                          conditionalPanel('input.input_resistance==true',checkboxInput("normalize_per_input_resistance","Normalize per input resistance")),
                          selectInput("factor_of_analysis","Factor of analysis",choices=""),
                          selectInput("Feature_to_study","Feature to study",choices=""),
                          checkboxInput('remove_outliers','Remove outliers from data'),
                          conditionalPanel('input.remove_outliers==true',selectInput("whichoutliers","Select outliers to remove",choices=c("All (Q1 or Q3 +/- 1.5IQ)",'Just extremes (Q1 or Q3 +/- 3IQ'),selected=c('Just extremes (Q1 or Q3 +/- 3IQ'))),
                          #numericInput("nbfactors","How many possible factors are they?",2),
                          checkboxInput('study_only_subset','Study only subset of the data'),
                          conditionalPanel('input.study_only_subset==true',textInput('whichsubset',"Select subset of data (Format: 'Factor=level')")),
                          
                          
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
             tabPanel(title = "Data description",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxInput("descriptive_repartition_in_two","Repartition in two groups"),
                          conditionalPanel("input.descriptive_repartition_in_two == true",selectInput("data_repartition_factor_of_analysis","Second factor of analysis",choices="")),
                          conditionalPanel("input.descriptive_repartition_in_two == true",selectInput("geom_bar_position","Bar scale",choices=c("stack","fill"))),
                          
                          checkboxInput("descriptive_show_mean","Show means"),
                          
                          
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title="Data table",
                                     dataTableOutput("descriptive_data_table")
                                     
                            ),
                            tabPanel(title="Data Plot",
                                     plotlyOutput("descriptive_data_repartition_plot"),
                                     actionButton("save_descriptive_data_plot","Save plot"),
                                     plotlyOutput("descriptive_data_plot"),
                                     actionButton("save_data_hist_plot","Save plot")
                                     
                            )
                            
                            
                            
                          )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),
             
             
             ##3
             tabPanel(title = "Overtime",
                      sidebarLayout(
                        sidebarPanel(
                          textOutput("function_to_save"),
                          
                          selectInput("current_factor_level","Category of analysis",choices=""),
                          
                          numericInput('which.degree','degree of the polynomial to fit',value=1,step=1),
                          checkboxInput("is.lm","Linear regression"),
                          checkboxInput('is.loess','Locally weighted regression'),
                          checkboxInput('is.confidencebands',"Show confidence bands"), 
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title="General time evolution",
                                     plotOutput("time_evol"),
                                     actionButton("update_parameters","Modify plot parameters"),actionButton("time_plot_saving","Save plot"),
                                     textOutput("t_test_name"),
                                     tableOutput("t_test"),
                                     actionButton("save_t_test_table","Save t-test table"),
                                     tableOutput("overtime_stat"),actionButton("save_overtime_stat_table","Save Mean Table")
                                     
                            ),
                            tabPanel(title="Time differences",
                                     plotOutput("mean_difference_over_time"),
                                     checkboxInput("over_time_per_factor","Perform analysis per factor"),
                                     checkboxInput('show_points',"Display points in plot"),
                                     actionButton("save_mean_diff_plot","Save plot"),
                                     textOutput("function_to_save_mean_diff_plot"),
                                     tableOutput("mean_diff_table"),
                                     actionButton("save_mean_diff_table","Save table")
                            )
                            
                            
                            
                          )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),#tabpanel
             tabPanel(title = "Single Time/spike",
                      sidebarLayout(
                        sidebarPanel(
                          
                          selectInput("Which_time_file","Select time to show",choices=""), 
                          checkboxInput("custom_y_range_anova_plot","Use custom Y range"),
                          conditionalPanel("input.custom_y_range_anova_plot==true",numericInput("maximum_y_anova",'Maximum Y axis',value=1)),
                          conditionalPanel("input.custom_y_range_anova_plot==true",numericInput("minimum_y_anova",'Minimum Y axis',value=0))
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title="Stats",
                                     tableOutput("Anova_Hypothesis"),
                                     actionButton("save_hypo_table","Save Hypothesis table"),
                                     tableOutput("single_time_stat
                                             "),
                                     actionButton("save_stat_table","Save stat table"),
                                     plotOutput("ANOVA_plot"),
                                     actionButton("save_ANOVA_plot","Save plot"),
                                     textOutput("save_var_plot")),
                            
                            tabPanel(title='Plotly',
                                     plotlyOutput("ANOVA_plotly")),
                          )#tabsetPanel
                        )
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
  
  
  output$descriptive_data_table <- renderDataTable({
    req(input$proceed_to_multiple_analysis)
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    factor_of_analysis=input$factor_of_analysis
    descriptive_full_table=myenv$descriptive_full_table
    without_outlier_table=myenv$full_table
    whichoutliers=input$whichoutliers
    for (current_colnames in seq(3,ncol(without_outlier_table))){
      colnames(without_outlier_table)[current_colnames] <- paste0(colnames(without_outlier_table)[current_colnames],'_WO')
    }
    descriptive_full_table=merge(descriptive_full_table,without_outlier_table,by=c("Cell_id"))
    descriptive_full_table
  })
  
  
  output$descriptive_data_plot <- renderPlotly({
    req(input$proceed_to_multiple_analysis)
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    whichoutliers=input$whichoutliers
    factor_of_analysis=input$factor_of_analysis
    full_table=myenv$full_table
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    
    
    mu=full_table %>%
      group_by(Ind_var,Factor_of_analysis) %>%
      summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
    data_hist_plot=ggplot(full_table,aes(x=symbol_val,color=full_table$Factor_of_analysis))+geom_histogram(fill="white")+scale_color_npg()
    if(input$descriptive_show_mean==TRUE){
      data_hist_plot=data_hist_plot+geom_vline(data=mu,aes(xintercept=symbol_val,color=Factor_of_analysis),linetype='dashed')+scale_color_npg()
    }
    
    data_hist_plot=data_hist_plot+facet_grid(Ind_var~.)+xlab(myenv$current_unit)+labs(color =input$factor_of_analysis)
    myenv$data_hist_plot=data_hist_plot
    data_hist_plot
    
  })
  
  output$descriptive_data_repartition_plot <- renderPlotly({
    req(input$proceed_to_multiple_analysis)
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    factor_of_analysis=input$factor_of_analysis
    full_table=myenv$full_table
    whichoutliers=input$whichoutliers
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    second_factor=input$data_repartition_factor_of_analysis
    full_table=na.omit(full_table)
    
    full_table$Ind_var<-factor(full_table$Ind_var, levels=c("5ms","10ms","25ms","50ms","100ms","250ms","500ms","1000ms"))
    
    if (input$descriptive_repartition_in_two==TRUE){
      data_repartition_table=myenv$data_repartition_table
      full_table=merge(full_table,data_repartition_table,by=c("Cell_id"))
      
      data_repartition_plot=ggplot(full_table,aes(x=Factor_of_analysis,fill=Second_factor_of_analysis))+
        geom_bar(position=input$geom_bar_position)
    }
    else{data_repartition_plot=ggplot(data =full_table, mapping = aes(x = Factor_of_analysis , color=Factor_of_analysis)) +
      geom_bar(fill='white',position="stack") +
      facet_wrap(~Ind_var)
    
    
    }
    data_repartition_plot=data_repartition_plot+facet_grid(Ind_var~.)+
      scale_color_npg()+
      xlab(input$factor_of_analysis)+labs(color=input$factor_of_analysis)
    
    myenv$data_repartition_plot=data_repartition_plot
    data_repartition_plot
  })
  
  output$multiplefiles <- renderText({
    req(input$import_files)
    options(digits=2)
    
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves',"plyr",'ggsci')
    #Check if the user have all required libraries and if not, install them
    have_library(required_packages = required_packages)
    
    if (myenv$factor_list_download==0){
      factor_list=c(colnames(read.csv(input$Pop_class_file$datapath,header=T)[1,]))
      factor_list=factor_list[2:length(factor_list)]
      
      
      updateSelectInput(session,"factor_of_analysis","Factor of analysis",choices=factor_list)
      updateSelectInput(session,"data_repartition_factor_of_analysis","Factor of analysis",choices=factor_list)
      myenv$factor_list_download=1
      print("Please select a factor and a variable to analyze")
    }
    
    
    
    factor_table=data.frame(read.csv(file=input$Pop_class_file$datapath,header=T)[,c("Cell_id",as.character(input$factor_of_analysis))])
    data_repartition_table=data.frame(read.csv(file=input$Pop_class_file$datapath,header=T)[,c("Cell_id",as.character(input$data_repartition_factor_of_analysis))])
    leveled_factor_table=factor_table
    leveled_factor_table[,2]=as.factor(leveled_factor_table[,2])
    data_repartition_table[,2]=as.factor(data_repartition_table[,2])
    colnames(data_repartition_table)[2]="Second_factor_of_analysis"
    myenv$data_repartition_table=data_repartition_table
    
    levels_list=c(levels(leveled_factor_table[,2]))
    updateSelectInput(session,"current_factor_level","Category of analysis",choices=levels_list)
    
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_5ms_file[,2]=as.numeric(FR_5ms_file[,2])
          current_outlier=FR_5ms_file %>% identify_outliers(colnames(FR_5ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_5ms_file[which(FR_5ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_5ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_10ms_file[,2]=as.numeric(FR_10ms_file[,2])
          current_outlier=FR_10ms_file %>% identify_outliers(colnames(FR_10ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_10ms_file[which(FR_10ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_10ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_25ms_file[,2]=as.numeric(FR_25ms_file[,2])
          current_outlier=FR_25ms_file %>% identify_outliers(colnames(FR_25ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_25ms_file[which(FR_25ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_25ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_50ms_file[,2]=as.numeric(FR_50ms_file[,2])
          current_outlier=FR_50ms_file %>% identify_outliers(colnames(FR_50ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_50ms_file[which(FR_50ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_50ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_100ms_file[,2]=as.numeric(FR_100ms_file[,2])
          current_outlier=FR_100ms_file %>% identify_outliers(colnames(FR_100ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_100ms_file[which(FR_100ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_100ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_250ms_file[,2]=as.numeric(FR_250ms_file[,2])
          current_outlier=FR_250ms_file %>% identify_outliers(colnames(FR_250ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_250ms_file[which(FR_250ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_250ms_file)[2])]=NA
          
        }
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
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_500ms_file[,2]=as.numeric(FR_500ms_file[,2])
          current_outlier=FR_500ms_file %>% identify_outliers(colnames(FR_500ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_500ms_file[which(FR_500ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_500ms_file)[2])]=NA
          
        }
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
        
        if(input$remove_outliers==TRUE){
          if (input$whichoutliers=="All (Q1 or Q3 +/- 1.5IQ)"){
            which_outliers="is.outlier"
          }
          else{
            which_outliers="is.extreme"
          }
          
          FR_1000ms_file[,2]=as.numeric(FR_1000ms_file[,2])
          current_outlier=FR_1000ms_file %>% identify_outliers(colnames(FR_1000ms_file)[2])
          
          extreme_outlier_id=current_outlier[which(current_outlier[,which_outliers]==TRUE),"Cell_id"]
          
          FR_1000ms_file[which(FR_1000ms_file[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(FR_1000ms_file)[2])]=NA
          
        }
        
        full_table=merge(full_table,FR_1000ms_file,by=c("Cell_id"))
        
        time_list=c(time_list,"1000ms")
        
        
      }
    }
    colnames(full_table)=c("Cell_id","Factor_of_analysis",time_list)
    updateSelectInput(session,"Which_time_file","Select time to show",choices=time_list)
    for (elt in seq(3,ncol(full_table))){
      
      full_table[,elt]=as.numeric(full_table[,elt])}
    
    
    #Normalization per input resistance values
    if (input$normalize_per_input_resistance == TRUE){
      IR_table=data.frame(read.csv(file = input$input_resistance_file$datapath,header=T))
      
      IR_table=IR_table[2:nrow(IR_table),]
      IR_table[,2]=as.numeric(IR_table[,2])
      
      full_table=merge(full_table,IR_table,by="Cell_id")
      
      if (grepl("/pA",current_unit)){
        for (elt in seq(3,(ncol(full_table)-1))){
          full_table[,elt]=full_table[,elt]/(full_table[,ncol(full_table)]*1e-3)
        }
        
      }
      else if (grepl("pA",current_unit)){
        for (elt in seq(3,(ncol(full_table)-1))){
          full_table[,elt]=full_table[,elt]*full_table[,ncol(full_table)]*1e-3
        }
        
      }
      current_unit=gsub('pA','mV',current_unit)
      modified_full_table=full_table
      
      full_table=full_table[,-ncol(full_table)]
    }
    
    if (input$study_only_subset == TRUE){
      full_pop_class_file=data.frame(read.csv(file=input$Pop_class_file$datapath,header=T))
      subset_crit=input$whichsubset
      print(subset_crit)
      subset_crit=str_split(subset_crit,'=')[[1]]
      
      
      Factor_col=subset_crit[1]
      Factor_level=subset_crit[2]
      
      
      subset_id=full_pop_class_file[which(full_pop_class_file[,Factor_col]==Factor_level),'Cell_id']
      subset_table=full_table[full_table$Cell_id %in% subset_id,]
      full_table=subset_table
    }
    
    
    
    myenv$descriptive_full_table=full_table
    
    
    myenv$full_table=full_table
    myenv$factor_order=time_list
    myenv$current_unit=current_unit
    
    print('Libraries and files successfully loaded')
  })
  
  
  
  output$t_test_name <- renderPrint({
    print("Statistical mean difference from 0 (one-sample t-test, p.val<0.05 = significantly different; -1 = not enough observation to compute t-test")
  })
  
  observeEvent(input$save_ANOVA_plot,{
    myenv$plot_to_save=myenv$anova_plot
    myenv$table_or_plot="plot"
    
    showModal(save_modal())
  })
  observeEvent(input$save_descriptive_data_plot,{
    myenv$plot_to_save=myenv$data_repartition_plot
    myenv$table_or_plot="plot"
    showModal(save_modal())
  })
  
  observeEvent(input$save_data_hist_plot,{
    myenv$plot_to_save=myenv$data_hist_plot
    myenv$table_or_plot="plot"
    showModal(save_modal())
  })
  
  variable_plot_saving_vals <- reactiveValues(
    saving_path=NULL,
    saving_name=NULL
  )
  
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
    myenv$table_to_save=myenv$t_test_table
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
  observeEvent(input$save_overtime_stat_table,{
    myenv$table_to_save=myenv$overtime_stat_table
    myenv$table_or_plot="table"
    showModal(save_modal())
  })
  
  
  observeEvent(input$save_mean_diff_table,{
    myenv$table_to_save=myenv$pwc
    myenv$table_or_plot="table"
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
        print("lzjf")
        if (saving_vals$is.custom_y_range ==TRUE){
          plot_to_save=plot_to_save+ylim(saving_vals$saving_y_min,saving_vals$saving_y_max)}
        
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
    
    full_table=myenv$full_table
    whichoutliers=input$whichoutliers
    
    # for (time in seq(3,ncol(full_table))){ #remove outliers for each time
    #   
    #   print(colnames(full_table)[time])
    #   current_outlier=full_table %>% identify_outliers(colnames(full_table)[time])
    #   
    #   extreme_outlier_id=current_outlier[which(current_outlier[,"is.extreme"]==TRUE),"Cell_id"]
    #   
    #   full_table[which(full_table[,"Cell_id"] %in% extreme_outlier_id),as.character(colnames(full_table)[time])]=NA
    # }
    
    my_full_table=full_table
    
    if (input$over_time_per_factor == TRUE){
      full_table=full_table[which(full_table[,"Factor_of_analysis"]==as.character(input$current_factor_level)),]
    }
    
    symbol_val=sym(input$Feature_to_study)
    
    RMA_full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id,Ind_var)
    
    
    RMA_full_table$Ind_var <- factor(RMA_full_table$Ind_var,levels=colnames(full_table[3:ncol(full_table)]))
    
    
    
    for (elt in levels(RMA_full_table[,"Ind_var"])){
      current_table=RMA_full_table[which(RMA_full_table[,"Ind_var"]==as.character(elt)),]
      current_table=current_table[-which(is.nan(current_table[,"symbol_val"])==TRUE),]
      if (nrow(current_table)<3){
        RMA_full_table=RMA_full_table[-which(RMA_full_table[,"Ind_var"]==as.character(elt)),]
        
      }
    }
    RMA_full_table$Ind_var=droplevels(RMA_full_table$Ind_var)
    
    
    is_normally_distributed=RMA_full_table %>%
      group_by(Ind_var) %>%
      shapiro_test(symbol_val)
    
    p_val_shapiro=is_normally_distributed$p>0.05
    if (FALSE %in% p_val_shapiro){all.normal=FALSE}
    else{all.normal=TRUE}
    
    colnames(RMA_full_table)[ncol(RMA_full_table)]="my_current_variable"
    
    if (input$over_time_per_factor == TRUE){
      my_RMA_full_table=gather(my_full_table,key="Ind_var",value=symbol_val,3:ncol(my_full_table))%>%convert_as_factor(Cell_id,Ind_var)
      #my_RMA_full_table[which(my_RMA_full_table[,as.character(input$factor_of_analysis)]!=as.character(input$current_factor_level)),as.character(input$factor_of_analysis)]=as.character("Other")
      
      my_RMA_full_table$Ind_var <- factor(my_RMA_full_table$Ind_var)
      old_RMA_table=my_RMA_full_table
      my_RMA_full_table[,"Factor_of_analysis"]<- as.character(my_RMA_full_table[,"Factor_of_analysis"])
      my_RMA_full_table[which(my_RMA_full_table[,"Factor_of_analysis"]!=as.character(input$current_factor_level)),"Factor_of_analysis"]=as.character("Other")
      my_RMA_full_table[,"Factor_of_analysis"]<- as.factor(my_RMA_full_table[,"Factor_of_analysis"])
      
      level_table=my_RMA_full_table[which(my_RMA_full_table[,"Factor_of_analysis"]==as.character(input$current_factor_level)),]
      other_level=my_RMA_full_table[which(my_RMA_full_table[,"Factor_of_analysis"]!=as.character(input$current_factor_level)),]
      
      old_RMA_table[,"Factor_of_analysis"]<- as.character(old_RMA_table[,"Factor_of_analysis"])
      old_RMA_table[,"Factor_of_analysis"]=as.character("All")
      old_RMA_table[,"Factor_of_analysis"]<- as.factor(old_RMA_table[,"Factor_of_analysis"])
      
      mytable=rbind(old_RMA_table,level_table)
      
      colnames(my_RMA_full_table)[ncol(my_RMA_full_table)]="my_current_variable"
      colnames(mytable)[ncol(mytable)]="my_current_variable"
      
      
      
      mytable$Ind_var=factor(mytable$Ind_var,levels=myenv$factor_order)
      
      
      bxp=mytable %>%
        drop_na()%>%
        ggplot(aes(x=Ind_var,y=my_current_variable, color=Factor_of_analysis)) +
        geom_boxplot(outlier.shape=NA)
      
      
    }
    else{
      
      bxp=RMA_full_table %>%
        drop_na()%>%
        ggplot(aes(x=Ind_var,y=my_current_variable)) +
        geom_boxplot(outlier.shape=NA)
      
      
      
    }
    
    if (all.normal == TRUE){
      my.res = anova_test(data=RMA_full_table,dv='my_current_variable',wid=id,within=Ind_var)
      pwc <- data.frame(Method=c(my.res$method),
                        p_value=c(my.res$p.value))
      myenv$pwc=pwc
      if (my.res$p.value<0.05){
        
        pwc=pairwise_t_test(RMA_full_table, formula = as.formula(my_current_variable ~ Ind_var),p.adjust.method = "bonferroni")
        myenv$pwc=pwc[,2:ncol(pwc)]
        pwc=add_xy_position(pwc,x="Ind_var",scales = 'free')
        if (input$over_time_per_factor == TRUE){
          bxp=bxp + 
            stat_pvalue_manual(pwc,hide.ns = TRUE,step.increase = 0.06,color="red")
        }
        else{
          bxp=bxp + 
            stat_pvalue_manual(pwc,hide.ns = TRUE,step.increase = 0.06)
        }
        
        if (input$pertime==TRUE){
          bxp=bxp +
            labs(
              caption = get_pwc_label(pwc),
              y=as.character(myenv$current_unit),x='Time(ms)')
        }
        else{
          bxp=bxp +
            labs(
              caption = get_pwc_label(pwc),
              y=as.character(myenv$current_unit),x='° Spike')
        }
      }
      
    }
    
    if (all.normal ==FALSE){
      
      
      res.fried=friedman.test(y=RMA_full_table$my_current_variable,groups=RMA_full_table$Ind_var,blocks=RMA_full_table$Cell_id)
      pwc <- data.frame(Method=c(res.fried$method),
                        p_value=c(res.fried$p.value))
      myenv$pwc=pwc
      
      if (res.fried$p.value<0.05){
        
        pwc=wilcox_test(data=RMA_full_table, formula=as.formula(my_current_variable ~ Ind_var), paired = TRUE, p.adjust.method = "bonferroni")
        
        myenv$pwc=pwc[,2:ncol(pwc)]
        
        pwc=add_xy_position(pwc,x="Ind_var",scales='free')
       
        bxp=bxp + 
          stat_pvalue_manual(pwc,hide.ns = TRUE,step.increase = 0.06)
        
        bxp=bxp +
          labs(
            caption = get_pwc_label(pwc),
            y=as.character(myenv$current_unit),x='Time(ms)')
        
        if (input$pertime==TRUE){
          bxp=bxp +
            labs(
              caption = get_pwc_label(pwc),
              y=as.character(myenv$current_unit),x='Time(ms)')
        }
        else{
          bxp=bxp +
            labs(
              caption = get_pwc_label(pwc),
              y=as.character(myenv$current_unit),x='° Spike')
        }
      }
      
      
    }
    
    myenv$mean_diff_plot=bxp
    bxp
    
  })
  
  output$mean_diff_table <- renderTable({
    full_table=myenv$full_table
    whichoutliers=input$whichoutliers
    over_time_per_factor=input$over_time_per_factor
    factor_level=input$current_factor_level
    mytest <- myenv$pwc
    mytest
  },digits=-2,align = 'c')
  
  
  output$t_test <- renderTable({
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    variable_list=myenv$variable_list
    whichoutliers=input$whichoutliers
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    full_table$Ind_var=as_factor(full_table$Ind_var)
    full_table$Factor_of_analysis=as_factor(full_table$Factor_of_analysis)
    
    
    
    t_test_table=data.frame(matrix(ncol=4,nrow=0, dimnames=list(NULL, c("Ind_var", "Factor_of_analysis", 'nb_obs',"t_test_p_val"))))
    
    
    
    
    for (current_ind_var in levels(full_table$Ind_var)){
      
      for (current_factor in levels(full_table$Factor_of_analysis)){
        
        current_subset=full_table[which(full_table$Ind_var == current_ind_var & full_table$Factor_of_analysis ==current_factor) ,]
        current_subset=na.omit(current_subset)
        if (nrow(current_subset)>1){res_t_test = current_subset%>%t_test(symbol_val ~ 1,mu=0)
        p_val=res_t_test$p
        nb_obs=res_t_test$n}
        else{ p_val=NA
        nb_obs=nrow(current_subset)
        }
        
        new_line=data.frame("Ind_var"=current_ind_var,
                            "Factor_of_analysis"=current_factor,
                            'nb_obs'=nb_obs,
                            "t_test_p_val"=p_val)
        
        
        t_test_table=rbind(t_test_table,new_line)
        
        
      }
      
      all_pop_current_subset=full_table[which(full_table$Ind_var == current_ind_var) ,]
      all_pop_current_subset=na.omit(all_pop_current_subset)
      if (nrow(all_pop_current_subset)>1){
        new_res_t_test = all_pop_current_subset%>%t_test(symbol_val ~ 1,mu=0)
        new_p_val=new_res_t_test$p
        new_nb_obs=new_res_t_test$n
      }
      else{
        new_p_val=-1
        new_nb_obs=nrow(all_pop_current_subset)
      }
      
      
      all_pop_new_line=data.frame("Ind_var"=current_ind_var,
                                  "Factor_of_analysis"="All_population",
                                  'nb_obs'=new_nb_obs,
                                  "t_test_p_val"=new_p_val)
      
      t_test_table=rbind(t_test_table,all_pop_new_line)
    }
    if (input$pertime==TRUE){
      colnames(t_test_table) <- c("Time(ms)",as.character(input$factor_of_analysis),'nb_observation','T-test p_val')
    }
    else{colnames(t_test_table) <- c("Spike",as.character(input$factor_of_analysis),'nb_observation','T-test p_val')}
    
    
    t_test_table=t(t_test_table)
    
    myenv$t_test_table=t_test_table
    t_test_table
    
  },rownames = TRUE,colnames=FALSE,
  digits=-2,align = 'c')
  
  output$overtime_stat <- renderTable({
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    whichoutliers=input$whichoutliers
    
    symbol_val=sym(input$Feature_to_study)
    overtime_stat_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    
    overtime_stat_table$Ind_var=str_remove(overtime_stat_table$Ind_var,"ms")
    overtime_stat_table$Ind_var=str_remove(overtime_stat_table$Ind_var,"_spikes")
    overtime_stat_table$Ind_var=as.numeric(overtime_stat_table$Ind_var)
    overtime_mean_table=overtime_stat_table %>%
      group_by(Ind_var,Factor_of_analysis) %>%
      summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
    overtime_mean_table_all_population=overtime_stat_table %>%
      group_by(Ind_var) %>%
      summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
    overtime_mean_table_all_population['Factor_of_analysis'] <- c(rep("All_population",nrow(overtime_mean_table_all_population)))
    
    columns_order <- c("Ind_var","Factor_of_analysis","symbol_val")
    overtime_mean_table <- overtime_mean_table[,columns_order]
    overtime_mean_table_all_population <- overtime_mean_table_all_population[,columns_order]
    new_overtime_mean_table=rbind(overtime_mean_table,overtime_mean_table_all_population)
    myfactor_levels=append(factor_list,'All_population')
    new_overtime_mean_table%>%
      mutate(Factor_of_analysis=factor(Factor_of_analysis,levels=myfactor_levels))
    new_overtime_mean_table=arrange(new_overtime_mean_table ,Ind_var,Factor_of_analysis)
    
    
    
    overtime_sd_table=overtime_stat_table %>%
      group_by(Ind_var,Factor_of_analysis) %>%
      summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
    overtime_sd_table_all_population=overtime_stat_table %>%
      group_by(Ind_var) %>%
      summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
    overtime_sd_table_all_population['Factor_of_analysis'] <- c(rep("All_population",nrow(overtime_sd_table_all_population)))
    
    
    columns_order <- c("Ind_var","Factor_of_analysis","symbol_val")
    overtime_sd_table <- overtime_sd_table[,columns_order]
    overtime_sd_table_all_population <- overtime_sd_table_all_population[,columns_order]
    new_overtime_sd_table=rbind(overtime_sd_table,overtime_sd_table_all_population)
    myfactor_levels=append(factor_list,'All_population')
    new_overtime_sd_table%>%
      mutate(Factor_of_analysis=factor(Factor_of_analysis,levels=myfactor_levels))
    new_overtime_sd_table=arrange(new_overtime_sd_table ,Ind_var,Factor_of_analysis)
    overtime_stat_table=cbind(new_overtime_mean_table,new_overtime_sd_table[,2:3])
    if (input$pertime==TRUE){
      colnames(overtime_stat_table) <- c("Time(ms)",as.character(input$factor_of_analysis),'Mean',as.character(input$factor_of_analysis),'SD')
    }
    else{colnames(overtime_stat_table) <- c("Spike",as.character(input$factor_of_analysis),'Mean',as.character(input$factor_of_analysis),'SD')}
    
    
    overtime_stat_table=t(overtime_stat_table)
    myenv$overtime_stat_table=overtime_stat_table
    overtime_stat_table
  },rownames = TRUE,
  colnames = FALSE,
  digits=-2,align = 'c')
  
  
  
  output$time_evol <- renderPlot({
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    whichoutliers=input$whichoutliers
    symbol_val=sym(input$Feature_to_study)
    time_evol_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    
    time_evol_table$Ind_var=str_remove(time_evol_table$Ind_var,"ms")
    time_evol_table$Ind_var=str_remove(time_evol_table$Ind_var,"_spikes")
    time_evol_table$Ind_var=as.numeric(time_evol_table$Ind_var)
    
    
    if (parameters_ggplot$is.perTimeonly == TRUE){
      myplot=ggplot(data=time_evol_table,aes(x=Ind_var,y=symbol_val))+
        geom_point(alpha=parameters_ggplot$alpha.geompoint,
                   size=parameters_ggplot$size.geompoint)
      
      mean_table=time_evol_table %>%
        group_by(Ind_var) %>%
        summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
      mean_table$Factor_of_analysis <- c(rep('All population',nrow(mean_table)))
      mean_table$Factor_of_analysis <- as.factor(mean_table$Factor_of_analysis)
      mean_table_pop=mean_table
      
      sd_table=time_evol_table %>%
        group_by(Ind_var) %>%
        summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
      sd_table$Factor_of_analysis <- c(rep('All population',nrow(sd_table)))
      sd_table$Factor_of_analysis <- as.factor(sd_table$Factor_of_analysis)
      
    }
    else{
      mean_table=time_evol_table %>%
        group_by(Ind_var,Factor_of_analysis) %>%
        summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
      
      sd_table=time_evol_table %>%
        group_by(Ind_var,Factor_of_analysis) %>%
        summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
      
      myplot=ggplot(data=time_evol_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis))+
        geom_point(alpha=parameters_ggplot$alpha.geompoint,
                   size=parameters_ggplot$size.geompoint)
      
      
    }
    if (parameters_ggplot$is.sd == TRUE){
      myplot <- myplot+geom_line(data = sd_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis),
                                 linetype= parameters_ggplot$line_type.sd,
                                 alpha=parameters_ggplot$alpha.geomlinesd,
                                 size=parameters_ggplot$size.geomlinesd)
      
    }
    
    if (parameters_ggplot$is.mean == TRUE){
      myplot=myplot+geom_line(data = mean_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis),
                              linetype= parameters_ggplot$line_type.mean,
                              alpha=parameters_ggplot$alpha.geomlinemean,
                              size=parameters_ggplot$size.geomlinemean)
      
    }
    
    if (parameters_ggplot$is_smooth==TRUE){
      if(parameters_ggplot$is.perTimeonly ==TRUE){
        
        if(input$is.lm==TRUE){
          
          myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(color = "Linear Model") )
          #:stat_regline_equation(label.y = 1000,label.x = 100,formula = y ~ poly(x,input$which.degree),output.type = "latex")+
          
        }
        if(input$is.loess==TRUE){
          myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(color = "LOESS"))
        }
      }
      
      if (parameters_ggplot$is.perTimeonly==FALSE){
        
        if(input$is.lm==TRUE){
          #To facet by group
          myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(group=Factor_of_analysis,color = Factor_of_analysis))
          
          
          #stat_cor(formula = y ~ poly(x,input$which.degree),output.type = "latex")
        }
        if(input$is.loess==TRUE){
          myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(group=Factor_of_analysis,color = Factor_of_analysis))
        }
      }
      
    }
    if (input$pertime==TRUE){
      myplot=myplot+
        labs(y=as.character(myenv$current_unit),x='Time(ms)')
    }
    if (input$perspike==TRUE){
      myplot=myplot+
        labs(y=as.character(myenv$current_unit),x='Spike number')
    }
    
    if (parameters_ggplot$is.logscale==TRUE){
      myplot=myplot+scale_x_continuous(trans="log10")
    }
    
    myenv$toplolty=myplot
    if(parameters_ggplot$use_custom_y_range==TRUE){
      myplot=myplot+ylim(parameters_ggplot$minimum_y_axis_range, parameters_ggplot$maximum_y_axis_range)
    }
    
    myenv$mean_table=mean_table
    myenv$sd_table=data.frame(sd_table)
    myenv$plot_MF=myplot
    myplot
  })
  
  output$plotly_time_evolution <- renderPlotly({
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    whichoutliers=input$whichoutliers
    symbol_val=sym(input$Feature_to_study)
    time_evol_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    
    time_evol_table$Ind_var=str_remove(time_evol_table$Ind_var,"ms")
    time_evol_table$Ind_var=str_remove(time_evol_table$Ind_var,"_spikes")
    time_evol_table$Ind_var=as.numeric(time_evol_table$Ind_var)
    
    
    if (parameters_ggplot$is.perTimeonly == TRUE){
      myplot=ggplot(data=time_evol_table,aes(x=Ind_var,y=symbol_val))+
        geom_point(alpha=parameters_ggplot$alpha.geompoint,
                   size=parameters_ggplot$size.geompoint)
      
      mean_table=time_evol_table %>%
        group_by(Ind_var) %>%
        summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
      mean_table$Factor_of_analysis <- c(rep('All population',nrow(mean_table)))
      mean_table$Factor_of_analysis <- as.factor(mean_table$Factor_of_analysis)
      mean_table_pop=mean_table
      
      sd_table=time_evol_table %>%
        group_by(Ind_var) %>%
        summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
      sd_table$Factor_of_analysis <- c(rep('All population',nrow(sd_table)))
      sd_table$Factor_of_analysis <- as.factor(sd_table$Factor_of_analysis)
      
    }
    else{
      mean_table=time_evol_table %>%
        group_by(Ind_var,Factor_of_analysis) %>%
        summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
      
      sd_table=time_evol_table %>%
        group_by(Ind_var,Factor_of_analysis) %>%
        summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
      
      myplot=ggplot(data=time_evol_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis))+
        geom_point(alpha=parameters_ggplot$alpha.geompoint,
                   size=parameters_ggplot$size.geompoint)
      
      
    }
    if (parameters_ggplot$is.sd == TRUE){
      myplot <- myplot+geom_line(data = sd_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis),
                                 linetype= parameters_ggplot$line_type.sd,
                                 alpha=parameters_ggplot$alpha.geomlinesd,
                                 size=parameters_ggplot$size.geomlinesd)
      
    }
    
    if (parameters_ggplot$is.mean == TRUE){
      myplot=myplot+geom_line(data = mean_table,aes(x=Ind_var,y=symbol_val,color=Factor_of_analysis),
                              linetype= parameters_ggplot$line_type.mean,
                              alpha=parameters_ggplot$alpha.geomlinemean,
                              size=parameters_ggplot$size.geomlinemean)
      
    }
    
    if (parameters_ggplot$is_smooth==TRUE){
      if(parameters_ggplot$is.perTimeonly ==TRUE){
        
        if(input$is.lm==TRUE){
          
          myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(color = "Linear Model") )
          #:stat_regline_equation(label.y = 1000,label.x = 100,formula = y ~ poly(x,input$which.degree),output.type = "latex")+
          
        }
        if(input$is.loess==TRUE){
          myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(color = "LOESS"))
        }
      }
      
      if (parameters_ggplot$is.perTimeonly==FALSE){
        
        if(input$is.lm==TRUE){
          #To facet by group
          myplot=myplot+ geom_smooth(method = "lm", formula = y ~ poly(x,input$which.degree), size = 0.4, se =input$is.confidencebands,level=parameters_ggplot$smooth.interval , aes(group=Factor_of_analysis,color = Factor_of_analysis))
          
          
          #stat_cor(formula = y ~ poly(x,input$which.degree),output.type = "latex")
        }
        if(input$is.loess==TRUE){
          myplot=myplot+ geom_smooth(method = "loess", formula = y ~ x, size = 0.4, se = input$is.confidencebands,level=parameters_ggplot$smooth.interval, aes(group=Factor_of_analysis,color = Factor_of_analysis))
        }
      }
      
    }
    if (input$pertime==TRUE){
      myplot=myplot+
        labs(y=as.character(myenv$current_unit),x='Time(ms)')
    }
    if (input$perspike==TRUE){
      myplot=myplot+
        labs(y=as.character(myenv$current_unit),x='Spike number')
    }
    
    if (parameters_ggplot$is.logscale==TRUE){
      myplot=myplot+scale_x_continuous(trans="log10")
    }
    
    myenv$toplolty=myplot
    if(parameters_ggplot$use_custom_y_range==TRUE){
      myplot=myplot+ylim(parameters_ggplot$minimum_y_axis_range, parameters_ggplot$maximum_y_axis_range)
    }
    
    
    myplot
  })
  
  
  output$Anova_Hypothesis <- renderTable( {
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    whichoutliers=input$whichoutliers
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    full_table$Ind_var=as_factor(full_table$Ind_var)
    full_table$Factor_of_analysis=as_factor(full_table$Factor_of_analysis)
    current_ind_var=input$Which_time_file
    
    full_table=full_table[which(full_table$Ind_var == current_ind_var) ,]
    anova_hypothesis_table=data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("Factor", 'nb_obs',"Normality p_val",'Normality assumption','Variance Homogeneity p_val','Variance homogeneity assumption','Variance test','Variance test p_val',"Is there Mean difference?","Pair-wise comparison test","Between"))))
    
    myd=full_table
    
    full_table=na.omit(full_table)
    full_table$Factor_of_analysis=droplevels.factor(full_table$Factor_of_analysis)
    
    #check normality
    res  <- aov(symbol_val~Factor_of_analysis, data = full_table)
    
    shapiro_p_val=shapiro_test(residuals(res))$p.value
    
    
    if (shapiro_p_val>0.05){
      Normality_assumption='Respected'
    }
    else{Normality_assumption='Non Respected'}
    
    #check variance homogeneity per group
    levene_test_p_val=levene_test(data=full_table,formula=symbol_val~Factor_of_analysis)$p
    
    if (levene_test_p_val>0.05){
      Variance_homogeneity_assumption='Respected'
    }
    else {Variance_homogeneity_assumption='Non Respected'}
    
    #decide which test to perform
    
    if (Normality_assumption == 'Non Respected' || Variance_homogeneity_assumption=='Non Respected'){
      Variance_test='Kruskal-Wallis'
    }
    else{Variance_test='ANOVA'}
    
    if (Variance_test=="ANOVA"){
      model=anova_test(symbol_val~Factor_of_analysis, data = full_table)
    }
    else{
      model=kruskal_test(symbol_val~Factor_of_analysis, data = full_table)
    }
    
    Variance_test_p_val=model$p
    myenv$full_table_anova=full_table
    myenv$model=model
    
    if (Variance_test_p_val<0.05 && Variance_test=="ANOVA"){
      Mean_difference="Yes"
      pwc_name='Tukey test'
      pwc <- full_table%>%tukey_hsd(symbol_val~Factor_of_analysis)
      all_pair=""
      for (current_pwc in seq(nrow(pwc))){
        
        if (pwc[current_pwc,"p.adj"]<0.05){
          current_pair=as.character(paste0(pwc[current_pwc,"group1"],pwc[current_pwc,"p.adj.signif"],pwc[current_pwc,"group2"]))
          all_pair=paste(all_pair,current_pair,sep='\n')
        }
      }
      pwc <- pwc%>%add_xy_position(x='Factor_of_analysis')
      
    }
    
    else if (Variance_test_p_val<0.05 && Variance_test=='Kruskal-Wallis'){
      Mean_difference="Yes"
      pwc_name='Dunn test'
      pwc <- full_table%>%dunn_test(symbol_val~Factor_of_analysis,p.adjust.method = 'bonferroni')
      all_pair=""
      for (current_pwc in seq(nrow(pwc))){
        
        if (pwc[current_pwc,"p.adj"]<0.05){
          current_pair=as.character(paste0(pwc[current_pwc,"group1"],pwc[current_pwc,"p.adj.signif"],pwc[current_pwc,"group2"]))
          all_pair=paste(all_pair,current_pair,sep='\n')
        }
      }
      pwc <- pwc%>%add_xy_position(x='Factor_of_analysis')
      
      
    }
    
    else{
      Mean_difference="No"
      pwc_name='--'
      all_pair="--"
      pwc=NULL
    }
    
    
    myenv$Variance_test_p_val=Variance_test_p_val
    myenv$pwc_anova=pwc
    
    new_line=data.frame("Factor"=input$factor_of_analysis,
                        'nb_obs'=nrow(full_table),
                        "Normality p_val"=shapiro_p_val,
                        'Normality assumption'=Normality_assumption,
                        'Variance Homogeneity p_val'=levene_test_p_val,
                        'Variance homogeneity assumption'=Variance_homogeneity_assumption,
                        'Variance test'=Variance_test,
                        'Variance test p_val'=Variance_test_p_val,
                        "Is there Mean difference?"=Mean_difference,
                        "Pair-wise comparison test"=pwc_name,
                        "Between"=all_pair
    )
    
    anova_hypothesis_table=rbind(anova_hypothesis_table,new_line)
    
    
    anova_hypothesis_table
    
    
  },rownames=FALSE,colnames=TRUE,digits=2)
  
  output$single_time_stat <- renderTable({
    
    req(input$proceed_to_multiple_analysis)
    full_table=myenv$full_table
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    which_time=input$Which_time_file
    whichoutliers=input$whichoutliers
    symbol_val=sym(input$Feature_to_study)
    full_table=full_table[,c('Cell_id',"Factor_of_analysis",which_time)]
    single_time_stat=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    single_time_stat$Ind_var=str_remove(single_time_stat$Ind_var,"ms")
    single_time_stat$Ind_var=str_remove(single_time_stat$Ind_var,"_spikes")
    single_time_stat$Ind_var=as.numeric(single_time_stat$Ind_var)
    overtime_mean_table=single_time_stat %>%
      group_by(Ind_var,Factor_of_analysis) %>%
      summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
    overtime_mean_table_all_population=single_time_stat %>%
      group_by(Ind_var) %>%
      summarise_at(vars(symbol_val),funs(mean(.,na.rm=TRUE)))
    overtime_mean_table_all_population['Factor_of_analysis'] <- c(rep("All_population",nrow(overtime_mean_table_all_population)))
    
    columns_order <- c("Ind_var","Factor_of_analysis","symbol_val")
    overtime_mean_table <- overtime_mean_table[,columns_order]
    overtime_mean_table_all_population <- overtime_mean_table_all_population[,columns_order]
    new_overtime_mean_table=rbind(overtime_mean_table,overtime_mean_table_all_population)
    myfactor_levels=append(factor_list,'All_population')
    new_overtime_mean_table%>%
      mutate(Factor_of_analysis=factor(Factor_of_analysis,levels=myfactor_levels))
    new_overtime_mean_table=arrange(new_overtime_mean_table ,Ind_var,Factor_of_analysis)
    
    
    
    overtime_sd_table=single_time_stat %>%
      group_by(Ind_var,Factor_of_analysis) %>%
      summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
    overtime_sd_table_all_population=single_time_stat %>%
      group_by(Ind_var) %>%
      summarise_at(vars(symbol_val),funs(sd(.,na.rm=TRUE)))
    overtime_sd_table_all_population['Factor_of_analysis'] <- c(rep("All_population",nrow(overtime_sd_table_all_population)))
    
    
    columns_order <- c("Ind_var","Factor_of_analysis","symbol_val")
    overtime_sd_table <- overtime_sd_table[,columns_order]
    overtime_sd_table_all_population <- overtime_sd_table_all_population[,columns_order]
    new_overtime_sd_table=rbind(overtime_sd_table,overtime_sd_table_all_population)
    myfactor_levels=append(factor_list,'All_population')
    new_overtime_sd_table%>%
      mutate(Factor_of_analysis=factor(Factor_of_analysis,levels=myfactor_levels))
    new_overtime_sd_table=arrange(new_overtime_sd_table ,Ind_var,Factor_of_analysis)
    single_time_stat=cbind(new_overtime_mean_table,new_overtime_sd_table[,2:3])
    if (input$pertime==TRUE){
      colnames(single_time_stat) <- c("Time(ms)",as.character(input$factor_of_analysis),'Mean',as.character(input$factor_of_analysis),'SD')
    }
    else{colnames(single_time_stat) <- c("Spike",as.character(input$factor_of_analysis),'Mean',as.character(input$factor_of_analysis),'SD')}
    
    
    single_time_stat=t(single_time_stat)
    myenv$single_time_stat=single_time_stat
    single_time_stat
    
    
  },colnames=FALSE,rownames=TRUE,digits=-2,align = 'c')
  
  output$counterglobal <- renderTable({
    req(input$multiple_file_factor,input$nbfactors)
    file_list=myenv$file_list
    whichoutliers=input$whichoutliers
    current_time_dataset=file_list[[input$Which_time_file]]
    table_count=count_samples(current_time_dataset,nbfactor=input$nbfactors,myfactor=input$multiple_file_factor,nbvariable = myenv$nbvariable)
    data.frame(table_count)
  },rownames=TRUE,digits = 2)
  
  
  
  output$ANOVA_plot <- renderPlot({ 
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(myenv$Variance_test_p_val)
    full_table=myenv$full_table
    whichoutliers=input$whichoutliers
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    mycurrent_factor=input$factor_of_analysis
    symbol_val=sym(input$Feature_to_study)
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    full_table$Ind_var=as_factor(full_table$Ind_var)
    full_table$Factor_of_analysis=as_factor(full_table$Factor_of_analysis)
    current_ind_var=input$Which_time_file
    
    droplevels(full_table$Factor_of_analysis)
    
    full_table=full_table[which(full_table$Ind_var == current_ind_var) ,]
    anova_hypothesis_table=data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("Factor", 'nb_obs',"Normality p_val",'Normality assumption','Variance Homogeneity p_val','Variance homogeneity assumption','Variance test','Variance test p_val',"Is there Mean difference?","Pair-wise comparison test","Between"))))
    
    
    full_table=na.omit(full_table)
    
    pwc=myenv$pwc_anova
    current_ind_var=input$Which_time_file
    model=myenv$model
    anova_plot=ggboxplot(full_table,x="Factor_of_analysis" ,y="symbol_val")
    
    if (myenv$Variance_test_p_val <0.05){
      if (input$custom_y_range_anova_plot==TRUE){
        anova_plot=anova_plot+ylim(input$minimum_y_anova,input$maximum_y_anova)
        min_pwc=min(pwc[,'y.position'])
        max_pwc=max(pwc[,'y.position'])
        for (current_pwc in seq(nrow(pwc))){
          pwc[current_pwc,'y.position']=pwc[current_pwc,'y.position']*((input$maximum_y_anova)/max_pwc)
        }
      }
      anova_plot=anova_plot+stat_pvalue_manual(pwc,hide.ns=TRUE)+
        labs(
          subtitle = get_test_label(model, detailed = TRUE),
          caption = get_pwc_label(pwc)
        )
    }
    if (input$custom_y_range_anova_plot==TRUE){
      anova_plot=anova_plot+ylim(input$minimum_y_anova,input$maximum_y_anova)
      min_pwc=min(pwc[,'y.position'])
    }
    
    anova_plot=anova_plot+
      labs(y=as.character(myenv$current_unit))
    
    
    myenv$anova_plot=anova_plot
    anova_plot
  })
  
  
  output$ANOVA_plotly <- renderPlotly({ 
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    full_table=myenv$full_table
    
    whichoutliers=input$whichoutliers
    variable_list=myenv$variable_list
    factor_list=myenv$factor_list
    time_list=myenv$factor_order
    symbol_val=sym(input$Feature_to_study)
    full_table=gather(full_table,key="Ind_var",value=symbol_val,3:ncol(full_table))%>%convert_as_factor(Cell_id)
    full_table$Ind_var=as_factor(full_table$Ind_var)
    full_table$Factor_of_analysis=as_factor(full_table$Factor_of_analysis)
    current_ind_var=input$Which_time_file
    
    full_table=full_table[which(full_table$Ind_var == current_ind_var) ,]
    anova_hypothesis_table=data.frame(matrix(ncol=11,nrow=0, dimnames=list(NULL, c("Factor", 'nb_obs',"Normality p_val",'Normality assumption','Variance Homogeneity p_val','Variance homogeneity assumption','Variance test','Variance test p_val',"Is there Mean difference?","Pair-wise comparison test","Between"))))
    
    
    full_table=na.omit(full_table)
    pwc=myenv$pwc_anova
    model=myenv$model
    current_ind_var=input$Which_time_file
    anova_plot=ggboxplot(full_table,x="Factor_of_analysis" ,y="symbol_val")
    
    
    
    anova_plot=anova_plot+
      labs(y=as.character(myenv$current_unit))
    
    
    anova_plot
  })
  output$plotly <- renderPlotly({
    #Only begin when the parametric tests have been performed, or when the factor of analysis is changed
    req(input$multiple_file_factor,input$Variabletoshow,input$Which_time_file,input$nbfactors,myenv$Hypothesis_table)
    myfactor=input$multiple_file_factor
    whichoutliers=input$whichoutliers
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
    use_custom_y_range=FALSE,
    maximum_y_axis_range=1,
    minimum_y_axis_range=0,
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
      checkboxInput("use_custom_y_range","Use custom y axis range",value=parameters_ggplot$use_custom_y_range),
      numericInput("maximum_custom_y_range","Choose maximum Y axis value",value=parameters_ggplot$maximum_y_axis_range),
      numericInput("minimum_custom_y_range","Choose minimum Y axis value",value=parameters_ggplot$minimum_y_axis_range),
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
    parameters_ggplot$use_custom_y_range <- input$use_custom_y_range
    parameters_ggplot$maximum_y_axis_range <- input$maximum_custom_y_range
    parameters_ggplot$minimum_y_axis_range <- input$minimum_custom_y_range
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
