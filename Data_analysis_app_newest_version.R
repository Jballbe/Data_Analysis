library(shiny)
required_packages=c("Cairo","Skillings.Mack","plyr","stringr","abind","dplyr","shiny","ggplot2",'bslib',"GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets','gtools','ggridges','rhdf5',"gsignal","RColorBrewer","processx",'ggh4x','reticulate')
install.packages(setdiff(required_packages,rownames(installed.packages())))
print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")
source_python ('/Users/julienballbe/My_Work/My_Librairies/Fit_library.py')
source_python ('/Users/julienballbe/My_Work/My_Librairies/Data_treatment.py')
source_python ("/Users/julienballbe/My_Work/My_Librairies/Electrophy_treatment.py")
source_python ("/Users/julienballbe/My_Work/Data_Analysis/Python_function_for_R.py")
source(file="/Users/julienballbe/My_Work/Data_Analysis/Import_h5_file.R")

#UI

ui <- fluidPage(
  titlePanel("Application Data Analysis"),
  navbarPage(title = "Population Data",
             ##1
             tabPanel("Files",
                      
                      sidebarLayout(
                        sidebarPanel(
                          shinyDirButton('population_data_folder', 'Select a folder', 'Please select a folder', FALSE),
                          selectInput('Selected_Response_Type','Select Response Type',choices=c('Time_based','Index_based','Interval_based'),selected='Time_based'),
                          
                          textOutput('file_path'),
                          checkboxGroupButtons('Population_files','Select files',choices = ""),
                          actionButton("import_files","Import_files"),
                        ),
                        mainPanel(
                                  actionButton("subset_data","Update Data"),
                                  uiOutput("selectInputs"),
                                  
                                  dataTableOutput('population_class')
                                  )
                      ),
                      
             ),##1
             tabPanel(title = "Data Analysis",
                      sidebarLayout(
                        sidebarPanel(width=3,
                          fluidRow(column(width=6,selectInput("Factor_of_analysis_data_repartition","Choose factor of analysis",choices="")),
                                   column(width=6,selectInput("Feature_to_analyse_data_repartition",'Choose feature to analyse',choices=""))),
                          selectInput("select_outliers_to_remove_repartition","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                          checkboxInput('normalise_descriptive_plot','Normalize'),
                          fluidRow(column(width=6,selectInput("Factor_to_facet"," Facet plot per",choices='')),
                                   column(width=6,checkboxInput("Facet_decriptive_plot","Facet plot"))),
                          
                          checkboxInput("normalize_per_input_resistance_Data_overview","Normalize current per input resistance"),
                          sliderInput(inputId = "bins_width_histogram",
                                      label = "Width of bins:",
                                      min = .01,
                                      max = 2,
                                      value = .1),
                          
                          checkboxInput("descriptive_show_mean","Show means"),
                          
                          
                        ),
                        mainPanel(width = 9,
                          tabsetPanel(
                            tabPanel(title="Data Overview",
                                     
                                     plotlyOutput('Data_repartition'),
                                     #plotlyOutput('Data_sunburst_repartition',height = 600),
                                     dataTableOutput("descriptive_data_table")
                                     
                            ),
                            
                            tabPanel(title="Over Response Duration",
                                     selectInput('variability_plot','Choose plot',choices=c('boxplot','jitterplot'),selected='boxplot'),
                                     plotlyOutput('varibility_plot_time_response'),
                                     checkboxInput('group_specific','Display groups'),
                                     checkboxInput('show_geom_jitter','Show points'),
                                     verbatimTextOutput("click"),
                                     
                                     tableOutput('summary_statistics'),tableOutput('outlier_count'),
                                     plotlyOutput('outliers_plot')
                                     
                            )
                          )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),
             
             
             ##3
             
             tabPanel(title="Repeated Measure Analysis",
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     
                                     fluidRow(column(width = 6,selectInput("Feature_to_analyse_RM",'Choose feature to analyse',choices="")),
                                              column(width = 6,selectInput("Factor_of_analysis_RM","Choose factor of analysis",choices=""))),
                                     fluidRow(column(width = 6,checkboxInput("normalize_per_input_resistance_RM","Normalize current per input resistance"))),
                                     fluidRow(column(width=6,checkboxInput("Remove_outliers_variance_RM","Remove outliers from analysis")),
                                              column(width=6,numericInput('Distance_from_Quartiles_variance_RM','Remove outliers outside of [Q1-n*IQR, Q3+n*IQR]',value=1)))
                                     
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    
                                    tabPanel(title='Repeated Measure Analysis',
                                             fluidPage(fluidRow(column(width = 4, plotlyOutput('Original_RM_Data_Plot')),
                                                                column(width = 8, tableOutput("Pooled_Variance_Data_table_Repeated_measures")))),
                                             fluidPage(fluidRow(column(width = 4,plotlyOutput('RM_Data_without_outliers_Plot')),
                                                                 column(width = 8,tableOutput('Pooled_Variance_Data_table_without_outliers_Repeated_measures'))), ),
                                             fluidPage(fluidRow(column(width = 4,tableOutput('category_count_table')),
                                                                column(width = 4,tableOutput('Normality_test_table')),
                                                                column(width = 4,tableOutput('Variance_test_table'))), ),
                                             fluidPage(fluidRow(column(width = 12,tableOutput('PWC_test_table'))), ),
                                             plotOutput("PWC_test_Plot")
                                             )
                                    
                                    
                                    
                                    
                                  )#Tabpanel
                                  #tabsetpanel
                                  
                        ),#mainpanel
                      )#sidebarLayout
                      ),
             
             tabPanel(title="Variance Analysis",
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     fluidRow(column(width=4,selectInput('File_to_select_Var','Select file to study',choices='')),
                                              column(width=4,selectInput("Feature_to_analyse_Variance",'Choose feature to analyse',choices="")),
                                              column(width=4,selectInput("Factor_of_analysis_Variance","Choose factor of analysis",choices=""))),
                                     fluidRow(column(width=6,checkboxInput("Remove_outliers_variance","Remove outliers from analysis")),
                                              column(width=6,numericInput('Distance_from_Quartiles_variance','Remove outliers outside of [Q1-n*IQR, Q3+n*IQR]',value=1))),
                                     
                                     fluidRow(column(width=6,checkboxInput("normalize_per_input_resistance_Variance","Normalize current per input resistance")),
                                              ),
                                     checkboxInput('Show_points_variance_analysis','Show points in plot'),
                                     checkboxInput('Variance_custom_y_range','Use custom y-axis range'),
                                     fluidRow(column(width=6,numericInput('Variance_Minimum_y_limit','Select minimum y',value=0)),
                                              column(width=6,numericInput('Variance_Maximum_y_limit','Select maximum y',value=1))),
                                    
                                     verbatimTextOutput('Variance_bin_estimation_rule_explanation'),
                                     
                                     
                                     
                                     
                                     
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                  
                                    
                                    tabPanel(title='Independant Measure Analysis',
                                             fluidPage(fluidRow(column(width = 4,plotlyOutput('Pooled_Variance_Data_plot')),
                                                                column(width = 8,tableOutput('Pooled_Variance_Data_table'))), ),
                                             fluidPage(fluidRow(column(width = 6,plotlyOutput('Original_Variance_Data_Plot')),
                                                                column(width = 6,plotlyOutput('Variance_Data_without_outliers_Plot'))), ),
                                             fluidPage(fluidRow(column(width = 12,tableOutput('category_count_table_Variance') ))),
                                             fluidPage(fluidRow(
                                               column(width = 4,value_box(title= 'Normality test, Perform Shapiro test. If pval > 0.05, data is normally distributed',
                                                                          
                                                                          value = uiOutput("Normality_test_table_Variance"),
                                                                          theme = NULL,
                                                                          fill = TRUE, height = NULL),
                                                      
                                                      ) ,
                                               column(width = 4, value_box(title = "Variance homogeneity, Perform Levene test. If pval > 0.05, variance across groups can be assumed equal",
                                                                           
                                                                           value = uiOutput('Var_homogen_test_table_Variance'),
                                                                           theme = NULL),
                                                      )  ,
                                               column(width = 4, value_box(title = "Variance test. If either of the Shapiro or Levene test was statistically significant, go for non parametric variance test Kruskal Wallis, otherwise go for Anova",
                                                                           
                                                                           value = uiOutput('Variance_test_table_Variance'),
                                                                           theme = NULL)
                                                      )), ),
                                             fluidPage(fluidRow(column(width = 12,tableOutput('PWC_test_table_Variance'))), ),
                                             plotOutput("PWC_test_Plot_Variance"),
                                             actionButton('Save_Variance_anlysis_plot','Save Plot'),
                                             renderText("function_to_save"),
                                             fluidRow(column(width=3,selectInput('bin_estim_rule_Variance','Bin estimation rule',choices=c("Sturge","Freedman-Diaconis",'Doane','Scott'),selected='Sturge'),
                                                             checkboxInput("Display_only_full_pop","Display only full population")),
                                                      column(width = 3, numericInput('Variance_bin_width','Width of bins:',1),
                                                              checkboxInput("display_histogram", "Display histograms")),
                                                      column(width = 3, numericInput('Variance_scaling_factor', "Fit curve scaling factor", 1)),
                                                      column(width = 3, selectizeInput("Variance_distribution_group_to_display", "Select groups to display",choices="", selected = "", multiple = TRUE,
                                                                                       options = NULL))),
                                             plotOutput("Distribution_Variance_Analysis"), 
                                             actionButton("Save_Variance_distrib_plot", "Save Plot"),
                                             tableOutput("Distribution_Variance_Analysis_table"),
                                             actionButton("Save_Variance_anlysis_table", "Save Table")
                                             
                                             
                                    ),
                                   


                                    
                                    
                                  )#Tabpanel
                                  #tabsetpanel
                                  
                        ),#mainpanel
                      )#sidebarLayout
             ),
             
             tabPanel(title = "Distribution",
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     fluidRow(column(width=6,
                                     selectInput('File_to_select_Distrib','Select file to study',choices='')),
                                     column(width=6,selectInput("Feature_to_analyse_Distrib",'Choose feature to analyse',choices=""))),
                                     checkboxInput("normalize_per_input_resistance_Distrib","Normalize current per input resistance"),
                                     
                                     selectInput("select_outliers_to_remove_Distrib","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                                     fluidRow(column(width=6,selectInput('bin_estim_rule','Bin estimation rule',choices=c("Sturge","Freedman-Diaconis",'Doane','Scott'),selected='Sturge')),
                                              column(width = 6, numericInput('distribution_bin_width','Width of bins:',1))),
                                     verbatimTextOutput('bin_estimation_rule_explanation'),
                                     fluidRow(column(width=6,fluidRow(checkboxInput("fit_distribution","Fit Distribution"),
                                                                      checkboxInput("fit_all_distribution","Super impose distributions"))),
                                              column(width=6,checkboxGroupInput('distribution_to_fit','Select distribution to fit',choices=c("Gaussian","Exponential",'LogNormal',"Skewed Gaussian"),selected='Gaussian'))),
                                     
                                     checkboxInput('Distrib_custom_x_range','Use custom x-axis range'),
                                     fluidRow(column(width=6,numericInput('Minimum_x_limit','Select minimum x',value=0)),
                                              column(width=6,numericInput('Maximum_x_limit','Select maximum x',value=1))),
                                     checkboxInput('show_stats','Show stats')
                                     
                                     
                                     
                                     
                        ),
                        mainPanel(
                           tabsetPanel(
                             tabPanel(title="Population distribution",
                                     
                                     textOutput("textOutput"),
                                    plotOutput('fit_distribution_plot'),
                                    actionButton("Save_distrib_plot",'Save_plot'),
                                    
                                    tableOutput("distribution_plot_table"),
                                    tableOutput("distrib_stats"),
                                    plotOutput('QQPlot'),
                                    
                             )



                           )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),
             tabPanel(title = "Correlation",
                      sidebarLayout(
                        sidebarPanel(width=3,
                                     
                                     textOutput('analysis_ready'),
                                     selectizeInput("Feature_list_correlation", "Choose features to analyse",choices="", selected = "", multiple = TRUE,
                                                    options = NULL),
                                     selectInput("Factor_of_analysis_correlation","Choose factor of analysis",choices='',selected = ""),
                                     checkboxInput("analysis_by_factor_corr","Perform analysis per factor level"),
                                     checkboxInput('flip_coord',"Flip lower plots"),
                                     
                                     
                                     selectInput("select_outliers_to_remove_correlation","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                                     
                                     
                                     
                                     actionButton("Update_analysis_correlation",'Launch analysis'),
                                     checkboxInput("normalize_per_input_resistance_correlation","Normalize current per input resistance"),
                                     uiOutput("update_range_var_corr"),
                                     
                                     actionButton("Update_plot_correlation",'Update plot')
                                    
                                     
                                     
                                     
                                     
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel(title="Features correlation",
                                     
                                     plotOutput("correlogram_plot",inline=TRUE),
                                     
                            )
                            
                            
                            
                          )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             )
             
             #Here
             #tabpanel
             
             
  ),

  ##3
  
)





#source_python ('/Users/julienballbe/My_Work/Data_Analysis/python_ref.py')


server <- function(session,input, output) {
  
  myenv=new.env()
  #reticulate::source_python('/Users/julienballbe/My_Work/My_Librairies/pytho_function_for_R.py')
# Choose input directory for population class files
  shinyDirChoose(
    input,
    'population_data_folder',
    roots = getVolumes()(),
    filetypes = c('','csv')
  )
  
  global <- reactiveValues(population_datapath = getwd())
  
  population_files_dir <- reactive(input$population_data_folder)
  
  
  output$file_path <- renderText({
    global$population_datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$population_data_folder
                 input$Selected_Response_Type
               },
               handlerExpr = {
                 if (!"path" %in% names(population_files_dir())) return()
                 home <- normalizePath("~")
                 #unlist(dir()$root)
                 
                 global$population_datapath <-
                   file.path('','Volumes',unlist(population_files_dir()$root),paste(unlist(population_files_dir()$path[-1]), collapse = .Platform$file.sep))
                 
                 file_list=list.files(path= global$population_datapath, pattern=".csv", all.files=FALSE,
                                      full.names=FALSE)
                 sub_file_list <- file_list[grepl(input$Selected_Response_Type, file_list) | grepl('linear_values', file_list)|grepl('Population_Class', file_list)]
                 updateCheckboxGroupButtons(session,"Population_files","Select_population_files",choices=sub_file_list)
                 print('ZEZEZE')
               })
  

  import_csv_files <- eventReactive(input$import_files,{
    req(input$Population_files)
    print(global$population_datapath)
    print(input$Population_files)
    print(input$Selected_Response_Type)
    current_cell_file <- load_population_csv_file(global$population_datapath,input$Population_files,input$Selected_Response_Type)
    
    print('MYGOD')
    return (current_cell_file)
    
  })
  
  import_csv_files_repeated_measures <- eventReactive(input$import_files,{
    req(input$Population_files)
    print(global$population_datapath)
    print(input$Population_files)
    print(input$Selected_Response_Type)
    current_cell_file <- load_population_csv_file_repeated_measures(global$population_datapath,input$Population_files,input$Selected_Response_Type)
    
    print('MYGODRM')
    return (current_cell_file)
    
  })
  
##### FILES #####
  output$selectInputs <- renderUI({ 
    req(input$Population_files)
    req(input$import_files)
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    
    
    category_list = colnames(population_class)
    category_list=category_list[category_list != "Cell_id"]
    
    
   
    selectInputs <- lapply(category_list, function(i) {
      population_class[,i] <- as.factor(population_class[,i])
      group_list=levels(population_class[,i])
      selectizeInput(paste0('Subset_',i),paste0('Subset ',i), choices=group_list, selected = group_list, multiple = TRUE,
                     options = NULL)
      
    }) 
    do.call(tagList, selectInputs) 
  })
  
  
  
  
  subset_filter<- eventReactive(input$subset_data,{
    req(input$Population_files)
    req(input$import_files)
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    
    
    category_list = colnames(population_class)
    category_list=category_list[category_list != "Cell_id"]
    
    subset_dict <- lapply(category_list, function(i) {
      paste0(input[[paste0('Subset_',i)]])
    })
    
    names(subset_dict) <- category_list
    
    return(subset_dict)
  })
  
  
  
  
  output$population_class <- renderDataTable ({
    req(input$Population_files)
    req(input$import_files)
    
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    subset_filter_dict <- subset_filter()
    
    
    category_list = colnames(population_class)
    category_list=category_list[category_list != "Cell_id"]
    
    file_list <- names(file_import)
    file_list = file_list[file_list != 'Population_Class']
    file_list = file_list[file_list != 'Linear_Values']
    file_list = file_list[file_list != 'Unit_File']
    

    #feature_file = file_import[[1]]
    feature_file = file_import$Unit_File
    
    feature_list=colnames(feature_file)
    

    feature_list=feature_list[feature_list!='Cell_id']
    feature_list=feature_list[feature_list!='Obs']
    

    updateSelectInput(session,"Factor_of_analysis_data_repartition","Choose factor of analysis",choices=category_list)
    updateSelectInput(session,"Feature_to_analyse_data_repartition","Choose feature to analyse",choices=feature_list)

    updateSelectInput(session,"Factor_of_analysis_RM","Choose factor of analysis",choices=category_list)
    updateSelectInput(session,"Feature_to_analyse_RM","Choose feature to analyse",choices=feature_list)

    updateSelectInput(session,"Factor_of_analysis_Variance","Choose factor of analysis",choices=category_list)
    updateSelectInput(session,"Feature_to_analyse_Variance","Choose feature to analyse",choices=feature_list)

    updateSelectInput(session,"Factor_of_analysis_Distrib","Choose factor of analysis",choices=category_list)
    updateSelectInput(session,"Feature_to_analyse_Distrib","Choose feature to analyse",choices=feature_list)
    
    
    category_list_faceting=category_list[category_list!=input$Factor_of_analysis_data_repartition]
    
    updateSelectInput(session,"Factor_to_facet"," Facet plot per",choices=category_list_faceting)
    density_category=append(category_list,'Response_Duration')
    
    updateSelectInput(session,'Factor_for_density_plot','Choose density factor',choices=density_category)
    
    
    updateSelectInput(session,"File_to_select_Var","Select file to analyse",choices = file_list)
    updateSelectInput(session,"File_to_select_Distrib","Select file to analyse",choices = file_list)
    print("FILE LIST")
    print(file_list)
    
    
    
    print('Population class ok')
    
    
    population_class
    

  })
  
  

  
  ##### DATA ANALYSIS ######
  
  ###### DATA OVERVIEW #######
  
  output$descriptive_data_table <- renderDataTable({
    
    req(input$import_files)
    print('Descriptive')
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame
    
    
  })
  
  output$data_density <- renderPlotly({
    
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    if (input$select_outliers_to_remove_repartition != 'None'){
      full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
    }
    
    histogram_plot = ggplot2::ggplot(full_data_frame, aes_string(x = input$Feature_to_analyse_data_repartition, fill = input$Factor_of_analysis_data_repartition))+
      ggplot2::geom_histogram( position='identity',alpha = 0.6, binwidth  = input$bins_width_histogram,center=input$bins_width_histogram/2,)
    
    
    # density_plot=ggplot(full_data_frame, aes_string(x = input$Feature_to_analyse_data_repartition, y = input$Factor_for_density_plot)) +
    #   geom_density_ridges(aes_string(fill = input$Factor_for_density_plot))
    # 
    if (input$Facet_decriptive_plot==TRUE){
      if (input$descriptive_show_mean == TRUE){
        mu <- ddply(full_data_frame,c( input$Factor_of_analysis_data_repartition,'Response_Duration',input$Factor_to_facet), summarise, grp.mean=mean(get(input$Feature_to_analyse_data_repartition)))
        histogram_plot=histogram_plot+ggplot2::geom_vline(data=mu, aes(xintercept=grp.mean, color=get(input$Factor_of_analysis_data_repartition)),linetype="dashed")
        
      }
      histogram_plot=histogram_plot+ggplot2::facet_grid(Response_Duration~get(input$Factor_to_facet))
    }
    else{
      if (input$descriptive_show_mean == TRUE){
        mu <- ddply(full_data_frame,c( input$Factor_of_analysis_data_repartition,'Response_Duration'), summarise, grp.mean=mean(get(input$Feature_to_analyse_data_repartition)))
        histogram_plot=histogram_plot+ggplot2::geom_vline(data=mu, aes(xintercept=grp.mean, color=get(input$Factor_of_analysis_data_repartition)),linetype="dashed")
        
      }
      histogram_plot=histogram_plot+ggplot2::facet_grid(Response_Duration~.)
    }
    
    
    #histogram_plot=histogram_plot+xlim(input$Slider_density_plot_xlim[1],input$Slider_density_plot_xlim[2])
    histogram_plot = ggplotly(histogram_plot,dynamicTicks = T)
    histogram_plot
    
  })
  
  output$Data_sunburst_repartition <- renderPlotly({
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    color_dict <- c(
      'Allen_Cell_Type_Database' = "#027510",
      "Lantyer_Database" = "#02ad19",
      "NVC_Database" = "#02de20",
      "Scala_2019_DB" = "#50d462",
      "Scala_2021" = "#399946",
      "Harisson_Database" = "#79f78a",
      "Room Temperature" = "#635cf7",
      "25 C" = "#837dfa",
      "32 C" = "#fc9338",
      "34 C" = "#d17626",
      "Physiological temperature" = "#f0a767",
      "VIS" = "#0249a6",
      "SS" = "#2b5894",
      "MO" = "#657e9e",
      "AUD" = "#768291",
      "TE" = "#768291",
      "RSP" = "#768291",
      "1" = "#038c9e",
      "2/3" = "#02a2b8",
      "4" = "#02bad4",
      "5" = '#44c2d4',
      "6" = "#6fc4d1",
      "Excitatory" = "#02a866",
      "PValb" = "#ad4102",
      "Sst" = "#e35502",
      "Htr3a" = "#d9804c",
      "Vip" = "#f76240",
      "NPY" = "#bf452a",
      "--" = "#ffffff",
      "Unknown" = "#ffffff"
    )
    
    # Create a sunburst plot
    fig <- plot_ly(
      data = full_data_frame,
      type = 'sunburst',
      labels = ~Database,
      parents = ~Recording_Temperature,
      textinfo = 'label+value'
    )
    
    # Update traces with custom colors
    fig <- fig %>% 
      layout(sunburstcolorway = unname(color_dict[full_data_frame$Database]))
    
    
    # Show the plot
    fig
    
    })
  
  output$Data_repartition <- renderPlotly({
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    if (input$select_outliers_to_remove_repartition != 'None'){
      full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
    }
    
    if (input$normalise_descriptive_plot == TRUE){
      geombar_position='fill'
    }
    else{
      geombar_position='stack'
    }
    
    
    
    repartition_plot=ggplot2::ggplot(full_data_frame,mapping=aes_string("Response_Duration",fill=input$Factor_of_analysis_data_repartition))
                                                                                                                                             
    if (input$Facet_decriptive_plot==TRUE){
      repartition_plot=repartition_plot+ggplot2::facet_wrap(~get(input$Factor_to_facet))+
        ggplot2::geom_bar(position=geombar_position)+
        ggplot2::ggtitle(paste0('Number of ',as.character(input$Feature_to_analyse_data_repartition) , ' observations at different Response Duration per ',as.character(input$Factor_to_facet)))
    }
    else{
      repartition_plot=repartition_plot+ggplot2::geom_bar(position=geombar_position)+
        ggplot2::ggtitle(paste0('Number of ',as.character(input$Feature_to_analyse_data_repartition) , ' observations at different Response Duration'))
    }
    
    repartition_plot=repartition_plot+ggplot2::theme(axis.text.x = element_text( angle = 45))
      
      
    #repartition_plot <- ggplotly(repartition_plot,dynamicTicks=TRUE)    
    repartition_plot
    
  })
  
  
  ###### OVER RESPONSE DURATION #######
 
  cell_id_to_analyse <- reactive({
    
    file_import <- import_csv_files()
    relayout <- event_data("plotly_click",source='varibility_plot_time_response')
    if (input$group_specific == TRUE){
      subset_filter_dict <- subset_filter()
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
      if (input$select_outliers_to_remove_repartition != 'None'){
        full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
      }
      
      Factor_list=levels(full_data_frame[,input$Factor_of_analysis_data_repartition])
      
      point_factor=Factor_list[relayout$curveNumber+1]
      
      subset_df=full_data_frame[full_data_frame[,input$Factor_of_analysis_data_repartition] %in% point_factor,]
      
      index=relayout$pointNumber+1
      
      cell_id=subset_df[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }
    
    else{
      subset_filter_dict <- subset_filter()
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
      if (input$select_outliers_to_remove_repartition != 'None'){
        full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
      }
      
      index=relayout$pointNumber+1
      
      cell_id=full_data_frame[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }
    
    return(cell_id)
  })

  
  
  output$varibility_plot_time_response <- renderPlotly({
    
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    if (input$select_outliers_to_remove_repartition != 'None'){
      full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
    }
    
    if (input$group_specific == TRUE){
      
      filling=input$Factor_of_analysis_data_repartition
      colouring=input$Factor_of_analysis_data_repartition
    }
    else{
      filling=NULL
      colouring=NULL
    }
    
    
    if (input$variability_plot == 'boxplot'){
      
      
      if (input$show_geom_jitter == F){
        p <- ggplot2::ggplot(full_data_frame, mapping=aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=filling,text =  "Cell_id"))+
          ggplot2::geom_boxplot(mapping=aes_string(text='Cell_id'))+
          ggplot2::labs(x='Response_Duration',y=as.character(input$Feature_to_analyse_data_repartition))+
          ggplot2::theme(axis.text.x=element_text(angle=45, vjust=0.4,hjust=1),legend.position="bottom")
        
        p <- ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response")%>%layout(boxmode = "group")
        
      }
      
      else{
        p <- ggplot2::ggplot(full_data_frame, aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=input$Factor_of_analysis_data_repartition,text =  "Cell_id"))+
          ggplot2::geom_jitter(position=position_jitterdodge())+ggplot2::geom_point(alpha=.2)
        
        ggplot2::labs(x='Response_Duration',y=as.character(input$Feature_to_analyse_data_repartition))+ggplot2::scale_fill_discrete(name=as.character(input$Factor_of_analysis_data_repartition))+
          ggplot2::theme(axis.text.x=element_text(angle=45, vjust=0.4,hjust=1),legend.position="bottom")
        
        p <-  ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response") %>% layout(boxmode = "group")
      }
      
      
      
    }
    
    if (input$variability_plot == 'jitterplot'){
      
      
      
      p=ggplot2::ggplot(full_data_frame, aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=colouring,text='Cell_id'))+
        ggplot2::geom_point(alpha=.4,width = .7)
      p <-  ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response")
    }
    
    p
  })

  output$click <- renderPrint({
    
    req(input$import_files)
    
    file_import <- import_csv_files()
    relayout <- event_data("plotly_click",source='varibility_plot_time_response')

    if (input$group_specific == TRUE){

      subset_filter_dict <- subset_filter()
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
      if (input$select_outliers_to_remove_repartition != 'None'){
        full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
      }
      
      Factor_list=levels(full_data_frame[,input$Factor_of_analysis_data_repartition])
      
      point_factor=Factor_list[relayout$curveNumber+1]
      
      subset_df=full_data_frame[full_data_frame[,input$Factor_of_analysis_data_repartition] %in% point_factor,]
      
      index=relayout$pointNumber+1
      
      cell_id=subset_df[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
      
    }

    else{
      subset_filter_dict <- subset_filter()
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
      if (input$select_outliers_to_remove_repartition != 'None'){
        full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
      }
      
      index=relayout$pointNumber+1
      
      cell_id=full_data_frame[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }

    print(cell_id)

  })


  
  output$summary_statistics <- renderTable({
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    if (input$select_outliers_to_remove_repartition != 'None'){
      full_data_frame <- perform_ANOVA(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,remove_outliers = input$select_outliers_to_remove_repartition,what_to_return = "DF_without_outliers")
    }
    
    
    summary_stats=full_data_frame %>%
      group_by_(input$Factor_of_analysis_data_repartition, "Response_Duration") %>%
      get_summary_stats(input$Feature_to_analyse_data_repartition, type = "mean_sd")
    
    print(summary_stats)
  },digits = -3)
  
  
  output$outlier_count <- renderTable({
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    outlier_df=dataframe_outliers(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition)
    if (input$select_outliers_to_remove == 'None'){
      outlier_count <- outlier_df%>%
        group_by_(input$Factor_of_analysis_data_repartition,"Response_Duration")%>%summarise(Count=n())
      
    }
    else{
      if (input$select_outliers_to_remove == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        outlier_count <- outlier_df%>%
          group_by_(input$Factor_of_analysis_data_repartition,"Response_Duration","is.outlier")%>%summarise(Count=n())
        outlier_count=subset(outlier_count, is.outlier == TRUE)
      }
      
      if (input$select_outliers_to_remove == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        outlier_count <- outlier_df%>%
          group_by_(input$Factor_of_analysis_data_repartition,"Response_Duration","is.extreme")%>%summarise(Count=n())
        outlier_count=subset(outlier_count, is.extreme == TRUE)
      }
      
    }
    
    outlier_count
  })
  
  output$outliers_plot <- renderPlotly({
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    outlier_df=dataframe_outliers(full_data_frame,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition)
    
    if (input$select_outliers_to_remove == 'None'){
      outlier_plot=ggplot2::ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+ggplot2::geom_boxplot(outlier.shape = NA )+ggplot2::facet_grid( ~ Response_Duration)
      outlier_plot=outlier_plot+ggplot2::ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)
      
    }
    else{
      if (input$select_outliers_to_remove == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        outlier_plot=ggplot2::ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+ggplot2::geom_boxplot(outlier.shape =NA )+ggplot2::geom_point(aes_string(color='is.extreme',text='Cell_id'))+ggplot2::facet_grid( ~ Response_Duration)+ ggplot2::scale_color_manual(values = c("darkblue", "red"))
        outlier_plot=outlier_plot+ggplot2::ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
      if (input$select_outliers_to_remove == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        outlier_plot=ggplot2::ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+ggplot2::geom_boxplot(outlier.shape =NA )+ggplot2::geom_point(aes_string(color='is.extreme',text='Cell_id'))+ggplot2::facet_grid( ~ Response_Duration)+ ggplot2::scale_color_manual(values = c("darkblue", "red"))
        outlier_plot=outlier_plot+ggplot2::ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
    }
    outlier_plotly
    
  })
  
  
 ##### REPEATED MEASURE ANALYSIS #####
  
  output$Pooled_Variance_Data_table_Repeated_measures <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    if (input$normalize_per_input_resistance_Variance){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    Test_DF = full_data_frame_RM_Anova
    
    
    stat_table = get_statistics_table_RM(full_data_frame_RM_Anova, Ind_Var, "value")
    stat_table
    
    
    
  })
  
  output$Pooled_Variance_Data_table_without_outliers_Repeated_measures <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    if (input$normalize_per_input_resistance_Variance){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
    if (input$Remove_outliers_variance == FALSE){
      original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = FALSE, distance_to_quartiles =0, what_to_return = "DF_without_outliers")
      
    }
    else{
      original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return = "DF_without_outliers")
      
    }
    Test_DF = full_data_frame_RM_Anova
    
    
    stat_table = get_statistics_table_RM(full_data_frame_RM_Anova, Ind_Var, "value")
    stat_table
    
    
    
  })
  
  
  output$Original_RM_Data_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
    
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
  
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return = "Oulier_df")
    
    if (input$Remove_outliers_variance_RM == FALSE){
      outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
        ggplot2::geom_boxplot(outlier.shape = NA )
      outlier_plot=outlier_plot+
        ggplot2::ggtitle('Original Data')
      outlier_plotly=ggplotly(outlier_plot,dynamicTicks = TRUE)
     }
    
    else{
      outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
        ggplot2::geom_boxplot(outlier.shape = NA )+
        ggplot2::geom_point(aes_string(color='is_outlier',text='Cell_id'))+ 
        ggplot2::scale_color_manual(values = c("blue", "red"))
      outlier_plot=outlier_plot+ggplot2::ggtitle('Original Data')+ggplot2::ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot, dynamicTicks = TRUE)
    }
    
    
    outlier_plotly
    })
  
  output$Pooled_Variance_Data_table_without_outliers_Repeated_measures <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return = "DF_without_outliers")
    
    Test_DF = full_data_frame_RM_Anova
    
    
    stat_table = get_statistics_table_RM(original_dataframe, Ind_Var, value)
    stat_table
    
    
    
  })
  
  
  output$RM_Data_without_outliers_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return = "DF_without_outliers")
    
    
    
    without_outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
      ggplot2::geom_boxplot(outlier.shape = NA )
    without_outlier_plot=without_outlier_plot+
      ggplot2::ggtitle('Analysed Data')
    without_outlier_plotly=ggplotly(without_outlier_plot)
    
    without_outlier_plotly
    
  })
  
  output$category_count_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    category_count_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="Categories_count")
    category_count_table
    
  },digits = -3)
  
  output$Normality_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    normality_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="Normality_table")
    normality_table
    
  },digits = -3)
  
  output$Variance_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="Variance_test")
    Variance_test_table
    
  },digits = -3)
  
  
  output$PWC_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="PWC_without_position")
    
    
    PWC_test_table
    
  },digits = -3)
  
  
  output$PWC_test_Plot <- renderPlot({
    req(input$import_files)
    file_import <- import_csv_files_repeated_measures()
    subset_filter_dict <- subset_filter()
    full_data_frame_RM_Anova <- create_full_df_RM_ANOVA(file_import,
                                                        input$Feature_to_analyse_RM,
                                                        subset_filter_dict,
                                                        input$Selected_Response_Type,
                                                        keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Ind_var'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_RM_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_GOhms","Time_constant_ms")
      
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Ind_var)
      
    }
    else{
      full_data_frame_RM_Anova=full_data_frame_RM_Anova%>%gather(key = "Ind_var", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Ind_var)
    }
    
    colnames(full_data_frame_RM_Anova)[colnames(full_data_frame_RM_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]=full_data_frame_RM_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_RM_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    full_data_frame_RM_Anova$Ind_var <- factor(full_data_frame_RM_Anova$Ind_var,levels=mixedsort(levels(full_data_frame_RM_Anova$Ind_var)))
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="DF_without_outliers")
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return = "Variance_test_original_table")
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_RM_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$Remove_outliers_variance_RM, distance_to_quartiles =input$Distance_from_Quartiles_variance_RM, what_to_return ="PWC")
    
    PWC_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
      ggplot2::geom_boxplot(outlier.shape =NA )+
      ggplot2::geom_jitter(mapping=aes_string(alpha=.8),width = 0.25)+
      ggplot2::labs(
        subtitle = get_test_label(Variance_test_table, detailed = TRUE)
      )+
      ggplot2::ylab(current_unit)
    
    
    if(nrow(PWC_test_table)!=0){
    PWC_plot=PWC_plot+stat_pvalue_manual(PWC_test_table, tip.length = 0, hide.ns = TRUE) }
    
    PWC_plot
    
  })
 

  
  ##### VARIANCE ANALYSIS #####
  

output$Pooled_Variance_Data_plot <- renderPlotly({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  
  if (input$normalize_per_input_resistance_Variance){
    
    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
    
    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  #  Distance_from_Quartiles_variance
  factor=input$Factor_of_analysis_Variance
  
  value=as.character(input$Feature_to_analyse_Variance)
  if (input$Remove_outliers_variance == FALSE){
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Outlier_df")
  }
  else{
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Outlier_df")
  }
  
  original_dataframe[,factor] = "Full population"
  original_dataframe[,factor] <- as.factor(original_dataframe[,factor])
  
  
  if (input$Remove_outliers_variance == FALSE){
  Full_population_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
    ggplot2::geom_boxplot(outlier.shape = NA )+
    ggplot2::ggtitle('Full population Data')+
    ggplot2::ylab(current_unit)
  Full_population_plotly=ggplotly(Full_population_plot,dynamicTicks = TRUE)}
  
  else{
    outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape =NA )+
      ggplot2::geom_point(aes_string(color='is_outlier',text='Cell_id'))+ 
      ggplot2::scale_color_manual(values = c("blue", "red"))+
      ggplot2::ggtitle('Full population Data')+
      ggplot2::ylab(current_unit)
    outlier_plotly=ggplotly(outlier_plot,dynamicTicks = TRUE)
  }
  
})
  
  output$Pooled_Variance_Data_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Variance,
                                                  input$File_to_select_Var,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Variance]
    
    if (input$normalize_per_input_resistance_Variance){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Variance
    
    value=as.character(input$Feature_to_analyse_Variance)
    
    
    if (input$Remove_outliers_variance == FALSE){
      global_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Outlier_df")
    }
    else{
      global_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Outlier_df")
    }
    
    stat_table = get_statistics_table(global_dataframe, factor, value)
    stat_table
    
    
    
  })
  
  
  
  
output$Original_Variance_Data_Plot <- renderPlotly({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                   input$Feature_to_analyse_Variance,
                                                   input$File_to_select_Var,
                                                   subset_filter=subset_filter_dict,
                                                   keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance

  value=as.character(input$Feature_to_analyse_Variance)

  #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Outlier_df")
  
  if (input$Remove_outliers_variance == FALSE){
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Outlier_df")
  }
  else{
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Outlier_df")
  }
  
  if (input$Remove_outliers_variance == FALSE){
    outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape = NA )+
      ggplot2::ggtitle('Original Data')+
      ggplot2::ylab(current_unit)
    outlier_plotly=ggplotly(outlier_plot,dynamicTicks = TRUE)

  }
  else{
    outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape =NA )+
      ggplot2::geom_point(aes_string(color='is_outlier',text='Cell_id'))+ 
      ggplot2::scale_color_manual(values = c("blue", "red"))+
      ggplot2::ggtitle('Original Data')+
      ggplot2::ylab(current_unit)
    outlier_plotly=ggplotly(outlier_plot,dynamicTicks = TRUE)

  }

  outlier_plotly
})


output$Variance_Data_without_outliers_Plot <- renderPlotly({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance

  value=as.character(input$Feature_to_analyse_Variance)

  
  
  #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  if (input$Remove_outliers_variance == FALSE){
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "DF_without_outliers")
  }
  else{
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  }
  
  
  unique_categories = unique(original_dataframe[,factor])
  
  categories_list = c()
  for (current_level in unique_categories) {
    # Subset the data for the current level
    sub_table = original_dataframe[original_dataframe[, factor] == current_level, ]
    sub_feature = array(sub_table[, value])
    
    if (length(sub_feature) < 15){
      next
    }
    else{
      categories_list <- c(categories_list, current_level)
    }
  }
  
  updateSelectizeInput(session,"Variance_distribution_group_to_display", "Select groups to display",choices=categories_list, selected = categories_list)
  
  
  
  without_outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = factor, y =  input$Feature_to_analyse_Variance))+
    ggplot2::geom_boxplot(outlier.shape = NA )+
    ggplot2::ggtitle('Analysed Data')+
    ggplot2::ylab(current_unit)
  without_outlier_plotly=ggplotly(without_outlier_plot,dynamicTicks = TRUE)

  without_outlier_plotly

})


output$category_count_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  
  #category_count_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Categories_count")
  if (input$Remove_outliers_variance == FALSE){
    category_count_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Categories_count")
  }
  else{
    category_count_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Categories_count")
  }
  
  category_count_table

})


output$Normality_test_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  
  #normality_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Normality_table")
  if (input$Remove_outliers_variance == FALSE){
    normality_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Normality_table")
  }
  else{
    normality_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Normality_table")
  }
  normality_table

},digits = -3)


output$Var_homogen_test_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  
  #Variance_homogeneity_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_homogeneity_table")
  if (input$Remove_outliers_variance == FALSE){
    Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Variance_homogeneity_table")
  }
  else{
    Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_homogeneity_table")
  }
  Variance_homogeneity_table

},digits = -3)


output$Variance_test_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  
  #Variance_homogeneity_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_test")
  if (input$Remove_outliers_variance == FALSE){
    Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Variance_test")
  }
  else{
    Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_test")
  }
  Variance_homogeneity_table

},digits = -3)


output$PWC_test_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  
  #Variance_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "PWC_without_position")
  if (input$Remove_outliers_variance == FALSE){
    Variance_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "PWC_without_position")
  }
  else{
    Variance_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "PWC_without_position")
  }
  Variance_test_table

},digits = -3)


output$PWC_test_Plot_Variance <- renderPlot({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){

    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance
  

  value=as.character(input$Feature_to_analyse_Variance)
  
  if (input$Remove_outliers_variance == FALSE){
    #Variance_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "PWC_without_position")
    original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "DF_without_removed_levels")
    Variance_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "Variance_test_original_table")
    PWC_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "PWC")
  }
  else{
    original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_removed_levels")
    
    Variance_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_test_original_table")
    PWC_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "PWC")
  }
  # Variance_test_table
  # 
  # 
  # original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_removed_levels")
  # 
  # Variance_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "Variance_test_original_table")
  # PWC_test_table <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "PWC")
  # 
  
  
  if (nrow(PWC_test_table)==0){
    PWC_plot=ggplot2::ggplot(original_dataframe,aes_string( x = factor, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape =NA )+
      ggplot2::ylab(current_unit)
  }
  else{
    PWC_plot=ggplot2::ggplot(original_dataframe,aes_string( x = factor, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape =NA )+
      ggplot2::labs(
        subtitle = get_test_label(Variance_test_table, detailed = TRUE),
        caption = get_pwc_label(PWC_test_table)
      )+
      ggplot2::ylab(current_unit)
    }
 if (input$Show_points_variance_analysis == TRUE){
   PWC_plot=PWC_plot+ggplot2::geom_jitter(mapping=aes_string(alpha=.8),width = 0.25)
 }
  if (input$Variance_custom_y_range){
    PWC_plot = PWC_plot+ylim(input$Variance_Minimum_y_limit,input$Variance_Maximum_y_limit)
  }
  if(nrow(PWC_test_table)!=0){
    if (input$Variance_custom_y_range){
      
      PWC_test_table[,'y.position']=(PWC_test_table[,'y.position']/max(PWC_test_table[,'y.position']))*input$Variance_Maximum_y_limit
      
      PWC_plot = PWC_plot+ylim(input$Variance_Minimum_y_limit,input$Variance_Maximum_y_limit)
    }
    PWC_plot=PWC_plot+stat_pvalue_manual(PWC_test_table, tip.length = 0, hide.ns = TRUE) }
  PWC_plot=PWC_plot+theme_classic()
  PWC_plot = PWC_plot + ggplot2::theme(text = element_text(size = 17,face="bold"),axis.text = element_text(size = 18),axis.text.x = element_text(angle=45,hjust = 1)) 
  PWC_plot = PWC_plot + ggplot2::theme(legend.position = "none")
  myenv$PWC_plot = PWC_plot
  PWC_plot


})


observeEvent(c(input$File_to_select_Variance,
               input$Feature_to_analyse_Variance,
               input$normalize_per_input_resistance_Variance,
               input$Factor_of_analysis_Variance,
               input$bin_estim_rule_Variance),{
                 req(input$import_files)
                 file_import <- import_csv_files()
                 subset_filter_dict <- subset_filter()
                 full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                               input$Feature_to_analyse_Variance,
                                                               input$File_to_select_Var,
                                                               subset_filter=subset_filter_dict,
                                                               keep_na=FALSE)
                 Unit_list=file_import$Unit_File
                 current_unit=Unit_list[,input$Feature_to_analyse_Variance]
                 if (input$normalize_per_input_resistance_Variance){
                   
                   if (grepl("/pA",current_unit)==TRUE){
                     full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                   
                   else if (grepl("pA",current_unit)==TRUE){
                     full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                 }
                 factor=input$Factor_of_analysis_Variance
                 
                 value=as.character(input$Feature_to_analyse_Variance)
                 
                 
                 
                 #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
                 if (input$Remove_outliers_variance == FALSE){
                   original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "DF_without_outliers")
                 }
                 else{
                   original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
                 }
                 distrib_dataframe = original_dataframe
                 distribution_array = array(distrib_dataframe[, value])
                 
                
                 nb_obs=length(distribution_array)
                 
                 if (input$bin_estim_rule_Variance == 'Sturge'){
                   bin_nb = 1 + 3.322*log2(nb_obs) 
                   bin_nb = as.integer(round(bin_nb,0))#bin_estim_rule_VarainceVariance_bin_estimation_rule_explanation
                   updateNumericInput(session, 'Variance_bin_width','Number of bins:', bin_nb)
                 }
                 
                 if (input$bin_estim_rule_Variance == 'Freedman-Diaconis'){
                   bin_width = 2*IQR(distribution_array)/(nb_obs**(1/3))
                   bin_width = round(bin_width,2)
                   updateNumericInput(session, 'Variance_bin_width','Width of bins:', bin_width)
                 }
                 
                 if (input$bin_estim_rule_Variance == 'Doane'){
                   skewness = 3*(mean(distribution_array)-median(distribution_array))/std(distribution_array)
                   sigma=sqrt((6*(nb_obs-2))/((nb_obs+1)*(nb_obs+3)))
                   bin_nb = 1 + log2(nb_obs) + log2(1+(skewness/sigma))
                   bin_nb = as.integer(round(bin_nb,0))
                   updateNumericInput(session, 'Variance_bin_width','Number of bins:', bin_nb)
                   
                 }
                 
                 if (input$bin_estim_rule_Variance == 'Scott'){
                   bin_width = (3.49*std(distribution_array))/(nb_obs**(1/3))
                   bin_width=round(bin_width,2)
                   updateNumericInput(session, 'Variance_bin_width','Width of bins:', bin_width)
                   
                 }
                 
                 
                 
                 
               })


output$Distribution_Variance_Analysis <- renderPlot({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){
    
    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
    
    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance
  
  value=as.character(input$Feature_to_analyse_Variance)
  
  
  
  #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  if (input$Remove_outliers_variance == FALSE){
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "DF_without_outliers")
  }
  else{
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  }
  distrib_dataframe = original_dataframe
  
  # Calculate the frequency of each unique category and arrange by descending count
  unique_categories <- distrib_dataframe %>%
    count(!!sym(factor)) %>%           # Count occurrences of each category
    arrange(desc(n)) %>%               # Sort in descending order of frequency
    pull(!!sym(factor))    
  
  max_bin_height = 0
  for (current_level in unique_categories) {
    # Subset the data for the current level
    sub_table = distrib_dataframe[distrib_dataframe[, factor] == current_level, ]
    sub_feature = array(sub_table[, value])
    
    if (length(sub_feature) < 15){
      next
    }
    # Add histogram to the plot for the current level
    if ((input$bin_estim_rule_Variance == "Sturge") | (input$bin_estim_rule_Variance == "Doane")){
      nb_bins = input$Variance_bin_width
      hist_data <- hist(sub_feature, plot = FALSE, breaks = input$bin_estim_rule_Variance)
      
    }
    
    if ((input$bin_estim_rule_Variance == "Freedman-Diaconis") | (input$bin_estim_rule_Variance == "Scott")){
      bin_width = input$Variance_bin_width
      hist_data <- hist(sub_feature, plot = FALSE, breaks = input$bin_estim_rule_Variance)
    }
    current_max_bin_height <- max(hist_data$counts)
    
    if (current_max_bin_height>=max_bin_height){
      max_bin_height <- current_max_bin_height
    }
  }
  
  
  # Define the range of values to compute fit lines (based on min and max of the 'value' column)
  value_range <- seq(min(distrib_dataframe[, value]), max(distrib_dataframe[, value]), by = .01)
  
  
  distribution_plot = ggplot2::ggplot()
  fit_table <- data.frame(Value = numeric(0), Fit_result = numeric(0), Factor_level = character(0))
  if (input$Display_only_full_pop == TRUE){
    
    # Create an empty dataframe to store fit results
    
    
    
    # Plot the aggregate histogram for all data (before the loop)
    if (input$display_histogram == TRUE){
      if ((input$bin_estim_rule_Variance == "Sturge") | (input$bin_estim_rule_Variance == "Doane")){
        nb_bins = input$Variance_bin_width
        
        distribution_plot = distribution_plot + 
          ggplot2::geom_histogram(data = distrib_dataframe, ggplot2::aes_string(x = value), fill = "black", alpha = 0.1, 
                                  bins = nb_bins,
                                  position = "identity") +
          labs(fill = "Category", color = "Category")
      }
      
      if ((input$bin_estim_rule_Variance == "Freedman-Diaconis") | (input$bin_estim_rule_Variance == "Scott")){
        
        bin_width = input$Variance_bin_width
        
        distribution_plot = distribution_plot + 
          ggplot2::geom_histogram(data = distrib_dataframe, ggplot2::aes_string(x = value), fill = "black", alpha = 0.1, 
                                  binwidth = bin_width,
                                  position = "identity") +
          labs(fill = "Category", color = "Category")
      }
    }
    
    # Fit the distribution using the Python function
    Full_pop_feature = array(distrib_dataframe[, value])
    Full_population_fit_result = fit_distribution_new(Full_pop_feature, c('Gaussian', 'Skewed Gaussian'))
    
    # Select the best fit based on the lowest AIC
    Full_pop_best_fit_result = Full_population_fit_result[Full_population_fit_result[, 'AIC'] == min(Full_population_fit_result[, 'AIC']), ]
    Full_pop_best_fit_type = Full_pop_best_fit_result$Fit
    
    
    # Compute the fit values for the given range (min to max of value column)
    if (Full_pop_best_fit_type == "Gaussian") {
      mean_val = Full_pop_best_fit_result$mu
      sd_val = Full_pop_best_fit_result$sigma
      
      # Compute Gaussian fit values for the value range
      fit_values = dnorm(value_range, mean = mean_val, sd = sd_val)
      
    } else if (Full_pop_best_fit_type == "Exponential") {
      rate_val = Full_pop_best_fit_result$tau
      
      # Compute Exponential fit values for the value range
      fit_values = dexp(value_range, rate = rate_val)
      
    } else if (Full_pop_best_fit_type == "Skewed Gaussian") {
      mu_val = Full_pop_best_fit_result$mu
      sigma_val = Full_pop_best_fit_result$sigma
      alpha_val = Full_pop_best_fit_result$gamma
      
      # Define a function for the Skewed Gaussian
      skew_normal <- function(x, mu, sigma, alpha) {
        2 * dnorm((x - mu) / sigma) * pnorm(alpha * (x - mu) / sigma)
      }
      
      # Compute Skewed Gaussian fit values
      fit_values = skew_normal(value_range, mu = mu_val, sigma = sigma_val, alpha = alpha_val)
    }
    # Combine fit values into a dataframe
    Full_pop_fit_data <- data.frame(Value = value_range, Fit_result = fit_values, Factor_level = "Full population")
    max_fit_data = max(fit_values)
    Full_pop_fit_data[,'Fit_result'] = Full_pop_fit_data[,'Fit_result']*1/max_fit_data
    
    # Append the fit data for this factor level to the fit_table
    fit_table <- rbind(fit_table, Full_pop_fit_data)
    fit_table[,'Fit_result'] = fit_table[,'Fit_result']*input$Variance_scaling_factor
    
    
    distribution_plot = distribution_plot + 
      ggplot2::geom_line(data = fit_table, ggplot2::aes(x = Value, y = Fit_result, color = Factor_level, group = Factor_level), size = 0.7)
    
    distribution_plot = distribution_plot + 
      ggplot2::scale_color_manual(values = c("Full population" = "black"))
    
  }
  
  
  else{
    
    # Loop through each level of the factor
    for (current_level in unique_categories) {
      
      if (!(current_level %in% input$Variance_distribution_group_to_display)){
        next
      }
      
      # Subset the data for the current level
      sub_table = distrib_dataframe[distrib_dataframe[, factor] == current_level, ]
      sub_feature = array(sub_table[, value])
      
      if (length(sub_feature) < 15){
        next
      }
      
      # Fit the distribution using the Python function
      current_level_fit_result = fit_distribution_new(sub_feature, c('Gaussian', 'Skewed Gaussian'))
      
      # Select the best fit based on the lowest AIC
      best_fit_result = current_level_fit_result[current_level_fit_result[, 'AIC'] == min(current_level_fit_result[, 'AIC']), ]
      best_fit_type = best_fit_result$Fit
      
      # Compute the fit values for the given range (min to max of value column)
      if (best_fit_type == "Gaussian") {
        mean_val = best_fit_result$mu
        sd_val = best_fit_result$sigma
        
        # Compute Gaussian fit values for the value range
        fit_values = dnorm(value_range, mean = mean_val, sd = sd_val)
        
      } else if (best_fit_type == "Exponential") {
        rate_val = best_fit_result$tau
        
        # Compute Exponential fit values for the value range
        fit_values = dexp(value_range, rate = rate_val)
        
      } else if (best_fit_type == "Skewed Gaussian") {
        mu_val = best_fit_result$mu
        sigma_val = best_fit_result$sigma
        alpha_val = best_fit_result$gamma
        
        # Define a function for the Skewed Gaussian
        skew_normal <- function(x, mu, sigma, alpha) {
          2 * dnorm((x - mu) / sigma) * pnorm(alpha * (x - mu) / sigma)
        }
        
        # Compute Skewed Gaussian fit values
        fit_values = skew_normal(value_range, mu = mu_val, sigma = sigma_val, alpha = alpha_val)
      }
      
      # Combine fit values into a dataframe
      fit_data <- data.frame(Value = value_range, Fit_result = fit_values, Factor_level = current_level)
      
      max_fit_data = max(fit_values)
      fit_data[,'Fit_result'] = fit_data[,'Fit_result']*1/max_fit_data
      
      # Append the fit data for this factor level to the fit_table
      fit_table <- rbind(fit_table, fit_data)
      
      if (input$display_histogram == TRUE){
      # Add histogram to the plot for the current level
        if ((input$bin_estim_rule_Variance == "Sturge") | (input$bin_estim_rule_Variance == "Doane")){
          nb_bins = input$Variance_bin_width
          distribution_plot = distribution_plot + 
            geom_histogram(data = sub_table, aes_string(x = value, fill = factor), alpha = 0.9, bins = nb_bins, position = "identity")
        }
        
        if ((input$bin_estim_rule_Variance == "Freedman-Diaconis") | (input$bin_estim_rule_Variance == "Scott")){
          
          bin_width = input$Variance_bin_width
          distribution_plot = distribution_plot + 
            geom_histogram(data = sub_table, aes_string(x = value, fill = factor), alpha = 0.9, binwidth = bin_width, position = "identity")
        }
      }
      
      
    }
    
    # After the loop, add the computed fit lines to the plot
    original_fit_table = fit_table
    
    fit_table[,'Fit_result'] = fit_table[,'Fit_result']*input$Variance_scaling_factor
    
    
    distribution_plot = distribution_plot + 
      ggplot2::geom_line(data = fit_table, ggplot2::aes(x = Value, y = Fit_result, color = Factor_level, group = Factor_level), size = 1)+
      ggplot2::scale_color_brewer(palette="Dark2")+
      ggplot2::scale_fill_brewer(palette="Dark2")
    
  }
  
  
  # # Set custom color for "Full population" and use default for other levels
  # distribution_plot = distribution_plot + 
  #   ggplot2::scale_color_manual(values = c("Full population" = "black")) +
  #   ggplot2::scale_color_discrete(guide = guide_legend(override.aes = list(linetype = c("solid"))))
  # Finalize the plot with labels and theme
  if (input$Variance_custom_y_range){
    
    
    
    distribution_plot = distribution_plot+ggplot2::xlim(input$Variance_Minimum_y_limit,input$Variance_Maximum_y_limit)
  }
  title = paste0("Distribution of ", value, " per ",factor )
  
  distribution_plot = distribution_plot +
    labs(title = title, 
         x = current_unit, y = "Count (Histogram)", 
         fill = "Factor Level", color = "Factor Level") +
    theme_minimal()
  distribution_plot = distribution_plot +
    ggplot2::theme(text = element_text(size = 17,face="bold"),axis.text = element_text(size = 18),panel.grid.major = element_line(linewidth = 1))
  myenv$Variance_distrib_plot = distribution_plot
  distribution_plot
  
})

observeEvent(input$Save_Variance_distrib_plot,{
  myenv$table_to_save=myenv$Variance_distrib_plot
  print('drdrd')
  myenv$table_or_plot="plot"
  showModal(save_modal())
})

output$Distribution_Variance_Analysis_table <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  subset_filter_dict <- subset_filter()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$File_to_select_Var,
                                                subset_filter=subset_filter_dict,
                                                keep_na=FALSE)
  Unit_list=file_import$Unit_File
  current_unit=Unit_list[,input$Feature_to_analyse_Variance]
  if (input$normalize_per_input_resistance_Variance){
    
    if (grepl("/pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
    
    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_GOhms']))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance
  
  value=as.character(input$Feature_to_analyse_Variance)
  
  
  
  #original_dataframe <- perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$Remove_outliers_variance,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  if (input$Remove_outliers_variance == FALSE){
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = FALSE,distance_to_quartiles = 0 ,what_to_return = "DF_without_outliers")
  }
  else{
    original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = TRUE,distance_to_quartiles = input$Distance_from_Quartiles_variance,what_to_return = "DF_without_outliers")
  }
  distrib_dataframe = original_dataframe
  
  # Calculate the frequency of each unique category and arrange by descending count
  unique_categories <- distrib_dataframe %>%
    count(!!sym(factor)) %>%           # Count occurrences of each category
    arrange(desc(n)) %>%               # Sort in descending order of frequency
    pull(!!sym(factor))    
  
    # Create an empty dataframe to store fit results
    
    
    
    
    
    # Fit the distribution using the Python function
    Full_pop_feature = array(distrib_dataframe[, value])
    Full_population_fit_result = fit_distribution_new(Full_pop_feature, c('Gaussian', 'Skewed Gaussian'))
    
    # Select the best fit based on the lowest AIC
    Full_pop_best_fit_result = Full_population_fit_result[Full_population_fit_result[, 'AIC'] == min(Full_population_fit_result[, 'AIC']), ]
    Full_pop_best_fit_result[,'Population'] = "Full population"
    
    
    # Compute the fit values for the given range (min to max of value column)
    
    
    
    
    # Loop through each level of the factor
    for (current_level in unique_categories) {
      
      if (!(current_level %in% input$Variance_distribution_group_to_display)){
        next
      }
      
      # Subset the data for the current level
      sub_table = distrib_dataframe[distrib_dataframe[, factor] == current_level, ]
      sub_feature = array(sub_table[, value])
      
      if (length(sub_feature) < 15){
        next
      }
      
      # Fit the distribution using the Python function
      current_level_fit_result = fit_distribution_new(sub_feature, c('Gaussian', 'Skewed Gaussian'))
      
      # Select the best fit based on the lowest AIC
      best_fit_result = current_level_fit_result[current_level_fit_result[, 'AIC'] == min(current_level_fit_result[, 'AIC']), ]
      best_fit_result[,'Population'] = current_level
      
      Full_pop_best_fit_result = rbind(Full_pop_best_fit_result,best_fit_result)
      
      
    }
    myenv$Variance_distrib_table = Full_pop_best_fit_result
    Full_pop_best_fit_result
})

observeEvent(input$Save_Variance_anlysis_table,{
  myenv$table_to_save=myenv$Variance_distrib_table
  print('drdrd')
  myenv$table_or_plot="table"
  showModal(save_modal())
})

# shinyDirChoose(
#   input,
#   'Saving_folder_table',
#   roots = getVolumes()()
# )
# 
# observeEvent(ignoreNULL = TRUE,
#              eventExpr = {
#                input$Saving_folder_table
#              },
#              handlerExpr = {
#                if (!"path" %in% names(saving_folder_dir_table())) return()
#                home <- normalizePath("~")
#                #unlist(dir()$root)
#                
#                myenv$saving_folder_path_table <-
#                  file.path('','Volumes',unlist(saving_folder_dir_table()$root),paste(unlist(saving_folder_dir_table()$path[-1]), collapse = .Platform$file.sep))
#                
#                
#                
#              })
# 
# saving_folder_dir_table <- reactive(input$Saving_folder_table)
# 
# saving_vals_table <- reactiveValues(
#   saving_path=NULL,
#   saving_name=NULL,
#   dimension_width=NULL,
#   dimension_height=NULL,
#   proceed=FALSE
# )
# 
# 
# observeEvent(input$Save_Variance_anlysis_table,{
#   myenv$table_to_save=myenv$Variance_distrib_table
#   myenv$table_or_plot="table"
#   showModal(save_table_modal())
# })
# 
# save_table_modal <- function(failed=FALSE){
#   modalDialog(
#     shinyDirButton('Saving_folder_table', 'Select a saving folder', 'Please select a saving folder', FALSE),
#     
#     textInput("file_name_save_table", label= "File name (without .csv)"),
#     
#     
#     if (failed)
#       div(tags$b("Please enter all required information")),
#     footer = tagList(
#       modalButton("Cancel"),
#       actionButton("execute_saving_table","Save file"),
#     )
#   )
# }
# 
# observeEvent(input$execute_saving_table,{
#   print(input$file_name_save_table)
#   if (input$file_name_save_table != ""){
#     print("observeEvent(input$execute_saving_table")
#     saving_vals_table$saving_path <- myenv$saving_folder_path
#     saving_vals_table$saving_name <- input$file_name_save_table
#     
#     saving_vals_table$proceed = TRUE
#     
#     removeModal()
#   }
#   else{
#     showModal(save_table_modal(failed = TRUE))
#   }
# })
# 
# observeEvent(ignoreNULL = TRUE,
#              eventExpr = {
#                saving_vals_table$proceed
#              },
#              handlerExpr = {
#                if (saving_vals_table$proceed == TRUE){
#                  
#                  table_to_save=myenv$table_to_save
#                  saving_path = file.path(saving_vals_table$saving_path, paste0(saving_vals_table$saving_name,".csv"))
#                  write.csv(table_to_save, saving_path, row.names=TRUE)
#                  print(paste0(saving_vals_table$saving_name,".csv"))
#                  saving_vals_table$proceed=FALSE
#                  saving_vals_table$dimension_width =NULL
#                  saving_vals_table$dimension_height =NULL
#                  myenv$PWC_plot=NULL
#                  
#                }
#              })
# 



shinyDirChoose(
  input,
  'Saving_folder',
  roots = getVolumes()()
)

observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               input$Saving_folder
             },
             handlerExpr = {
               if (!"path" %in% names(saving_folder_dir())) return()
               home <- normalizePath("~")
               #unlist(dir()$root)
               
               myenv$saving_folder_path <-
                 file.path('','Volumes',unlist(saving_folder_dir()$root),paste(unlist(saving_folder_dir()$path[-1]), collapse = .Platform$file.sep))
               
               
               
             })

observeEvent(input$Save_Variance_anlysis_plot,{
  myenv$table_to_save=myenv$PWC_plot
  myenv$table_or_plot="plot"
  showModal(save_modal())
})




saving_folder_dir <- reactive(input$Saving_folder)





save_modal <- function(failed=FALSE){
  modalDialog(
    shinyDirButton('Saving_folder', 'Select a saving folder', 'Please select a saving folder', FALSE),
    
    textInput("file_name_save", label= "File name (without .pdf)"),
   
    
    numericInput("plot_width_saving","Width of the plot (mm)",200),
    numericInput('plot_height_saving',"Height of the plot (mm)",300),
    
    if (failed)
      div(tags$b("Please enter all required information")),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("execute_saving","Save file"),
    )
  )
}

saving_vals <- reactiveValues(
  saving_path=NULL,
  saving_name=NULL,
  dimension_width=NULL,
  dimension_height=NULL,
  proceed=FALSE
)


observeEvent(input$execute_saving,{
  if (input$file_name_save != ""){
    print("observeEvent(input$execute_saving")
    saving_vals$saving_path <- myenv$saving_folder_path
    saving_vals$saving_name <- input$file_name_save
    saving_vals$dimension_width <- input$plot_width_saving
    saving_vals$dimension_height <- input$plot_height_saving
    saving_vals$proceed = TRUE
    
    removeModal()
  }
  else{
    showModal(save_modal(failed = TRUE))
  }
})


observeEvent(ignoreNULL = TRUE,
             eventExpr = {
               saving_vals$proceed
             },
             handlerExpr = {
               if (saving_vals$proceed == TRUE){
                 
                 plot_to_save=myenv$table_to_save
                 if (myenv$table_or_plot=="plot"){
                   ggsave(filename = paste0(saving_vals$saving_name,".pdf"),plot=plot_to_save,path=saving_vals$saving_path,device = cairo_pdf,width=saving_vals$dimension_width,height = saving_vals$dimension_height,units="mm")
                   print(paste0(saving_vals$saving_name,".pdf ","succesfully saved!"))
                 }
                 else if (myenv$table_or_plot=="table"){
                   saving_path = file.path(saving_vals$saving_path, paste0(saving_vals$saving_name,".csv"))
                   write.csv(plot_to_save, saving_path, row.names=TRUE)
                   print(paste0(saving_vals$saving_name,".csv"))
                 }
                 saving_vals$proceed=FALSE
                 saving_vals$dimension_width =NULL
                 saving_vals$dimension_height =NULL
                 myenv$PWC_plot=NULL
                
               }
             })



##### DISTRIBUTION #####

observeEvent(c(input$File_to_select_Distrib,
               input$Feature_to_analyse_Distrib,
               input$normalize_per_input_resistance_Distrib,
               input$Factor_of_analysis_Distrib,
               input$select_outliers_to_remove_Distrib,
               input$bin_estim_rule),{
                 file_import <- import_csv_files()
                 subset_filter_dict <- subset_filter()
                 full_data_frame_distrib <- create_full_df(file_import,
                                                               input$Feature_to_analyse_Distrib,
                                                               input$File_to_select_Distrib,
                                                               subset_filter=subset_filter_dict,
                                                               keep_na=FALSE)
                 
                 Unit_list=file_import$Unit_File
                 current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
                 if (input$normalize_per_input_resistance_Distrib){
                   
                   if (grepl("/pA",current_unit)==TRUE){
                     full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                   
                   else if (grepl("pA",current_unit)==TRUE){
                     full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                 }
                 factor=input$Factor_of_analysis_Distrib
                 value=as.character(input$Feature_to_analyse_Distrib)
                 outliers_df=full_data_frame_distrib %>%
                   identify_outliers(value)
                 
                 full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
                 full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
                 full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
                 full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
                 if (input$select_outliers_to_remove_Distrib != 'None'){
                   if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
                     cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
                   }
                   if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
                     cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
                   }
                   
                   if (length(cell_id_to_remove)!=0){
                     full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
                   }
                 }
                 
                 
                 distribution_array=array(full_data_frame_distrib[,value])
                 nb_obs=length(distribution_array)
                 
                 if (input$bin_estim_rule == 'Sturge'){
                   bin_nb = 1 + 3.322*log2(nb_obs) 
                   bin_nb = as.integer(round(bin_nb,0))
                   updateNumericInput(session, 'distribution_bin_width','Number of bins:', bin_nb)
                 }
                 
                 if (input$bin_estim_rule == 'Freedman-Diaconis'){
                   bin_width = 2*IQR(distribution_array)/(nb_obs**(1/3))
                   bin_width = round(bin_width,2)
                   updateNumericInput(session, 'distribution_bin_width','Width of bins:', bin_width)
                 }
                 
                 if (input$bin_estim_rule == 'Doane'){
                   skewness = 3*(mean(distribution_array)-median(distribution_array))/std(distribution_array)
                   sigma=sqrt((6*(nb_obs-2))/((nb_obs+1)*(nb_obs+3)))
                   bin_nb = 1 + log2(nb_obs) + log2(1+(skewness/sigma))
                   bin_nb = as.integer(round(bin_nb,0))
                   updateNumericInput(session, 'distribution_bin_width','Number of bins:', bin_nb)
                   
                 }
                 
                 if (input$bin_estim_rule == 'Scott'){
                   bin_width = (3.49*std(distribution_array))/(nb_obs**(1/3))
                   bin_width=round(bin_width,2)
                   updateNumericInput(session, 'distribution_bin_width','Width of bins:', bin_width)
                   
                 }
                 
                 
                 
                 
               })
  output$fit_distribution_plot <- renderPlot({
    req(input$import_files)
    file_import <- import_csv_files()

    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
    full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
    full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
    
    if (input$select_outliers_to_remove_Distrib != 'None'){
      if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
      }
      if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
      }
      
      if (length(cell_id_to_remove)!=0){
        full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
      }
    }
    
    
    
    
    distribution_array=array(full_data_frame_distrib[,value])
    shift_to_positive_values = FALSE
    shift= abs(min(distribution_array,na.rm = T))+1
    if (min(distribution_array,na.rm = T)<=0){
      shift= abs(min(distribution_array,na.rm = T))+1
      distribution_array = distribution_array + shift
      shift_to_positive_values = TRUE
    }
    
    
    
   
    
    if ((input$bin_estim_rule == "Sturge") | (input$bin_estim_rule == "Doane")){
      nb_bins=as.integer(input$distribution_bin_width)
      bin_df=get_data_bin_df(distribution_array,nb_bins,'Number')
    }
    if ((input$bin_estim_rule == "Freedman-Diaconis") | (input$bin_estim_rule == "Scott")){
      bin_width=input$distribution_bin_width
      bin_df=get_data_bin_df(distribution_array,bin_width,'Width')
      nb_bins=nrow(bin_df)
    }
   

   
    data_x_array=array(seq(min(distribution_array,na.rm = T),max(distribution_array,na.rm = T),.01))
    
    if (input$fit_distribution == TRUE){
     
      full_fit_df = data.frame(feature_value = numeric(),
                                Count = numeric(),
                                Model= character())
      # bin_df --> center of each bins

      if (length(c(input$distribution_to_fit)) == 1){
        fit_result_table <- fit_distribution(bin_df,list(input$distribution_to_fit))
      }
      else{
      fit_result_table <- fit_distribution(bin_df,c(input$distribution_to_fit))
      }
      
      for (model in fit_result_table$Fit){
       
      
      if (model == 'Gaussian'){
        
          amplitude = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"A"])
          center = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"mu"])
          sigma = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"sigma"])
          
          
        
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * exp((-(data_x_array - center)**2)/(2*(sigma**2)))
        
      }
      if (model == 'Exponential'){
        
        amplitude = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"A"])
        tau=as.numeric(fit_result_table[which(fit_result_table$Fit==model),"tau"])
        C=as.numeric(fit_result_table[which(fit_result_table$Fit==model),"C"])
       
        fitted_values <- amplitude*exp((-data_x_array)/(tau))+C
      }
      if (model == 'LogNormal'){
        
        amplitude = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"A"])
        center = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"mu"])
        sigma = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"sigma"])
        
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * (exp((-(log(data_x_array) - center)**2)/(2*(sigma**2))))/(data_x_array)
        
      }
      if (model == 'Skewed Gaussian'){
        
        
        amplitude = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"A"])
        center = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"mu"])
        sigma = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"sigma"])
        gamma = as.numeric(fit_result_table[which(fit_result_table$Fit==model),"gamma"])
        
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * exp((-(data_x_array - center)**2)/(2*(sigma**2))) * (1 + erf((gamma*(data_x_array-center)/(sigma*sqrt(2)))))
      }
      
      fit_df=data.frame(cbind(data_x_array,fitted_values,as.character(model)))
      colnames(fit_df) <- c("feature_value",'Count','Model')
      fit_df$feature_value=as.numeric(fit_df$feature_value)
      if (model == 'LogNormal'){
        fit_df[,'feature_value'] = fit_df[,'feature_value']-shift
      }
      
      full_fit_df <- rbind(full_fit_df,fit_df)
      
      
      }
      full_fit_df$feature_value=as.numeric(full_fit_df$feature_value)
      colnames(full_fit_df) <- c(value,'Count','Model')
      full_fit_df$Model=as.factor(full_fit_df$Model)
      full_fit_df$Count=as.numeric(full_fit_df$Count)
      
    }
    
    if (shift_to_positive_values == TRUE){
      
      bin_df[,'Feature']=bin_df[,'Feature']-shift
    }
    
    
    bin_edges <- bin_df
    
    bin_edges['Feature']=bin_edges['Feature']-(bin_edges[2,"Feature"]-bin_edges[1,"Feature"])/2
    
    colnames(bin_edges) <- c(as.character(value),"Count")
    colnames(bin_df) <- c(as.character(value),'Count')
    
    distribution_plot=ggplot2::ggplot(full_data_frame_distrib,mapping=aes_string(x=as.character(value)))+
      ggplot2::geom_histogram(breaks=unlist(bin_edges[as.character(value)]),fill='darkgrey')+#ggplot2::geom_point(bin_df,mapping=aes(x=unlist(bin_df[,as.character(value)]),y=unlist(bin_df[,"Count"])))+
      ggplot2::geom_point(bin_df,mapping=aes_string(x=as.character(value),y="Count"))+
      ggplot2::labs(y='Nb_of_observation')+ggplot2::xlab(current_unit)+
      ggplot2::ggtitle(paste0(as.character(value),' distribution fit'))
    
    if (input$fit_distribution == TRUE){
      if (shift_to_positive_values == TRUE){
        full_fit_df[,value] = full_fit_df[,value]-shift
      }
      
      if (input$fit_all_distribution == TRUE){
        
      distribution_plot=distribution_plot+ #ggplot2::geom_line(fit_df,mapping=aes_string(x=unlist(fit_df[,as.character(value)]),y=unlist(fit_df[,"Count"])))
        ggplot2::geom_line(full_fit_df,mapping=aes_string(x=as.character(value),y="Count",color="Model"),size=.8)}
      else{
        distribution_plot=distribution_plot+ #ggplot2::geom_line(fit_df,mapping=aes_string(x=unlist(fit_df[,as.character(value)]),y=unlist(fit_df[,"Count"])))
          ggplot2::geom_line(full_fit_df,mapping=aes_string(x=as.character(value),y="Count",color="Model"),size=1)
      }
      
    

    
    if (input$show_stats == TRUE && input$fit_all_distribution == FALSE){
      
      if (shift_to_positive_values == TRUE){
        distribution_array=distribution_array - shift
      }
      distribution_stats <- data.frame(matrix(ncol = 4, nrow = 0))
      x <- c("Q1", "Med", "Q3","Mean")
      colnames(distribution_stats) <- x
      distribution_stats[1,] <- c(unname(quantile(distribution_array,.25,na.rm=TRUE)),
                                  unname(quantile(distribution_array,.50,na.rm=TRUE)),
                                  unname(quantile(distribution_array,.75,na.rm=TRUE)),
                                  mean(distribution_array,na.rm=TRUE))
      stat_table = data.frame(matrix(ncol = 5, nrow = 0))

      colnames(stat_table) <- c('x_min','x_max','y_min','y_max','Stat')
      for(my_stat in x){


        nb_to_match=distribution_stats[1,my_stat]
        my_value=full_fit_df[which.min(abs(nb_to_match-full_fit_df[,value])),][,value][1]
        ymin=0
        ymax=full_fit_df[which.min(abs(nb_to_match-full_fit_df[,value])),][,'Count'][1]

        stat_table[nrow(stat_table)+1,] <- c(my_value,my_value,ymin,ymax,my_stat)

        



      }
      for (elt in seq(1,ncol(stat_table)-1)){
        stat_table[,elt]=as.numeric(unlist(stat_table[,elt]))
      }

      stat_table$Stat <- as.factor(stat_table$Stat)
      distribution_plot=distribution_plot+ggplot2::geom_segment(stat_table,mapping=aes_string(x = "x_min",
                                                                              y = "y_min",
                                                                              xend = "x_max",
                                                                              yend = "y_max",color="Stat"),size=.8)


    }
    }
    
    
    if (input$Distrib_custom_x_range == TRUE){
      distribution_plot=distribution_plot+ggplot2::xlim(input$Minimum_x_limit,input$Maximum_x_limit)+
        ggplot2::xlab(current_unit)
    }
    
    distribution_plot=distribution_plot+ggplot2::theme(text = element_text(size = 17,face="bold"),axis.text = element_text(size = 18),panel.grid.major = element_line(linewidth = 1))
    myenv$distrib_plot = distribution_plot
    distribution_plot
  })
  
  observeEvent(input$Save_distrib_plot,{
    myenv$table_to_save=myenv$distrib_plot
    print('drdrd')
    myenv$table_or_plot="plot"
    showModal(save_modal())
  })
  
  
  output$distribution_plot_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    
    
    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
    full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
    full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
    if (input$select_outliers_to_remove_Distrib != 'None'){
      if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
      }
      if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
      }
      
      if (length(cell_id_to_remove)!=0){
        full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
      }
    }
    
    distribution_array=array(full_data_frame_distrib[,value])
    if (min(distribution_array,na.rm=TRUE)<=0){
      shift= abs(min(distribution_array,na.rm = TRUE))+1
      distribution_array = distribution_array + shift
      shift_to_positive_values = TRUE
    }
    
    
    if ((input$bin_estim_rule == "Sturge") | (input$bin_estim_rule == "Doane")){
      nb_bins=as.integer(input$distribution_bin_width)
      bin_df=get_data_bin_df(distribution_array,nb_bins,'Number')
    }
    if ((input$bin_estim_rule == "Freedman-Diaconis") | (input$bin_estim_rule == "Scott")){
      bin_width=input$distribution_bin_width
      bin_df=get_data_bin_df(distribution_array,bin_width,'Width')
      nb_bins=nrow(bin_df)
    }
    
    
    
    fit_list=list('Gaussian','Exponential','LogNormal','Skewed Gaussian')
    
    # bin_df --> center of each bins
    
    fit_result_table <- fit_distribution(bin_df,fit_list)
    
    fit_result_table
    
  })
  
 
  
  output$QQPlot <- renderPlot({
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
    full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
    full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
    if (input$select_outliers_to_remove_Distrib != 'None'){
      if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
      }
      if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
      }
      
      if (length(cell_id_to_remove)!=0){
        full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
      }
    }
    distribution_array=array(full_data_frame_distrib[,value])
    nb_obs=length(distribution_array)
    df <- data.frame(y=distribution_array)
    p <- ggplot2::ggplot(full_data_frame_distrib, mapping=ggplot2::aes_string(sample = as.character(value)))
    p <- p + ggplot2::stat_qq() + ggplot2::stat_qq_line()
    p
  })

  output$distrib_fit_parameters <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()


    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
    full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
    full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
    if (input$select_outliers_to_remove_Distrib != 'None'){
      if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
      }
      if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
      }
      
      if (length(cell_id_to_remove)!=0){
        full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
      }
    }
    distribution_array=array(full_data_frame_distrib[,value])


    bin_width=input$distribution_bin_width

    nb_bins=as.integer((max(distribution_array,na.rm=TRUE)-min(distribution_array,na.rm=TRUE))/bin_width)
    
    bin_df=reticulate_data_distribution(distribution_array,nb_bins)

    parameters_df=fit_distribution(distribution_array,nb_bins)

    parameters_df
  })

  output$bin_estimation_rule_explanation <- renderPrint({
    if (input$bin_estim_rule == 'Sturge'){
      description = cat(paste( "Thumb Rule \nEstimates number of bins, K=1 + 3.322*log2(n) "),"\n")
    }
    
    if (input$bin_estim_rule == 'Freedman-Diaconis'){
      description = cat(paste("Estimates bin width \nresilient to outliers \nh = 2*IQR(distribution_array)/(nb_obs**(1/3)) "),'\n')
    }
    
    if (input$bin_estim_rule == 'Doane'){
      description = cat(paste("Estimates number of bins 
                              \ntakes into account the skewness of the data 
                              \nK = 1 + log2(nb_obs) + log2(1+(skewness/sigma))  \nskewness = 3*(mean(distribution_array)-median(distribution_array))/std(distribution_array) \nsigma=sqrt((6*(nb_obs-2))/((nb_obs+1)*(nb_obs+3)))"),'\n')
      
      
    }
    
    if (input$bin_estim_rule == 'Scott'){
      description = cat(paste ("Estimates bin width, takes into account data variability and size. \nGood for large datasets
                               \nh = (3.49*std(distribution_array))/(nb_obs**(1/3))"),'\n')
      
    }
    description
  })
  output$distrib_stats <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()

    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_GOhms']))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib["is.outlier"][is.na(full_data_frame_distrib["is.outlier"])] <- FALSE
    full_data_frame_distrib["is.extreme"][is.na(full_data_frame_distrib["is.extreme"])] <- FALSE
    
    full_data_frame_distrib=full_data_frame_distrib[!is.na(full_data_frame_distrib[,value]),] 
    if (input$select_outliers_to_remove_Distrib != 'None'){
      if (input$select_outliers_to_remove_Distrib == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.outlier == TRUE),'Cell_id']
      }
      if (input$select_outliers_to_remove_Distrib == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        cell_id_to_remove =  full_data_frame_distrib[which(full_data_frame_distrib$is.extreme == TRUE),'Cell_id']
      }
      
      if (length(cell_id_to_remove)!=0){
        full_data_frame_distrib=full_data_frame_distrib[-which(full_data_frame_distrib$Cell_id %in% cell_id_to_remove), ]
      }
    }
    
    distribution_array=array(full_data_frame_distrib[,value])

    distribution_stats <- data.frame(matrix(ncol = 9, nrow = 0))
    x <- c("Min", "Q1", "Med", "Q3", "Max",'IQ',"Mean","SD",'Amplitude')
    colnames(distribution_stats) <- x
    distribution_stats[1,] <- c(min(distribution_array,na.rm = TRUE),
                                unname(quantile(distribution_array,.25,na.rm = TRUE)),
                                unname(quantile(distribution_array,.50,na.rm = TRUE)),
                                unname(quantile(distribution_array,.75,na.rm = TRUE)),
                                max(distribution_array,na.rm=TRUE),
                                unname(quantile(distribution_array,.75,na.rm = TRUE))-unname(quantile(distribution_array,.25,na.rm = TRUE)),
                                mean(distribution_array,na.rm = TRUE),
                                sd(distribution_array,na.rm=TRUE),
                                max(distribution_array,na.rm = TRUE)-min(distribution_array,na.rm=TRUE))


    distribution_stats
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
  
  ##### Correlation #####
  
  output$analysis_ready <- renderText({
    req(input$import_files)
    file_import <- import_csv_files()
    population_class_df = file_import$Population_Class
    subset_filter_dict <- subset_filter()
    
    correlation_full_df_list <- create_full_df_correlation(file_import, input$Selected_Response_Type, subset_filter_dict)
    unit_list = correlation_full_df_list$Unit_line
    full_df = correlation_full_df_list$Full_df_correlation
    
    
    updateSelectizeInput(session,"Feature_list_correlation", "Choose features to analyse",choices=names(unit_list), selected = names(unit_list))
    category_list = colnames(population_class_df)
    category_list = category_list[category_list != "Cell_id"]
    updateSelectInput(session,"Factor_of_analysis_correlation","Choose factor of analysis",choices=category_list,selected = category_list[1])
    
    print("Ready for analysis")
    
    
    
  })
  
  output$update_range_var_corr <- renderUI({
    req(input$Population_files)
    req(input$import_files)
    req(input$Update_analysis_correlation)
    
    
    file_import <- import_csv_files()
    population_class_df = file_import$Population_Class
    subset_filter_dict <- subset_filter()

    correlation_full_df_list <- create_full_df_correlation(file_import, input$Selected_Response_Type, subset_filter_dict)
    Full_df_correlation = correlation_full_df_list$Full_df_correlation
    feature_list = input$Feature_list_correlation
    
    if (input$normalize_per_input_resistance_correlation == TRUE){
      for (column in feature_list){
        if (grepl("/pA",column)==TRUE){
          Full_df_correlation[,column]=Full_df_correlation[,column]*(1/(Full_df_correlation[,'Input_Resistance_GOhms']))
          new_colname = chartr('pA','mV',column)
          colnames(Full_df_correlation)[which(names(Full_df_correlation) == column)] <- new_colname
          feature_list[feature_list==column] <- new_colname
        }
        
        else if (grepl("pA",column)==TRUE){
          Full_df_correlation[,column]=Full_df_correlation[,column]*((Full_df_correlation[,'Input_Resistance_GOhms']))
          new_colname = chartr('pA','mV',column)
          colnames(Full_df_correlation)[which(names(Full_df_correlation) == column)] <- new_colname
          feature_list[feature_list==column] <- new_colname
        }
        
      }
    }
    
    
    update_range_var_corr <- lapply(feature_list, function(i) {
     
      Full_df_correlation[,i]=as.numeric(Full_df_correlation[,i])
      
      max_val = max(Full_df_correlation[,i],na.rm=TRUE)
      min_val = min(Full_df_correlation[,i],na.rm=TRUE)
      print(min_val)
      print(max_val)
      if (grepl("Adaptation",i)==TRUE){
        
        numericInput(paste0('Range_var_',i), paste0('Max ',i),value=max_val)
                    
      }
      else{
      sliderInput(paste0('Range_var_',i), paste0('Range ',i),
                  min = min_val, max = max_val,
                  value = c(min_val,max_val))
      }


    })
    do.call(tagList, update_range_var_corr)
  })

  
  
  
  get_plot_ranges<- eventReactive(input$Update_plot_correlation,{
    req(input$Population_files)
    req(input$import_files)
    req(input$Update_analysis_correlation)
    
    file_import <- import_csv_files()
    population_class_df = file_import$Population_Class
    subset_filter_dict <- subset_filter()
    
    correlation_full_df_list <- create_full_df_correlation(file_import, input$Selected_Response_Type, subset_filter_dict)
    Full_df_correlation = correlation_full_df_list$Full_df_correlation
    feature_list = input$Feature_list_correlation
    if (input$normalize_per_input_resistance_correlation == TRUE){
      for (column in feature_list){
        if (grepl("pA",column)==TRUE){
          new_colname = chartr('pA','mV',column)
          feature_list[feature_list==column] <- new_colname
        }
      }
    }
    
    range_dict <- lapply(feature_list, function(i) {
      paste0(input[[paste0('Range_var_',i)]])
    })

    names(range_dict) <- feature_list
    
    return(range_dict)
  })
  
  output$correlogram_plot <- renderPlot({
    req(input$Update_analysis_correlation)
    file_import <- import_csv_files()
    population_class_df = file_import$Population_Class
    subset_filter_dict <- subset_filter()
    
    correlation_full_df_list <- create_full_df_correlation(file_import, input$Selected_Response_Type, subset_filter_dict)
    unit_list = correlation_full_df_list$Unit_line
    full_df = correlation_full_df_list$Full_df_correlation
    
    feature_list = input$Feature_list_correlation
    nb_features = length(feature_list)
    
    if (input$normalize_per_input_resistance_correlation == TRUE){
      for (column in feature_list){
        if (grepl("/pA",column)==TRUE){
          full_df[,column]=full_df[,column]*(1/(full_df[,'Input_Resistance_GOhms']))
          new_colname = chartr('pA','mV',column)
          colnames(full_df)[which(names(full_df) == column)] <- new_colname
          feature_list[feature_list==column] <- new_colname
        }
        
        else if (grepl("pA",column)==TRUE){
          full_df[,column]=full_df[,column]*((full_df[,'Input_Resistance_GOhms']))
          new_colname = chartr('pA','mV',column)
          colnames(full_df)[which(names(full_df) == column)] <- new_colname
          feature_list[feature_list==column] <- new_colname
        }
        
      }
    }
    
    
    if(input$analysis_by_factor_corr == TRUE){
      
      correlogram_plot = GGally::ggpairs(full_df[,feature_list],ggplot2::aes(colour=full_df[,input$Factor_of_analysis_correlation],alpha=.8), upper = list(continuous = wrap("cor", method = "kendall")))+ggplot2::theme( text = element_text(size = 15))
    }
    else{
      correlogram_plot = ggpairs(full_df[,feature_list],ggplot2::aes(alpha=.8),upper = list(continuous = wrap("cor", method = "kendall")))+ggplot2::theme( text = element_text(size = 15))
    }
    
    range_dict = get_plot_ranges()
    
    
    for (feature in names(range_dict)){
      
      feature_index = which(feature_list==feature)
      
      feature_plot_range = range_dict[[feature]]
      if (grepl("Adaptation",feature)==TRUE){
        feature_min = 0
        feature_max = as.numeric(feature_plot_range[1])
      }
      else{
        feature_min = as.numeric(feature_plot_range[1])
        feature_max = as.numeric(feature_plot_range[2])
      }  
      
      for (col in seq(1:nb_features)){
        for (row in seq(1:nb_features)){
          if (col == feature_index & col<=row ){
            correlogram_plot[row,col]=correlogram_plot[row,col]+ggplot2::xlim(feature_min,feature_max)
          }
          if (row == feature_index & col<row){
            correlogram_plot[row,col]=correlogram_plot[row,col]+ggplot2::ylim(feature_min,feature_max)
          }
        }
      }
    }
    
    
    
    if (input$flip_coord == TRUE){
    #flip plots in second half diag and below the two diagonals
      for (col in seq(1:nb_features)){
        for (row in seq(1:nb_features)){
  
          if (col == row & col>(nb_features/2)){
            correlogram_plot[row,col]= correlogram_plot[row,col]+coord_flip()
          }
  
          
        }
      }
    }
    
    
    
    return(correlogram_plot)
    
  },height = 800,width=800)
  
  
  # 
  # output$function_to_save <- renderText({
  #   
  #   if (saving_vals$proceed==TRUE){
  #     req(saving_vals$saving_path,saving_vals$saving_name)
  #     
  #     
  #     if(myenv$table_or_plot=="table"){
  #       if (myenv$is.stat.table==TRUE){
  #         write.csv(myenv$mean_table,file=paste0(saving_vals$saving_path,saving_vals$saving_name,"_mean.csv"),row.names = TRUE,col.names = TRUE)
  #         write.csv(myenv$sd_table,file=paste0(saving_vals$saving_path,saving_vals$saving_name,"_sd.csv"),row.names = TRUE,col.names = TRUE)
  #         print(paste0("Stats tables successfully saved!"))
  #         myenv$is.stat.table=FALSE
  #       }
  #       else{
  #         write.csv(myenv$table_to_save,file=paste0(saving_vals$saving_path,saving_vals$saving_name,".csv"),row.names = TRUE,col.names = TRUE)
  #         print(paste0(saving_vals$saving_name,".csv"," successfully saved!"))
  #       }
  #       saving_vals$proceed=FALSE
  #       myenv$table_to_save=NULL
  #       
  #     }
  #     
  #     if(myenv$table_or_plot=="plot"){
  #       plot_to_save=myenv$plot_to_save
  #       
  #       if (saving_vals$is.custom_y_range ==TRUE){
  #         plot_to_save=plot_to_save+ylim(saving_vals$saving_y_min,saving_vals$saving_y_max)}
  #       
  #       ggsave(filename = paste0(saving_vals$saving_name,".pdf"),plot=plot_to_save,path=saving_vals$saving_path,device = cairo_pdf,width=200,height = 100,units="mm")
  #       print(paste0(saving_vals$saving_name,".pdf ","succesfully saved!"))
  #       saving_vals$proceed=FALSE
  #       myenv$plot_to_save=NULL
  #     }
  #     
  #   }
  # })
  # 

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
  

  ##### CELL FILES #####

  

  
}
shinyApp(ui, server)
