library(shiny)
required_packages=c("Cairo","Skillings.Mack","plyr","stringr","abind","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves','shinyWidgets','gtools','ggridges','rhdf5',"gsignal","RColorBrewer","processx",'ggh4x','reticulate')
install.packages(setdiff(required_packages,rownames(installed.packages())))
print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")
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
                                     plotlyOutput('data_density',height = 600),
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
                                     fluidRow(column(width = 6,checkboxInput("normalize_per_input_resistance_RM","Normalize current per input resistance")),
                                              column(width = 6,selectInput("select_outliers_to_remove","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None')))
                                     
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    
                                    tabPanel(title='Repeated Measure Analysis',
                                             fluidPage(fluidRow(column(width = 6,plotlyOutput('Original_RM_Data_Plot')),
                                                                 column(width = 6,plotlyOutput('RM_Data_without_outliers_Plot'))), ),
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
                                     fluidRow(column(width=6,checkboxInput("normalize_per_input_resistance_Variance","Normalize current per input resistance")),
                                              column(width=6,selectInput("select_outliers_to_remove_Variance","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None')))
                                     
                                     
                                     
                                     
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    
                                     tabPanel(title='Independant Measure Analysis',
                                              fluidPage(fluidRow(column(width = 6,plotlyOutput('Original_Variance_Data_Plot')),
                                                                column(width = 6,plotlyOutput('Variance_Data_without_outliers_Plot'))), ),
                                             fluidPage(fluidRow(column(width = 4,tableOutput('category_count_table_Variance')),
                                                                column(width = 4,tableOutput('Normality_test_table_Variance')) ,
                                                                 column(width = 4, tableOutput("Var_homogen_test_table_Variance"))  ,
                                                                 column(width = 4,tableOutput('Variance_test_table_Variance'))), ),
                                              fluidPage(fluidRow(column(width = 12,tableOutput('PWC_test_table_Variance'))), ),
                                              plotOutput("PWC_test_Plot_Variance")
                                     )


                                    
                                    
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
                                              column(width=6,selectInput('distribution_to_fit','Select distribution to fit',choices=c("Gaussian","Exponential",'LogNormal',"Skewed Gaussian"),selected='Gaussian'))),
                                     
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
                                    
                                    tableOutput("distribution_plot_table"),
                                    tableOutput("distrib_stats"),
                                    plotOutput('QQPlot'),
                                    
                             )



                           )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),
             
             #Here
             #tabpanel
             
             
  ),
  navbarPage(title = "Single Cell Data",
             tabPanel("Cell id",


                      sidebarLayout(
                        sidebarPanel(
                          shinyDirButton('cell_file_folder', 'Select a folder', 'Please select a folder', FALSE),
                          selectizeInput("Cell_id_file_to_analyse","Select_cell_id",choices="", selected = NULL, multiple = FALSE, options = NULL),

                          actionButton("Change_cell", "Show Cell Trace")

                        ),


                        mainPanel(

                          dataTableOutput('cell_list_csv')


                        )
                      )
             ),
             tabPanel("Cell Information",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(width=1,

                        ),
                        mainPanel(width=11,
                          textOutput('single_cell_file_path'),
                          tableOutput('Metadata'),
                          dataTableOutput('Sweep_info'),
                          fluidRow(
                          column(6,
                                 plotlyOutput('BE_plot')),
                          column(3,plotlyOutput('BE_boxplot')))
                          ,
                          fluidRow(column(6,
                                  plotlyOutput('Time_cst_plot')),
                                  column(3,plotlyOutput('Time_cst_boxplot'))),
                          fluidRow(column(6,
                                 plotlyOutput('Input_resistance_plot')),
                                 column(3,plotlyOutput('Input_resistance_boxplot')))

                        )
                      )
             ),

             tabPanel("Single sweep traces",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(width=2,

                          selectInput("Sweep_to_analyse","Sweep to analysis",choices=""),
                          checkboxInput("Apply_BE_correction_single_sweep","Apply Bridge Error Correction"),
                          checkboxInput('superimpose','Superimpose_BE_corrected_trace'),
                          checkboxGroupInput("Derivative_to_display","Select derivative to display",choices=c("First_derivative","Second_derivative"))

                        ),

                        # Show a plot of the generated distribution
                        mainPanel(width=10,
                          plotlyOutput("Spike_feature_plot",height = 800),
                          actionButton('Save_spike_feature_plot',"Save plot"),
                          plotlyOutput("linear_properties_plot")

  
                        )
                      )
             ),
             tabPanel("I/O",

                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(width=1,
                                     checkboxInput("for_saving_plot","For plot saving")

                        ),

                        # Show a plot of the generated distribution ;;; ;;;;
                        mainPanel(width = 11,
                                  tabsetPanel(
                                    tabPanel("Time_based",
                                             plotlyOutput("I_O_feature_plot_time_based",height = 800),
                                             fluidRow(
                                               column(3,
                                                      plotlyOutput("Gain_time_plot"),
                                                      tableOutput("Gain_time_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Threshold_time_plot"),
                                                      tableOutput("Threshold_time_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_stimulus_time_plot"),
                                                      tableOutput("Saturation_stimulus_time_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_freq_time_plot"),
                                                      tableOutput("Saturation_freq_time_metrics")
                                               )),
                                             tableOutput('IO_table_time_fit'),
                                             tableOutput('IO_table_time_feature'),

                                             tableOutput('Stim_freq_time_table')

                                    ),
                                    tabPanel("Index_based",
                                             plotlyOutput("I_O_feature_plot_index_based",height = 800),
                                             fluidRow(
                                               column(3,
                                                      plotlyOutput("Gain_index_plot"),
                                                      tableOutput("Gain_index_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Threshold_index_plot"),
                                                      tableOutput("Threshold_index_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_stimulus_index_plot"),
                                                      tableOutput("Saturation_stimulus_index_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_freq_index_plot"),
                                                      tableOutput("Saturation_freq_index_metrics")
                                               )),
                                             tableOutput('IO_table_index_fit'),
                                             tableOutput('IO_index_table_feature'),



                                             tableOutput('Stim_freq_index_table')
                                    ),
                                    tabPanel("Interval_based",
                                             plotlyOutput("I_O_feature_plot_interval_based",height = 800),
                                             fluidRow(
                                               column(3,
                                                      plotlyOutput("Gain_interval_plot"),
                                                      tableOutput("Gain_interval_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Threshold_interval_plot"),
                                                      tableOutput("Threshold_interval_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_stimulus_interval_plot"),
                                                      tableOutput("Saturation_stimulus_interval_metrics")
                                               ),
                                               column(3,
                                                      plotlyOutput("Saturation_freq_interval_plot"),
                                                      tableOutput("Saturation_freq_interval_metrics")
                                               )),
                                             tableOutput('IO_table_interval_fit'),
                                             tableOutput('IO_interval_table_feature'),
                                             tableOutput('Stim_freq_interval_table')
                                    ),
                                    tabPanel("Adaptation",
                                             plotlyOutput("Adaptation_plot",height=800),
                                             checkboxInput("Normalize_adapt_params","Normalize Adaptation Parameters"),
                                             tableOutput('Adapt_table'),))
                        )
                      )),
             tabPanel("Raw traces",
                      sidebarLayout(
                        sidebarPanel(width=1,
                                     checkboxInput("Apply_BE_correction","Apply Bridge Error Correction")
                        ),
                        mainPanel(width = 11,
                                  plotlyOutput("traces_plot",height = 800),

                        )
                      )
             )
  )
  ##3
  
)





#source_python ('/Users/julienballbe/My_Work/Data_Analysis/python_ref.py')
source_python ('/Users/julienballbe/My_Work/My_Librairies/Fit_library.py')
source_python ('/Users/julienballbe/My_Work/My_Librairies/Data_treatment.py')
#source_python ("/Users/julienballbe/My_Work/My_Librairies/Electrophy_treatment.py")

server <- function(session,input, output) {
  
  #=new.env()
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
                 
               })
  

  import_csv_files <- eventReactive(input$import_files,{
    req(input$Population_files)
    
    
    current_cell_file <- load_population_csv_file(global$population_datapath,input$Population_files,input$Selected_Response_Type)
    
   
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


    feature_file = file_import[[1]]

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
    
    
    
   

    population_class


  })

  
  ##### DATA ANALYSIS ######
  
  ###### DATA OVERVIEW #######
  
  output$descriptive_data_table <- renderDataTable({
    
    req(input$import_files)
    
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
  
  output$Data_repartition <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,subset_filter_dict,input$Selected_Response_Type,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_data_repartition]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
  })
  
  
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
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_data_repartition]=full_data_frame[,input$Feature_to_analyse_data_repartition]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
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
  
  output$Original_RM_Data_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
    
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
  
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Oulier_df")
    
    if (input$select_outliers_to_remove == 'None'){
      outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
        ggplot2::geom_boxplot(outlier.shape = NA )
      outlier_plot=outlier_plot+
        ggplot2::ggtitle('Original Data')
      outlier_plotly=ggplotly(outlier_plot)
      
    }
    else{
      if (input$select_outliers_to_remove == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
          ggplot2::geom_boxplot(outlier.shape =NA )+
          ggplot2::geom_point(aes_string(color='is.outlier',text='Cell_id'))+ 
          ggplot2::scale_color_manual(values = c("blue", "red"))
        outlier_plot=outlier_plot+ggplot2::ggtitle('Original Data')+ggplot2::ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
      if (input$select_outliers_to_remove == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
          ggplot2::geom_boxplot(outlier.shape =NA )+
          ggplot2::geom_point(aes_string(color='is.extreme',text='Cell_id'))+ 
          ggplot2::scale_color_manual(values = c("blue", "red"))
        outlier_plot=outlier_plot+ggplot2::ggtitle('Original Data')+ggplot2::ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
    }
    
    outlier_plotly
    })
  
  output$RM_Data_without_outliers_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "DF_without_outliers")
    without_outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
      ggplot2::geom_boxplot(outlier.shape = NA )
    without_outlier_plot=without_outlier_plot+
      ggplot2::ggtitle('Analysed Data')
    without_outlier_plotly=ggplotly(without_outlier_plot)
    
    without_outlier_plotly
    
  })
  
  output$category_count_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    category_count_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Categories_count")
    category_count_table
    
  },digits = -3)
  
  output$Normality_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    normality_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Normality_table")
    normality_table
    
  },digits = -3)
  
  output$Variance_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Variance_test")
    Variance_test_table
    
  },digits = -3)
  
  
  output$PWC_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "PWC_without_position")
    
    
    PWC_test_table
    
  },digits = -3)
  
  
  output$PWC_test_Plot <- renderPlot({
    req(input$import_files)
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     subset_filter_dict,
                                                     input$Selected_Response_Type,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    
    if ('Linear_Values' %in% names(file_import)){
      population_class_list <- colnames(file_import$Population_Class)
      col_list <- colnames(full_data_frame_Anova)
      col_list_to_remove <- c(population_class_list,"Input_Resistance_MOhms")
      
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -col_list_to_remove) %>%
        convert_as_factor(Cell_id, Response_Duration)
      
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "DF_without_outliers")
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Variance_test_original_table")
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "PWC")
    
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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance

  value=as.character(input$Feature_to_analyse_Variance)

  original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Outlier_df")

  if (input$select_outliers_to_remove_Variance == 'None'){
    outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
      ggplot2::geom_boxplot(outlier.shape = NA )+
      ggplot2::ggtitle('Original Data')+
      ggplot2::ylab(current_unit)
    outlier_plotly=ggplotly(outlier_plot)

  }
  else{
    if (input$select_outliers_to_remove_Variance == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
      outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
        ggplot2::geom_boxplot(outlier.shape =NA )+
        ggplot2::geom_point(aes_string(color='is.outlier',text='Cell_id'))+ 
        ggplot2::scale_color_manual(values = c("blue", "red"))+
        ggplot2::ggtitle('Original Data')+
        ggplot2::ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)

    }

    if (input$select_outliers_to_remove_Variance == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
      outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+
        ggplot2::geom_boxplot(outlier.shape =NA )+
        ggplot2::geom_point(aes_string(color='is.extreme',text='Cell_id'))+ 
        ggplot2::scale_color_manual(values = c("blue", "red"))+
        ggplot2::ggtitle('Original Data')+
        ggplot2::ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)

    }

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance

  value=as.character(input$Feature_to_analyse_Variance)

  original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "DF_without_outliers")
  without_outlier_plot=ggplot2::ggplot(original_dataframe,aes_string( x = factor, y =  input$Feature_to_analyse_Variance))+
    ggplot2::geom_boxplot(outlier.shape = NA )+
    ggplot2::ggtitle('Analysed Data')+
    ggplot2::ylab(current_unit)
  without_outlier_plotly=ggplotly(without_outlier_plot)

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  category_count_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Categories_count")
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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  normality_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Normality_table")

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Variance_homogeneity_table")

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  Variance_homogeneity_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Variance_test")

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  Variance_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "PWC_without_position")

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
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }

    else if (grepl("pA",current_unit)==TRUE){
      full_data_frame_Anova[,input$Feature_to_analyse_Variance]=full_data_frame_Anova[,input$Feature_to_analyse_Variance]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
      current_unit=chartr('pA','mV',current_unit)
    }
  }
  factor=input$Factor_of_analysis_Variance


  value=as.character(input$Feature_to_analyse_Variance)
  original_dataframe=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return =  "DF_without_removed_levels")

  Variance_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "Variance_test_original_table")
  PWC_test_table=perform_ANOVA(full_data_frame_Anova,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Variance,what_to_return = "PWC")
  PWC_plot=ggplot2::ggplot(original_dataframe,aes_string( x = factor, y = input$Feature_to_analyse_Variance))+
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

  

  
  

##### DISTRIBUTION #####

observeEvent(c(input$File_to_select_Distrib,
               input$Feature_to_analyse_Distrib,
               input$normalize_per_input_resistance_Distrib,
               input$Factor_of_analysis_Distrib,
               input$select_outliers_to_remove_Distrib,
               input$bin_estim_rule),{
                 file_import <- import_csv_files()
                 subset_filter_dict <- subset_filter()
                 full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                               input$Feature_to_analyse_Distrib,
                                                               input$File_to_select_Distrib,
                                                               subset_filter=subset_filter_dict,
                                                               keep_na=FALSE)
                 
                 Unit_list=file_import$Unit_File
                 current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
                 if (input$normalize_per_input_resistance_Distrib){
                   
                   if (grepl("/pA",current_unit)==TRUE){
                     full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                   
                   else if (grepl("pA",current_unit)==TRUE){
                     full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
                     current_unit=chartr('pA','mV',current_unit)
                   }
                 }
                 factor=input$Factor_of_analysis_Distrib
                 value=as.character(input$Feature_to_analyse_Distrib)
                 outliers_df=full_data_frame_distrib %>%
                   identify_outliers(value)
                 
                 full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
                 
                 full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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
    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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
    
    if ((input$bin_estim_rule == "Sturge") | (input$bin_estim_rule == "Doane")){
      nb_bins=as.integer(input$distribution_bin_width)
      bin_df=get_data_bin_df(distribution_array,nb_bins,'Number')
    }
    if ((input$bin_estim_rule == "Freedman-Diaconis") | (input$bin_estim_rule == "Scott")){
      bin_width=input$distribution_bin_width
      bin_df=get_data_bin_df(distribution_array,bin_width,'Width')
      nb_bins=nrow(bin_df)
    }
   

    
    data_x_array=array(seq(min(distribution_array),max(distribution_array),.01))
    if (input$fit_distribution == TRUE){
     
      full_fit_df = data.frame(feature_value = numeric(),
                                Count = numeric(),
                                Model= character())
      # bin_df --> center of each bins
      if (input$fit_all_distribution == FALSE){
      fit_result_table <- fit_population_distribution(distribution_array,bin_df,list(input$distribution_to_fit))
      }
      if (input$fit_all_distribution == TRUE){
        fit_result_table <- fit_population_distribution(distribution_array,bin_df,list("Gaussian","Exponential",'LogNormal',"Skewed Gaussian"))
      }
      for (model in fit_result_table$Model){
        parameters_df=fit_result_table[which(fit_result_table$Model==model),"best_fit_parameters"][[1]]
      
      
      
      
      
      if (model == 'Gaussian'){
        if (length(parameters_df)==1){
          amplitude=parameters_df[1]
          center = parameters_df[2]
          sigma = parameters_df[3]}
        else{
          
          amplitude=parameters_df[[1]]
          center = parameters_df[[2]]
          sigma = parameters_df[[3]]
        }
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * exp((-(data_x_array - center)**2)/(2*(sigma**2)))
      }
      if (model == 'Exponential'){
        if (length(parameters_df)==1){
          amplitude = parameters_df[1]
          tau=parameters_df[2]}
        else{
          amplitude = parameters_df[[1]]
          tau=parameters_df[[2]]
        }
       
        fitted_values <- amplitude*exp((-data_x_array)/(tau))
      }
      if (model == 'LogNormal'){
        
        if (length(parameters_df)==1){
        amplitude=parameters_df[1]
        center = parameters_df[2]
        sigma = parameters_df[3]}
        else{
          amplitude=parameters_df[[1]]
          center = parameters_df[[2]]
          sigma = parameters_df[[3]]
        }
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * (exp((-(log(data_x_array) - center)**2)/(2*(sigma**2))))/data_x_array
      }
      if (model == 'Skewed Gaussian'){
        
        
        if (length(parameters_df)==1){
          amplitude=parameters_df[1]
          center = parameters_df[2]
          sigma = parameters_df[3]
          gamma = parameters_df[4]}
        else{
          amplitude=parameters_df[[1]]
          center = parameters_df[[2]]
          sigma = parameters_df[[3]]
          gamma = parameters_df[[4]]
        }
        
        fitted_values <- (amplitude/(sigma*sqrt(2*pi))) * exp((-(data_x_array - center)**2)/(2*(sigma**2))) * (1 + erf((gamma*(data_x_array-center)/(sigma*sqrt(2)))))
      }
      
      fit_df=data.frame(cbind(data_x_array,fitted_values,as.character(model)))
      colnames(fit_df) <- c("feature_value",'Count','Model')
      full_fit_df <- rbind(full_fit_df,fit_df)
      
      }
      full_fit_df$feature_value=as.numeric(full_fit_df$feature_value)
      colnames(full_fit_df) <- c(value,'Count','Model')
      full_fit_df$Model=as.factor(full_fit_df$Model)
      full_fit_df$Count=as.numeric(full_fit_df$Count)
      
    }
    bin_edges <- bin_df
    
    bin_edges['Feature']=bin_edges['Feature']-(bin_edges[2,"Feature"]-bin_edges[1,"Feature"])/2
    colnames(bin_edges) <- c(as.character(value),"Count")
    colnames(bin_df) <- c(as.character(value),'Count')
    
    distribution_plot=ggplot2::ggplot(full_data_frame_distrib,mapping=aes_string(x=as.character(value)))+
      ggplot2::geom_histogram(breaks=unlist(bin_edges[as.character(value)]),fill='darkgrey')+#ggplot2::geom_point(bin_df,mapping=aes(x=unlist(bin_df[,as.character(value)]),y=unlist(bin_df[,"Count"])))+
      ggplot2::geom_point(bin_df,mapping=aes_string(x=as.character(value),y="Count"))+
      ggplot2::labs(y='Nb_of_observation',x=as.character(value))+
      ggplot2::ggtitle(paste0(as.character(value),' distribution fit'))
    if (input$fit_distribution == TRUE){
      if (input$fit_all_distribution == TRUE){
      distribution_plot=distribution_plot+ #ggplot2::geom_line(fit_df,mapping=aes_string(x=unlist(fit_df[,as.character(value)]),y=unlist(fit_df[,"Count"])))
        ggplot2::geom_line(full_fit_df,mapping=aes_string(x=as.character(value),y="Count",color="Model"),size=.8)}
      else{
        distribution_plot=distribution_plot+ #ggplot2::geom_line(fit_df,mapping=aes_string(x=unlist(fit_df[,as.character(value)]),y=unlist(fit_df[,"Count"])))
          ggplot2::geom_line(full_fit_df,mapping=aes_string(x=as.character(value),y="Count"),size=.8)
      }
      
    
    

   

    
    if (input$show_stats == TRUE && input$fit_all_distribution == FALSE){
      distribution_stats <- data.frame(matrix(ncol = 4, nrow = 0))
      x <- c("Q1", "Med", "Q3","Mean")
      colnames(distribution_stats) <- x
      distribution_stats[1,] <- c(unname(quantile(distribution_array,.25)),
                                  unname(quantile(distribution_array,.50)),
                                  unname(quantile(distribution_array,.75)),
                                  mean(distribution_array))
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
                                                                              yend = "y_max",color="Stat"))


    }
    }
    
    
    if (input$Distrib_custom_x_range == TRUE){
      distribution_plot=distribution_plot+ggplot2::xlim(input$Minimum_x_limit,input$Maximum_x_limit)+
        ggplot2::xlab(current_unit)
    }
    distribution_plot=distribution_plot+ggplot2::theme(text = element_text(size = 12),axis.text = element_text(size = 10))
    distribution_plot
  })
  
  
  
  output$distribution_plot_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    
    
    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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
    
    fit_result_table <- fit_population_distribution(distribution_array,bin_df,fit_list)
    fit_result_table <- select(fit_result_table, -c("best_fit_parameters"))
    fit_result_table
    
  })
  
 
  
  output$QQPlot <- renderPlot({
    file_import <- import_csv_files()
    subset_filter_dict <- subset_filter()
    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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
    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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

    nb_bins=as.integer((max(distribution_array)-min(distribution_array))/bin_width)
    
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
    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$File_to_select_Distrib,
                                                  subset_filter=subset_filter_dict,
                                                  keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Distrib){

      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }

      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame_distrib[,input$Feature_to_analyse_Distrib]=full_data_frame_distrib[,input$Feature_to_analyse_Distrib]*((full_data_frame_distrib[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
   
    
    factor=input$Factor_of_analysis_Distrib
    value=as.character(input$Feature_to_analyse_Distrib)
    outliers_df=full_data_frame_distrib %>%
      identify_outliers(value)
    
    full_data_frame_distrib=merge(full_data_frame_distrib,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
    
    full_data_frame_distrib[is.na(full_data_frame_distrib)] <- FALSE
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
    distribution_stats[1,] <- c(min(distribution_array),
                                unname(quantile(distribution_array,.25)),
                                unname(quantile(distribution_array,.50)),
                                unname(quantile(distribution_array,.75)),
                                max(distribution_array),
                                unname(quantile(distribution_array,.75))-unname(quantile(distribution_array,.25)),
                                mean(distribution_array),
                                std(distribution_array),
                                max(distribution_array)-min(distribution_array))


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

  shinyDirChoose(
    input,
    'cell_file_folder',
    roots = getVolumes()(),
    filetypes = c('', 'h5')
  )

  global <- reactiveValues(datapath_cell_file = getwd())

  dir <- reactive(input$cell_file_folder)

  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$cell_file_folder
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 #unlist(dir()$root)
                 global$datapath_cell_file <-
                   file.path('','Volumes',unlist(dir()$root),paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
                 file_list=list.files(path= global$datapath_cell_file, pattern=".h5", all.files=FALSE,
                                      full.names=FALSE)
                 updateSelectizeInput(session,"Cell_id_file_to_analyse","Select_cell_id",choices=file_list)
               })
  
  output$cell_list_csv <- renderDataTable({
    cell_list_csv=read_csv('/Volumes/Work_Julien/Cell_Data_File/Cell_file_information.csv',)
    
    cell_list_csv
  })
  
  get_cell_file <- eventReactive(input$Change_cell,{
    req(input$Cell_id_file_to_analyse)
    
    current_cell_file <- load_h5_file(file=paste0(global$datapath_cell_file,'/',input$Cell_id_file_to_analyse))
    
    sweep_info_table=current_cell_file$Sweep_info_table
    sweep_list=sweep_info_table$Sweep
    
    
    updateSelectInput(session,"Sweep_to_analyse","Sweep to analysis",choices=sweep_list)
    print('file_loaded')
    
    return (current_cell_file)
    
  })
  


 ##### CELL INFORMATION #####


  output$single_cell_file_path <- renderText({
    global$datapath_cell_file
  })

  output$Metadata <- renderTable({
    cell_tables_list=get_cell_file()
    Metadata_table=cell_tables_list$Metadata_table

    Metadata_table
  },rownames=TRUE)

  output$Sweep_info <- renderDataTable({
    cell_tables_list=get_cell_file()
    Sweep_info_table=cell_tables_list$Sweep_info_table

    Sweep_info_table
  })

  output$BE_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    BE_table <- cell_sweep_info_table[,c('Bridge_Error_GOhms','Bridge_Error_extrapolated','Sweep','Trace_id','Protocol_id')]
    levels=unique(BE_table$Protocol_id)
    BE_table$Protocol_id <- factor(BE_table$Protocol_id,levels=levels)
    BE_plot=ggplot2::ggplot(BE_table,mapping=aes_string(x='Trace_id',y='Bridge_Error_GOhms',color='Bridge_Error_extrapolated'))+
      ggplot2::geom_line(aes_string(group="Protocol_id",color="Bridge_Error_extrapolated"))+
      ggplot2::geom_point()
    BE_plotly <- ggplotly(BE_plot,dynamicTicks=TRUE)
    BE_plotly
  }))

  output$BE_boxplot <- renderPlotly(({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    BE_table <- cell_sweep_info_table[,c('Bridge_Error_GOhms','Bridge_Error_extrapolated','Sweep','Trace_id','Protocol_id')]
    levels=unique(BE_table$Protocol_id)
    BE_table$Protocol_id <- factor(BE_table$Protocol_id,levels=levels)
    BE_boxplot=ggplot2::ggplot(BE_table,mapping=aes_string(x=factor(0),y="Bridge_Error_GOhms"))+
      ggplot2::geom_boxplot()+
      ggplot2::geom_jitter(color="black", size=0.9, alpha=0.9)+
      ggplot2::theme(axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank())
    BE_boxplotly <- ggplotly(BE_boxplot,dynamicTicks=TRUE)
    BE_boxplotly
  }))

  output$Time_cst_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    TC_table <- cell_sweep_info_table[,c('Time_constant_ms','Sweep','Trace_id','Protocol_id')]
    levels=unique(TC_table$Protocol_id)
    TC_table$Protocol_id <- factor(TC_table$Protocol_id,levels=levels)
    TC_plot=ggplot2::ggplot(TC_table,mapping=aes_string(x="Trace_id",y="Time_constant_ms",color="Protocol_id"))+
      ggplot2::geom_line(aes_string(group="Protocol_id"))+
      ggplot2::geom_point()

    TC_plotly <- ggplotly(TC_plot,dynamicTicks=TRUE)
    TC_plotly
  }))

  output$Time_cst_boxplot <- renderPlotly(({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    TC_table <- cell_sweep_info_table[,c('Time_constant_ms','Sweep','Trace_id','Protocol_id')]
    levels=unique(TC_table$Protocol_id)
    TC_table$Protocol_id <- factor(TC_table$Protocol_id,levels=levels)
    TC_boxplot=ggplot2::ggplot(TC_table,mapping=aes_string(x=factor(0),y='Time_constant_ms'))+
      ggplot2::geom_boxplot()+
      ggplot2::geom_jitter(color="black", size=0.9, alpha=0.9)+
      ggplot2::theme(axis.title.x=element_blank(),
                                axis.text.x=element_blank(),
                                axis.ticks.x=element_blank())
    TC_boxplotly <- ggplotly(TC_boxplot,dynamicTicks=TRUE)
    TC_boxplotly
  }))
  
  output$Input_resistance_plot <- renderPlotly({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    IR_table <- cell_sweep_info_table[,c('Input_Resistance_MOhm','Sweep','Trace_id','Protocol_id')]
    levels=unique(IR_table$Protocol_id)
    IR_table$Protocol_id <- factor(IR_table$Protocol_id,levels=levels)
    IR_plot=ggplot2::ggplot(IR_table,mapping=aes_string(x="Trace_id",y="Input_Resistance_MOhm",color="Protocol_id"))+
      ggplot2::geom_line(aes_string(group="Protocol_id"))+
      ggplot2::geom_point()
    
    IR_plotly <- ggplotly(IR_plot,dynamicTicks=TRUE)
    IR_plotly
  })
  
  output$Input_resistance_boxplot <- renderPlotly(({
    cell_tables_list=get_cell_file()
    cell_sweep_info_table=cell_tables_list$Sweep_info_table
    IR_table <- cell_sweep_info_table[,c('Input_Resistance_MOhm','Sweep','Trace_id','Protocol_id')]
    levels=unique(IR_table$Protocol_id)
    IR_table$Protocol_id <- factor(IR_table$Protocol_id,levels=levels)
    IR_boxplot=ggplot2::ggplot(IR_table,mapping=aes_string(x=factor(0),y='Input_Resistance_MOhm'))+
      ggplot2::geom_boxplot()+
      ggplot2::geom_jitter(color="black", size=0.9, alpha=0.9)+
      ggplot2::theme(axis.title.x=element_blank(),
                     axis.text.x=element_blank(),
                     axis.ticks.x=element_blank())
    IR_boxplotly <- ggplotly(IR_boxplot,dynamicTicks=TRUE)
    IR_boxplotly
  }))
  


  ##### SINGLE SWEEP TRACES #####

  SF_plolty_test <- reactive({
    
    cell_tables_list=get_cell_file()
    
    sweep_info_table=cell_tables_list$Sweep_info_table
    sweep_list=sweep_info_table$Sweep
    selected_sweep=as.character(input$Sweep_to_analyse)
    sampling_freq=mean(sweep_info_table$Sampling_Rate_Hz)
    sweep_trace=data.frame(cell_tables_list$Full_TPC[[as.character(selected_sweep)]])
    
    colnames(sweep_trace) <- cell_tables_list$Full_TPC$TPC_colnames
    filt_coeff = (5 * 1e3) / (sampling_freq / 2.)
    bf <- butter(2, filt_coeff)
    zi <- filter_zi(bf)
    
    
    filtered_potential_trace <- filter(bf, sweep_trace[,"Membrane_potential_mV"],zi)$y
    filtered_current_trace <-  filter(bf, sweep_trace[,"Input_current_pA"],zi)$y
    
    
    sweep_trace[,"Membrane_potential_mV"] <- filtered_potential_trace
    sweep_trace[,"Input_current_pA"] <- filtered_current_trace
    #sweep_trace[,"Membrane_potential_mV"] <- bwfilter(sweep_trace[,"Membrane_potential_mV"], f=sampling_freq, n=2, to=5000)
    stim_start=sweep_info_table[as.character(selected_sweep),"Stim_start_s"]
    stim_end=sweep_info_table[as.character(selected_sweep),"Stim_end_s"]
    sweep_trace=sweep_trace[which(sweep_trace$Time_s <= (stim_end+.05) & sweep_trace$Time_s >= (stim_start-.05) ),]
    SF_table=data.frame(cell_tables_list$Full_SF[[as.character(selected_sweep)]])
    
    if (input$Apply_BE_correction_single_sweep == TRUE){
      BE=sweep_info_table[as.character(selected_sweep),"Bridge_Error_GOhms"]
      if (input$superimpose == TRUE && input$Apply_BE_correction_single_sweep == TRUE){
        raw_sweep_trace=sweep_trace
      }
      pre_stim_amp=mean(sweep_trace[which(sweep_trace$Time_s <= (stim_start-0.005) & sweep_trace$Time_s >= (stim_start-0.055)),"Input_current_pA"])
      
      post_stim_amp=mean(sweep_trace[which(sweep_trace$Time_s <= (stim_end+0.055) & sweep_trace$Time_s >= (stim_end+0.005) ),"Input_current_pA"])
      sweep_trace[which(sweep_trace$Time_s < (stim_start)),"Membrane_potential_mV"]=sweep_trace[which(sweep_trace$Time_s < (stim_start)),"Membrane_potential_mV"]-BE*pre_stim_amp
      sweep_trace[which(sweep_trace$Time_s > (stim_end)),"Membrane_potential_mV"]=sweep_trace[which(sweep_trace$Time_s > (stim_end)),"Membrane_potential_mV"]-BE*post_stim_amp
      sweep_trace[which(sweep_trace$Time_s <= (stim_end) & sweep_trace$Time_s >= (stim_start) ),"Membrane_potential_mV"]=sweep_trace[which(sweep_trace$Time_s <= (stim_end) & sweep_trace$Time_s >= (stim_start) ),"Membrane_potential_mV"]-BE*sweep_info_table[as.character(selected_sweep),"Stim_amp_pA"]
      
      SF_table[,"Membrane_potential_mV"]=SF_table[,"Membrane_potential_mV"]-BE*sweep_info_table[as.character(selected_sweep),"Stim_amp_pA"]
    }
    colnames(SF_table) <- cell_tables_list$Full_TPC$TPC_colnames
    
    for (elt in colnames(sweep_trace)){
      sweep_trace[,elt]=as.numeric(sweep_trace[,elt])
    }
    full_table=sweep_trace[,c('Time_s','Membrane_potential_mV')]
    full_table['Measure']='Membrane_potential_mV'
    full_table["Trace"]= "Raw_trace"
    colnames(full_table) <-  c("Time_s",'Value','Measure','Trace')
    
    if (input$Apply_BE_correction_single_sweep == TRUE ){
      full_table["Trace"]= "BE_corrected"
      
      if (input$superimpose == TRUE ){
        
        raw_sweep_trace_table=raw_sweep_trace[,c('Time_s','Membrane_potential_mV')]
        raw_sweep_trace_table['Measure']='Membrane_potential_mV'
        raw_sweep_trace_table["Trace"]= "Raw_trace"
        colnames(raw_sweep_trace_table) <-  c("Time_s",'Value','Measure','Trace')
        full_table=rbind(full_table,raw_sweep_trace_table)
      }
    }
    
    SF_Current_table=sweep_trace[,c('Time_s','Input_current_pA')]
    SF_Current_table['Measure']='Input_current_pA'
    SF_Current_table['Trace']='Raw_trace'
    colnames(SF_Current_table) <-  c("Time_s",'Value','Measure','Trace')
    full_table=rbind(full_table,SF_Current_table)
    
    if ( "First_derivative" %in% input$Derivative_to_display){
      second_table=sweep_trace[,c('Time_s','Potential_first_time_derivative_mV/s')]
      second_table['Measure']="Potential_first_time_derivative_mV/s"
      second_table["Trace"]= "Raw_trace"
      colnames(second_table) <-  c("Time_s",'Value','Measure','Trace')
      full_table=rbind(full_table,second_table)
    }
    if ('Second_derivative' %in% input$Derivative_to_display){
      third_table=sweep_trace[,c('Time_s','Potential_second_time_derivative_mV/s/s')]
      third_table['Measure']="Potential_second_time_derivative_mV/s/s"
      third_table["Trace"]= "Raw_trace"
      colnames(third_table) <-  c("Time_s",'Value','Measure','Trace')
      full_table=rbind(full_table,third_table)
      
    }
    
    
    # full_table=rbind(full_table,SF_Current_table,second_table,third_table)
    full_table$Measure=factor(full_table$Measure,levels=c('Membrane_potential_mV','Input_current_pA',"Potential_first_time_derivative_mV/s","Potential_second_time_derivative_mV/s/s"))
    full_table=as.data.frame(lapply(full_table, unlist))
    
    
    for (elt2 in colnames(SF_table)[-length(colnames(SF_table))]){
      SF_table[,elt2]=as.numeric(SF_table[,elt2])
    }
    colnames(SF_table) <- c(cell_tables_list$Full_TPC$TPC_colnames,'Feature')
    
    
    SF_plot=ggplot2::ggplot()+
      ggplot2::geom_line(full_table,mapping=aes_string(x="Time_s",y="Value",group="Trace",color="Trace"),size=.96)+
      ggplot2::facet_grid(Measure ~ .,scales = "free",space = 'free')+
      ggplot2::scale_colour_manual(values=c(BE_corrected="red",Raw_trace="black"))+
      force_panelsizes(rows=c(2,1))
    
    if (dim(SF_table)[1] != 0){
      fullSF_table=SF_table[,c('Time_s','Membrane_potential_mV','Feature')]
      fullSF_table['Measure']='Membrane_potential_mV'
      colnames(fullSF_table) <-  c("Time_s",'Value','Feature','Measure')
      
      
      if ( "First_derivative" %in% input$Derivative_to_display){
        SF_Second_table=SF_table[,c('Time_s','Potential_first_time_derivative_mV/s','Feature')]
        SF_Second_table['Measure']='Potential_first_time_derivative_mV/s'
        colnames(SF_Second_table) <-  c("Time_s",'Value','Feature','Measure')
        fullSF_table=rbind(fullSF_table,SF_Second_table)
      }
      
      if ('Second_derivative' %in% input$Derivative_to_display){
        SF_Third_table=SF_table[,c('Time_s','Potential_second_time_derivative_mV/s/s','Feature')]
        SF_Third_table['Measure']='Potential_second_time_derivative_mV/s/s'
        colnames(SF_Third_table) <-  c("Time_s",'Value','Feature','Measure')
        fullSF_table=rbind(fullSF_table,SF_Third_table)
      }
      
      #fullSF_table=rbind(SF_First_table,SF_Second_table,SF_Third_table)
      fullSF_table$Measure=factor(fullSF_table$Measure,levels=c('Membrane_potential_mV',"Potential_first_time_derivative_mV/s","Potential_second_time_derivative_mV/s/s"))
      SF_plot=SF_plot+ggplot2::geom_point(fullSF_table,mapping = aes_string(x="Time_s",y="Value",fill="Feature"),stroke=0,size=2)
    }
    
    SF_plotly <- ggplotly(SF_plot,dynamicTicks=TRUE)
    return (SF_plot)
    
  })
  
  output$Spike_feature_plot <- renderPlotly({
    
    SF_plot <- SF_plolty_test()
    SF_plot=SF_plot+force_panelsizes(rows=c(2,1))+ ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16))
    SF_plolty_test_result <- ggplotly(SF_plot,dynamicTicks=TRUE)
    SF_plolty_test_result
  })

  output$linear_properties_plot <- renderPlotly({
    cell_tables_list=get_cell_file()
    selected_sweep=as.character(input$Sweep_to_analyse)
    
    
    sweep_info_table=cell_tables_list$Sweep_info_table
    full_TPC_table=cell_tables_list$Full_TPC
    full_TPC_table_nested <- nest_Full_DF(Full_SF = full_TPC_table,'TPC')
    sub_TPC_filtered <- get_filtered_TPC_table(full_TPC_table_nested,selected_sweep)
    stim_start_time <- sweep_info_table[selected_sweep,"Stim_start_s"]
    time_cst_fit <- fit_membrane_time_cst(sub_TPC_filtered,stim_start_time,(stim_start_time+0.3))
    best_A <- time_cst_fit[[1]]
    best_tau <- time_cst_fit[[2]]
    best_Response_SS <- time_cst_fit[[3]]
    best_resting_potnetial <- time_cst_fit[[4]]
    NRMSE <- time_cst_fit[[5]]
    
   
    time_array=sub_TPC_filtered[which(sub_TPC_filtered$Time_s <= (stim_start_time+.3) & sub_TPC_filtered$Time_s >= (stim_start_time) ),]
    time_array <- time_array$Time_s
    time_cst_fit <- time_cst_model(array(time_array),best_A,best_tau,best_Response_SS)
    time_cst_fit_table <- data.frame(cbind(time_array,time_cst_fit))
    colnames(time_cst_fit_table) <- c('Time_s','Membrane_potential_mV')
    time_cst_fit_table['Legend'] <- 'Exponential_Fit'
    
    resting_potential_time_array <- sub_TPC_filtered[which(sub_TPC_filtered$Time_s <= (stim_start_time)) ,]
    resting_potential_time_array <- resting_potential_time_array$Time_s
    resting_potential_table <- data.frame(cbind(resting_potential_time_array,rep(best_resting_potnetial,length(resting_potential_time_array)),rep("Resting_Potential",length(resting_potential_time_array))))
    colnames(resting_potential_table) <- c('Time_s','Membrane_potential_mV','Legend')
    full_time_cst_fit_table=rbind(time_cst_fit_table,resting_potential_table)
    
    full_time_cst_fit_table$Time_s=as.numeric(full_time_cst_fit_table$Time_s)
    full_time_cst_fit_table$Membrane_potential_mV=as.numeric(full_time_cst_fit_table$Membrane_potential_mV)
    full_time_cst_fit_table$Legend=as.factor(full_time_cst_fit_table$Legend)
    
    time_cst_plot = ggplot2::ggplot(sub_TPC_filtered,mapping=aes_string(x='Time_s',y="Membrane_potential_mV"))+
    ggplot2::geom_line()+
    ggplot2::geom_line(full_time_cst_fit_table,mapping=aes_string(x='Time_s',y="Membrane_potential_mV",group="Legend",color='Legend'))

    time_cst_plotly <- ggplotly(time_cst_plot,dynamicTicks=TRUE)
    time_cst_plotly
  })

  ##### I/O #####

  ###### TIME_BASED #######

  output$I_O_feature_plot_time_based <- renderPlotly({
    cell_tables_list=get_cell_file()
    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Time_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))
    fit_table=cell_tables_list$Cell_fit_table
    if (dim(fit_table)[1]==1){
      #if the fit was rejected
      if (input$for_saving_plot == TRUE){
        IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
          ggplot2::geom_point(aes_string(text="Sweep"),color="black")+
          ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
          ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
      }
      else{
        IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
          ggplot2::geom_point(aes_string(text="Sweep"),color="black")+
          ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
      }

    }

    else{

      fit_table_list=get_fit_tables(cell_tables_list,"Time_based")

      scale_dict=c("TRUE" = "16","FALSE" = "1")

      fit_table = fit_table_list$fit_table
      IO_table = fit_table_list$IO_table
      Sat_table = fit_table_list$Sat_table


      if (input$for_saving_plot == TRUE){
        my_blues = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I excluded the two lighter hues
        IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
          ggplot2::geom_point(aes_string(text="Sweep"))+
          ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),size=.95)+
          ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
          ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=25)+
          ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
          ggplot2::scale_colour_manual(values=my_blues)+
          ggplot2::scale_shape_manual(values=scale_dict)+
          ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
      }
      else{
        my_blues = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I exluded the two lighter hues
        IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
          ggplot2::geom_point(aes_string(text="Sweep"))+
          ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"))+
          ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
          ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=10)+
          ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
          ggplot2::scale_colour_manual(values=my_blues)+
          ggplot2::scale_shape_manual(values=scale_dict)

      }
    }
    IO_plotly <- ggplotly(IO_plot,dynamicTicks=TRUE)

    IO_plotly

  })

  output$IO_table_time_fit <- renderTable({
    cell_tables_list=get_cell_file()
    cell_fit_table=cell_tables_list$Cell_fit_table
    cell_fit_table = cell_fit_table[which(cell_fit_table$Response_type == 'Time_based' ),]
    cell_fit_table=cell_fit_table[,c('Response_type','Output_Duration', 'I_O_obs', 'I_O_QNRMSE', 'Hill_amplitude',
                                     'Hill_coef', 'Hill_Half_cst','Hill_x0','Sigmoid_x0','Sigmoid_sigma')]

    cell_fit_table
  })

  output$IO_table_time_feature <- renderTable({
    cell_tables_list=get_cell_file()
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]

    Cell_feature_table
  })

  output$Stim_freq_time_table <- renderTable({
    cell_tables_list=get_cell_file()
    
    original_SF_table=cell_tables_list$Full_SF
    original_cell_sweep_info_table=cell_tables_list$Sweep_info_table
    sweep_QC_table=cell_tables_list$Sweep_QC_table
    
    nested_full_SF_table=nest_Full_DF(original_SF_table,'SF')
    test_stim_freq_table=get_stim_freq_table(nested_full_SF_table, original_cell_sweep_info_table,sweep_QC_table,response_duration=0.005, response_based="Time_based")
    test_stim_freq_table['Output_duration']='5ms'
    test_stim_freq_table=test_stim_freq_table[order(test_stim_freq_table$Stim_amp_pA, decreasing = FALSE), ]
    for (current_time in c(0.010,0.025,0.05,0.1,0.25,0.5)){
      new_stim_freq_table=get_stim_freq_table(nested_full_SF_table, original_cell_sweep_info_table,sweep_QC_table,response_duration=current_time, response_based="Time_based")
      new_stim_freq_table['Output_duration']=paste0(as.character(as.integer(current_time*1000)),'ms')
      new_stim_freq_table=new_stim_freq_table[order(new_stim_freq_table$Stim_amp_pA, decreasing = FALSE), ]
      test_stim_freq_table=rbind(test_stim_freq_table,new_stim_freq_table)
      }
    #test_stim_freq_table=get_stim_freq_table(nested_full_SF_table, original_cell_sweep_info_table,sweep_QC_table,response_duration=0.5, response_based="Time_based")
    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Time_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))

    test_stim_freq_table

  })

  output$Gain_time_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Gain"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')


    gain_time_plotly <- ggplotly(gain_time_plot,dynamicTicks=TRUE)
    gain_time_plotly
  }))

  output$Gain_time_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_df=data.frame(Intercept = numeric(),
                       Slope = numeric(),
                       `R^2` = numeric())
    rsquared=summary(lmgain)$r.squared

    gain_df[1,] <- list(lmgain$coefficients[1],lmgain$coefficients[2],rsquared)

    gain_df
  }))

  output$Threshold_time_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Threshold"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')


    threshold_time_plot <- ggplotly(threshold_time_plot,dynamicTicks=TRUE)
    threshold_time_plot
  }))

  output$Threshold_time_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_df=data.frame(Intercept = numeric(),
                            Slope = numeric(),
                            `R^2` = numeric())
    rsquared=summary(lmthreshold)$r.squared
    threshold_df[1,] <- list(lmthreshold$coefficients[1],lmthreshold$coefficients[2],rsquared)

    threshold_df
  }))

  output$Saturation_stimulus_time_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Stimulus"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')


    sat_stim_time_plot <- ggplotly(sat_stim_time_plot,dynamicTicks=TRUE)
    sat_stim_time_plot
  }))

  output$Saturation_stimulus_time_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_stim)$r.squared
    sat_stim_df[1,] <- list(lmsat_stim$coefficients[1],lmsat_stim$coefficients[2],rsquared)

    sat_stim_df
  }))

  output$Saturation_freq_time_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Frequency"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')


    sat_freq_time_plot <- ggplotly(sat_freq_time_plot,dynamicTicks=TRUE)
    sat_freq_time_plot
  }))

  output$Saturation_freq_time_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_freq)$r.squared
    sat_freq_df[1,] <- list(lmsat_freq$coefficients[1],lmsat_freq$coefficients[2],rsquared)

    sat_freq_df
  }))

  ###### INDEX_BASED #######

  output$I_O_feature_plot_index_based <- renderPlotly({
    cell_tables_list=get_cell_file()
    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Index_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))
    fit_table_list=get_fit_tables(cell_tables_list,"Index_based")

    scale_dict=c("TRUE" = "16","FALSE" = "1")

    fit_table = fit_table_list$fit_table
    IO_table = fit_table_list$IO_table
    Sat_table = fit_table_list$Sat_table


    if (input$for_saving_plot == TRUE){
      green_palet = colorRampPalette(brewer.pal(9, "Greens")[3:9])
      IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
        ggplot2::geom_point(aes_string(text="Sweep"))+
        ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),size=.95)+
        ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
        ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=25)+
        ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
        ggplot2::scale_colour_manual(values=green_palet(10))+
        ggplot2::scale_shape_manual(values=scale_dict)+
        ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
    }
    else{
      green_palet = colorRampPalette(brewer.pal(9, "Greens")[3:9])
      IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
        ggplot2::geom_point(aes_string(text="Sweep"))+
        ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"))+
        ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
        ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=10)+
        ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
        ggplot2::scale_colour_manual(values=green_palet(10))+
        ggplot2::scale_shape_manual(values=scale_dict)

    }
    IO_plotly <- ggplotly(IO_plot,dynamicTicks=TRUE)

    IO_plotly

  })

  output$IO_table_index_fit <- renderTable({
    cell_tables_list=get_cell_file()
    cell_fit_table=cell_tables_list$Cell_fit_table
    cell_fit_table = cell_fit_table[which(cell_fit_table$Response_type == 'Index_based' ),]
    cell_fit_table=cell_fit_table[,c('Response_type','Output_Duration', 'I_O_obs', 'I_O_QNRMSE', 'Hill_amplitude',
                                     'Hill_coef', 'Hill_Half_cst','Hill_x0','Sigmoid_x0','Sigmoid_sigma')]

    cell_fit_table
  })

  output$IO_index_table_feature <- renderTable({
    cell_tables_list=get_cell_file()
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]

    Cell_feature_table
  })

  output$Stim_freq_index_table <- renderTable({
    cell_tables_list=get_cell_file()

    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Index_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))

    stim_freq_table

  })

  output$Gain_index_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Gain"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')


    gain_time_plotly <- ggplotly(gain_time_plot,dynamicTicks=TRUE)
    gain_time_plotly
  }))

  output$Gain_index_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_df=data.frame(Intercept = numeric(),
                       Slope = numeric(),
                       `R^2` = numeric())
    rsquared=summary(lmgain)$r.squared

    gain_df[1,] <- list(lmgain$coefficients[1],lmgain$coefficients[2],rsquared)

    gain_df
  }))

  output$Threshold_index_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Threshold"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')


    threshold_time_plot <- ggplotly(threshold_time_plot,dynamicTicks=TRUE)
    threshold_time_plot
  }))

  output$Threshold_index_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_df=data.frame(Intercept = numeric(),
                            Slope = numeric(),
                            `R^2` = numeric())
    rsquared=summary(lmthreshold)$r.squared
    threshold_df[1,] <- list(lmthreshold$coefficients[1],lmthreshold$coefficients[2],rsquared)

    threshold_df
  }))

  output$Saturation_stimulus_index_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Stimulus"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')


    sat_stim_time_plot <- ggplotly(sat_stim_time_plot,dynamicTicks=TRUE)
    sat_stim_time_plot
  }))

  output$Saturation_stimulus_index_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_stim)$r.squared
    sat_stim_df[1,] <- list(lmsat_stim$coefficients[1],lmsat_stim$coefficients[2],rsquared)

    sat_stim_df
  }))

  output$Saturation_freq_index_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Frequency"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')


    sat_freq_time_plot <- ggplotly(sat_freq_time_plot,dynamicTicks=TRUE)
    sat_freq_time_plot
  }))

  output$Saturation_freq_index_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Index_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_freq)$r.squared
    sat_freq_df[1,] <- list(lmsat_freq$coefficients[1],lmsat_freq$coefficients[2],rsquared)

    sat_freq_df
  }))


  ###### INTERVAL_BASED #######

  output$I_O_feature_plot_interval_based <- renderPlotly({
    cell_tables_list=get_cell_file()
    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Interval_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))
    fit_table_list=get_fit_tables(cell_tables_list,"Interval_based")

    scale_dict=c("TRUE" = "16","FALSE" = "1")

    fit_table = fit_table_list$fit_table
    IO_table = fit_table_list$IO_table
    Sat_table = fit_table_list$Sat_table




    if (input$for_saving_plot == TRUE){
      green_palet = colorRampPalette(brewer.pal(9, "Reds")[3:9])
      IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x='Stim_amp_pA',y='Frequency_Hz',colour='Response_Duration'))+
        ggplot2::geom_point(aes_string(text="Sweep"))+
        ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),size=.95)+
        ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
        ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=25)+
        ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
        ggplot2::scale_colour_manual(values=green_palet(10))+
        ggplot2::scale_shape_manual(values=scale_dict)+
        ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
    }
    else{
      green_palet = colorRampPalette(brewer.pal(9, "Reds")[3:9])
      IO_plot=ggplot2::ggplot(stim_freq_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",colour="Response_Duration"))+
        ggplot2::geom_point(aes_string(text="Sweep"))+
        ggplot2::geom_line(fit_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"))+
        ggplot2::geom_line(IO_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),linetype='dashed')+
        ggplot2::geom_point(Sat_table,mapping=aes_string(x="Stim_amp_pA",y="Frequency_Hz",color="Response_Duration"),shape=3,size=10)+
        ggplot2::ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))+
        ggplot2::scale_colour_manual(values=green_palet(10))+
        ggplot2::scale_shape_manual(values=scale_dict)

    }

    IO_plotly <- ggplotly(IO_plot,dynamicTicks=TRUE)

    IO_plotly

  })

  output$IO_table_interval_fit <- renderTable({
    cell_tables_list=get_cell_file()
    cell_fit_table=cell_tables_list$Cell_fit_table
    cell_fit_table = cell_fit_table[which(cell_fit_table$Response_type == 'Interval_based' ),]
    cell_fit_table=cell_fit_table[,c('Response_type','Output_Duration', 'I_O_obs', 'I_O_QNRMSE', 'Hill_amplitude',
                                     'Hill_coef', 'Hill_Half_cst','Hill_x0','Sigmoid_x0','Sigmoid_sigma')]

    cell_fit_table
  })

  output$IO_interval_table_feature <- renderTable({
    cell_tables_list=get_cell_file()
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]

    Cell_feature_table
  })

  output$Stim_freq_interval_table <- renderTable({
    cell_tables_list=get_cell_file()

    Sweep_QC_table=cell_tables_list$Sweep_QC_table
    stim_freq_table=get_stim_freq_table_R(cell_tables_list,'Interval_based')
    stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')],
                            by=c("Sweep"))

    stim_freq_table

  })

  output$Gain_interval_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Gain"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')


    gain_time_plotly <- ggplotly(gain_time_plot,dynamicTicks=TRUE)
    gain_time_plotly
  }))

  output$Gain_interval_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)

    gain_df=data.frame(Intercept = numeric(),
                       Slope = numeric(),
                       `R^2` = numeric())
    rsquared=summary(lmgain)$r.squared

    gain_df[1,] <- list(lmgain$coefficients[1],lmgain$coefficients[2],rsquared)

    gain_df
  }))

  output$Threshold_interval_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Threshold"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')


    threshold_time_plot <- ggplotly(threshold_time_plot,dynamicTicks=TRUE)
    threshold_time_plot
  }))

  output$Threshold_interval_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmthreshold = lm(Threshold~Output_Duration, data = Cell_feature_table)

    threshold_df=data.frame(Intercept = numeric(),
                            Slope = numeric(),
                            `R^2` = numeric())
    rsquared=summary(lmthreshold)$r.squared
    threshold_df[1,] <- list(lmthreshold$coefficients[1],lmthreshold$coefficients[2],rsquared)

    threshold_df
  }))

  output$Saturation_stimulus_interval_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Stimulus"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')


    sat_stim_time_plot <- ggplotly(sat_stim_time_plot,dynamicTicks=TRUE)
    sat_stim_time_plot
  }))

  output$Saturation_stimulus_interval_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmsat_stim = lm(Saturation_Stimulus~Output_Duration, data = Cell_feature_table)

    sat_stim_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_stim)$r.squared
    sat_stim_df[1,] <- list(lmsat_stim$coefficients[1],lmsat_stim$coefficients[2],rsquared)

    sat_stim_df
  }))

  output$Saturation_freq_interval_plot <- renderPlotly(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_time_plot=ggplot2::ggplot(Cell_feature_table,mapping=aes_string(x="Output_Duration",y="Saturation_Frequency"))+
      ggplot2::geom_point()+
      ggplot2::geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')


    sat_freq_time_plot <- ggplotly(sat_freq_time_plot,dynamicTicks=TRUE)
    sat_freq_time_plot
  }))

  output$Saturation_freq_interval_metrics <- renderTable(({
    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Interval_based' ),]
    lmsat_freq = lm(Saturation_Frequency~Output_Duration, data = Cell_feature_table)

    sat_freq_df=data.frame(Intercept = numeric(),
                           Slope = numeric(),
                           `R^2` = numeric())
    rsquared=summary(lmsat_freq)$r.squared
    sat_freq_df[1,] <- list(lmsat_freq$coefficients[1],lmsat_freq$coefficients[2],rsquared)

    sat_freq_df
  }))

  ###### ADAPTATION #######

  output$Adaptation_plot <- renderPlotly({

    cell_tables_list=get_cell_file()

    sweep_info_table=cell_tables_list$Sweep_info_table

    cell_fit_table=cell_tables_list$Cell_fit_table
    sub_cell_fit_table=cell_fit_table[which(cell_fit_table$Adaptation_obs == '--'),]
    sweep_list=sweep_info_table$Sweep
    sweep_list <-  unlist(unname(sweep_list))
    Stim_amp_pA = sweep_info_table$Stim_amp_pA
    Stim_amp_pA =  unlist(unname(Stim_amp_pA))
    if (dim(sub_cell_fit_table)[1] == 0){
      Adapt_plot=ggplot2::ggplot() + 
        ggplot2::theme_void() + 
        ggplot2::ggtitle('Not able to compute adaptation')

    }

    else{


      full_median_table=data.frame(Interval = numeric(),    # Create empty data frame
                                   Inst_freq_WU = numeric(),
                                   Nb_of_obs = numeric(),
                                   Output_Duration=character())

      full_inst_freq_table=data.frame(Stim_amp_pA=numeric(),
                                      Interval=numeric(),
                                      Inst_freq_WU = numeric())

    }


    #Full_Sweep_metadata=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`))

    sweep_info_table=cell_tables_list$Sweep_info_table




    full_median_table=data.frame(Interval = numeric(),    # Create empty data frame
                                 Inst_freq_WU = numeric(),
                                 Nb_of_obs = numeric(),
                                 Output_Duration=character())

    full_inst_freq_table=data.frame(Stim_amp_pA=numeric(),
                                    Interval=numeric(),
                                    Inst_freq_WU = numeric())


    maximum_nb_interval =0



    ## Get maximum number of spike across all sweeps
    for (current_sweep in sweep_list){
      current_SF_table = data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
      start_time=sweep_info_table[current_sweep,'Stim_start_s']
      current_SF_tableSpike_time=subset(current_SF_table, Feature == "Upstroke")
      current_SF_tableSpike_time=subset(current_SF_tableSpike_time,Time_s<=(start_time+.5))

      nb_spikes=dim(current_SF_tableSpike_time)[1]

      if (nb_spikes>maximum_nb_interval ){
        maximum_nb_interval=nb_spikes
      }

    }


    if (maximum_nb_interval>1){
      new_columns=as.character(seq(1,(maximum_nb_interval-1)))
      Inst_freq_table = data.frame(sweep_list,Stim_amp_pA)

      rownames(Inst_freq_table) <- Inst_freq_table$sweep_list

      
      for (elt in new_columns){
        Inst_freq_table[elt]=NaN
      }

      for (current_sweep in sweep_list){
        current_SF_table = data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        start_time=sweep_info_table[current_sweep,'Stim_start_s']
        current_SF_tableSpike_time=subset(current_SF_table, Feature == "Upstroke")
        current_SF_tableSpike_time=subset(current_SF_tableSpike_time,Time_s<=(start_time+.5))
        spike_times=unlist(unname(current_SF_tableSpike_time$Time_s))

        if (length(spike_times) >2){

          for (current_spike_time_index in 2:length(spike_times)){
            current_frequency=1/(spike_times[current_spike_time_index]-spike_times[current_spike_time_index-1])
            current_interval=current_spike_time_index-1
            Inst_freq_table[as.character(current_sweep),as.character(current_interval)]=current_frequency

          }
        }
        initial_first_freq=Inst_freq_table[as.character(current_sweep),"1"]
        for (col in new_columns){

          Inst_freq_table[as.character(current_sweep),col]=Inst_freq_table[as.character(current_sweep),col]/initial_first_freq
        }
      }

      Inst_freq_table=Inst_freq_table[,seq(2,dim(Inst_freq_table)[2])]

      Inst_freq_table$Stim_amp_pA <- as.numeric(Inst_freq_table$Stim_amp_pA)
      Inst_freq_table=gather(Inst_freq_table, key="Interval", value="Inst_freq_WU", 2:dim(Inst_freq_table)[2])

      Inst_freq_table$Interval=str_remove(Inst_freq_table$Interval,'Interval_')
      Inst_freq_table <- Inst_freq_table %>% replace(.=="NULL", NaN)
      Inst_freq_table <- Inst_freq_table[!is.na(Inst_freq_table$Inst_freq_WU),]
      Inst_freq_table$Inst_freq_WU <- unlist(Inst_freq_table$Inst_freq_WU)
      Inst_freq_table$Interval=as.numeric(Inst_freq_table$Interval)

      count_df=data.frame(table(Inst_freq_table$Interval))
      if (dim(count_df)[1]>0){
        colnames(count_df) <- c("Interval","Nb_of_obs")


        median_table=Inst_freq_table %>% group_by(Interval) %>%
          summarise(Inst_freq_WU=median(Inst_freq_WU))
        median_table$Interval <- as.numeric(median_table$Interval)
        count_df$Interval <- as.numeric(count_df$Interval)
        median_table <- merge(median_table,count_df,by=c("Interval"))
        median_table['Output_Duration']='500ms'

        full_median_table=rbind(full_median_table,median_table)
        full_inst_freq_table=rbind(full_inst_freq_table,Inst_freq_table)
      }
    }

    full_median_table$Output_Duration=factor(full_median_table$Output_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
    full_median_table$Nb_of_obs=factor(full_median_table$Nb_of_obs)
    Adapt_plot=ggplot2::ggplot()+
      ggplot2::geom_point(full_inst_freq_table,mapping=aes_string(x="Interval",y="Inst_freq_WU",color="Stim_amp_pA"))+
      ggplot2::geom_point(full_median_table,mapping=aes_string(x="Interval",y="Inst_freq_WU",alpha="Nb_of_obs"),shape="square",color='red')
    fit_table=sub_cell_fit_table
    fit_table=subset(fit_table,Adaptation_obs =="--")
    fit_table=subset(fit_table,Response_type =="Time_based")

    Interval_seq = seq(1,max(Inst_freq_table$Interval),0.1)


    #Interval_seq=seq(1,length(seq(7,dim(Full_Sweep_metadata)[2])))
    if (dim(fit_table)[1] != 0){



      A=fit_table[which(fit_table["Output_Duration"]==0.5),"A"]
      Index_cst=fit_table[which(fit_table["Output_Duration"]==0.5),"B"]
      C=fit_table[which(fit_table["Output_Duration"]==0.5),"C"]


      A_norm=A/(A+C)
      C_norm=C/(A+C)
      #
      if (input$Normalize_adapt_params == TRUE){
        inst_freq_array=A_norm*exp(-Interval_seq/Index_cst)+C_norm
      }
      else{
        inst_freq_array=A*exp(-Interval_seq/Index_cst)+C
      }
      #inst_freq_array=A_norm*exp(-Interval_seq/Index_cst)+C_norm

      new_table=data.frame(cbind(Interval_seq,inst_freq_array))
      Adapt_fit_table=data.frame(cbind(Interval_seq,inst_freq_array))
      Adapt_fit_table['Output_Duration']='500ms'
      new_table['Output_Duration']='500ms'
      Adapt_fit_table=rbind(Adapt_fit_table,new_table)


      colnames(Adapt_fit_table) <- c("Interval","Inst_freq_WU",'Output_Duration')
      Adapt_fit_table$Output_Duration=as.factor(Adapt_fit_table$Output_Duration)
      Adapt_fit_table$Output_Duration=factor(Adapt_fit_table$Output_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))

      Adapt_plot=Adapt_plot+ggplot2::geom_line(Adapt_fit_table,mapping=aes_string(x="Interval",y="Inst_freq_WU"))
    }

    Adapt_plotly <- ggplotly(Adapt_plot,dynamicTicks=TRUE)
    Adapt_plotly




  })

  output$Adapt_table <- renderTable({
    cell_tables_list=get_cell_file()
    cell_fit_table=cell_tables_list$Cell_fit_table
    cell_fit_table=cell_fit_table[which(cell_fit_table$Response_type == 'Time_based' ),]
    cell_fit_table=cell_fit_table[which(cell_fit_table$Output_Duration == 0.5 ),]
    sub_cell_fit_table=cell_fit_table[,c('Adaptation_obs','Adaptation_RMSE','A','B','C')]

    sub_cell_fit_table
  })

  ##### RAW TRACES #####


  output$traces_plot <- renderPlotly({


    # json_file=get_json_file()
    cell_tables_list=get_cell_file()


    my_plot=ggplot2::ggplot()

    sweep_info_table=cell_tables_list$Sweep_info_table
    sweep_list=sweep_info_table$Sweep
    
    sampling_freq=mean(sweep_info_table$Sampling_Rate_Hz)

    full_trace_df=data.frame(cell_tables_list$Full_TPC[as.character(sweep_list[1])])
    





    # full_trace_df[,"Membrane_potential_mV"] <- bwfilter(full_trace_df[,"Membrane_potential_mV"], f=sampling_freq, n=2, to=5000)


    colnames(full_trace_df) <- cell_tables_list$Full_TPC$TPC_colnames
    full_trace_df["Sweep"]=as.character(sweep_list[1])

    stim_start=sweep_info_table[as.character(sweep_list[1]),"Stim_start_s"]
    stim_end=sweep_info_table[as.character(sweep_list[1]),"Stim_end_s"]
    full_trace_df=full_trace_df[which(full_trace_df$Time_s <= (stim_end+.05) & full_trace_df$Time_s >= (stim_start-.05) ),]
    if (input$Apply_BE_correction == TRUE){
      BE=sweep_info_table[as.character(sweep_list[1]),"Bridge_Error_GOhms"]
      full_trace_df[,"Membrane_potential_mV"]=full_trace_df[,"Membrane_potential_mV"]-BE*full_trace_df[,"Input_current_pA"]
    }
    for (current_sweep in sweep_list[2:length(sweep_list)]){
      current_sweep=as.character(current_sweep)
      df=data.frame( cell_tables_list$Full_TPC[as.character(current_sweep)])
      colnames(df) <- cell_tables_list$Full_TPC$TPC_colnames
      
      df["Sweep"]=current_sweep

      stim_start=sweep_info_table[as.character(current_sweep),"Stim_start_s"]
      stim_end=sweep_info_table[as.character(current_sweep),"Stim_end_s"]
      df=df[which(df$Time_s <= (stim_end+.05) & df$Time_s >= (stim_start-.05) ),]

      if (input$Apply_BE_correction == TRUE){
        BE=sweep_info_table[as.character(current_sweep),"Bridge_Error_GOhms"]
        full_trace_df[,"Membrane_potential_mV"]=full_trace_df[,"Membrane_potential_mV"]-BE*full_trace_df[,"Input_current_pA"]
      }

      filt_coeff = (5 * 1e3) / (sampling_freq / 2.)
      bf <- butter(2, filt_coeff)
      zi <- filter_zi(bf)


      df[,"Membrane_potential_mV"] <- filter(bf, df[,"Membrane_potential_mV"],zi)$y
      df[,"Input_current_pA"] <-  filter(bf, df[,"Input_current_pA"],zi)$y

      full_trace_df=rbind(full_trace_df,df)

    }

    for (elt in colnames(full_trace_df)){
      if (elt != 'Sweep'){
        full_trace_df[,elt]=as.numeric(full_trace_df[,elt])
      }
    }

    full_trace_df[,"Sweep"]=as.factor(full_trace_df[,"Sweep"])
    new_df=full_trace_df[,c('Time_s','Membrane_potential_mV','Sweep')]
    colnames(new_df) <- c('Time_s','Value','Sweep')
    new_df['Measure']="Membrane_potential_mV"


    second_new_df=full_trace_df[,c('Time_s','Input_current_pA','Sweep')]
    colnames(second_new_df) <- c('Time_s','Value','Sweep')
    second_new_df['Measure']="Input_current_pA"

    third_new_df=rbind(new_df,second_new_df)

    third_new_df$Measure=factor(third_new_df$Measure,levels=c('Membrane_potential_mV','Input_current_pA'))
    third_new_df=as.data.frame(lapply(third_new_df, unlist))
    
    full_trace_plot=ggplot2::ggplot()+
      ggplot2::geom_line(third_new_df,mapping=aes_string(x="Time_s",y="Value",group="Sweep",color="Sweep"))+
      ggplot2::facet_grid(Measure ~ .,scales = "free")+ 
      ggplot2::theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16))

    full_trace_plotly <- ggplotly(full_trace_plot,dynamicTicks=TRUE)
    # full_trace_plot=ggplot(data=full_trace_df,aes(x=Time_s,y=Membrane_potential_mV,group=Sweep))+geom_line(aes(color=Sweep))
    # full_trace_plotly <- ggplotly(full_trace_plot,dynamicTicks=TRUE)

    full_trace_plotly
  })

  
}
shinyApp(ui, server)
