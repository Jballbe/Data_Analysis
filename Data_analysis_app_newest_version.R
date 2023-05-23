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
  navbarPage(title = "Files",
             ##1
             tabPanel("Files",
                      sidebarLayout(
                        sidebarPanel(
                          shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
                          textOutput('file_path'),
                          checkboxGroupButtons('Population_files','Select files',choices = ""),
                          
                          shinyDirButton('Cell_file_dir', 'Select a folder for cell files', 'Please select a folder', FALSE),
                          textOutput("cell_file_path"),
                          
                          
                          fileInput("Pop_class_file","Choose Population Class file"),
                          
                          actionButton("import_files","Import_files"),
                          
                          
                        ),
                        mainPanel(textOutput("checklibraries"),
                                  textOutput("multiplefiles"),
                                  dataTableOutput('mytest')
                                  )
                      ),
                      
             ),##1
             tabPanel(title = "Data Analysis",
                      sidebarLayout(
                        sidebarPanel(width=2,
                          
                          selectInput("Factor_of_analysis_data_repartition","Choose factor of analysis",choices=""),
                          selectInput("Feature_to_analyse_data_repartition",'Choose feature to analyse',choices=""),
                          checkboxInput('normalise_descriptive_plot','Normalize'),
                          checkboxInput("Facet_decriptive_plot","Facet plot"),
                          selectInput("Factor_to_facet"," Facet plot per",choices=''),
                          checkboxInput("normalize_per_input_resistance_Data_overview","Normalize current per input resistance"),
                          sliderInput(inputId = "bins_width_histogram",
                                      label = "Width of bins:",
                                      min = .01,
                                      max = 2,
                                      value = .1),
                          
                          checkboxInput("descriptive_show_mean","Show means"),
                          
                          
                        ),
                        mainPanel(width = 10,
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
                        sidebarPanel(width=2,
                                     
                                     selectInput("Feature_to_analyse_RM",'Choose feature to analyse',choices=""),
                                     selectizeInput('Subset_population_RM','Select group', choices="", selected = "", multiple = TRUE,
                                                    options = NULL),
                                     
                                     selectInput("Factor_of_analysis_RM","Choose factor of analysis",choices=""),
                                     selectInput("select_outliers_to_remove","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                                     
                                     checkboxInput("normalize_per_input_resistance_RM","Normalize current per input resistance"),
                                     
                                     
                        ),
                        mainPanel(width = 10,
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
                        sidebarPanel(width=2,
                                     selectInput('File_to_select_Var','Select file to study',choices=''),
                                     selectInput("Feature_to_analyse_Variance",'Choose feature to analyse',choices=""),
                                     selectizeInput('Subset_population_Variance','Select group', choices="", selected = "", multiple = TRUE,
                                                    options = NULL),
                                     
                                     selectInput("Factor_of_analysis_Variance","Choose factor of analysis",choices=""),
                                     selectInput("select_outliers_to_remove_Variance","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                                     checkboxInput("normalize_per_input_resistance_Variance","Normalize current per input resistance"),
                                     
                                     
                        ),
                        mainPanel(width = 10,
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
                        sidebarPanel(width=2,
                                     selectInput('File_to_select_Distrib','Select file to study',choices=''),
                                     selectInput("Feature_to_analyse_Distrib",'Choose feature to analyse',choices=""),
                                     selectizeInput('Subset_population_Distrib','Select group', choices="", selected = "", multiple = TRUE,
                                                    options = NULL),
                                     
                                     selectInput("Factor_of_analysis_Distrib","Choose factor of analysis",choices=""),
                                     selectInput("select_outliers_to_remove_Distrib","Select outliers to remove",choices=c('None','Outliers (Q1/Q3 ± 1.5*IQ)','Extreme outliers (Q1/Q3 ± 3*IQ)'),selected='None'),
                                     checkboxInput("normalize_per_input_resistance_Distrib","Normalize current per input resistance"),
                                     numericInput('distribution_bin_width','Width of bins:',value = 1.,min=.001,max=100),
                                     checkboxInput('Distrib_custom_x_range','Use custom x-axis range'),
                                     numericInput('Minimum_x_limit','Select minimum x',value=0),
                                     numericInput('Maximum_x_limit','Select maximum x',value=1),
                                     checkboxInput('show_stats','Show stats')
                                     
                                     
                                     
                        ),
                        mainPanel(
                           tabsetPanel(
                             tabPanel(title="General time evolution",
                                     
                                     textOutput("textOutput"),
                                    plotOutput('Skew_Gauss'),
                                    tableOutput("distrib_fit_parameters"),
                                    tableOutput("distrib_stats")
                             )



                           )#Tabpanel
                          #tabsetpanel
                          
                        ),#mainpanel
                      )#sidebarLayout
             ),#tabpanel
             
             
  )
  ##3
  
)





source_python ('/Users/julienballbe/My_Work/Data_Analysis/python_ref.py')

server <- function(session,input, output) {
  
  myenv=new.env()
  #reticulate::source_python('/Users/julienballbe/My_Work/My_Librairies/pytho_function_for_R.py')
# Choose input directory for population class files
  shinyDirChoose(
    input,
    'folder',
    roots = getVolumes()(),
    filetypes = c('','csv')
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$folder)
  
  
  output$file_path <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$folder
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 #unlist(dir()$root)
                 global$datapath <-
                   file.path('','Volumes',unlist(dir()$root),paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
                 
                 file_list=list.files(path= global$datapath, pattern=".csv", all.files=FALSE,
                                      full.names=FALSE)
                 
                 updateCheckboxGroupButtons(session,"Population_files","Select_population_files",choices=file_list)
                 
               })
  
#Choose inpput directory for cell files
  
  shinyDirChoose(
    input,
    'Cell_file_dir',
    roots = getVolumes()(),
    filetypes = c('','h5')
  )
  
  global <- reactiveValues(datapath_cell_files = getwd())
  
  dir_cell_file <- reactive(input$Cell_file_dir)
  
  output$cell_file_path <- renderText({
    cell_file_dir=global$datapath_cell_files
    cell_file_dir
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$Cell_file_dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir_cell_file())) return()
                 home <- normalizePath("~")
                 #unlist(dir()$root)
                 global$datapath_cell_files <-
                   file.path('','Volumes',unlist(dir_cell_file()$root),paste(unlist(dir_cell_file()$path[-1]), collapse = .Platform$file.sep))
                 
                 cell_file_list=list.files(path= global$datapath_cell_files, pattern=".h5", all.files=FALSE,
                                      full.names=FALSE)
                 
                 
                 print(cell_file_list[1:5])
                 #updateCheckboxGroupButtons(session,"Population_files","Select_population_files",choices=cell_file_list)
                 
               })
  
  
  import_csv_files <- eventReactive(input$import_files,{
    req(input$Population_files)
    print(input$Population_files)
    current_cell_file <- load_population_csv_file(global$datapath,input$Population_files)
    
    
    # 
    # updateSelectInput(session,"Factor_of_analysis_data_repartition","Choose factor of analysis",choices=category_list)
    # updateSelectInput(session,"Feature_to_analyse_data_repartition","Choose factor of analysis",choices=feature_list)
    # 
    return (current_cell_file)
    
  })
  

  output$mytest <- renderDataTable ({
    req(input$Population_files)
    req(input$import_files)
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)

    
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



    # factor_RM=input$Factor_of_analysis_RM
    #
    # population_class[,factor_RM] <- as.factor(population_class[,factor_RM])
    #
    #
    # group_list=levels(population_class[,factor_RM])
    #
    # updateSelectizeInput(session,'Subset_population_RM','Select group',choices=group_list,selected=group_list)
    #
    category_list_faceting=category_list[category_list!=input$Factor_of_analysis_data_repartition]

    updateSelectInput(session,"Factor_to_facet"," Facet plot per",choices=category_list_faceting)
    density_category=append(category_list,'Response_Duration')
    
  
    
    

    
    updateSelectInput(session,'Factor_for_density_plot','Choose density factor',choices=density_category)


    updateSelectInput(session,"File_to_select_Var","Select file to analyse",choices = file_list)
    updateSelectInput(session,"File_to_select_Distrib","Select file to analyse",choices = file_list)
    print(paste0('fefe',input$Feature_to_analyse_data_repartition))
    View(file_import)
    # full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,'Database','Database')
    # 
    # minimum_value=min(full_data_frame[,as.character(input$Feature_to_analyse_data_repartition)])
    # maximum_value=max(full_data_frame[,as.character(input$Feature_to_analyse_data_repartition)])
    # 
    # updateSliderInput(session, "Slider_density_plot_xlim", value = c(minimum_value,maximum_value),
    #                   min = minimum_value, max =maximum_value, step = 0.5)

    population_class


  })

  output$descriptive_data_table <- renderDataTable({
    
    req(input$import_files)
    
    file_import <- import_csv_files()
    
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
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
  
  output$Data_repartition <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_Distrib]=full_data_frame[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_Distrib]=full_data_frame[,input$Feature_to_analyse_Distrib]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    if (input$normalise_descriptive_plot == TRUE){
      geombar_position='fill'
    }
    else{
      geombar_position='stack'
    }
    
    
    
    repartition_plot=ggplot(full_data_frame,mapping=aes_string("Response_Duration",fill=input$Factor_of_analysis_data_repartition))
                                                                                                                                             
    if (input$Facet_decriptive_plot==TRUE){
      repartition_plot=repartition_plot+facet_wrap(~get(input$Factor_to_facet))+
        geom_bar(position=geombar_position)+
        ggtitle(paste0('Number of ',as.character(input$Feature_to_analyse_data_repartition) , ' observations at different Response Duration per ',as.character(input$Factor_to_facet)))
    }
    else{
      repartition_plot=repartition_plot+geom_bar(position=geombar_position)+
        ggtitle(paste0('Number of ',as.character(input$Feature_to_analyse_data_repartition) , ' observations at different Response Duration'))
    }
    
    repartition_plot=repartition_plot+theme(axis.text.x = element_text( angle = 45))
      
      
    #repartition_plot <- ggplotly(repartition_plot,dynamicTicks=TRUE)    
    repartition_plot
    
  })
  
  output$data_density <- renderPlotly({
  
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_Distrib]
    if (input$normalize_per_input_resistance_Data_overview){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_Distrib]=full_data_frame[,input$Feature_to_analyse_Distrib]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_Distrib]=full_data_frame[,input$Feature_to_analyse_Distrib]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    histogram_plot = ggplot(full_data_frame, aes_string(x = input$Feature_to_analyse_data_repartition, fill = input$Factor_of_analysis_data_repartition))+
      geom_histogram( position='identity',alpha = 0.6, binwidth  = input$bins_width_histogram,center=input$bins_width_histogram/2,)
    
    
    # density_plot=ggplot(full_data_frame, aes_string(x = input$Feature_to_analyse_data_repartition, y = input$Factor_for_density_plot)) +
    #   geom_density_ridges(aes_string(fill = input$Factor_for_density_plot))
    # 
    if (input$Facet_decriptive_plot==TRUE){
      if (input$descriptive_show_mean == TRUE){
        mu <- ddply(full_data_frame,c( input$Factor_of_analysis_data_repartition,'Response_Duration',input$Factor_to_facet), summarise, grp.mean=mean(get(input$Feature_to_analyse_data_repartition)))
        histogram_plot=histogram_plot+geom_vline(data=mu, aes(xintercept=grp.mean, color=get(input$Factor_of_analysis_data_repartition)),linetype="dashed")
        
      }
      histogram_plot=histogram_plot+facet_grid(Response_Duration~get(input$Factor_to_facet))
    }
    else{
      if (input$descriptive_show_mean == TRUE){
        mu <- ddply(full_data_frame,c( input$Factor_of_analysis_data_repartition,'Response_Duration'), summarise, grp.mean=mean(get(input$Feature_to_analyse_data_repartition)))
        histogram_plot=histogram_plot+geom_vline(data=mu, aes(xintercept=grp.mean, color=get(input$Factor_of_analysis_data_repartition)),linetype="dashed")
        
      }
      histogram_plot=histogram_plot+facet_grid(Response_Duration~.)
    }
    
    
    #histogram_plot=histogram_plot+xlim(input$Slider_density_plot_xlim[1],input$Slider_density_plot_xlim[2])
    histogram_plot = ggplotly(histogram_plot,dynamicTicks = T)
    histogram_plot

  })
  
  output$varibility_plot_time_response <- renderPlotly({
    
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
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
          p <- ggplot(full_data_frame, aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=filling,text =  "Cell_id"))+
            geom_boxplot(aes(text=full_data_frame[,'Cell_id']))+
            
            labs(x='Response_Duration',y=as.character(input$Feature_to_analyse_data_repartition))+
            theme(axis.text.x=element_text(angle=45, vjust=0.4,hjust=1),legend.position="bottom")
          
          p <- ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response")%>%layout(boxmode = "group")
          
        }
        
        else{
          p <- ggplot(full_data_frame, aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=input$Factor_of_analysis_data_repartition,text =  "Cell_id"))+
            geom_jitter(position=position_jitterdodge())+geom_point(alpha=.2)
          
          labs(x='Response_Duration',y=as.character(input$Feature_to_analyse_data_repartition))+scale_fill_discrete(name=as.character(input$Factor_of_analysis_data_repartition))+
            theme(axis.text.x=element_text(angle=45, vjust=0.4,hjust=1),legend.position="bottom")
          
         p <-  ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response") %>% layout(boxmode = "group")
        }
      
      
      
    }
    
    if (input$variability_plot == 'jitterplot'){
      
      
      
      p=ggplot(full_data_frame, aes_string(x="Response_Duration", y=input$Feature_to_analyse_data_repartition,colour=colouring,text='Cell_id'))+
        geom_point(alpha=.4,width = .7)
      p <-  ggplotly(p,dynamicTicks = T,source = "varibility_plot_time_response")
    }
    
    p
  })
  
  output$click <- renderPrint({
    req(input$import_files)
    file_import <- import_csv_files()
    relayout <- event_data("plotly_click",source='varibility_plot_time_response')
    
    if (input$group_specific == TRUE){
      
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)

      Factor_list=levels(full_data_frame[,input$Factor_of_analysis_data_repartition])

      point_factor=Factor_list[relayout$curveNumber+1]

      subset_df=full_data_frame[full_data_frame[,input$Factor_of_analysis_data_repartition] %in% point_factor,]

      index=relayout$pointNumber+1

      cell_id=subset_df[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }
    
    else{
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet)

      index=relayout$pointNumber+1

      cell_id=full_data_frame[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }
    
    print(cell_id)

    })

  cell_id_to_analyse <- reactive({

    file_import <- import_csv_files()
    relayout <- event_data("plotly_click",source='varibility_plot_time_response')
    if (input$group_specific == TRUE){
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet)
      
      Factor_list=levels(full_data_frame[,input$Factor_of_analysis_data_repartition])
      
      point_factor=Factor_list[relayout$curveNumber+1]
      
      subset_df=full_data_frame[full_data_frame[,input$Factor_of_analysis_data_repartition] %in% point_factor,]
      
      index=relayout$pointNumber+1
      
      cell_id=subset_df[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }
    
    else{
      full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet)
      
      index=relayout$pointNumber+1
      
      cell_id=full_data_frame[index,"Cell_id"]
      global$cell_id_to_analyse=cell_id
    }

    return(cell_id)
  })

  observeEvent(input$Factor_of_analysis_RM,{
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    factor_RM=input$Factor_of_analysis_RM
    
    population_class[,factor_RM] <- as.factor(population_class[,factor_RM])
    
    
    group_list=levels(population_class[,factor_RM])
    
    updateSelectizeInput(session,'Subset_population_RM','Select group',choices=group_list,selected=group_list)
  })
  
  observeEvent(input$Factor_of_analysis_Variance,{
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    factor_Variance=input$Factor_of_analysis_Variance
    
    population_class[,factor_Variance] <- as.factor(population_class[,factor_Variance])
    
    
    group_list=levels(population_class[,factor_Variance])
    
    updateSelectizeInput(session,'Subset_population_Variance','Select group ',choices=group_list,selected=group_list)
    
  })

  
  observeEvent(input$Factor_of_analysis_Distrib,{
    file_import <- import_csv_files()
    population_class = data.frame(file_import$Population_Class)
    factor_Distrib=input$Factor_of_analysis_Distrib
   
    population_class[,factor_Distrib] <- as.factor(population_class[,factor_Distrib])
    
    
    group_list=levels(population_class[,factor_Distrib])
    print(paste0('coucou',factor_Distrib))
    mytest=population_class
    View(mytest)
    print(paste0('hyhy',group_list))
    
    updateSelectizeInput(session,'Subset_population_Distrib','Select group ',choices=group_list,selected=group_list)
    print(paste0('mpmp',input$Subset_population_Distrib))
  })
  
  
  get_cell_file <- eventReactive(input$Change_cell,{
    req(input$Cell_id_to_analyse)

    current_cell_file <- load_h5_file(file=paste0(global$datapath,'/',cell_id_to_analyse()))

    sweep_info_table=current_cell_file$Sweep_info_table
    sweep_list=sweep_info_table$Sweep


    updateSelectInput(session,"Sweep_to_analyse","Sweep to analysis",choices=sweep_list)
    print('file_loaded')

    return (current_cell_file)

  })

 
  
  output$I_O_feature_plot <- renderPlotly({

    cell_tables_list=load_h5_file(file=paste0(paste0(global$datapath_cell_files,'/Cell_',cell_id_to_analyse(),'_data_file.h5')))

    sweep_info_table=cell_tables_list$Sweep_info_table
    Cell_feature_table=cell_tables_list$Cell_feature_table
    rownames(Cell_feature_table) <- Cell_feature_table$Response_time_ms
    time_list <- unique(Cell_feature_table$Response_time_ms)


    Full_Sweep_metadata=cell_tables_list$Sweep_info_table
    sweep_list=Full_Sweep_metadata$Sweep

    Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                                  Stim_amp_pA = numeric(),
                                  Frequency_Hz = numeric(),
                                  Response_time = character(),
                                  stringsAsFactors = FALSE)

    if (time_list[1] == 0){
      time_list=c(500.0)
      fit_IO=FALSE

    }
    else {fit_IO=TRUE}

    for (current_time in time_list){

      sub_cell_feature_table=Cell_feature_table[which(Cell_feature_table$Response_time_ms == current_time),]
      for (current_sweep in sweep_list){
        stim_start=sweep_info_table[as.character(current_sweep),"Stim_start_s"]
        stim_end=sweep_info_table[as.character(current_sweep),"Stim_end_s"]
        stim_amp=sweep_info_table[as.character(current_sweep),"Stim_amp_pA"]
        SF_table=data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        spike_table=SF_table[which(SF_table$Feature == 'Upstroke'),]
        spike_table=spike_table[which(spike_table$Time_s<=(stim_start+current_time*1e-3)),]
        current_frequency=dim(spike_table)[1]/(current_time*1e-3)
        time=paste0(as.character(current_time),'ms')
        new_line=data.frame(list(current_sweep,stim_amp,current_frequency,time))
        colnames(new_line) <- c('Sweep','Stim_amp_pA',"Frequency_Hz","Response_time")
        Stim_freq_table=rbind(Stim_freq_table,new_line)


      }
    }

    Stim_freq_table$Stim_amp_pA=as.numeric(Stim_freq_table$Stim_amp_pA)
    Stim_freq_table$Frequency_Hz=as.numeric(Stim_freq_table$Frequency_Hz)
    Stim_freq_table$Response_time=factor(Stim_freq_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
    if (fit_IO==FALSE){

      IO_plot=ggplot(Stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz),color='grey')+geom_point(aes(text=Sweep))
      IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : No computation of I/O relationship"))
    }

    else{
      IO_plot=ggplot(Stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_time))+geom_point(aes(text=Sweep))
      # Create Hill fit traces  --> fit trace

      fit_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                              Stim_amp_pA = numeric(),
                              Frequency_Hz = numeric(),
                              Response_time = character(),
                              stringsAsFactors = FALSE)


      cell_fit_table=cell_tables_list$Cell_fit_table

      sub_cell_fit_table = cell_fit_table[which(cell_fit_table$I_O_obs == "Hill-Sigmoid" | (cell_fit_table$I_O_obs == "Hill")),]

      #sub_cell_fit_table=cell_fit_table[which(cell_fit_table$I_O_obs == "--"),]
      if (dim(sub_cell_fit_table)[1]!=0){

        sub_time_list=sub_cell_fit_table$Response_time_ms
        stim_array=Full_Sweep_metadata$Stim_amp_pA
        stim_array=seq(min(stim_array,na.rm = TRUE),max(stim_array,na.rm = TRUE),.1)
        stim_array_shifted=stim_array+abs(min(stim_array))
        min_x=min(Cell_feature_table$Threshold, na.rm = TRUE )-10
        max_x=max(Full_Sweep_metadata$Stim_amp_pA, na.rm = TRUE )+10

        IO_stim_array=seq(min_x,max_x,.1)
        IO_table <- data.frame(Stim_amp_pA = numeric(),
                               Frequency_Hz = numeric(),
                               Response_time = character(),
                               stringsAsFactors = FALSE)
        Sat_table=data.frame(Stim_amp_pA = numeric(),
                             Frequency_Hz = numeric(),
                             Response_time = character(),
                             stringsAsFactors = FALSE)

        sat_table_line=1

        for (current_time in sub_time_list ){

          I_O_obs = sub_cell_fit_table[as.character(current_time),"I_O_obs"]
          Hill_amplitude=sub_cell_fit_table[as.character(current_time),"Hill_amplitude"]
          Hill_coef=sub_cell_fit_table[as.character(current_time),"Hill_coef"]
          Hill_Half_cst=sub_cell_fit_table[as.character(current_time),"Hill_Half_cst"]
          Hill_x0 = sub_cell_fit_table[as.character(current_time),"Hill_x0"]
          x0=sub_cell_fit_table[as.character(current_time),"Sigmoid_x0"]
          sigma=sub_cell_fit_table[as.character(current_time),"Sigmoid_sigma"]
          freq_array = c(rep(0,length(stim_array_shifted)))

          I_O_obs = sub_cell_fit_table[as.character(current_time),"I_O_obs"]
          Hill_amplitude=sub_cell_fit_table[as.character(current_time),"Hill_amplitude"]
          Hill_coef=sub_cell_fit_table[as.character(current_time),"Hill_coef"]
          Hill_Half_cst=sub_cell_fit_table[as.character(current_time),"Hill_Half_cst"]
          Hill_x0 = sub_cell_fit_table[as.character(current_time),"Hill_x0"]
          x0=sub_cell_fit_table[as.character(current_time),"Sigmoid_x0"]
          sigma=sub_cell_fit_table[as.character(current_time),"Sigmoid_sigma"]
          freq_array = c(rep(0,length(stim_array_shifted)))

          if (I_O_obs == 'Hill-Sigmoid'){
            if (Hill_x0<min(stim_array_shifted)){
              freq_array =Hill_amplitude* (((stim_array_shifted-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array_shifted-Hill_x0)**(Hill_coef)))) *  (1-(1/(1+exp((stim_array_shifted-x0)/sigma))))
            }
            else{
              x0_index=which(stim_array_shifted < Hill_x0)[length(which(stim_array_shifted < Hill_x0))]
              freq_array [x0_index:length(freq_array)] =Hill_amplitude* (((stim_array_shifted[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array_shifted[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef)))) *  (1-(1/(1+exp((stim_array_shifted[x0_index:length(freq_array)]-x0)/sigma))))
            }


          }

          if (I_O_obs == 'Hill'){

            if (Hill_x0<min(stim_array_shifted)){
              freq_array =Hill_amplitude* (((stim_array_shifted-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array_shifted-Hill_x0)**(Hill_coef))))
            }
            else{
              x0_index=which(stim_array_shifted < Hill_x0)[length(which(stim_array_shifted < Hill_x0))]

              freq_array [x0_index:length(freq_array)] =Hill_amplitude* (((stim_array_shifted[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array_shifted[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))))
            }
          }

          new_table=data.frame(cbind(stim_array,freq_array))
          new_table['Response_time']=paste0(as.character(current_time),'ms')
          colnames(new_table) <- c('Stim_amp_pA','Frequency_Hz','Response_time')
          fit_table=rbind(fit_table,new_table)

          Threshold=Cell_feature_table[as.character(current_time),"Threshold"]
          Gain=Cell_feature_table[as.character(current_time),"Gain"]


          Intercept=-Gain*Threshold
          IO_freq_array=Gain*IO_stim_array+Intercept
          current_IO_table=data.frame(cbind(IO_stim_array,IO_freq_array))
          current_IO_table['Response_time']=paste0(as.character(current_time),'ms')


          if (is.null(Cell_feature_table[as.character(current_time),"Saturation_Frequency"]) == FALSE){
            current_sat_table=data.frame(Stim_amp_pA = numeric(),
                                         Frequency_Hz = numeric(),
                                         Response_time = character())

            Saturation_Frequency=as.numeric(Cell_feature_table[as.character(current_time),"Saturation_Frequency"])
            Saturation_Stimulus=as.numeric(Cell_feature_table[as.character(current_time),"Saturation_Stimulus"])

            current_sat_table <- c(Saturation_Stimulus,Saturation_Frequency, paste0(as.character(current_time),'ms'))

            Sat_table[sat_table_line,] <- current_sat_table
            sat_table_line=sat_table_line+1


          }

          IO_table=rbind(IO_table,current_IO_table)


        }


        colnames(fit_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_time')
        fit_table$Response_time=as.factor(fit_table$Response_time)
        fit_table$Response_time=factor(fit_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))

        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time))

        colnames(IO_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_time')
        IO_table$Response_time=as.factor(IO_table$Response_time)
        IO_table$Response_time=factor(IO_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time),linetype='dashed')
        colnames(Sat_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_time')
        Sat_table$Stim_amp_pA=as.numeric(Sat_table$Stim_amp_pA)
        Sat_table$Frequency_Hz=as.numeric(Sat_table$Frequency_Hz)
        Sat_table$Response_time=as.factor(Sat_table$Response_time)
        Sat_table$Response_time=factor(Sat_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))

        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time),shape=3)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        my_orange = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I excluded the two lighter hues
        IO_plot=IO_plot + scale_colour_manual(values=my_orange)
      }
    }
    
    IO_plot=IO_plot+ theme(text = element_text(size = 10,face="bold"),axis.text = element_text(size = 12)) #All font sizes
    IO_plotly <- ggplotly(IO_plot,dynamicTicks=TRUE)

    IO_plotly

  })

  cell_modal <- function(current_cell_file,failed=FALSE){
    modalDialog(
      plotlyOutput('I_O_feature_plot'),

      footer = tagList(
        modalButton("Cancel"),
      ), size="l"
    )
  }

  observeEvent(event_data("plotly_click",source='varibility_plot_time_response'),{
    print(paste0(global$datapath_cell_files,'/Cell_',cell_id_to_analyse(),'_data_file.h5'))
    current_cell_file <- load_h5_file(file=paste0(global$datapath_cell_files,'/Cell_',cell_id_to_analyse(),'_data_file.h5'))

    showModal(cell_modal(current_cell_file))
  })
  
  
  output$summary_statistics <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
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
    summary_stats=full_data_frame %>%
      group_by_(input$Factor_of_analysis_data_repartition, "Response_Duration") %>%
      get_summary_stats(input$Feature_to_analyse_data_repartition, type = "mean_sd")
    
    print(summary_stats)
  })
  
  output$outliers_plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
    
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
      outlier_plot=ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+geom_boxplot(outlier.shape = NA )+facet_grid( ~ Response_Duration)
      outlier_plot=outlier_plot+ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)
      
      }
    else{
      if (input$select_outliers_to_remove == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        outlier_plot=ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.extreme',text='Cell_id'))+facet_grid( ~ Response_Duration)+ scale_color_manual(values = c("darkblue", "red"))
        outlier_plot=outlier_plot+ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
        }
      
      if (input$select_outliers_to_remove == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        outlier_plot=ggplot(outlier_df,aes_string( x = input$Factor_of_analysis_data_repartition, y = input$Feature_to_analyse_data_repartition))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.extreme',text='Cell_id'))+facet_grid( ~ Response_Duration)+ scale_color_manual(values = c("darkblue", "red"))
        outlier_plot=outlier_plot+ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
        }
      
    }
    outlier_plotly
    
  })
  
  output$outlier_count <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame <- create_full_df(file_import,input$Feature_to_analyse_data_repartition,input$Factor_of_analysis_data_repartition,input$Factor_to_facet,keep_na=FALSE)
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
  
 ### REPEATED MEASURE ANALYSIS
  
  output$Original_RM_Data_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
   
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Oulier_df")
    
    if (input$select_outliers_to_remove == 'None'){
      outlier_plot=ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+geom_boxplot(outlier.shape = NA )
      outlier_plot=outlier_plot+ggtitle('Original Data')
      outlier_plotly=ggplotly(outlier_plot)
      
    }
    else{
      if (input$select_outliers_to_remove == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
        outlier_plot=ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.outlier',text='Cell_id'))+ scale_color_manual(values = c("blue", "red"))
        outlier_plot=outlier_plot+ggtitle('Original Data')+ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
      if (input$select_outliers_to_remove == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
        outlier_plot=ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.extreme',text='Cell_id'))+ scale_color_manual(values = c("blue", "red"))
        outlier_plot=outlier_plot+ggtitle('Original Data')+ylab(current_unit)
        outlier_plotly=ggplotly(outlier_plot)
        
      }
      
    }
    
    outlier_plotly
    })
  
  output$RM_Data_without_outliers_Plot <- renderPlotly({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
    
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_RM]=full_data_frame[,input$Feature_to_analyse_RM]*(1/(full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        full_data_frame[,input$Feature_to_analyse_RM]=full_data_frame[,input$Feature_to_analyse_RM]*((full_data_frame[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "DF_without_outliers")
    without_outlier_plot=ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+geom_boxplot(outlier.shape = NA )
    without_outlier_plot=without_outlier_plot+ggtitle('Analysed Data')
    without_outlier_plotly=ggplotly(without_outlier_plot)
    
    without_outlier_plotly
    
  })
  
  output$category_count_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    category_count_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Categories_count")
    category_count_table
    
  },digits = -3)
  
  output$Normality_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    normality_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Normality_table")
    normality_table
    
  },digits = -3)
  
  output$Variance_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Variance_test")
    Variance_test_table
    
  },digits = -3)
  
  
  output$PWC_test_table <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
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
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "PWC_without_position")
    
    
    PWC_test_table
    
  },digits = -3)
  
  
  output$PWC_test_Plot <- renderPlot({
    req(input$import_files)
    file_import <- import_csv_files()
    full_data_frame_Anova <- create_full_df_RM_ANOVA(file_import,
                                                     input$Feature_to_analyse_RM,
                                                     input$Factor_of_analysis_RM,
                                                     'Response_Duration',
                                                     First_factor_subset=input$Subset_population_RM,
                                                     keep_na=FALSE)
    Unit_list=file_import$Unit_File
    current_unit=Unit_list[,input$Feature_to_analyse_RM]
    
    if (input$normalize_per_input_resistance_RM){
      
      if (grepl("/pA",current_unit)==TRUE){
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*(1/(full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
      
      else if (grepl("pA",current_unit)==TRUE){
        View(full_data_frame_Anova)
        
        full_data_frame_Anova[,input$Feature_to_analyse_RM]=full_data_frame_Anova[,input$Feature_to_analyse_RM]*((full_data_frame_Anova[,'Input_Resistance_MOhms']*1e-3))
        current_unit=chartr('pA','mV',current_unit)
      }
    }
    factor=input$Factor_of_analysis_RM
    Ind_Var='Response_Duration'
    value=as.character(input$Feature_to_analyse_RM)
    if ('Linear_Values' %in% names(file_import)){
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor, -"Input_Resistance_MOhms") %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    else{
      full_data_frame_Anova=full_data_frame_Anova%>%gather(key = "Response_Duration", value =value , -"Cell_id", -factor) %>%
        convert_as_factor(Cell_id, Response_Duration)
    }
    colnames(full_data_frame_Anova)[colnames(full_data_frame_Anova) == "value"] =value
    
    full_data_frame_Anova$Response_Duration <- factor(full_data_frame_Anova$Response_Duration,levels=mixedsort(levels(full_data_frame_Anova$Response_Duration)))
    original_dataframe=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "DF_without_outliers")
    Variance_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "Variance_test_original_table")
    PWC_test_table=perform_repeated_measure_one_way_ANOVA(full_data_frame_Anova,feature_col = value,factor = Ind_Var,remove_outliers = input$select_outliers_to_remove,what_to_return = "PWC")
    
    PWC_plot=ggplot(original_dataframe,aes_string( x = Ind_Var, y = input$Feature_to_analyse_RM))+
      geom_boxplot(outlier.shape =NA )+geom_jitter(aes(alpha=.8),width = 0.25)+
      labs(
        subtitle = get_test_label(Variance_test_table, detailed = TRUE)
      )+ylab(current_unit)
    
    
    if(nrow(PWC_test_table)!=0){
    PWC_plot=PWC_plot+stat_pvalue_manual(PWC_test_table, tip.length = 0, hide.ns = TRUE) }
    
    PWC_plot
    
  })
 

  
  #VARIANCE


output$Original_Variance_Data_Plot <- renderPlotly({
  req(input$import_files)
  file_import <- import_csv_files()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                   input$Feature_to_analyse_Variance,
                                                   input$Factor_of_analysis_Variance,
                                                   input$File_to_select_Var,
                                                   Factor_Second=NULL,
                                                   First_factor_subset=input$Subset_population_Variance,
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
    outlier_plot=ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+geom_boxplot(outlier.shape = NA )
    outlier_plot=outlier_plot+ggtitle('Original Data')+ylab(current_unit)
    outlier_plotly=ggplotly(outlier_plot)

  }
  else{
    if (input$select_outliers_to_remove_Variance == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
      outlier_plot=ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.outlier',text='Cell_id'))+ scale_color_manual(values = c("blue", "red"))
      outlier_plot=outlier_plot+ggtitle('Original Data')+ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)

    }

    if (input$select_outliers_to_remove_Variance == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
      outlier_plot=ggplot(original_dataframe,aes_string( x = input$Factor_of_analysis_Variance, y = input$Feature_to_analyse_Variance))+geom_boxplot(outlier.shape =NA )+geom_point(aes_string(color='is.extreme',text='Cell_id'))+ scale_color_manual(values = c("blue", "red"))
      outlier_plot=outlier_plot+ggtitle('Original Data')+ylab(current_unit)
      outlier_plotly=ggplotly(outlier_plot)

    }

  }

  outlier_plotly
})


output$Variance_Data_without_outliers_Plot <- renderPlotly({
  req(input$import_files)
  file_import <- import_csv_files()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  without_outlier_plot=ggplot(original_dataframe,aes_string( x = factor, y =  input$Feature_to_analyse_Variance))+geom_boxplot(outlier.shape = NA )
  without_outlier_plot=without_outlier_plot+ggtitle('Analysed Data')+ylab(current_unit)
  without_outlier_plotly=ggplotly(without_outlier_plot)

  without_outlier_plotly

})


output$category_count_table_Variance <- renderTable({
  req(input$import_files)
  file_import <- import_csv_files()
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  full_data_frame_Anova <- create_full_df_ANOVA(file_import,
                                                input$Feature_to_analyse_Variance,
                                                input$Factor_of_analysis_Variance,
                                                input$File_to_select_Var,
                                                Factor_Second=NULL,
                                                First_factor_subset=input$Subset_population_Variance,
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
  PWC_plot=ggplot(original_dataframe,aes_string( x = factor, y = input$Feature_to_analyse_Variance))+
    geom_boxplot(outlier.shape =NA )+geom_jitter(aes(alpha=.8),width = 0.25)+
    labs(
      subtitle = get_test_label(Variance_test_table, detailed = TRUE)
    )+ylab(current_unit)


  if(nrow(PWC_test_table)!=0){

    PWC_plot=PWC_plot+stat_pvalue_manual(PWC_test_table, tip.length = 0, hide.ns = TRUE) }

  PWC_plot


})

  

  # observeEvent({
  #   #input$cohort_file
  #   input$Feature_to_analyse_Distrib
  #   input$Factor_of_analysis_Distrib
  #   input$File_to_select_Distrib
  #   input$Subset_population_Distrib
  # 
  # },{
  #   req(input$import_files)
  #   file_import <- import_csv_files()
  #   full_data_frame_distrib <- create_full_df_ANOVA(file_import,
  #                                                   input$Feature_to_analyse_Distrib,
  #                                                   input$Factor_of_analysis_Distrib,
  #                                                   input$File_to_select_Distrib,
  #                                                   Factor_Second=NULL,
  #                                                   First_factor_subset=input$Subset_population_Distrib,
  #                                                   keep_na=FALSE)
  #   print(paste0('rere',input$Subset_population_Distrib))
  #   factor=input$Factor_of_analysis_Distrib
  #   value=as.character(input$Feature_to_analyse_Distrib)
  #   original_dataframe_distrib=perform_ANOVA(full_data_frame_distrib,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Distrib,what_to_return =  "DF_without_removed_levels")
  #   distribution_array=array(original_dataframe_distrib[,value])
  # 
  # 
  #   maximum_width=round((max(distribution_array)-min(distribution_array))/10,2)
  # 
  #   minimum_width=.01
  #   original_value=round((minimum_width+maximum_width)/2,2)
  #   minimum_x=min(distribution_array)
  #   maximum_x=max(distribution_array)
  #   updateNumericInput(session,"Minimum_x_limit",value=minimum_x)
  #   updateNumericInput(session,"Maximum_x_limit",value=maximum_x)
  #   updateSliderInput(session,'distribution_bin_width','Select bin width ',min=minimum_width, max=maximum_width, value=original_value,step=.01)
  # })

  

#### DISTRIBUTION
  output$Skew_Gauss <- renderPlot({
    req(input$import_files)
    file_import <- import_csv_files()



    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$Factor_of_analysis_Distrib,
                                                  input$File_to_select_Distrib,
                                                  Factor_Second=NULL,
                                                  First_factor_subset=input$Subset_population_Distrib,
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
    my_test_second=full_data_frame_distrib
    View(my_test_second)
    original_dataframe_distrib=perform_ANOVA(full_data_frame_distrib,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Distrib,what_to_return =  "DF_without_removed_levels")

    distribution_array=array(original_dataframe_distrib[,value])

    bin_width=input$distribution_bin_width

    nb_bins=as.integer((max(distribution_array)-min(distribution_array))/bin_width)
    #nb_bins=input$distribution_bins
    bin_df=reticulate_data_distribution(distribution_array,nb_bins)
    # bin_df --> center of each bins
    # if (input$distrib_fit_function == "Skewed Gaussian"){
    #   
    # }
    parameters_df=fit_distribution(distribution_array,nb_bins)
    
    A=parameters_df[1,"A"]
    gamma=parameters_df[1,"gamma"]
    mu=parameters_df[1,"mu"]
    sigma=parameters_df[1,"sigma"]
    
    
    data_x_array=array(seq(min(distribution_array),max(distribution_array),.01))
    fitted_values=skewedgaussian(data_x_array, A, gamma, mu, sigma)
    fit_df=data.frame(cbind(data_x_array,fitted_values))
    colnames(fit_df) <- c(as.character(value),'Count')


    bin_edges <- bin_df

    bin_edges['Feature']=bin_edges['Feature']-(bin_edges[2,"Feature"]-bin_edges[1,"Feature"])/2
    #bin_edges['Feature']=bin_edges['Feature']-bin_edges[1,"Feature"]
    colnames(bin_edges) <- c(as.character(value),"Count")
    colnames(bin_df) <- c(as.character(value),'Count')



    distribution_plot=ggplot(original_dataframe_distrib,mapping=aes_string(x=as.character(value)))+
      geom_histogram(breaks=unlist(bin_edges[as.character(value)]),fill='darkgrey')+
      geom_point(bin_df,mapping=aes(x=unlist(bin_df[,as.character(value)]),y=unlist(bin_df[,"Count"])))


    distribution_plot=distribution_plot+ geom_line(fit_df,mapping=aes(x=unlist(fit_df[,as.character(value)]),y=unlist(fit_df[,"Count"])))

    distribution_plot=distribution_plot+labs(y='Nb_of_observation',x=as.character(value))+ggtitle(paste0(as.character(value),' distribution fit'))

    if (input$show_stats == TRUE){
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
        my_value=fit_df[which.min(abs(nb_to_match-fit_df[,value])),][,value]
        ymin=0
        ymax=fit_df[which.min(abs(nb_to_match-fit_df[,value])),][,'Count']

        stat_table[nrow(stat_table)+1,] <- c(my_value,my_value,ymin,ymax,my_stat)

        print(my_stat)



      }
      for (elt in seq(1,ncol(stat_table)-1)){
        stat_table[,elt]=as.numeric(unlist(stat_table[,elt]))
      }

      stat_table$Stat <- as.factor(stat_table$Stat)
      distribution_plot=distribution_plot+geom_segment(stat_table,mapping=aes_string(x = "x_min",
                                                                              y = "y_min",
                                                                              xend = "x_max",
                                                                              yend = "y_max",color="Stat"))


    }
    if (input$Distrib_custom_x_range == TRUE){
      distribution_plot=distribution_plot+xlim(input$Minimum_x_limit,input$Maximum_x_limit)+xlab(current_unit)
    }
    
    distribution_plot
  })


  output$distrib_fit_parameters <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()



    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$Factor_of_analysis_Distrib,
                                                  input$File_to_select_Distrib,
                                                  Factor_Second=NULL,
                                                  First_factor_subset=input$Subset_population_Distrib,
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

    value=input$Feature_to_analyse_Distrib
    original_dataframe_distrib=perform_ANOVA(full_data_frame_distrib,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Distrib,what_to_return =  "DF_without_removed_levels")

    distribution_array=array(original_dataframe_distrib[,value])


    bin_width=input$distribution_bin_width

    nb_bins=as.integer((max(distribution_array)-min(distribution_array))/bin_width)
    print(distribution_array)
    print(bin_width)
    print(nb_bins)
    bin_df=reticulate_data_distribution(distribution_array,nb_bins)

    parameters_df=fit_distribution(distribution_array,nb_bins)

    parameters_df
  })


  output$distrib_stats <- renderTable({
    req(input$import_files)
    file_import <- import_csv_files()



    full_data_frame_distrib <- create_full_df_ANOVA(file_import,
                                                  input$Feature_to_analyse_Distrib,
                                                  input$Factor_of_analysis_Distrib,
                                                  input$File_to_select_Distrib,
                                                  Factor_Second=NULL,
                                                  First_factor_subset=input$Subset_population_Distrib,
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

    original_dataframe_distrib=perform_ANOVA(full_data_frame_distrib,feature_col = value,factor = factor,remove_outliers = input$select_outliers_to_remove_Distrib,what_to_return =  "DF_without_removed_levels")


    distribution_array=array(original_dataframe_distrib[,value])

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
  
  
  
  
}
shinyApp(ui, server)
