#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

required_packages=c("rhdf5","plyr","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","gghighlight","ggpubr","shinyFiles",'gghalves','shinyWidgets',"gsignal","RColorBrewer","processx",'ggh4x')
install.packages(setdiff(required_packages,rownames(installed.packages())))
source(file="/Users/julienballbe/My_Work/Data_Analysis/Import_h5_file.R")
#source_python("/Users/julienballbe/My_Work/My_Librairies/read_pickle.py")

print ("All required packages installed")
for (package_name in required_packages){
  library(package_name,character.only =TRUE);
}
print("All required packages loaded")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Cell Viewer"),
    navbarPage(title = "Traces",
               tabPanel("Cell id",

    # Sidebar with a slider input for number of bins 
          sidebarLayout(
              sidebarPanel(
                shinyDirButton('folder', 'Select a folder', 'Please select a folder', FALSE),
                selectizeInput("Cell_id_to_analyse","Select_cell_id",choices="", selected = NULL, multiple = FALSE, options = NULL),
            
            actionButton("Change_cell", "Show Cell Trace")
            
        ),

        
        mainPanel(
          
          dataTableOutput('cell_list_csv')
         # plotlyOutput("Spike_feature_plot",height = 800)
          #plotlyOutput("traces_plot")
          
          
           
           
        )
        )
    ),
    tabPanel("Cell Information",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
               ),
               
               
               mainPanel(
                 textOutput('file_path'),
                 tableOutput('Metadata'),
                 dataTableOutput('Sweep_info'),
                 plotlyOutput('BE_plot'),
                 plotlyOutput('BE_boxplot'),
                 plotlyOutput('Time_cst_plot'),
                 plotlyOutput('Time_cst_boxplot')
                 # plotlyOutput("Spike_feature_plot",height = 800)
                 #plotlyOutput("traces_plot")
                 
                 
                 
                 
               )
             )
    ),
    
    tabPanel("Single sweep traces",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Sweep_to_analyse","Sweep to analysis",choices=""),
                 checkboxInput("Apply_BE_correction_single_sweep","Apply Bridge Error Correction"),
                 checkboxInput('superimpose','Superimpose_BE_corrected_trace'),
                 
                 checkboxGroupInput("Derivative_to_display","Select derivative to display",choices=c("First_derivative","Second_derivative"))
                 
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("Spike_feature_plot",height = 800),
                 actionButton('Save_spike_feature_plot',"Save plot")
                 
                 
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
                        
                        tableOutput('Stim_freq_interval_table')),
               tabPanel("Adaptation",
                        plotlyOutput("Adaptation_plot",height=800),
                        checkboxInput("Normalize_adapt_params","Normalize Adaptation Parameters"),
                        tableOutput('Adapt_table'),))
             )
    )),
    tabPanel("Raw traces",

             # Sidebar with a slider input for number of bins
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
)

# Define server logic required to draw a histogram
server <- function(session,input, output) {
  #volumes = getVolumes()() # this makes the directory at the base of your computer.
  shinyDirChoose(
    input,
    'folder',
    roots = getVolumes()(),
    filetypes = c('', 'h5')
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
                 
                 file_list=list.files(path= global$datapath, pattern=".h5", all.files=FALSE,
                            full.names=FALSE)
                 updateSelectizeInput(session,"Cell_id_to_analyse","Select_cell_id",choices=file_list)
               })
  
  output$cell_list_csv <- renderDataTable({
    cell_list_csv=read_csv('/Volumes/Work_Julien/Cell_Data_File/Cell_file_information.csv',)
    
    cell_list_csv
  })
  
  get_cell_file <- eventReactive(input$Change_cell,{
    req(input$Cell_id_to_analyse)
    
    current_cell_file <- load_h5_file(file=paste0(global$datapath,'/',input$Cell_id_to_analyse))
   
    sweep_info_table=current_cell_file$Sweep_info_table
    sweep_list=sweep_info_table$Sweep
    
   
    updateSelectInput(session,"Sweep_to_analyse","Sweep to analysis",choices=sweep_list)
    print('file_loaded')
    
    return (current_cell_file)
    
  })
  

    output$traces_plot <- renderPlotly({
        
      
     # json_file=get_json_file()
      cell_tables_list=get_cell_file()
      
      
      my_plot=ggplot()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      sweep_list=sweep_info_table$Sweep
      View(sweep_info_table)
      sampling_freq=mean(sweep_info_table$Sampling_Rate_Hz)
      
      full_trace_df=data.frame(cell_tables_list$Full_TPC[as.character(sweep_list[1])])
      print(sweep_list)
      
      
      
      
      
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
        print(current_sweep)
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
      print(head(third_new_df))
      full_trace_plot=ggplot()+geom_line(third_new_df,mapping=aes(x=Time_s,y=Value,group=Sweep,color=Sweep))+facet_grid(Measure ~ .,scales = "free")+ theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16))
    
      full_trace_plotly <- ggplotly(full_trace_plot,dynamicTicks=TRUE)      
      # full_trace_plot=ggplot(data=full_trace_df,aes(x=Time_s,y=Membrane_potential_mV,group=Sweep))+geom_line(aes(color=Sweep))
      # full_trace_plotly <- ggplotly(full_trace_plot,dynamicTicks=TRUE)
      
      full_trace_plotly
    })
    
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
        
        
        SF_plot=ggplot()+geom_line(full_table,mapping=aes(x=Time_s,y=Value,group=Trace,color=Trace),size=.96)+facet_grid(Measure ~ .,scales = "free",space = 'free')+scale_colour_manual(values=c(BE_corrected="red",Raw_trace="black"))
        SF_plot=SF_plot+force_panelsizes(rows=c(2,1))
        
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
          SF_plot=SF_plot+geom_point(fullSF_table,mapping = aes(x=Time_s,y=Value,fill=Feature),stroke=0,size=2)
        }
        
        SF_plotly <- ggplotly(SF_plot,dynamicTicks=TRUE)
        return (SF_plot)
      
    })
    
    output$Spike_feature_plot <- renderPlotly({
      
        SF_plot <- SF_plolty_test()
        SF_plot=SF_plot+force_panelsizes(rows=c(2,1))+ theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16))
        SF_plolty_test_result <- ggplotly(SF_plot,dynamicTicks=TRUE)
        SF_plolty_test_result
    })
    
    
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
        Adapt_plot=ggplot() + theme_void() + ggtitle('Not able to compute adaptation')
        
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
        
        print(rownames(Inst_freq_table))
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
      Adapt_plot=ggplot()+geom_point(full_inst_freq_table,mapping=aes(x=Interval,y=Inst_freq_WU,color=Stim_amp_pA))
      
      Adapt_plot=Adapt_plot+geom_point(full_median_table,mapping=aes(x=Interval,y=Inst_freq_WU,alpha=Nb_of_obs),shape="square",color='red')
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
        
        Adapt_plot=Adapt_plot+geom_line(Adapt_fit_table,mapping=aes(x=Interval,y=Inst_freq_WU))
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
    ####HEre
    
    
    output$I_O_feature_plot_time_based <- renderPlotly({
      cell_tables_list=get_cell_file()
      Sweep_QC_table=cell_tables_list$Sweep_QC_table
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Time_based')
      stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')], 
                             by=c("Sweep"))
      fit_table_list=get_fit_tables(cell_tables_list,"Time_based")
      
      scale_dict=c("TRUE" = "16","FALSE" = "1")
      
      fit_table = fit_table_list$fit_table
      IO_table = fit_table_list$IO_table
      Sat_table = fit_table_list$Sat_table
      View(Sat_table)
      
      if (input$for_saving_plot == TRUE){
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),size=.95)
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=25)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        my_blues = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I exluded the two lighter hues
        IO_plot=IO_plot + scale_colour_manual(values=my_blues)
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        
        IO_plot=IO_plot+ theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
      }
      else{
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration))
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=10)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        my_blues = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I exluded the two lighter hues
        IO_plot=IO_plot + scale_colour_manual(values=my_blues)
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        
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
      
      Sweep_QC_table=cell_tables_list$Sweep_QC_table
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Time_based')
      stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')], 
                              by=c("Sweep"))
      
      stim_freq_table
      
    })
    
    
    output$Gain_time_plot <- renderPlotly(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      Cell_feature_table = Cell_feature_table[which(Cell_feature_table$Response_type == 'Time_based' ),]
      lmgain = lm(Gain~Output_Duration, data = Cell_feature_table)
      
      gain_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Gain))+geom_point()
      gain_time_plot=gain_time_plot+geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')
      
      
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
      
      threshold_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Threshold))+geom_point()
      threshold_time_plot=threshold_time_plot+geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')
      
      
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
      
      sat_stim_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Stimulus))+geom_point()
      sat_stim_time_plot=sat_stim_time_plot+geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')
      
      
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
      
      sat_freq_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Frequency))+geom_point()
      sat_freq_time_plot=sat_freq_time_plot+geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')
      
      
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
    
    
    ##### Index
    
    output$I_O_feature_plot_index_based <- renderPlotly({
      cell_tables_list=get_cell_file()
      Sweep_QC_table=cell_tables_list$Sweep_QC_table
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Index_based')
      stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')], 
                              by=c("Sweep"))
      fit_table_list=get_fit_tables(cell_tables_list,"Index_based")
      
      scale_dict=c("TRUE" = "16","FALSE" = "1")
      
      fit_table = fit_table_list$fit_table
      IO_table = fit_table_list$IO_table
      Sat_table = fit_table_list$Sat_table
      
      
      if (input$for_saving_plot == TRUE){
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),size=.95)
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=25)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
       
        green_palet = colorRampPalette(brewer.pal(9, "Greens")[3:9])
        
        IO_plot=IO_plot + scale_colour_manual(values=green_palet(10))
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        
        IO_plot=IO_plot+ theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
      }
      else{
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration))
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=10)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        
        green_palet = colorRampPalette(brewer.pal(9, "Greens")[3:9])
        
        IO_plot=IO_plot + scale_colour_manual(values=green_palet(10))
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        
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
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Index_based')
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
      
      gain_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Gain))+geom_point()
      gain_time_plot=gain_time_plot+geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')
      
      
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
      
      threshold_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Threshold))+geom_point()
      threshold_time_plot=threshold_time_plot+geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')
      
      
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
      
      sat_stim_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Stimulus))+geom_point()
      sat_stim_time_plot=sat_stim_time_plot+geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')
      
      
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
      
      sat_freq_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Frequency))+geom_point()
      sat_freq_time_plot=sat_freq_time_plot+geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')
      
      
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
    
    ##### Interval
    
    output$I_O_feature_plot_interval_based <- renderPlotly({
      cell_tables_list=get_cell_file()
      Sweep_QC_table=cell_tables_list$Sweep_QC_table
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Interval_based')
      stim_freq_table<- merge(x=stim_freq_table,y=Sweep_QC_table[,c('Passed_QC','Sweep')], 
                              by=c("Sweep"))
      fit_table_list=get_fit_tables(cell_tables_list,"Interval_based")
      
      scale_dict=c("TRUE" = "16","FALSE" = "1")
      
      fit_table = fit_table_list$fit_table
      IO_table = fit_table_list$IO_table
      Sat_table = fit_table_list$Sat_table
      
      
      
     
      if (input$for_saving_plot == TRUE){
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),size=.95)
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=25)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        
        green_palet = colorRampPalette(brewer.pal(9, "Reds")[3:9])
        
        IO_plot=IO_plot + scale_colour_manual(values=green_palet(10))
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        IO_plot=IO_plot+ theme(text = element_text(size = 15,face="bold"),axis.text = element_text(size = 16)) #All font sizes
      }
      else{
        IO_plot=ggplot(stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_Duration))+geom_point(aes(text=Sweep))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration))
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),linetype='dashed')
        IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_Duration),shape=3,size=10)
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
        
        green_palet = colorRampPalette(brewer.pal(9, "Reds")[3:9])
        
        IO_plot=IO_plot + scale_colour_manual(values=green_palet(10))
        IO_plot=IO_plot + scale_shape_manual(values=scale_dict)
        
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
      stim_freq_table=get_stim_freq_table(cell_tables_list,'Interval_based')
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
      
      gain_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Gain))+geom_point()
      gain_time_plot=gain_time_plot+geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')
      
      
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
      
      threshold_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Threshold))+geom_point()
      threshold_time_plot=threshold_time_plot+geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')
      
      
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
      
      sat_stim_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Stimulus))+geom_point()
      sat_stim_time_plot=sat_stim_time_plot+geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')
      
      
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
      
      sat_freq_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Output_Duration,y=Saturation_Frequency))+geom_point()
      sat_freq_time_plot=sat_freq_time_plot+geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')
      
      
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
      BE_plot=ggplot(BE_table,mapping=aes(x=Trace_id,y=Bridge_Error_GOhms,color=Bridge_Error_extrapolated))+geom_line(aes(group=Protocol_id,color=Bridge_Error_extrapolated))+geom_point()
      BE_plotly <- ggplotly(BE_plot,dynamicTicks=TRUE)
      BE_plotly
    }))
    
    output$BE_boxplot <- renderPlotly(({
      cell_tables_list=get_cell_file()
      cell_sweep_info_table=cell_tables_list$Sweep_info_table
      BE_table <- cell_sweep_info_table[,c('Bridge_Error_GOhms','Bridge_Error_extrapolated','Sweep','Trace_id','Protocol_id')]
      levels=unique(BE_table$Protocol_id)
      BE_table$Protocol_id <- factor(BE_table$Protocol_id,levels=levels)
      BE_boxplot=ggplot(BE_table,mapping=aes(x=factor(0),y=Bridge_Error_GOhms))+geom_boxplot()+geom_jitter(color="black", size=0.9, alpha=0.9)
      BE_boxplot=BE_boxplot+theme(axis.title.x=element_blank(),
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
      TC_plot=ggplot(TC_table,mapping=aes(x=Trace_id,y=Time_constant_ms,color=Protocol_id))+geom_line(aes(group=Protocol_id))+geom_point()
      
      TC_plotly <- ggplotly(TC_plot,dynamicTicks=TRUE)
      TC_plotly
    }))
    
    
    output$Time_cst_boxplot <- renderPlotly(({
      cell_tables_list=get_cell_file()
      cell_sweep_info_table=cell_tables_list$Sweep_info_table
      TC_table <- cell_sweep_info_table[,c('Time_constant_ms','Sweep','Trace_id','Protocol_id')]
      levels=unique(TC_table$Protocol_id)
      TC_table$Protocol_id <- factor(TC_table$Protocol_id,levels=levels)
      TC_boxplot=ggplot(TC_table,mapping=aes(x=factor(0),y=Time_constant_ms))+geom_boxplot()+geom_jitter(color="black", size=0.9, alpha=0.9)
      TC_boxplot=TC_boxplot+theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank())
      TC_boxplotly <- ggplotly(TC_boxplot,dynamicTicks=TRUE)
      TC_boxplotly
    }))
    
 

}

# Run the application 
shinyApp(ui = ui, server = server)

