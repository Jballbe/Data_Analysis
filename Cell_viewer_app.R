#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

required_packages=c("rhdf5","plyr","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","gghighlight","ggpubr","shinyFiles",'gghalves','shinyWidgets','seewave',"gsignal")
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
                 plotlyOutput("Spike_feature_plot",height = 800)
                 
                 
               )
             )
    ),
    tabPanel("I/O",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(width=1
                 
               ),
               
               # Show a plot of the generated distribution ;;; ;;;;
               mainPanel(width = 11,
                 plotlyOutput("I_O_feature_plot",height = 800),
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
                 tableOutput('IO_table_fit'),
                 tableOutput('IO_table_feature'),
                 plotlyOutput("Adaptation_plot",height=800),
                 tableOutput('Adapt_table'),
                 tableOutput('Stim_freq_table')
                 
               )
             )
    ),
    tabPanel("Raw traces",

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(
                 checkboxInput("Apply_BE_correction","Apply Bridge Error Correction")

               ),


               mainPanel(
                 plotlyOutput("traces_plot"),

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
      sampling_freq=mean(sweep_info_table$Sampling_frequency_Hz)
      
      full_trace_df=data.frame(cell_tables_list$Full_TPC[as.character(sweep_list[1])])
      
      full_trace_df[,"Membrane_potential_mV"] <- bwfilter(full_trace_df[,"Membrane_potential_mV"], f=sampling_freq, n=2, to=5000)
      
      
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
        full_trace_df=rbind(full_trace_df,df)
        
      }
      
      for (elt in colnames(full_trace_df)){
        full_trace_df[,elt]=as.numeric(full_trace_df[,elt])
      }
      
      full_trace_df[,"Sweep"]=as.factor(full_trace_df[,"Sweep"])
      
      
      full_trace_plot=ggplot(data=full_trace_df,aes(x=Time_s,y=Membrane_potential_mV,group=Sweep))+geom_line(aes(color=Sweep))
      full_trace_plotly <- ggplotly(full_trace_plot,dynamicTicks=TRUE)
      
      full_trace_plotly
    })
    
    output$Spike_feature_plot <- renderPlotly({
      
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
      
      
      SF_plot=ggplot()+geom_line(full_table,mapping=aes(x=Time_s,y=Value,group=Trace,color=Trace))+facet_grid(Measure ~ .,scales = "free")+scale_colour_manual(values=c(BE_corrected="red",Raw_trace="black"))
      
      
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
      
      SF_plotly
      
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
                                     Response_time_ms=character())
        
        full_inst_freq_table=data.frame(Stim_amp_pA=numeric(),
                                        Interval=numeric(),
                                        Inst_freq_WU = numeric())
        
      }
      
      
      #Full_Sweep_metadata=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`))
      
      sweep_info_table=cell_tables_list$Sweep_info_table
     
      
      
      
      full_median_table=data.frame(Interval = numeric(),    # Create empty data frame
                                   Inst_freq_WU = numeric(),
                                   Nb_of_obs = numeric(),
                                   Response_time_ms=character())
      
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
      print('maximum_nb_interval=')
      print(maximum_nb_interval)
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
        View(Inst_freq_table)
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
          median_table['Response_time_ms']=paste0(as.character(0.500*1e3),'ms')
          
          full_median_table=rbind(full_median_table,median_table)
          full_inst_freq_table=rbind(full_inst_freq_table,Inst_freq_table)
        }
      }
      
      full_median_table$Response_time_ms=factor(full_median_table$Response_time_ms,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
      full_median_table$Nb_of_obs=factor(full_median_table$Nb_of_obs)
      Adapt_plot=ggplot()+geom_point(full_inst_freq_table,mapping=aes(x=Interval,y=Inst_freq_WU,color=Stim_amp_pA))
      print('coucou')
      Adapt_plot=Adapt_plot+geom_point(full_median_table,mapping=aes(x=Interval,y=Inst_freq_WU,alpha=Nb_of_obs),shape="square",color='red')
      fit_table=sub_cell_fit_table
      fit_table=subset(fit_table,Adaptation_obs =="--")
      Interval_seq = seq(1,max(Inst_freq_table$Interval))
      View(full_inst_freq_table)
      
      #Interval_seq=seq(1,length(seq(7,dim(Full_Sweep_metadata)[2])))
      if (dim(fit_table)[1] != 0){
        
        # A=unname(unlist(fit_table[1,"A"]))
        # Index_cst=unname(unlist(fit_table[1,"B"]))
        # C=unname(unlist(fit_table[1,"C"]))
        # 
        # A_norm=A/(A+C)
        # C_norm=C/(A+C)
        # 
        # inst_freq_array=A*exp(-Interval_seq/Index_cst)+C
        
        # Adapt_fit_table=data.frame(cbind(Interval_seq,inst_freq_array))
        # Adapt_fit_table['Response_time_ms']=paste0(as.character(unname(unlist(fit_table[1,"Response_time_ms"]))*1e3),'ms')
        # 
        A_norm=unname(unlist(fit_table["500","A"]))
        Index_cst=unname(unlist(fit_table["500","B"]))
        C_norm=unname(unlist(fit_table["500","C"]))
        print('A')
        print(A_norm)
        View(fit_table)
        # A_norm=A/(A+C)
        # C_norm=C/(A+C)
        # 
        A=(A_norm*C_norm-A_norm+C_norm)/(A_norm-2)
        C=(C_norm*A_norm-C_norm+A_norm)/(C_norm-2)
        inst_freq_array=A_norm*exp(-Interval_seq/Index_cst)+C_norm
        #inst_freq_array=A*exp(-Interval_seq/Index_cst)+C
        new_table=data.frame(cbind(Interval_seq,inst_freq_array))
        Adapt_fit_table=data.frame(cbind(Interval_seq,inst_freq_array))
        Adapt_fit_table['Response_time_ms']='500ms'
        new_table['Response_time_ms']='500ms'
        Adapt_fit_table=rbind(Adapt_fit_table,new_table)
        # 
        # for (time in 2:dim(fit_table)[1]){
        #   
        #   A_norm=unname(unlist(fit_table[time,"A"]))
        #   Index_cst=unname(unlist(fit_table[time,"B"]))
        #   C_norm=unname(unlist(fit_table[time,"C"]))
        #   # A_norm=A/(A+C)
        #   # C_norm=C/(A+C)
        #   # 
        #   inst_freq_array=A_norm*exp(-Interval_seq/Index_cst)+C_norm
        #   new_table=data.frame(cbind(Interval_seq,inst_freq_array))
        #   new_table['Response_time_ms']=paste0(as.character(unname(unlist(fit_table[time,"Response_time_ms"]))*1e3),'ms')
        #   Adapt_fit_table=rbind(Adapt_fit_table,new_table)
        #   
        #   
        #   
        # }
        print('fhufr')
        colnames(Adapt_fit_table) <- c("Interval","Inst_freq_WU",'Response_time_ms')
        Adapt_fit_table$Response_time_ms=as.factor(Adapt_fit_table$Response_time_ms)
        Adapt_fit_table$Response_time_ms=factor(Adapt_fit_table$Response_time_ms,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
        View(Adapt_fit_table)
        Adapt_plot=Adapt_plot+geom_line(Adapt_fit_table,mapping=aes(x=Interval,y=Inst_freq_WU))
      }
      
      Adapt_plot
      
      
    })
    ####HEre
    output$Adaptation_plot_old <- renderPlotly({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Adaptation_fit_table=cell_tables_list$Adaptation_fit_table
      cell_fit_table=cell_tables_list$Cell_fit_table
      sub_cell_fit_table=cell_fit_table[which(cell_fit_table$Adaptation_obs == '--'),]
      if (dim(sub_cell_fit_table)[1] == 0){
        Adapt_plot=ggplot() + theme_void() + ggtitle('Not able to compute adaptation')
        
      }
      
      else{
      
      time_list <- unique(Adaptation_fit_table$Response_time_ms)
      
      
      
      
     
      
      full_inst_freq_table=data.frame(Stim_amp_pA=numeric(),
                                      Interval=numeric(),
                                      Inst_freq_WU = numeric(),
                                      Max_nb_spike = numeric(),
                                      Sweep = numeric(),
                                      Response_time_ms = character())
      
        
      
      for(current_time in time_list){
      maximum_nb_interval = 0
      sub_adaptation_table=Adaptation_fit_table[which(Adaptation_fit_table$Response_time_ms == current_time),]
      sweep_list=sub_adaptation_table$Sweep
      sweep_list <-  unlist(unname(sweep_list))
      for (current_sweep in sweep_list){
        current_SF_table = data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        
        start_time=sweep_info_table[current_sweep,'Stim_start_s']
        current_SF_tableSpike_time=current_SF_table[which(current_SF_table$Feature == 'Upstroke' & current_SF_table$Time_s <=(start_time+current_time*1e-3)),]
        
        nb_spikes=dim(current_SF_tableSpike_time)[1]
        
      if (nb_spikes>maximum_nb_interval ){
        maximum_nb_interval=nb_spikes
      }
        
      }
      
      
      new_columns=as.character(seq(1,(maximum_nb_interval-1)))
      
      
      Inst_freq_table=data.frame(Stim_amp_pA=numeric(),
                                 Interval=numeric(),
                                 Inst_freq_WU = numeric(),
                                 Max_nb_spike = numeric(),
                                 Sweep = numeric(),
                                 Response_time_ms = character())
   
      
      for (current_sweep in sweep_list){
        current_SF_table = data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        start_time=sweep_info_table[as.character(current_sweep),'Stim_start_s']
        stim_amp=sweep_info_table[as.character(current_sweep),'Stim_amp_pA']
        current_SF_tableSpike_time=current_SF_table[which(current_SF_table$Feature == 'Upstroke' & current_SF_table$Time_s <=(start_time+current_time*1e-3)),]
        spike_times=unlist(unname(current_SF_tableSpike_time$Time_s))
        max_nb_of_spike=length(spike_times)
        
        if (max_nb_of_spike>=5){
          sub_Adaptation_table=Adaptation_fit_table[which(Adaptation_fit_table$Response_time_ms == current_time),]
          
          Max_nb_spike=sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'Nb_of_spikes']
          
          first_inst_freq=1/(spike_times[2]-spike_times[1])
          for (current_spike_time_index in 2:length(spike_times)){
            current_frequency=1/(spike_times[current_spike_time_index]-spike_times[current_spike_time_index-1])
            current_frequency_normalized=current_frequency/first_inst_freq
            current_interval=current_spike_time_index-1
            #Inst_freq_table[as.character(current_sweep),as.character(current_interval)]=current_frequency
            
            new_df=data.frame(sweep_info_table[as.character(current_sweep),"Stim_amp_pA"],
                       current_interval,
                       current_frequency_normalized,
                       Max_nb_spike,
                       current_sweep,
                       paste0(as.character(current_time),'ms'))
            colnames(new_df) <- c("Stim_amp_pA","Interval","Inst_freq_WU","Nb_of_spikes","Sweep","Response_time_ms")
            
            
            Inst_freq_table=rbind(Inst_freq_table,new_df)
          }
         
        }
      }
      full_inst_freq_table=rbind(full_inst_freq_table,Inst_freq_table)
      
    
      
      }
      
      
      full_inst_freq_table$Interval=as.numeric(full_inst_freq_table$Interval)
      full_inst_freq_table$Inst_freq_WU=as.numeric(full_inst_freq_table$Inst_freq_WU)
      full_inst_freq_table$Stim_amp_pA=as.numeric(full_inst_freq_table$Stim_amp_pA)
      full_inst_freq_table$Response_time_ms=factor(full_inst_freq_table$Response_time_ms,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
      full_inst_freq_table$Nb_of_spikes=as.factor(full_inst_freq_table$Nb_of_spikes)
      
      #Adapt_plot=ggplot()+geom_point(full_inst_freq_table,mapping=aes(x=Interval,y=Inst_freq_WU))
      
      Adapt_plot=ggplot()+geom_point(full_inst_freq_table,mapping=aes(x=Interval,y=Inst_freq_WU,colour=Stim_amp_pA,alpha=Nb_of_spikes))
      
      #,color=Stim_amp_pA,alpha=Max_nb_spike
      
      
      full_Adapt_fit_table=data.frame(Interval=numeric(),
                                 Inst_freq_WU = numeric(),
                                 Response_time_ms = character())
      sweep_adapt_fit_table=data.frame(Interval=numeric(),
                                       Inst_freq_WU = numeric(),
                                       Response_time_ms = character(),
                                       Sweep = character(),
                                       Stim_amp_pA = numeric(),
                                       Nb_of_spikes = numeric())
      
      Interval_seq=seq(1,max(full_inst_freq_table$Interval),.1)
      
      
      
      for (current_time in time_list){
        sub_Adaptation_table=Adaptation_fit_table[which(Adaptation_fit_table$Response_time_ms == current_time),]
        sub_sweep_list=sub_Adaptation_table$Sweep
        for (current_sweep in sub_sweep_list){
          if (is.na(sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'A']) == FALSE){
            sweep_current_A=sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'A']
            sweep_current_B=sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'B']
            sweep_current_C=sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'C']
            sweep_inst_freq_array=sweep_current_A*exp(-(Interval_seq-1)/sweep_current_B)+sweep_current_C
            
            current_sweep_Adapt_fit_table=data.frame(cbind(Interval_seq,sweep_inst_freq_array))
            current_sweep_Adapt_fit_table['Response_time_ms']=paste0(as.character(current_time),'ms')
            current_sweep_Adapt_fit_table['Sweep']=as.character(current_sweep)
            current_sweep_Adapt_fit_table['Stim_amp_pA']=sweep_info_table[as.character(current_sweep),"Stim_amp_pA"]
            Max_nb_spike=sub_Adaptation_table[which(sub_Adaptation_table$Sweep == current_sweep),'Nb_of_spikes']
            current_sweep_Adapt_fit_table['Nb_of_spikes']=Max_nb_spike
            
            sweep_adapt_fit_table=rbind(sweep_adapt_fit_table,current_sweep_Adapt_fit_table)
           }
        }
        
        current_A=weighted.mean(sub_Adaptation_table$A,sub_Adaptation_table$Nb_of_spikes,na.rm=TRUE)
        current_B=weighted.mean(sub_Adaptation_table$B,sub_Adaptation_table$Nb_of_spikes,na.rm=TRUE)
        current_C=weighted.mean(sub_Adaptation_table$C,sub_Adaptation_table$Nb_of_spikes,na.rm=TRUE)
        A_norm=current_A/(current_A+current_C)
        C_norm=current_C/(current_A+current_C)
        
        inst_freq_array=current_A*exp(-(Interval_seq-1)/current_B)+current_C
        
        current_Adapt_fit_table=data.frame(cbind(Interval_seq,inst_freq_array))
        # current_Adapt_fit_table['Stim_amp_pA']=
        current_Adapt_fit_table['Response_time_ms']=paste0(as.character(current_time),'ms')
        
        full_Adapt_fit_table=rbind(full_Adapt_fit_table,current_Adapt_fit_table)
      }
      
      colnames(sweep_adapt_fit_table) <- c("Interval","Inst_freq_WU",'Response_time_ms','Sweep','Stim_amp_pA','Nb_of_spikes')
      sweep_adapt_fit_table$Interval=as.numeric(sweep_adapt_fit_table$Interval)
      sweep_adapt_fit_table$Inst_freq_WU=as.numeric(sweep_adapt_fit_table$Inst_freq_WU)
      sweep_adapt_fit_table$Response_time_ms=as.factor(sweep_adapt_fit_table$Response_time_ms)
      sweep_adapt_fit_table$Response_time_ms=factor(sweep_adapt_fit_table$Response_time_ms,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
      sweep_adapt_fit_table$Sweep=as.factor(sweep_adapt_fit_table$Sweep)
      sweep_adapt_fit_table$Sweep=factor(sweep_adapt_fit_table$Sweep,levels=sweep_list)
      
      sweep_adapt_fit_table$Nb_of_spikes = as.factor(sweep_adapt_fit_table$Nb_of_spikes)
      
      colnames(full_Adapt_fit_table) <- c("Interval","Inst_freq_WU",'Response_time_ms')
      full_Adapt_fit_table$Response_time_ms=as.factor(full_Adapt_fit_table$Response_time_ms)
      full_Adapt_fit_table$Response_time_ms=factor(full_Adapt_fit_table$Response_time_ms,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
      Adapt_plot=Adapt_plot+geom_line(sweep_adapt_fit_table,mapping=aes(x=Interval,y=Inst_freq_WU,colour=Stim_amp_pA,alpha=Nb_of_spikes,group=Sweep),linetype = "dashed")
      Adapt_plot=Adapt_plot+geom_line(full_Adapt_fit_table,mapping=aes(x=Interval,y=Inst_freq_WU),color="red")
      Adapt_plot=Adapt_plot+ggtitle('Spike_frequency_adaptation')
      
      }
      Adapt_plotly <- ggplotly(Adapt_plot,dynamicTicks=TRUE)
      Adapt_plotly
      
      
    })
    output$I_O_feature_plot <- renderPlotly({
      
      cell_tables_list=get_cell_file()
      
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
        IO_plot=ggplot(Stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz),color='grey')+geom_point()
        IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : No computation of I/O relationship"))
      }
      
      else{
        IO_plot=ggplot(Stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,colour=Response_time))+geom_point()
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
      
      IO_plot=IO_plot+geom_point(Sat_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time),shape=3,size=25)
      IO_plot=IO_plot+ggtitle(paste0(input$Cell_id_to_analyse," : I/O relationship"))
      my_orange = brewer.pal(n = 9, "Blues")[3:9] #there are 9, I exluded the two lighter hues
      IO_plot=IO_plot + scale_colour_manual(values=my_orange)
      }
      }
      IO_plotly <- ggplotly(IO_plot,dynamicTicks=TRUE)
      IO_plotly
      
    })
    
    output$IO_table_fit <- renderTable({
      cell_tables_list=get_cell_file()
      cell_fit_table=cell_tables_list$Cell_fit_table
      sub_cell_fit_table=cell_fit_table[,c('Response_time_ms', 'I_O_obs', 'I_O_QNRMSE', 'Hill_amplitude',
                                           'Hill_coef', 'Hill_Half_cst','Hill_x0','Sigmoid_x0','Sigmoid_sigma')]
      
      sub_cell_fit_table
    })
    
    
    output$IO_table_feature <- renderTable({
      cell_tables_list=get_cell_file()
      cell_feature_table=cell_tables_list$Cell_feature_table
     
      cell_feature_table
    })
    
    
    output$Adapt_table <- renderTable({
      cell_tables_list=get_cell_file()
      cell_fit_table=cell_tables_list$Cell_fit_table
      sub_cell_fit_table=cell_fit_table[,c("Response_time_ms", 'Adaptation_obs','Adaptation_RMSE','A','B','C')]
      
      sub_cell_fit_table
    })
    
    
    
    output$Stim_freq_table <- renderTable({
      cell_tables_list=get_cell_file()
      
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
      
      
      Stim_freq_table
      
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
    
    output$Gain_time_plot <- renderPlotly(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      lmgain = lm(Gain~Response_time_ms, data = Cell_feature_table)
      
      gain_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Response_time_ms,y=Gain))+geom_point()
      gain_time_plot=gain_time_plot+geom_abline(intercept=lmgain$coefficients[1],slope=lmgain$coefficients[2],color='red')
      
      
      gain_time_plotly <- ggplotly(gain_time_plot,dynamicTicks=TRUE)
      gain_time_plotly
    }))
    
    output$Gain_time_metrics <- renderTable(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      lmgain = lm(Gain~Response_time_ms, data = Cell_feature_table)
      
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
      lmthreshold = lm(Threshold~Response_time_ms, data = Cell_feature_table)
      
      threshold_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Response_time_ms,y=Threshold))+geom_point()
      threshold_time_plot=threshold_time_plot+geom_abline(intercept=lmthreshold$coefficients[1],slope=lmthreshold$coefficients[2],color='red')
      
      
      threshold_time_plot <- ggplotly(threshold_time_plot,dynamicTicks=TRUE)
      threshold_time_plot
    }))
    
    output$Threshold_time_metrics <- renderTable(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      lmthreshold = lm(Threshold~Response_time_ms, data = Cell_feature_table)
      
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
      lmsat_stim = lm(Saturation_Stimulus~Response_time_ms, data = Cell_feature_table)
      
      sat_stim_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Response_time_ms,y=Saturation_Stimulus))+geom_point()
      sat_stim_time_plot=sat_stim_time_plot+geom_abline(intercept=lmsat_stim$coefficients[1],slope=lmsat_stim$coefficients[2],color='red')
      
      
      sat_stim_time_plot <- ggplotly(sat_stim_time_plot,dynamicTicks=TRUE)
      sat_stim_time_plot
    }))
    
    output$Saturation_stimulus_time_metrics <- renderTable(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      lmsat_stim = lm(Saturation_Stimulus~Response_time_ms, data = Cell_feature_table)
      
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
      lmsat_freq = lm(Saturation_Frequency~Response_time_ms, data = Cell_feature_table)
      
      sat_freq_time_plot=ggplot(Cell_feature_table,mapping=aes(x=Response_time_ms,y=Saturation_Frequency))+geom_point()
      sat_freq_time_plot=sat_freq_time_plot+geom_abline(intercept=lmsat_freq$coefficients[1],slope=lmsat_freq$coefficients[2],color='red')
      
      
      sat_freq_time_plot <- ggplotly(sat_freq_time_plot,dynamicTicks=TRUE)
      sat_freq_time_plot
    }))
    
    output$Saturation_freq_time_metrics <- renderTable(({
      cell_tables_list=get_cell_file()
      
      sweep_info_table=cell_tables_list$Sweep_info_table
      Cell_feature_table=cell_tables_list$Cell_feature_table
      lmsat_freq = lm(Saturation_Frequency~Response_time_ms, data = Cell_feature_table)
      
      sat_freq_df=data.frame(Intercept = numeric(),  
                             Slope = numeric(),
                             `R^2` = numeric())
      rsquared=summary(lmsat_freq)$r.squared
      sat_freq_df[1,] <- list(lmsat_freq$coefficients[1],lmsat_freq$coefficients[2],rsquared)
      
      sat_freq_df
    }))
    
   

}

# Run the application 
shinyApp(ui = ui, server = server)

