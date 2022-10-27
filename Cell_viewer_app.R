#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

required_packages=c("rjson","plyr","dplyr","shiny","ggplot2","GGally","plotly","tidyverse","gghighlight","ggpubr","shinyFiles",'gghalves','shinyWidgets')
install.packages(setdiff(required_packages,rownames(installed.packages())))
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
               tabPanel("Information",

    # Sidebar with a slider input for number of bins 
          sidebarLayout(
              sidebarPanel(
                  
            textInput("Cell_id","Enter cell id"),
            actionButton("Change_cell", "Show Cell Trace")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
         # plotlyOutput("Spike_feature_plot",height = 800)
          #plotlyOutput("traces_plot")
          #tableOutput('Metadata'),
          #tableOutput('IO_table')
          #tableOutput('Stim_freq_table')
          
           
           
        )
        )
    ),
    tabPanel("Single sweep traces",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("Sweep_to_analyse","Sweep to analysis",choices="")
                 
                 
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("Spike_feature_plot",height = 800),
                 tableOutput('Metadata')
                 
               )
             )
    ),
    tabPanel("I/O",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotlyOutput("I_O_feature_plot",height = 800),
                 tableOutput('IO_table'),
                 tableOutput('Stim_freq_table')
                 
               )
             )
    ),
    tabPanel("Raw traces",

             # Sidebar with a slider input for number of bins
             sidebarLayout(
               sidebarPanel(

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
  
  
  get_json_file <- eventReactive(input$Change_cell,{
    
    current_json_file=fromJSON(file=paste0('/Users/julienballbe/Downloads/Data_test_',as.character(input$Cell_id),'.json'))
    
    sweep_list=unname(unlist(current_json_file$TPC$`0`$Sweep))
    updateSelectInput(session,"Sweep_to_analyse","Sweep to analysis",choices=sweep_list)
    print('file_loaded')
    return (current_json_file)
    
  })
  
  
    
    output$traces_plot <- renderPlotly({
        
      
      json_file=get_json_file()
      
      sweep_list=unname(unlist(json_file$TPC$`0`$Sweep))
      my_plot=ggplot()
      print(sweep_list)
      full_trace_df=as.data.frame(do.call("cbind",json_file$TPC$`0`$TPC[[as.character(sweep_list[1])]]))
      full_trace_df["Sweep"]=as.character(sweep_list[1])
      
      for (current_sweep in sweep_list[2:length(sweep_list)]){
        current_sweep=as.character(current_sweep)
        df=as.data.frame(do.call("cbind",json_file$TPC$`0`$TPC[[current_sweep]]))
        df["Sweep"]=current_sweep
        full_trace_df=rbind(full_trace_df,df)
        print(current_sweep)
        
      }
      
      for (elt in colnames(full_trace_df)){
        full_trace_df[,elt]=as.numeric(full_trace_df[,elt])
      }
      full_trace_df[,"Sweep"]=as.factor(full_trace_df[,"Sweep"])
      
      
      my_plot=ggplot(data=full_trace_df,aes(x=Time_s,y=Membrane_potential_mV,group=Sweep))+geom_line(aes(color=Sweep))
      my_plot
    })
    
    output$Spike_feature_plot <- renderPlotly({
      selected_sweep=as.character(input$Sweep_to_analyse)
      current_json_file=get_json_file()
      
      sweep_trace=as.data.frame(do.call("cbind",current_json_file$TPC$`0`$TPC[[selected_sweep]]))
      SF_table=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`$SF[[selected_sweep]]))
      SF_table=as.data.frame(lapply(SF_table, unlist))
      for (elt in colnames(sweep_trace)){
        sweep_trace[,elt]=as.numeric(sweep_trace[,elt])
      }
      full_table=sweep_trace[,c('Time_s','Membrane_potential_mV')]
      full_table['Trace']='Membrane_potential_mV'
      colnames(full_table) <-  c("Time_s",'Value','Trace')
      
      second_table=sweep_trace[,c('Time_s','Potential_first_time_derivative_mV/s')]
      second_table['Trace']="Potential_first_time_derivative_mV/s"
      colnames(second_table) <-  c("Time_s",'Value','Trace')
      
      third_table=sweep_trace[,c('Time_s','Potential_second_time_derivative_mV/s/s')]
      third_table['Trace']="Potential_second_time_derivative_mV/s/s"
      colnames(third_table) <-  c("Time_s",'Value','Trace')
      
      full_table=rbind(full_table,second_table,third_table)
      full_table$Trace=factor(full_table$Trace,levels=c('Membrane_potential_mV',"Potential_first_time_derivative_mV/s","Potential_second_time_derivative_mV/s/s"))
      full_table=as.data.frame(lapply(full_table, unlist))
      
      
      for (elt2 in colnames(SF_table)[-length(colnames(SF_table))]){
        SF_table[,elt2]=as.numeric(SF_table[,elt2])
      }
      
      
      
      SF_plot=ggplot(full_table,aes(x=Time_s,y=Value))+geom_line()+facet_grid(Trace ~ .,scales = "free")
      
      if (dim(SF_table)[1] != 0){
        SF_First_table=SF_table[,c('Time_s','Membrane_potential_mV','Feature')]
        SF_First_table['Trace']='Membrane_potential_mV'
        colnames(SF_First_table) <-  c("Time_s",'Value','Feature','Trace')
        
        SF_Second_table=SF_table[,c('Time_s','Potential_first_time_derivative_mV.s','Feature')]
        SF_Second_table['Trace']='Potential_first_time_derivative_mV/s'
        colnames(SF_Second_table) <-  c("Time_s",'Value','Feature','Trace')
        
        SF_Third_table=SF_table[,c('Time_s','Potential_second_time_derivative_mV.s.s','Feature')]
        SF_Third_table['Trace']='Potential_second_time_derivative_mV/s/s'
        colnames(SF_Third_table) <-  c("Time_s",'Value','Feature','Trace')
        
        fullSF_table=rbind(SF_First_table,SF_Second_table,SF_Third_table)
        fullSF_table$Trace=factor(fullSF_table$Trace,levels=c('Membrane_potential_mV',"Potential_first_time_derivative_mV/s","Potential_second_time_derivative_mV/s/s"))
        SF_plot=SF_plot+geom_point(fullSF_table,mapping = aes(x=Time_s,y=Value,color=Feature))
      }
      
      SF_plot
      
    })
    
    output$I_O_feature_plot <- renderPlotly({
      
      current_json_file=get_json_file()
      Full_Sweep_metadata=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`))
      Sweep_metadata=Full_Sweep_metadata[,-(dim(Full_Sweep_metadata)[2])]
      
      Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                          Stim_amp_pA = numeric(),
                          Frequency_Hz = numeric(),
                          Response_time = character(),
                          stringsAsFactors = FALSE)
      
      time_list=c(0.005,0.010,0.025,0.050,0.100,0.250,0.500)
      current_line=1
      for (line in 1:dim(Full_Sweep_metadata)[1]){
        sweep=unname(unlist(Full_Sweep_metadata[line,1]))
        stim_start=unname(unlist(Full_Sweep_metadata[line,2]))
        stim_end=unname(unlist(Full_Sweep_metadata[line,3]))
        stim_amp=unname(unlist(Full_Sweep_metadata[line,4]))
        current_SF_table=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`$SF[[as.character(sweep)]]))
        
        current_SF_tableSpike_time=subset(current_SF_table, Feature == "Upstroke")
        if (dim(current_SF_tableSpike_time)[1] != 0){
          for (response_time in time_list){
            spike_table=subset(current_SF_tableSpike_time, Time_s <= unname(unlist(Full_Sweep_metadata[line,2]))+response_time)
            current_frequency=dim(spike_table)[1]/(response_time)
            
            time=paste0(as.character(response_time*1e3),'ms')
            new_line=c(sweep,stim_amp,current_frequency,time)
            Stim_freq_table[current_line, ] <- new_line 
            current_line=current_line+1
            
            
          }
        }
        
        else if (dim(current_SF_tableSpike_time)[1] == 0){
          for (response_time in time_list){
            
            current_frequency=0.0
            
            time=paste0(as.character(response_time*1e3),'ms')
            new_line=c(sweep,stim_amp,current_frequency,time)
            Stim_freq_table[current_line, ] <- new_line 
            current_line=current_line+1
            
            
          }
        }
        
       
        
      }
      
      Stim_freq_table$Stim_amp_pA=as.numeric(Stim_freq_table$Stim_amp_pA)
      Stim_freq_table$Frequency_Hz=as.numeric(Stim_freq_table$Frequency_Hz)
      Stim_freq_table$Response_time=factor(Stim_freq_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
      IO_plot=ggplot(Stim_freq_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time))+geom_point()
      
      
      # Create sigmoid fit traces  --> fit trace
      fit_trace=as.data.frame(do.call("cbind",current_json_file$Fit_table$`0`))
      fit_trace=subset(fit_trace,Fit =="TypeI")
      
      if (dim(fit_trace)[1] != 0){
        stim_array=seq(min(unname(unlist(Full_Sweep_metadata$Stim_amp_pA))),
                       max(unname(unlist(Full_Sweep_metadata$Stim_amp_pA))),
                       1)
        sigmoid_amplitude=unname(unlist(fit_trace[1,"Amplitude"]))
        sigmoid_center=unname(unlist(fit_trace[1,"Center"]))
        sigmoid_sigma=unname(unlist(fit_trace[1,"Sigma"]))
        
        freq_array=sigmoid_amplitude*(1-(1/(1+exp((stim_array-sigmoid_center)/sigmoid_sigma))))
        fit_table=data.frame(cbind(stim_array,freq_array))
        fit_table['Response_time']=paste0(as.character(unname(unlist(fit_trace[1,"Response_time_ms"]))),'ms')
        
        for (time in 2:dim(fit_trace)[1]){
          sigmoid_amplitude=unname(unlist(fit_trace[time,"Amplitude"]))
          sigmoid_center=unname(unlist(fit_trace[time,"Center"]))
          sigmoid_sigma=unname(unlist(fit_trace[time,"Sigma"]))
          freq_array=sigmoid_amplitude*(1-(1/(1+exp((stim_array-sigmoid_center)/sigmoid_sigma))))
          new_table=data.frame(cbind(stim_array,freq_array))
          new_table['Response_time']=paste0(as.character(unname(unlist(fit_trace[time,"Response_time_ms"]))),'ms')
          fit_table=rbind(fit_table,new_table)
          
        }
        
        colnames(fit_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_time')
        fit_table$Response_time=as.factor(fit_table$Response_time)
        fit_table$Response_time=factor(fit_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
        IO_plot=IO_plot+geom_line(fit_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time))
      }
      
      original_IO_table=as.data.frame(do.call("cbind",current_json_file$IO_table$`0`))
      original_IO_table=subset(original_IO_table,Fit=="TypeI")
      
      if (dim(original_IO_table)[1] !=0){
        min_x=min(unname(unlist(original_IO_table$Threshold)))-10
        max_x=max(unname(unlist(Full_Sweep_metadata$Stim_amp_pA)))+10
        
        Threshold=unname(unlist(original_IO_table[1,"Threshold"]))
        Gain=unname(unlist(original_IO_table[1,"Gain"]))
        Intercept=-Gain*Threshold
        
        IO_stim_array=seq(min_x,max_x,1)
        IO_freq_array=Gain*IO_stim_array+Intercept
        IO_table=data.frame(cbind(IO_stim_array,IO_freq_array))
        IO_table['Response_time']=paste0(as.character(unname(unlist(original_IO_table[1,"Response_time_ms"]))),'ms')
        
        for (time in 2:dim(original_IO_table)[1]){
          Threshold=unname(unlist(original_IO_table[time,"Threshold"]))
          Gain=unname(unlist(original_IO_table[time,"Gain"]))
          Intercept=-Gain*Threshold
          IO_freq_array=Gain*IO_stim_array+Intercept
          current_IO_table=data.frame(cbind(IO_stim_array,IO_freq_array))
          current_IO_table['Response_time']=paste0(as.character(unname(unlist(original_IO_table[time,"Response_time_ms"]))),'ms')
          
          IO_table=rbind(IO_table,current_IO_table)
          
        }
        colnames(IO_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_time')
        IO_table$Response_time=as.factor(IO_table$Response_time)
        IO_table$Response_time=factor(IO_table$Response_time,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
        IO_plot=IO_plot+geom_line(IO_table,mapping=aes(x=Stim_amp_pA,y=Frequency_Hz,color=Response_time),linetype='dashed')
      }
      
      IO_plot
      
    })
    
    output$IO_table <- renderTable({
      current_json_file=get_json_file()
      original_IO_table=as.data.frame(do.call("cbind",current_json_file$IO_table$`0`))
      original_IO_table <- original_IO_table %>% replace(.=="NULL", NaN)
      original_IO_table=original_IO_table[,-1]
      original_IO_table=data.frame(original_IO_table)
      original_IO_table
    })
    
    
    
    output$Stim_freq_table <- renderTable({
      current_json_file=get_json_file()
      Full_Sweep_metadata=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`))
      Sweep_metadata=Full_Sweep_metadata[,-(dim(Full_Sweep_metadata)[2])]
      
      Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                                    Stim_amp_pA = numeric(),
                                    Frequency_Hz_5ms = numeric(),
                                    Frequency_Hz_10ms = numeric(),
                                    Frequency_Hz_25ms = numeric(),
                                    Frequency_Hz_50ms = numeric(),
                                    Frequency_Hz_100ms = numeric(),
                                    Frequency_Hz_250ms = numeric(),
                                    Frequency_Hz_500ms = numeric(),
                                    
                                    stringsAsFactors = FALSE)
      
      time_list=c(0.005,0.010,0.025,0.050,0.100,0.250,0.500)
      current_line=1
      for (line in 1:dim(Full_Sweep_metadata)[1]){
        sweep=unname(unlist(Full_Sweep_metadata[line,1]))
        stim_start=unname(unlist(Full_Sweep_metadata[line,2]))
        stim_end=unname(unlist(Full_Sweep_metadata[line,3]))
        stim_amp=unname(unlist(Full_Sweep_metadata[line,4]))
        current_SF_table=as.data.frame(do.call("cbind",current_json_file$Spike_feature_table$`0`$SF[[as.character(sweep)]]))
        
        current_SF_tableSpike_time=subset(current_SF_table, Feature == "Upstroke")
        new_line=c(sweep,stim_amp)
        
          for (response_time in time_list){
            spike_table=subset(current_SF_tableSpike_time, Time_s <= unname(unlist(Full_Sweep_metadata[line,2]))+response_time)
            current_frequency=dim(spike_table)[1]/(response_time)
            
            new_line <- c(new_line,as.numeric(current_frequency))
          }
          
        
        Stim_freq_table[line, ] <- new_line 
        
        
      }
      
      
      Stim_freq_table
      
    })
    
    output$Metadata <- renderTable({
      my_json_file=get_json_file()
      metadata=as.data.frame(do.call('cbind',my_json_file$Metadata$`0`))
      
      Tmetadata=data.frame(t(metadata))
      
      colnames(Tmetadata) <- as.character(input$Cell_id)
      Tmetadata
    },rownames=TRUE)
    
   
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
