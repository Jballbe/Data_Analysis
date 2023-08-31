

load_h5_file <- function(filename){
  
  h5groups=names(h5dump(file=filename,load = FALSE))
  
  
  if ('Cell_Feature' %in% h5groups){
  Cell_Feature_group=h5read(file=filename,name='/Cell_Feature')
  Cell_feature_table=data.frame(Cell_Feature_group)
  col_order=c('Response_type', 'Output_Duration', 'Gain', 'Threshold',
               'Saturation_Frequency', 'Saturation_Stimulus', 'Adaptation_index')
  Cell_feature_table <- Cell_feature_table[,col_order]
  }
  else{
    Cell_feature_table = data.frame()
  }
  
  
  if ("Metadata_table" %in% h5groups){
  Metadata_table = data.frame(h5read(file=filename,name='/Metadata_table'))}
  else {
    Metadata_table=data.frame()
  }
  
  if ("Sweep_info" %in% h5groups){
    Sweep_info_table=data.frame(h5read(file=filename,name='/Sweep_info'))
    rownames(Sweep_info_table) <- Sweep_info_table$Sweep
    Sweep_list=Sweep_info_table$Sweep
  }
  else{
    Sweep_info_table=data.frame()
  }
  
  
  if ("Sweep_QC" %in% h5groups){
    Sweep_QC_table=data.frame((h5read(file=filename,name='/Sweep_QC')))
  }
  else{
    Sweep_QC_table=data.frame()
  }
  
  
  
  if ('Cell_Fit' %in% h5groups){
    Cell_fit_table=data.frame(h5read(file=filename,name='/Cell_Fit'))
    
    
    col_order <- c('Response_type', 'Output_Duration', 'I_O_obs', 'I_O_QNRMSE',
                    'Hill_amplitude', 'Hill_coef', 'Hill_Half_cst', 'Hill_x0', 'Sigmoid_x0',
                    'Sigmoid_sigma', 'Adaptation_obs', 'Adaptation_RMSE', 'A', 'B', 'C')
   
    Cell_fit_table <- Cell_fit_table[, col_order]
    
  }
  else{
    Cell_fit_table=data.frame()
  }
  
  if ('TPC_tables' %in% h5groups){
    TPC_group=h5read(file=filename,name='/TPC_tables')
    TPC_colnames=TPC_group$TPC_colnames
    SF_group=h5read(file=filename,name='/SF_tables')
    SF_list=list()
    for (elt in Sweep_list){
      
      elt_ch=as.character(elt)
      current_TPC=TPC_group[elt_ch]
      current_TPC_table=current_TPC[[1]]
      
      
      current_TPC_table <- t(current_TPC_table)
      current_TPC_table=data.frame(current_TPC_table)
      colnames(current_TPC_table) <- TPC_colnames
      rownames(current_TPC_table) <- seq(1:nrow(current_TPC_table))
      
      
      sampling_freq=mean(Sweep_info_table$Sampling_Rate_Hz)
      filt_coeff = (5 * 1e3) / (sampling_freq / 2.)
      bf <- butter(2, filt_coeff)
      zi <- filter_zi(bf)
      
      
      filtered_potential_trace <- filter(bf, current_TPC_table[,"Membrane_potential_mV"],zi)$y 
      filtered_current_trace <-  filter(bf, current_TPC_table[,"Input_current_pA"],zi)$y
      current_TPC_table_filtered = current_TPC_table
      
      current_TPC_table_filtered[,"Membrane_potential_mV"] <- filtered_potential_trace
      current_TPC_table_filtered[,"Input_current_pA"] <- filtered_current_trace
      
      
      current_SF_group = SF_group[elt_ch][[1]]
      if (length(current_SF_group) == 0){
        current_SF_df = data.frame(matrix(ncol = length(TPC_colnames)+1, nrow = 0))
        colnames(current_SF_df) <- c(TPC_colnames,"Feature")
      }
      
      else {
        current_SF_df = data.frame(matrix(ncol = length(TPC_colnames)+1, nrow = 0))
        colnames(current_SF_df) <- c(TPC_colnames,"Feature")
        
        for (current_Feature in names(current_SF_group)){
          
          
          current_feature_array = unlist(unname(current_SF_group[current_Feature]))
          current_feature_array=current_feature_array+1
          
          
          sub_SF_table=current_TPC_table_filtered[current_feature_array,]
          
          sub_SF_table['Feature']=current_Feature
          
          current_SF_df=rbind(current_SF_df,sub_SF_table)
          
        }
        rownames(current_SF_df) <- seq(1:dim(current_SF_df)[1])
        
      }
      
      current_TPC[[1]] <- current_TPC_table
      TPC_group[elt_ch] <- current_TPC
      SF_list[[elt_ch]] <- current_SF_df
      #SF_group[elt_ch] <- current_SF_df
      
    }
  }
  
  else{
    TPC_group=data.frame()
    SF_list=data.frame()
  }
  
  #names(SF_list) <- Sweep_list
  cell_tables_list=list(TPC_group,SF_list,Sweep_QC_table,Cell_feature_table,Sweep_info_table,Metadata_table,Cell_fit_table)
  cell_tables_list_names=c('Full_TPC','Full_SF','Sweep_QC_table','Cell_feature_table','Sweep_info_table','Metadata_table','Cell_fit_table')
  names(cell_tables_list) <- cell_tables_list_names
  return(cell_tables_list)
}

nest_Full_DF <- function(Full_SF,table_type){
  
  sweep_list=names(Full_SF)
  sweep_list = sweep_list[sweep_list %in% "TPC_colnames" == FALSE]
  first_df_nested=nest(Full_SF[[as.character(sweep_list[1])]])
  first_df_nested["Sweep"]=sweep_list[1]
  
  
  for (current_sweep in sweep_list[2:length(sweep_list)]){
    new_line_nested=nest(Full_SF[[as.character(current_sweep)]])
    new_line_nested["Sweep"]=current_sweep
    
    first_df_nested=rbind(first_df_nested,new_line_nested)
    
  }
  colnames(first_df_nested) <- c(as.character(table_type),"Sweep")
  rownames(first_df_nested) <- first_df_nested$Sweep
  return(first_df_nested)
}
get_stim_freq_table_R <- function(cell_tables_list,response_type){
  
  sweep_info_table=cell_tables_list$Sweep_info_table
  Cell_feature_table=cell_tables_list$Cell_feature_table
  sub_Cell_feature_table=Cell_feature_table[which(Cell_feature_table$Response_type == response_type),]
  output_duration_list <- unique(sub_Cell_feature_table$Output_Duration)
  
  Full_Sweep_metadata=cell_tables_list$Sweep_info_table
  sweep_list=Full_Sweep_metadata$Sweep
  
  if (response_type == "Time_based"){
    Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                                  Stim_amp_pA = numeric(),
                                  Frequency_Hz = numeric(),
                                  Response_Duration = character(),
                                  stringsAsFactors = FALSE)
    for (current_response_duration in output_duration_list){
      
      sub_Cell_feature_table=sub_Cell_feature_table[which(sub_Cell_feature_table$Output_Duration == current_response_duration),]
      for (current_sweep in sweep_list){
        stim_start=sweep_info_table[as.character(current_sweep),"Stim_start_s"]
        stim_end=sweep_info_table[as.character(current_sweep),"Stim_end_s"]
        stim_amp=sweep_info_table[as.character(current_sweep),"Stim_amp_pA"]
        SF_table=data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        spike_table=SF_table[which(SF_table$Feature == 'Upstroke'),]
        spike_table=spike_table[which(spike_table$Time_s<=(stim_start+current_response_duration)),]
        current_frequency=dim(spike_table)[1]/(current_response_duration)
        time=paste0(as.character(current_response_duration*1e3),'ms')
        new_line=data.frame(list(current_sweep,stim_amp,current_frequency,time))
        colnames(new_line) <- c('Sweep','Stim_amp_pA',"Frequency_Hz","Response_Duration")
        Stim_freq_table=rbind(Stim_freq_table,new_line)
        
        
      }
    }
    Stim_freq_table$Stim_amp_pA=as.numeric(Stim_freq_table$Stim_amp_pA)
    Stim_freq_table$Frequency_Hz=as.numeric(Stim_freq_table$Frequency_Hz)
    Stim_freq_table$Response_Duration=factor(Stim_freq_table$Response_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
  }
  
  if (response_type == "Index_based"){
    Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                                  Stim_amp_pA = numeric(),
                                  Frequency_Hz = numeric(),
                                  Response_Duration = character(),
                                  stringsAsFactors = FALSE)
    for (current_response_duration in output_duration_list){
      
      sub_Cell_feature_table=sub_Cell_feature_table[which(sub_Cell_feature_table$Output_Duration == current_response_duration),]
      for (current_sweep in sweep_list){
        stim_start=sweep_info_table[as.character(current_sweep),"Stim_start_s"]
        stim_end=sweep_info_table[as.character(current_sweep),"Stim_end_s"]
        stim_amp=sweep_info_table[as.character(current_sweep),"Stim_amp_pA"]
        SF_table=data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        spike_table=SF_table[which(SF_table$Feature == 'Upstroke'),]
        
        if(as.integer(current_response_duration)<=nrow(spike_table)){
          spike_table=spike_table[1:as.integer(current_response_duration),]
        }
        
        
        if (nrow(spike_table)==0){
          current_frequency=0
        }
        else{
          current_frequency=dim(spike_table)[1]/(max(spike_table[,"Time_s"])-stim_start)
        }
        output_duration=paste0('Index_',as.character(as.integer(current_response_duration)))
        new_line=data.frame(list(current_sweep,stim_amp,current_frequency,output_duration))
        colnames(new_line) <- c('Sweep','Stim_amp_pA',"Frequency_Hz","Response_Duration")
        Stim_freq_table=rbind(Stim_freq_table,new_line)
        
      }
    }
    Stim_freq_table$Stim_amp_pA=as.numeric(Stim_freq_table$Stim_amp_pA)
    Stim_freq_table$Frequency_Hz=as.numeric(Stim_freq_table$Frequency_Hz)
    Stim_freq_table$Response_Duration=factor(Stim_freq_table$Response_Duration,levels=c("Index_1","Index_2","Index_3","Index_4","Index_5","Index_6","Index_7","Index_8","Index_9","Index_10"))
  }
  
  
  if (response_type == "Interval_based"){
    Stim_freq_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                                  Stim_amp_pA = numeric(),
                                  Frequency_Hz = numeric(),
                                  Response_Duration = character(),
                                  stringsAsFactors = FALSE)
    for (current_response_duration in output_duration_list){
      
       for (current_sweep in sweep_list){
        stim_start=sweep_info_table[as.character(current_sweep),"Stim_start_s"]
        stim_end=sweep_info_table[as.character(current_sweep),"Stim_end_s"]
        stim_amp=sweep_info_table[as.character(current_sweep),"Stim_amp_pA"]
        SF_table=data.frame(cell_tables_list$Full_SF[[as.character(current_sweep)]])
        spike_table=SF_table[which(SF_table$Feature == 'Upstroke'),]
        
        if( as.integer(current_response_duration) < nrow(spike_table) ) {
          #spike_table=spike_table[1:as.integer(current_response_duration)+1,]
          current_frequency=1/(spike_table[ as.integer(current_response_duration+1),"Time_s"]-spike_table[ as.integer(current_response_duration),"Time_s"])
        }
        else{
          current_frequency=0
          
        }
        
        if (nrow(spike_table)==0){
          
        }
        
        output_duration=paste0('Interval_',as.character(as.integer(current_response_duration)))
        new_line=data.frame(list(current_sweep,stim_amp,current_frequency,output_duration))
        colnames(new_line) <- c('Sweep','Stim_amp_pA',"Frequency_Hz","Response_Duration")
        Stim_freq_table=rbind(Stim_freq_table,new_line)
        
      }
    }
    Stim_freq_table$Stim_amp_pA=as.numeric(Stim_freq_table$Stim_amp_pA)
    Stim_freq_table$Frequency_Hz=as.numeric(Stim_freq_table$Frequency_Hz)
    Stim_freq_table$Response_Duration=factor(Stim_freq_table$Response_Duration,levels=c("Interval_1","Interval_2","Interval_3","Interval_4","Interval_5","Interval_6","Interval_7","Interval_8","Interval_9","Interval_10"))
  }
  
  
  return(Stim_freq_table)
}


get_fit_tables <- function(cell_tables_list,response_type){
  
  
  fit_table <- data.frame(Sweep = numeric(),    # Create empty data frame
                          Stim_amp_pA = numeric(),
                          Frequency_Hz = numeric(),
                          Response_Duration = character(),
                          stringsAsFactors = FALSE)
  
  Full_Sweep_metadata=cell_tables_list$Sweep_info_table
  sweep_list=Full_Sweep_metadata$Sweep
  Cell_feature_table=cell_tables_list$Cell_feature_table
  cell_fit_table=cell_tables_list$Cell_fit_table
  sub_cell_fit_table = cell_fit_table[which(cell_fit_table$Response_type == response_type ),]
  sub_cell_fit_table = sub_cell_fit_table[which(sub_cell_fit_table$I_O_obs == "Hill-Sigmoid" | (sub_cell_fit_table$I_O_obs == "Hill")),]
  
  
  output_duration_list=sub_cell_fit_table$Output_Duration
  stim_array=Full_Sweep_metadata$Stim_amp_pA
  
  stim_array=seq(min(stim_array,na.rm = TRUE),max(stim_array,na.rm = TRUE),.1)
  stim_array_shifted=stim_array+abs(min(stim_array))
  min_x=min(Cell_feature_table$Threshold, na.rm = TRUE )-10
  max_x=max(Full_Sweep_metadata$Stim_amp_pA, na.rm = TRUE )+10
  
  
  IO_stim_array=seq(min_x,max_x,.1)
  IO_table <- data.frame(Stim_amp_pA = numeric(),
                         Frequency_Hz = numeric(),
                         Response_Duration = character(),
                         stringsAsFactors = FALSE)
  Sat_table=data.frame(Stim_amp_pA = numeric(),
                       Frequency_Hz = numeric(),
                       Response_Duration = character(),
                       stringsAsFactors = FALSE)
  
  sat_table_line=1
  
  for (current_output_duration in output_duration_list){
    
    sub_cell_feature_table=Cell_feature_table[which(Cell_feature_table$Response_type == response_type ),]
    sub_cell_feature_table=sub_cell_feature_table[which(sub_cell_feature_table$Output_Duration == current_output_duration ),]
    I_O_obs = sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"I_O_obs"]
    Hill_amplitude=sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Hill_amplitude"]
    Hill_coef=sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Hill_coef"]
    Hill_Half_cst=sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Hill_Half_cst"]
    Hill_x0 = sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Hill_x0"]
    x0=sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Sigmoid_x0"]
    sigma=sub_cell_fit_table[which(sub_cell_fit_table["Output_Duration"]==current_output_duration),"Sigmoid_sigma"]
    freq_array = c(rep(0,length(stim_array)))
    
    if (I_O_obs == 'Hill-Sigmoid'){
      if (Hill_x0<min(stim_array)){
        freq_array =Hill_amplitude* (((stim_array-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array-Hill_x0)**(Hill_coef)))) *  (1-(1/(1+exp((stim_array-x0)/sigma))))
      }
      else{
        x0_index=which(stim_array < Hill_x0)[length(which(stim_array < Hill_x0))]
        freq_array [x0_index:length(freq_array)] =Hill_amplitude* (((stim_array[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef)))) *  (1-(1/(1+exp((stim_array[x0_index:length(freq_array)]-x0)/sigma))))
      }
      
      
    }
    
    if (I_O_obs == 'Hill'){
      
      if (Hill_x0<min(stim_array)){
        
        freq_array =Hill_amplitude*(((stim_array-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array-Hill_x0)**(Hill_coef))))
        
        }
      else{
        x0_index=which(stim_array < Hill_x0)[length(which(stim_array < Hill_x0))]
        
        freq_array [x0_index:length(freq_array)] =Hill_amplitude* (((stim_array[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))/((Hill_Half_cst**Hill_coef)+((stim_array[x0_index:length(freq_array)]-Hill_x0)**(Hill_coef))))
      }
    }
    
    new_table=data.frame(cbind(stim_array,freq_array))
    if (response_type == 'Time_based'){
      new_table['Response_Duration']=paste0(as.character(current_output_duration*1e3),'ms')
    }
    else{
      base=str_remove(response_type,"based")
      new_table['Response_Duration']=paste0(base,as.character(as.integer(current_output_duration)))
    }
    
    
    colnames(new_table) <- c('Stim_amp_pA','Frequency_Hz','Response_time')
    fit_table=rbind(fit_table,new_table)
    
    Threshold=sub_cell_feature_table[1,"Threshold"]
    Gain=sub_cell_feature_table[1,"Gain"]
    
    Intercept=-Gain*Threshold
    IO_freq_array=Gain*IO_stim_array+Intercept
    current_IO_table=data.frame(cbind(IO_stim_array,IO_freq_array))
    if (response_type == 'Time_based'){
      current_IO_table['Response_Duration']=paste0(as.character(current_output_duration*1e3),'ms')
    }
    else{
      base=str_remove(response_type,"based")
      current_IO_table['Response_Duration']=paste0(base,as.character(as.integer(current_output_duration)))
      
    }
    
    if (is.null(sub_cell_feature_table[1,"Saturation_Frequency"]) == FALSE){
      
      current_sat_table=data.frame(Stim_amp_pA = numeric(),
                                   Frequency_Hz = numeric(),
                                   Response_Duration = character())
      
      Saturation_Frequency=as.numeric(sub_cell_feature_table[1,"Saturation_Frequency"])
      Saturation_Stimulus=as.numeric(sub_cell_feature_table[1,"Saturation_Stimulus"])
      
      
      if (response_type == 'Time_based'){
        
        current_sat_table <- c(Saturation_Stimulus,Saturation_Frequency, paste0(as.character(as.integer(current_output_duration*1e3)),'ms'))
        
      }
      else{
        base=str_remove(response_type,"based")
        current_sat_table <- c(Saturation_Stimulus,Saturation_Frequency, paste0(base,as.character(as.integer(current_output_duration))))
      }
    
      Sat_table[sat_table_line,] <- current_sat_table
      sat_table_line=sat_table_line+1
      
  }
    IO_table=rbind(IO_table,current_IO_table)
    
  }
  
  colnames(fit_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_Duration')
  fit_table$Response_Duration=as.factor(fit_table$Response_Duration)
  
  colnames(IO_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_Duration')
  IO_table$Response_Duration=as.factor(IO_table$Response_Duration)
  
  colnames(Sat_table) <- c("Stim_amp_pA","Frequency_Hz",'Response_Duration')
  Sat_table$Stim_amp_pA=as.numeric(Sat_table$Stim_amp_pA)
  Sat_table$Frequency_Hz=as.numeric(Sat_table$Frequency_Hz)
  Sat_table$Response_Duration=as.factor(Sat_table$Response_Duration)
  
  
  
  if (response_type == 'Time_based'){
    fit_table$Response_Duration=factor(fit_table$Response_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
    IO_table$Response_Duration=factor(IO_table$Response_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
    Sat_table$Response_Duration=factor(Sat_table$Response_Duration,levels=c('5ms',"10ms","25ms","50ms",'100ms','250ms','500ms'))
  }
  else{
    base=as.character(str_remove(response_type,"based"))
    my_levels=as.character(seq(10))
    my_levels=paste0(base,my_levels)
    fit_table$Response_Duration=factor(fit_table$Response_Duration,levels=my_levels)
    IO_table$Response_Duration=factor(IO_table$Response_Duration,levels=my_levels)
    Sat_table$Response_Duration=factor(Sat_table$Response_Duration,levels=my_levels)
  }
  
  
  plot_table_list=list(fit_table,IO_table,Sat_table)
  plot_table_list_names=c("fit_table","IO_table","Sat_table")
  names(plot_table_list) <- plot_table_list_names
  return(plot_table_list)
  
  
  
}

load_population_csv_file <- function(directory,file_list,Selected_Response_Type){
  
  response_duration_list <- c()
  file_dict <- list()
  
  for(file in file_list){
    if (grepl("Full_Feature_Table_",file)==TRUE | grepl("FR_Hz_",file)==TRUE){
      if (grepl("Full_Feature_Table_",file)==TRUE){
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1))
      }
      
      else{
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T))
      }
      unit_file=my_file[1,3:ncol(my_file)]
      file_dict <- append(file_dict,list(unit_file))
      response_duration_list=append(response_duration_list,'Unit_File')
      my_colnames=colnames(my_file)
      break
    }
  }
  
  
  for (file in file_list){
    
    if (grepl("Population_Class",file)==TRUE){
      if (grepl("NVC",file)==TRUE){
        population_class_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T))
      }
      else{
        population_class_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1))
      }
      
      file_dict <- append(file_dict,list(population_class_file))
      response_duration_list=append(response_duration_list,'Population_Class')
      
    }
    
    else if (grepl("linear_values",file)==TRUE){
      
      linear_values_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1))
      
      linear_values_file=linear_values_file[2:nrow(linear_values_file),c('Cell_id','Input_Resistance_MOhms')]
      
      linear_values_file[,"Input_Resistance_MOhms"]=as.numeric(linear_values_file[,"Input_Resistance_MOhms"])
      
      file_dict <- append(file_dict,list(linear_values_file))
      
      
      response_duration_list=append(response_duration_list,'Linear_Values')
    }
    else{
      
      if (grepl("Full_Feature_Table_",file)==TRUE){
        
        current_duration=str_remove(file, "Full_Feature_Table_")
        current_duration=str_remove(current_duration, "Fernandez_")
        current_duration=str_remove(current_duration,Selected_Response_Type)
        current_duration=str_remove(current_duration,'_')
       
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1,skip=1))
      }
      else if (grepl("FR_Hz_",file)==TRUE){
        current_duration=str_remove(file, "FR_Hz_")
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,skip=1))
      }
      
      if (Selected_Response_Type=="Time_based"){
        current_duration=as.numeric(str_remove(current_duration, "ms.csv"))
      }
      else{
        current_duration=as.numeric(str_remove(current_duration, ".csv"))
      }
      
   
      
      response_duration_list=append(response_duration_list,current_duration)
      
     
      
      for (col in seq(3,ncol(my_file))){
        my_file[,col]=as.numeric(my_file[,col])
      }
      
      colnames(my_file) <- my_colnames
      
      file_dict <- append(file_dict,list(my_file))
    }
   
  }
  
  names(file_dict) <- response_duration_list
  
  return(file_dict)
  
}


create_full_df <- function(file_import,Feature_to_analyse,subset_filter,Response_type,keep_na=FALSE){
  
  
  population_class = data.frame(file_import$Population_Class)
  current_population_file <- population_class
  
  
  for (category in names(subset_filter)){
    current_population_file <-current_population_file[which(current_population_file[,category] %in% subset_filter[category][[1]]),]
    
  }
  
  for (current_col in colnames(current_population_file)){
    current_population_file[,current_col]=as.factor(current_population_file[,current_col])
  }
  
  
  file_list <- names(file_import)
 
  file_list = file_list[file_list != 'Population_Class']
  file_list = file_list[file_list != 'Linear_Values']
  file_list = file_list[file_list != 'Unit_File']
  file_list = mixedsort(file_list)
  
  full_data_frame=data.frame()
  
  for (file in file_list){
    
    current_file = file_import[[file]]
    
    current_file <- current_file[,c('Cell_id',Feature_to_analyse)]
    if (keep_na == FALSE){
      current_file <- current_file %>% drop_na()
    }
    #
    current_file <- merge(current_file,current_population_file,by='Cell_id')
    
    if (nrow(current_file)==0){
      current_file[1,]=NA  # ad a temporary new row of NA values
      if (Response_type == 'Time_based'){
        current_file[,'Response_Duration'] = paste0(file,'ms')
      }
      else{
        current_file[,'Response_Duration'] = paste0(str_replace(Response_type,'based',""),file)
      }
      
    }
    else{
      if (Response_type == 'Time_based'){
        current_file[,'Response_Duration'] = paste0(file,'ms')
      }
      else{
        current_file[,'Response_Duration'] = paste0(str_replace(Response_type,'based',""),file)
      }
    }
    
    
    full_data_frame <- rbind(full_data_frame,current_file)
    
  }
  
  
  full_data_frame$Response_Duration <- as.factor(full_data_frame$Response_Duration)
  full_data_frame$Response_Duration <- factor(full_data_frame$Response_Duration,levels=mixedsort(levels(full_data_frame$Response_Duration)))
  
  
  if (keep_na == FALSE){
    full_data_frame <- full_data_frame %>% drop_na()
  }
  
  if ('Linear_Values' %in% names(file_import)){
    
    Linear_values=file_import$Linear_Values
    full_data_frame=inner_join(full_data_frame, Linear_values, by=c('Cell_id'='Cell_id'))

  }

  
  return (full_data_frame)
}

create_full_df_RM_ANOVA <- function(file_import,Feature_to_analyse,subset_filter,Response_type,keep_na=FALSE){
  
  
  population_class = data.frame(file_import$Population_Class)
  current_population_file <- population_class
  
  
  for (category in names(subset_filter)){
    current_population_file <-current_population_file[which(current_population_file[,category] %in% subset_filter[category][[1]]),]
     
  }
  
  for (current_col in colnames(current_population_file)){
    current_population_file[,current_col]=as.factor(current_population_file[,current_col])
  }
  
  
  
  file_list <- names(file_import)
  
  file_list = file_list[file_list != 'Population_Class']
  file_list = file_list[file_list != 'Linear_Values']
  file_list = file_list[file_list != 'Unit_File']
  file_list = mixedsort(file_list)
  full_data_frame=data.frame(current_population_file)
  
  for (file in file_list){
    current_file = file_import[[file]]
    
    
    current_file <- current_file[,c('Cell_id',Feature_to_analyse)]
    test_current_file=current_file%>%drop_na()
    if (nrow(test_current_file)==0) next
    
    if (Response_type == 'Time_based'){
      colnames(current_file) <- c('Cell_id',paste0(file,'ms'))
    }
    else{
      colnames(current_file) <- c('Cell_id',paste0(str_replace(Response_type,'based',""),file))
    }
    
    

    #current_file <- merge(current_file,current_population_file,by='Cell_id')
    
    
    full_data_frame <- merge(full_data_frame,current_file)
    
  }
  if(keep_na==F){
    full_data_frame=full_data_frame%>%drop_na()
  }

  if ('Linear_Values' %in% names(file_import)){
    Linear_values=file_import$Linear_Values
    
    full_data_frame=inner_join(full_data_frame, Linear_values, by=c('Cell_id'='Cell_id'))
  }
  
  return (full_data_frame)
}

###

dataframe_outliers <- function(population_dataframe,feature_col,factor){
 
  outliers_df=population_dataframe %>%
    group_by_(factor) %>%
    identify_outliers(feature_col)
  
  full_df_outlier=merge(population_dataframe,outliers_df[,c("Cell_id",factor,'is.outlier','is.extreme')],by.x = c('Cell_id',factor), by.y =c('Cell_id',factor),all.x=T)
  
  full_df_outlier[is.na(full_df_outlier)] <- FALSE
  
  return (full_df_outlier)
  
  
}

assess_normality <- function(population_dataframe,feature_col,factor){
  population_dataframe=data.frame(population_dataframe)
  shapiro_table=population_dataframe %>%
    rename(Measure=feature_col)%>%
    group_by_(factor) %>%
    shapiro_test(Measure)
  shapiro_table$variable=feature_col
  
  return(shapiro_table)
}

perform_repeated_measure_one_way_ANOVA <- function(population_dataframe,feature_col,factor,remove_outliers,what_to_return){
  
  #### if required, remove outliers per factor levels
  
  
  population_dataframe=dataframe_outliers(population_dataframe,feature_col,factor)
  if (what_to_return=='Oulier_df'){
    oulier_df=population_dataframe
    return(oulier_df)
  }
  
  if (remove_outliers != 'None'){
    if (remove_outliers == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
      cell_id_to_remove =  population_dataframe[which(population_dataframe$is.outlier == TRUE),'Cell_id']
      population_dataframe=population_dataframe[-which(population_dataframe$Cell_id %in% cell_id_to_remove), ]
    }
    if (remove_outliers == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
      cell_id_to_remove =  population_dataframe[which(population_dataframe$is.extreme == TRUE),'Cell_id']
      population_dataframe=population_dataframe[-which(population_dataframe$Cell_id %in% cell_id_to_remove), ]
    }
  }
  
  if (what_to_return=='DF_without_outliers'){
    population_dataframe_WO_outliers=population_dataframe
    return(population_dataframe_WO_outliers)
  }
  #### if required, remove outliers per factor levels
  
  removed_levels=c()
  
  #### remove factor combination with less than 3 observations
  
  categories_count <- population_dataframe%>%
    group_by_(factor)%>%summarise(Count=n())
  
  
  for (line in seq(1,nrow(categories_count))){
    if (categories_count[line,'Count'] <3){
    
      population_dataframe=population_dataframe[-which(population_dataframe[,factor]==as.character(categories_count[line,factor][[1]])),]
      
      removed_level = paste0(as.character(categories_count[line,factor][[1]]))
      removed_levels=append(removed_levels,removed_level)
      
    }
  }
  population_dataframe$Cell_id <- droplevels(population_dataframe$Cell_id)
  if (what_to_return=='Removed_levels'){
    return(removed_levels)
  }
  
  categories_count<- population_dataframe%>%
    group_by_(factor)%>%summarise(Count=n())
  population_dataframe[,factor]=droplevels( population_dataframe[,factor])
  
  if (what_to_return=='Categories_count'){
    return(categories_count)
  }
  #### remove factor combination with less than 3 observations
  
  #### perform Shapiro normality test for remaining factors
  
  normality_table=assess_normality(population_dataframe,feature_col,factor)
  if (what_to_return=='Normality_table'){
    return(normality_table)
  }
  
  #### if any p_val for the Shapiro test if lower than 0.05 --> can't assume normality --> go for unpa
  
  
  normality_p_val <- normality_table$p>.05
  if (FALSE %in% normality_p_val){all.normal=FALSE}
  else{all.normal=TRUE}
  
  
  if (all.normal==FALSE){
    count_n=categories_count[1,'Count']
    all_same_size=TRUE
    for (elt in seq(1,nrow(categories_count))){
      if (categories_count[elt,'Count']!=count_n){
        all_same_size=FALSE
      }
    }
    
    
    #return(population_dataframe)
    if (all_same_size == TRUE){
      friedman_formula=as.formula(paste0(feature_col,'~',factor,'|',"Cell_id",sep=''))
      
      res <- population_dataframe%>%friedman_test(friedman_formula)
      
      is.difference <- res$p<0.05
      variance_test <- data.frame(Method=c(res$method),
                                  p_value=c(res$p))
      }
    else{
      res=skillingsMackTest(y=population_dataframe[,feature_col],groups=population_dataframe[,factor],blocks=population_dataframe[,'Cell_id'])
      is.difference <- res$p.value<0.05
      variance_test <- data.frame(Method=c(res$method),
                                  p_value=c(res$p.value))
      }
    
    
    
    if (is.difference == TRUE){
      my_formula = as.formula( paste0(feature_col,'~', factor) )
      pwc <- wilcox_test(data=population_dataframe, formula=my_formula, paired = TRUE, p.adjust.method = "bonferroni")
      
    }
    else{
      pwc = data.frame(matrix(nrow = 0, ncol = 0))
    }
    
  }### End if (all.normal==FALSE)
  
  if (all.normal==TRUE){
    res = anova_test(data=population_dataframe,dv= feature_col,wid=Cell_id,within=factor)
    is.difference <- res$ANOVA$p<0.05
    variance_test <- data.frame(Method=c(res$method),
                      p_value=c(res$p))
    
    if (is.difference == TRUE){
      my_formula = as.formula( paste0(feature_col,'~', factor) )
      pwc <- pairwise_t_test(data=population_dataframe, formula=my_formula, paired = TRUE, p.adjust.method = "bonferroni")
      
    }
    
    else{
      pwc = data.frame(matrix(nrow = 0, ncol = 0))
    }
  }### end if (all.normal==TRUE)
  
  if(what_to_return=='Variance_test_original_table'){
    return(res)
  }
  
  if (what_to_return=='Variance_test'){
    return(variance_test)
  }
  
  if (what_to_return == 'PWC_without_position'){
    return(pwc)
  }
  if (what_to_return=='PWC'){
    if (is.difference == TRUE){
      pwc <- pwc %>% add_xy_position(x = factor)
    }
    return(pwc)
  }
  
  
  
  
  return (removed_levels)
  
}

create_full_df_ANOVA <- function(file_import,Feature_to_analyse,file_to_select,subset_filter,keep_na=FALSE){
  
  population_class = data.frame(file_import$Population_Class)
  current_population_file <- population_class
  
  for (category in names(subset_filter)){
    current_population_file <-current_population_file[which(current_population_file[,category] %in% subset_filter[category][[1]]),]
    
  }
  
  for (current_col in colnames(current_population_file)){
    current_population_file[,current_col]=as.factor(current_population_file[,current_col])
  }
  
  
  
  
 
  file_list <- names(file_import)
  
  file_list = file_list[file_list != 'Population_Class']
  file_list = file_list[file_list != 'Linear_Values']
  file_list = file_list[file_list != 'Unit_File']
  file_list = mixedsort(file_list)
  full_data_frame=data.frame(current_population_file)
  
  current_file=file_import[[file_to_select]]
  current_file <- current_file[,c('Cell_id',Feature_to_analyse)]
 
  
  full_data_frame <- merge(full_data_frame,current_file)
  
 
  if(keep_na==F){
    full_data_frame=full_data_frame%>%drop_na()
  }
  
 
  
  if ('Linear_Values' %in% names(file_import)){
    Linear_values=file_import$Linear_Values
    file_import_test=file_import
   
    full_df=full_data_frame
    
    full_data_frame=inner_join(full_data_frame, Linear_values, by=c('Cell_id'='Cell_id'))
  }

  return (full_data_frame)
}


assess_variance_homogeneity <- function(population_dataframe,feature_col,factor){
  population_dataframe=data.frame(population_dataframe)
  my_formula <- as.formula(paste0(feature_col,'~',factor))
  levene_table <- population_dataframe %>% 
    levene_test(my_formula)
  return(levene_table)
}

perform_ANOVA <- function(population_dataframe,feature_col,factor,remove_outliers,what_to_return){
  
  #### if required, remove outliers per factor levels
  
  
  population_dataframe=dataframe_outliers(population_dataframe,feature_col,factor)
  
  if (what_to_return=='Outlier_df'){
    outlier_df=population_dataframe
    
    return(outlier_df)
  }
  
  if (remove_outliers != 'None'){
    if (remove_outliers == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
      cell_id_to_remove =  population_dataframe[which(population_dataframe$is.outlier == TRUE),'Cell_id']
      
    }
    if (remove_outliers == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
      cell_id_to_remove =  population_dataframe[which(population_dataframe$is.extreme == TRUE),'Cell_id']
    }
    
    if (length(cell_id_to_remove)!=0){
      population_dataframe=population_dataframe[-which(population_dataframe$Cell_id %in% cell_id_to_remove), ]
    }
    
    
  }
  
  if (what_to_return=='DF_without_outliers'){
    population_dataframe_WO_outliers=population_dataframe
    return(population_dataframe_WO_outliers)
  }
  #### if required, remove outliers per factor levels
  
  removed_levels=c()
 
  #### remove factor combination with less than 3 observations
  
  categories_count <- population_dataframe%>%
    group_by_(factor)%>%summarise(Count=n())
  
  
  for (line in seq(1,nrow(categories_count))){
    if (categories_count[line,'Count'] <3){
      
      population_dataframe=population_dataframe[-which(population_dataframe[,factor]==as.character(categories_count[line,factor][[1]])),]
      
      removed_level = paste0(as.character(categories_count[line,factor][[1]]))
      removed_levels=append(removed_levels,removed_level)
      
    }
  }
  population_dataframe$Cell_id=as.factor(population_dataframe$Cell_id)
  population_dataframe$Cell_id <- droplevels(population_dataframe$Cell_id)
  if (what_to_return=='Removed_levels'){
    return(removed_levels)
  }
  
  categories_count<- population_dataframe%>%
    group_by_(factor)%>%summarise(Count=n())
  population_dataframe[,factor]=droplevels( population_dataframe[,factor])
  
  if (what_to_return=='Categories_count'){
    return(categories_count)
  }
  #### remove factor combination with less than 3 observations
  if (what_to_return=='DF_without_removed_levels'){
    return(population_dataframe)
  }
  #### perform Shapiro normality test for remaining factors
  
  normality_table=assess_normality(population_dataframe,feature_col,factor)
  if (what_to_return=='Normality_table'){
    return(normality_table)
  }
  
  #### if any p_val for the Shapiro test if lower than 0.05 --> can't assume normality --> go for unpa
  
  
  normality_p_val <- normality_table$p>.05
  if (FALSE %in% normality_p_val){all.normal=FALSE}
  else{all.normal=TRUE}
  
  #### check variance homogeneity
  
  variance_homogeneity_table=assess_variance_homogeneity(population_dataframe,feature_col,factor)
  if (what_to_return=='Variance_homogeneity_table'){
    return(variance_homogeneity_table)
  }
  
  variance_homogeneity_p_val <- variance_homogeneity_table$p>.05
  if (FALSE %in% variance_homogeneity_p_val){all.var_homogen=FALSE}
  else{all.var_homogen=TRUE}
  
  if (all.normal ==TRUE & all.var_homogen==TRUE){
    Variance_test='ANOVA'
  }
  else if (all.normal ==FALSE | all.var_homogen==FALSE){
    Variance_test='Kruskal-Wallis'
  }
  
  
  if (Variance_test=='ANOVA'){
    
    variance_formula <- as.formula(paste0(feature_col,'~',factor))
    res <- population_dataframe %>% anova_test(variance_formula)
    
    is.difference <- res$p<0.05
    variance_test_table <- data.frame(Method=c(res$method),
                                      Size_effect=c(res$ges),
                                p_value=c(res$p))
    
    if (is.difference == TRUE){
      pwc_formula = as.formula( paste0(feature_col,'~', factor) )
      pwc_table <- population_dataframe %>% tukey_hsd(pwc_formula)
      
    }
    else{pwc_table <-data.frame(matrix(nrow = 0, ncol = 0) )}
    
  }
  
  if (Variance_test == 'Kruskal-Wallis'){
    variance_formula <- as.formula(paste0(feature_col,'~',factor))
    res <- population_dataframe %>% kruskal_test(variance_formula)
    
    is.difference <- res$p<0.05
    size_effect_table=population_dataframe %>% kruskal_effsize(variance_formula)
    variance_test_table <- data.frame(Method=c(res$method),
                                      Size_effect=c(size_effect_table$effsize),
                                      p_value=c(res$p))
    
    if (is.difference == TRUE){
      pwc_formula = as.formula( paste0(feature_col,'~', factor) )
      pwc_table <- population_dataframe %>% dunn_test(pwc_formula,p.adjust.method = 'bonferroni')
      
    }
    else{pwc_table <-data.frame(matrix(nrow = 0, ncol = 0) )}
    
  }
  
  if(what_to_return=='Variance_test_original_table'){
    return(res)
  }
  
  if (what_to_return=='Variance_test'){
    return(variance_test_table)
  }
  
  if (what_to_return == 'PWC_without_position'){
    return(pwc_table)
  }
  if (what_to_return=='PWC'){
    if (is.difference == TRUE){
      pwc_table <- pwc_table %>% add_xy_position(x = factor)
    }
    return(pwc_table)
  }
  
  
  
  
  return (removed_levels)
  
}

