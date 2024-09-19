
RM_anova <- function(df,columns_list, feature_col, factor_col){
  #Gain_500ms_Current_based, Gain_Hz.pA_model
  #Threshold_500ms_Current_based, Threshold_pA_model
  # Time_constant_ms, Time_constant_ms_model
  #Input_Resistance_GOhms, Input_Resistance_GOhms_model
  sub_table = comparison_table[,columns_list]
  sub_table=na.omit(sub_table)
  #View(new_table)
  new_table <- sub_table %>%
    gather(key = Cell_or_Model, value = Adaptation, Adaptation_index_500ms, Adaptation_index_model) %>%
    convert_as_factor(Cell_id, Model_id)
  
  print('"oijfr')
  
  new_table$Cell_or_Model <- gsub("Adaptation_index_500ms", "Cell", new_table$Cell_or_Model)
  new_table$Cell_or_Model <- gsub("Adaptation_index_model", "Model", new_table$Cell_or_Model)
  
  df_outlier = dataframe_outliers(new_table,feature_col, factor_col)
 
  
  
  
  cell_id_to_remove =  df_outlier[which(df_outlier$is.extreme == TRUE),'Cell_id']
  new_table_without_outlier=df_outlier[-which(df_outlier$Cell_id %in% cell_id_to_remove), ]
  new_table_without_outlier$Cell_or_Model <- as.factor(new_table_without_outlier$Cell_or_Model)
  normality_table=assess_normality(new_table_without_outlier,feature_col, factor_col)
  
  print(normality_table)
  
  res=skillingsMackTest(y=new_table_without_outlier[,'Adaptation'],groups=new_table_without_outlier[,'Cell_or_Model'],blocks=new_table_without_outlier[,'Cell_id'])
  is.difference <- res$p.value<0.05
  variance_test <- data.frame(Method=c(res$method),
                              p_value=c(res$p.value))
  
  
  if (is.difference == TRUE){
    my_formula = as.formula( paste0(feature_col,'~', factor_col) )
    pwc <- wilcox_test(data=new_table_without_outlier, formula=my_formula, paired = TRUE, p.adjust.method = "bonferroni")
    
  }
  else{
    pwc = data.frame(matrix(nrow = 0, ncol = 0))
  }
  
  print('herjner')
  if (is.difference == TRUE){
    pwc <- pwc %>% add_xy_position(x = 'Cell_or_Model')
  }
  
  
  
  
  PWC_plot=ggplot2::ggplot(new_table_without_outlier,aes_string( x = 'Cell_or_Model', y = 'Adaptation'))+
    ggplot2::geom_boxplot(outlier.shape =NA )+
    ggplot2::geom_jitter(mapping=aes_string(alpha=.8),width = 0.25) +stat_pvalue_manual(pwc, tip.length = 0, hide.ns = TRUE)

  # print('okdoker')
  # View(pwc)
  # if(nrow(PWC_test_table)!=0){
  #   PWC_plot=PWC_plot+stat_pvalue_manual(pwc, tip.length = 0, hide.ns = FALSE) }
  # print('doekdodk')
  
  #Gain_500ms_Current_based, Gain_Hz.pA_model
  line_plot = ggplot2::ggplot(sub_table,aes_string( x = 'Adaptation_index_500ms', y = 'Adaptation_index_model',alpha=.8))+geom_point()+ geom_abline(intercept = 0, slope = 1,color="red")+xlim(0,25)+ylim(0,25)
  print(line_plot)
  #+
  # ggplot2::labs(
  #   subtitle = get_test_label(res, detailed = TRUE)
  # )
  
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
      
      for(linear_file in file_list){
        if (grepl("linear_values",linear_file)==TRUE){
          
          linear_values_file=data.frame(read.csv(file = paste0(directory,'/',linear_file),header=T,row.names = 1))
          
          linear_values_file=linear_values_file[,c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
          my_file = merge(my_file,linear_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
          break
        }
      }
      
      for(Adaptation_file in file_list){
        if (grepl("Adaptation",Adaptation_file)==TRUE){
        
          Adaptation_values_file=data.frame(read.csv(file = paste0(directory,'/',Adaptation_file),header=T,row.names = 1))
          
          Adaptation_values_file[["Obs"]] = NULL
          
          my_file = merge(my_file,Adaptation_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
          break
        }
      }
      
      unit_file=my_file[1,2:ncol(my_file)]
      
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
      
      linear_values_file=linear_values_file[2:nrow(linear_values_file),c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
      
      linear_values_file[,"Input_Resistance_GOhms"]=as.numeric(linear_values_file[,"Input_Resistance_GOhms"])
      linear_values_file[,"Time_constant_ms"]=as.numeric(linear_values_file[,"Time_constant_ms"])
      file_dict <- append(file_dict,list(linear_values_file))
      
      
      response_duration_list=append(response_duration_list,'Linear_Values')
    }
    
   
    
    else{
      
      if (grepl("Full_Feature_Table_",file)==TRUE){
        current_duration = str_split(file,'_')[[1]][6]
        # current_duration=str_remove(file, "Full_Feature_Table_")
        # current_duration=str_remove(current_duration, "Fernandez_")
        # current_duration=str_remove(current_duration,Selected_Response_Type)
        # current_duration=str_remove(current_duration,'_')
        # current_duration=gsub("\\D", "", file)
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1))
      }
      else if (grepl("FR_Hz_",file)==TRUE){
        current_duration=str_remove(file, "FR_Hz_")
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,skip=1))
      }
      else if(grepl("Adaptation",file)==TRUE){
        next
      }
      
      
      # if (Selected_Response_Type=="Time_based"){
      #   
      #   
      #   current_duration=as.numeric(str_remove(current_duration, "ms.csv"))
      # }
      # else{
      #   
      #   current_duration=as.numeric(str_remove(current_duration, ".csv"))
      # }
      # 
      current_duration=str_remove(current_duration, "ms")
      current_duration=str_remove(current_duration, ".csv")
      
      
      
      response_duration_list=append(response_duration_list,current_duration)
      
      for (test_file in file_list){
        if (grepl("linear_values",test_file)==TRUE){
          linear_values_file=data.frame(read.csv(file = paste0(directory,'/',test_file),header=T,row.names = 1))
          
          linear_values_file=linear_values_file[,c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
      
          my_file = merge(my_file,linear_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
        }
        
        if (grepl("Adaptation",test_file)==TRUE){
          Adaptation_values_file=data.frame(read.csv(file = paste0(directory,'/',test_file),header=T,row.names = 1))
          Adaptation_values_file[['Obs']] = NULL
          
          my_file = merge(my_file,Adaptation_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
         
        }
      }
      
      #colnames(my_file) <- my_colnames
      for (col in seq(2,ncol(my_file))){
        my_file[,col]=as.numeric(my_file[,col])
      }
      
      
      
      
      file_dict <- append(file_dict,list(my_file))
    }
   
  }
  
  names(file_dict) <- response_duration_list
  print(response_duration_list)
  return(file_dict)
  
}


load_population_csv_file_repeated_measures <- function(directory,file_list,Selected_Response_Type){
  
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
      
      for(linear_file in file_list){
        if (grepl("linear_values",linear_file)==TRUE){
          
          linear_values_file=data.frame(read.csv(file = paste0(directory,'/',linear_file),header=T,row.names = 1))
          
          linear_values_file=linear_values_file[,c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
          my_file = merge(my_file,linear_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
          break
        }
      }
      
      for(Adaptation_file in file_list){
        if (grepl("Adaptation",Adaptation_file)==TRUE){
          
          Adaptation_values_file=data.frame(read.csv(file = paste0(directory,'/',Adaptation_file),header=T,row.names = 1))
          
          Adaptation_values_file[["Obs"]] = NULL
          
          my_file = merge(my_file,Adaptation_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
          break
        }
      }
      
      unit_file=my_file[1,2:ncol(my_file)]
      
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
      
      linear_values_file=linear_values_file[2:nrow(linear_values_file),c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
      
      linear_values_file[,"Input_Resistance_GOhms"]=as.numeric(linear_values_file[,"Input_Resistance_GOhms"])
      linear_values_file[,"Time_constant_ms"]=as.numeric(linear_values_file[,"Time_constant_ms"])
      file_dict <- append(file_dict,list(linear_values_file))
      
      
      response_duration_list=append(response_duration_list,'Linear_Values')
    }
    
    
    
    else{
      
      if (grepl("Full_Feature_Table_",file)==TRUE){
        current_duration = str_split(file, "_")[[1]][6]
        
        # current_duration=str_remove(file, "Full_Feature_Table_")
        # current_duration=str_remove(current_duration, "Fernandez_")
        # current_duration=str_remove(current_duration,Selected_Response_Type)
        # current_duration=str_remove(current_duration,'_')
        # current_duration=gsub("\\D", "", file)
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,row.names = 1))
      }
      else if (grepl("FR_Hz_",file)==TRUE){
        current_duration=str_remove(file, "FR_Hz_")
        my_file=data.frame(read.csv(file = paste0(directory,'/',file),header=T,skip=1))
      }
      else if(grepl("Adaptation",file)==TRUE){
        next
      }
      current_duration=str_remove(current_duration, "ms")
      current_duration=str_remove(current_duration, ".csv")
      
      
      
      response_duration_list=append(response_duration_list,current_duration)
      
      for (test_file in file_list){
        if (grepl("linear_values",test_file)==TRUE){
          linear_values_file=data.frame(read.csv(file = paste0(directory,'/',test_file),header=T,row.names = 1))
          
          linear_values_file=linear_values_file[,c('Cell_id','Input_Resistance_GOhms','Time_constant_ms')]
          
          my_file = merge(my_file,linear_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
        }
        
        if (grepl("Adaptation",test_file)==TRUE){
          Adaptation_values_file=data.frame(read.csv(file = paste0(directory,'/',test_file),header=T,row.names = 1))
          Adaptation_values_file[['Obs']] = NULL
          
          my_file = merge(my_file,Adaptation_values_file,by.x = c('Cell_id'), by.y =c('Cell_id'),all.x=T)
          
        }
      }
      
      
      for (col in seq(2,ncol(my_file))){
        my_file[,col]=as.numeric(my_file[,col])
      }
      
      
      
      
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
    
    current_file <- current_file[,c('Cell_id',Feature_to_analyse,'Input_Resistance_GOhms')]
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
    
    # if (Response_type == 'Time_based'){
    #   colnames(current_file) <- c('Cell_id',paste0(file,'ms'))
    # }
    # else{
    #   colnames(current_file) <- c('Cell_id',paste0(str_replace(Response_type,'based',""),file))
    # }
    
    colnames(current_file) <- c('Cell_id',file)

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

create_full_df_correlation <- function(file_import,Response_type, subset_filter){
  
  new_df = file_import[['Population_Class']]
  for (category in names(subset_filter)){
    new_df <-new_df[which(new_df[,category] %in% subset_filter[category][[1]]),]
    
  }
  
  file_list <- names(file_import)
  
  file_list = file_list[file_list != 'Population_Class']
  
  file_list = file_list[file_list != 'Unit_File']
  
  
  unit_line=c()
  unit_line_name = c()
  list_to_return = c()
  names_list_to_return = c()
  
  for (file in file_list){
    
    if (file == "Linear_Values"){
      current_file = file_import[[as.character(file)]]
      current_file = current_file[2:nrow(current_file),]
      
      new_columns_names=c()
      columns_names = colnames(current_file)
      for (column_name in columns_names){
        if (column_name != 'Cell_id'){
          splited_name = strsplit(column_name, split = "_")
          #new_column_name = as.character(paste0(splited_name[[1]][1],"_",splited_name[[1]][2] ))
          unit = splited_name[[1]][3]
          
          new_columns_names = append(new_columns_names,column_name)
          
          unit_line = append(unit_line,unit)
          unit_line_name = append(unit_line_name,as.character(column_name))
          
        }
        else{
          new_columns_names = append(new_columns_names,"Cell_id")
          
        }
        
      }
    }
    
    else{
      if (Response_type == 'Time_based'){
        suffix <- paste0(file,'ms')
      }
      else{
        suffix <- c('Cell_id',paste0(str_replace(Response_type,'based',""),file))
      }
      current_file = file_import[[as.character(file)]]
      current_file = current_file[2:nrow(current_file),]
      current_file = select(current_file, -c("Input_Resistance_GOhms","Time_constant_ms"))
      unit_file = file_import[["Unit_File"]]
      
      columns_names = colnames(current_file)
      new_columns_names=c()
      for (column_name in columns_names){
        if (column_name != 'Cell_id'){
          unit = unit_file[as.character(column_name)][1,1]
          new_column_name = as.character(paste0(column_name,'_',suffix))
          new_column_name = as.character(paste0(new_column_name,'_',unit))
          
          new_columns_names = append(new_columns_names,new_column_name)
          
          
          unit_line = append(unit_line,unit)
          unit_line_name = append(unit_line_name,as.character(new_column_name))
        }
        else{
          new_columns_names = append(new_columns_names,"Cell_id")
          
        }
      
      }
    }
    
    colnames(current_file) <- new_columns_names
    
    new_df = merge(new_df, current_file,by="Cell_id",no.dups=FALSE)
    
  }

  
  names(unit_line) <- unit_line_name
  list_to_return = append(list_to_return,list(unit_line))
  list_to_return = append(list_to_return,list(new_df))
  names_list_to_return = append(names_list_to_return,"Unit_line")
  names_list_to_return = append(names_list_to_return,"Full_df_correlation")
  
  names(list_to_return) <- names_list_to_return
  
  return (list_to_return)
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
dataframe_outliers_second <- function(population_dataframe,feature_col, Distance_from_Quartiles_variance){
  
  
  
  # Calculate the summary statistics needed for outlier detection
  Q1 <- quantile(population_dataframe[[feature_col]], probs = 0.25, na.rm = TRUE)  # 1st Quartile
  Q3 <- quantile(population_dataframe[[feature_col]], probs = 0.75, na.rm = TRUE)  # 3rd Quartile
  IQR_value <- IQR(population_dataframe[[feature_col]], na.rm = TRUE)               # Interquartile Range (IQR)
  
  # Calculate the outlier thresholds
  lower_bound <- Q1 - Distance_from_Quartiles_variance * IQR_value
  upper_bound <- Q3 + Distance_from_Quartiles_variance * IQR_value
  
  # Add a new column "is_outlier" to mark outliers
  population_dataframe$is_outlier <- (population_dataframe[[feature_col]] < lower_bound) | (population_dataframe[[feature_col]] > upper_bound)
  
  
  return (population_dataframe)
  
  
}

dataframe_outliers_per_factor_level <- function(population_dataframe, feature_col, factor_col, Distance_from_Quartiles_variance) {
  # Loop through each level of the factor_col
  population_dataframe$is_outlier <- NA  # Initialize the outlier column
  
  mytest_pop = population_dataframe
  
  population_dataframe$Ind_var <- factor(population_dataframe$Ind_var)
  
  for (level in unique(population_dataframe[["Ind_var"]])) {
    # Subset the dataframe for the current factor level
    subset_data <- population_dataframe[population_dataframe[["Ind_var"]] == level, ]
    
    # Calculate the summary statistics for this subset
    Q1 <- quantile(subset_data[[feature_col]], probs = 0.25, na.rm = TRUE)  # 1st Quartile
    Q3 <- quantile(subset_data[[feature_col]], probs = 0.75, na.rm = TRUE)  # 3rd Quartile
    IQR_value <- IQR(subset_data[[feature_col]], na.rm = TRUE)              # Interquartile Range (IQR)
    
    # Calculate the outlier thresholds
    lower_bound <- Q1 - Distance_from_Quartiles_variance * IQR_value
    upper_bound <- Q3 + Distance_from_Quartiles_variance * IQR_value
   
    # Mark outliers for this subset
    population_dataframe$is_outlier[population_dataframe[["Ind_var"]] == level] <- 
      (subset_data[[feature_col]] < lower_bound) | (subset_data[[feature_col]] > upper_bound)
  }
  
  return(population_dataframe)
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

perform_repeated_measure_one_way_ANOVA <- function(population_dataframe,feature_col,factor,remove_outliers, distance_to_quartiles, what_to_return){
  #### if required, remove outliers per factor levels
  print('HEEEEEEEEEEEEEEEEERE')
  population_dataframe = dataframe_outliers_per_factor_level(population_dataframe,feature_col,"Ind_Var", distance_to_quartiles)
  #population_dataframe=dataframe_outliers(population_dataframe,feature_col,factor)
  
  if (what_to_return=='Oulier_df'){
    oulier_df=population_dataframe
    
    return(oulier_df)
  }
  
  if (remove_outliers == TRUE){
    
    cell_id_to_remove =  population_dataframe[which(population_dataframe$is_outlier == TRUE),'Cell_id']
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
  print('JGTGO')
  
  
  
 
  file_list <- names(file_import)
  
  file_list = file_list[file_list != 'Population_Class']
  file_list = file_list[file_list != 'Linear_Values']
  file_list = file_list[file_list != 'Unit_File']
  file_list = mixedsort(file_list)
  full_data_frame=data.frame(current_population_file)
  
  current_file=file_import[[file_to_select]]
  current_file <- current_file[,c('Cell_id',Feature_to_analyse,'Input_Resistance_GOhms')]
  
  
  full_data_frame <- merge(full_data_frame,current_file)
  
 
  if(keep_na==F){
    full_data_frame=full_data_frame%>%drop_na()
  }
  
 
  # 
  # if ('Linear_Values' %in% names(file_import)){
  #   Linear_values=file_import$Linear_Values
  #   file_import_test=file_import
  #  
  #   full_df=full_data_frame
  #   
  #   full_data_frame=inner_join(full_data_frame, Linear_values, by=c('Cell_id'='Cell_id'))
  # }

  return (full_data_frame)
}


assess_variance_homogeneity <- function(population_dataframe,feature_col,factor){
  population_dataframe=data.frame(population_dataframe)
  my_formula <- as.formula(paste0(feature_col,'~',factor))
  levene_table <- population_dataframe %>% 
    levene_test(my_formula)
  return(levene_table)
}

perform_ANOVA <- function(population_dataframe,feature_col,factor,remove_outliers, distance_to_quartiles,what_to_return){
  
  #### if required, remove outliers per factor levels
  
  
  #population_dataframe=dataframe_outliers(population_dataframe,feature_col,factor)
  population_dataframe=dataframe_outliers_second(population_dataframe,feature_col,distance_to_quartiles)
  
  if (what_to_return=='Outlier_df'){
    outlier_df=population_dataframe
    
    return(outlier_df)
  }
  
  if (remove_outliers == TRUE){
    cell_id_to_remove =  population_dataframe[which(population_dataframe$is_outlier == TRUE),'Cell_id']
    if (length(cell_id_to_remove)!=0){
      population_dataframe=population_dataframe[-which(population_dataframe$Cell_id %in% cell_id_to_remove), ]
    }
  }
  
  
  
  # if (remove_outliers != 'None'){
  #   if (remove_outliers == 'Outliers (Q1/Q3 ± 1.5*IQ)'){
  #     cell_id_to_remove =  population_dataframe[which(population_dataframe$is.outlier == TRUE),'Cell_id']
  #     
  #   }
  #   if (remove_outliers == 'Extreme outliers (Q1/Q3 ± 3*IQ)'){
  #     cell_id_to_remove =  population_dataframe[which(population_dataframe$is.extreme == TRUE),'Cell_id']
  #   }
  #   
  #   if (length(cell_id_to_remove)!=0){
  #     population_dataframe=population_dataframe[-which(population_dataframe$Cell_id %in% cell_id_to_remove), ]
  #   }
  #   
  #   
  # }
  
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
    if (categories_count[line,'Count'] < 3){
      
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
  ## Compute category statistics
  categories_count <- get_statistics_table(population_dataframe, factor, feature_col)
  
  
  
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
    
    variance_test_table <- data.frame(Method="ANOVA",
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

get_statistics_table <- function(original_dataframe, factor, feature_col){
  
  global_dataframe <- original_dataframe
  global_dataframe[,factor] = "Full population"
  global_dataframe[,factor] <- as.factor(global_dataframe[,factor])
  
  
  mean_value <- mean(global_dataframe[,feature_col], na.rm = TRUE)       # Mean
  sd_value <- sd(global_dataframe[,feature_col], na.rm = TRUE)           # Standard Deviation
  median_value <- median(global_dataframe[,feature_col], na.rm = TRUE)   # Median
  quantiles <- quantile(global_dataframe[,feature_col], probs = c(0.25, 0.75), na.rm = TRUE)  # 1st and 3rd Quartiles
  Q1 <- quantiles[1]
  Q3 <- quantiles[2]
  min_value <- min(global_dataframe[,feature_col], na.rm = TRUE)         # Minimum
  max_value <- max(global_dataframe[,feature_col], na.rm = TRUE)         # Maximum 
  IQR <- Q3 - Q1 # Interquartile range
  # Calculate the number of non-missing observations
  nb_obs <- sum(!is.na(global_dataframe[,feature_col]))
  # Creating a table (data frame) with the summary statistics
  
  summary_table <- data.frame(
    
    Value = c(nb_obs, mean_value, sd_value,min_value, Q1, median_value, Q3,  max_value, IQR)
  )
  
  # Transpose the table
  summary_table <- as.data.frame(t(summary_table))
  
  # Set column names
  colnames(summary_table) <- c("Number of Observations", "Mean", "Standard Deviation", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Interquartile Range")
  
  # Add the "Data" column
  summary_table$Data <- "Full population"
  
  # Reorder the columns to place "Data" first
  summary_table <- summary_table[, c("Data", colnames(summary_table)[1:ncol(summary_table)-1])]
  
  
  
  # Get unique levels of the column "My_col"
  levels <- unique(original_dataframe[,factor])
  
  # Loop through each level of "My_col"
  for (level in levels) {
    # Subset the dataframe for the current level
    subset_table <- original_dataframe[original_dataframe[,factor] == level, feature_col, drop = FALSE]
    subset_data  <- subset_table[,feature_col]
    # Calculate summary statistics for the subset
    n_obs <- sum(!is.na(subset_data))                                      # Number of Observations
    mean_value <- mean(subset_data, na.rm = TRUE)                          # Mean
    sd_value <- sd(subset_data, na.rm = TRUE)                              # Standard Deviation
    min_value <- min(subset_data, na.rm = TRUE)                            # Minimum
    Q1 <- quantile(subset_data, probs = 0.25, na.rm = TRUE)                # 1st Quartile
    median_value <- median(subset_data, na.rm = TRUE)                      # Median
    Q3 <- quantile(subset_data, probs = 0.75, na.rm = TRUE)                # 3rd Quartile
    max_value <- max(subset_data, na.rm = TRUE)                            # Maximum
    IQR_value <- IQR(subset_data, na.rm = TRUE)                            # Interquartile Range
    
    new_summary_table <- data.frame(
      
      Value = c(n_obs, mean_value, sd_value,min_value, Q1, median_value, Q3,  max_value, IQR_value)
    )
    
    # Transpose the table
    new_summary_table <- as.data.frame(t(new_summary_table))
    
    # Set column names
    colnames(new_summary_table) <- c("Number of Observations", "Mean", "Standard Deviation", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Interquartile Range")
    
    # Add the "Data" column
    new_summary_table$Data <- as.character(level)
    
    # Reorder the columns to place "Data" first
    new_summary_table <- new_summary_table[, c("Data", colnames(new_summary_table)[1:ncol(new_summary_table)-1])]
    
    # Add a new row to the summary table for the current level
    summary_table <- rbind(summary_table, new_summary_table)
  }
  return (summary_table)
}


get_statistics_table_RM <- function(original_dataframe, factor, feature_col){
  
  
  # Transpose the table
  summary_table <- data.frame(matrix(nrow = 0, ncol = 10)) 
  
  # Set column names
  colnames(summary_table) <- c("Data","Number of Observations", "Mean", "Standard Deviation", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Interquartile Range")
  
 
  # Get unique levels of the column "My_col"
  levels <- unique(original_dataframe[,"Ind_var"])
  print("EREREREREREE")
  print(levels)
  # Loop through each level of "My_col"
  for (level in levels) {
    # Subset the dataframe for the current level
    print('HERERMOM')
    print(level)
    print(colnames(original_dataframe))
    print(feature_col)
    subset_table <- original_dataframe[original_dataframe[,"Ind_var"] == level, feature_col, drop = FALSE]
    print('LFRF')
    subset_data  <- subset_table[,feature_col]
    # Calculate summary statistics for the subset
    n_obs <- sum(!is.na(subset_data))                                      # Number of Observations
    mean_value <- mean(subset_data, na.rm = TRUE)                          # Mean
    sd_value <- sd(subset_data, na.rm = TRUE)                              # Standard Deviation
    min_value <- min(subset_data, na.rm = TRUE)                            # Minimum
    Q1 <- quantile(subset_data, probs = 0.25, na.rm = TRUE)                # 1st Quartile
    median_value <- median(subset_data, na.rm = TRUE)                      # Median
    Q3 <- quantile(subset_data, probs = 0.75, na.rm = TRUE)                # 3rd Quartile
    max_value <- max(subset_data, na.rm = TRUE)                            # Maximum
    IQR_value <- IQR(subset_data, na.rm = TRUE)                            # Interquartile Range
    
    new_summary_table <- data.frame(
      
      Value = c(n_obs, mean_value, sd_value,min_value, Q1, median_value, Q3,  max_value, IQR_value)
    )
    
    # Transpose the table
    new_summary_table <- as.data.frame(t(new_summary_table))
    
    # Set column names
    colnames(new_summary_table) <- c("Number of Observations", "Mean", "Standard Deviation", "Minimum", "1st Quartile", "Median", "3rd Quartile", "Maximum", "Interquartile Range")
    
    # Add the "Data" column
    new_summary_table$Data <- as.character(level)
    
    # Reorder the columns to place "Data" first
    new_summary_table <- new_summary_table[, c("Data", colnames(new_summary_table)[1:ncol(new_summary_table)-1])]
    
    # Add a new row to the summary table for the current level
    summary_table <- rbind(summary_table, new_summary_table)
  }
  return (summary_table)
}

