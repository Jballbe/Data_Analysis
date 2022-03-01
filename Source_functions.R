
#This function check if the user has the required packages, if not install them, and load them 
have_library <- function (required_packages){
  install.packages(setdiff(required_packages,rownames(installed.packages())))
  print ("All required packages installed")
  for (package_name in required_packages){
    library(package_name,character.only =TRUE);
  }
  print("All required packages loaded")
}
required_packages=c("Cairo","plyr","stringr","shiny","dplyr","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
#Check if the user have all required libraries and if not, install them
have_library(required_packages = required_packages)

#This function create the full dataset table 
create_fulldataset <- function(population_class, data_file, nbfactors){
  #transform the factors columns into factor type
  for (elt in seq(1,(nbfactors+1))){
    population_class[,elt]=as.factor(population_class[,elt])
  }
  #remove unit line from data_file
  unit_dict=data_file[1,2:ncol(data_file)]
  
  data_file=data_file[2:nrow(data_file),]
  
  #create full dataset
  full_dataset=data.frame(merge(population_class,data_file,all=T))
  new_row_names=c(full_dataset[,1])
  full_dataset=full_dataset[,2:(ncol(full_dataset))]
  
  rownames(full_dataset)=new_row_names
  for (elt in seq((nbfactors+1),ncol(full_dataset))){
    full_dataset[,elt]=as.numeric(full_dataset[,elt])
  }
  factor_list=colnames(full_dataset[,1:nbfactors])
  #Remove unwanted data
  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Neuron"),]

  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Glia"),]

  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Unspecified"),]

  
  variable_list=colnames(full_dataset[(nbfactors+1):ncol(full_dataset)])
  Firing_Type=full_dataset[,2]
  factor_columns=full_dataset[,1:nbfactors]
  Species=full_dataset[,1]
  return (list("full_dataset"=full_dataset,
               "factor_list"=factor_list,
               "Firing_Type"=Firing_Type,
               "Species"=Species,
               "variable_list"=variable_list,
               "unit_dict"=unit_dict,
               "factor_columns"=factor_columns))
}

#Perform tests (data normality and variance homogeneity) to know which test is to be performed



parametric_test <- function(full_dataset, nbfactors, myfactor){
  #options(scipen = -100, digits=3)
  variable_list=colnames(full_dataset[(nbfactors+1):ncol(full_dataset)])
  #Create a table for normality test
  normality_p_value_table=data.frame(c(rep(0,length(variable_list))))
  normality_p_value_table=t(normality_p_value_table)
  normality_p_value_table=rbind(normality_p_value_table,c(rep(0,length(variable_list))))
  colnames(normality_p_value_table)=variable_list
  #Create a table for homogeneity test
  homogeneity_p_value_table=data.frame(c(rep(0,length(variable_list))))
  homogeneity_p_value_table=t(homogeneity_p_value_table)
  homogeneity_p_value_table=rbind(homogeneity_p_value_table,c(rep(0,length(variable_list))))
  colnames(homogeneity_p_value_table)=variable_list
  #Create a table to indicate the required test
  Variance_test=data.frame(c(rep("Anova",length(variable_list))))
  Variance_test=t(Variance_test)
  colnames(Variance_test)=variable_list

  Mean_Difference=data.frame(c(rep("No",length(variable_list))))
  Mean_Difference=t(Mean_Difference)
  colnames(Mean_Difference)=variable_list

  Which_groups=data.frame(c(rep("None",length(variable_list))))
  Which_groups=t(Which_groups)
  colnames(Which_groups)=variable_list
  #Gather all the table into Hypothesis_table
  Hypothesis_table=rbind(data.frame(normality_p_value_table),data.frame(homogeneity_p_value_table),data.frame(Variance_test),data.frame(Mean_Difference),data.frame(Which_groups))
  rownames(Hypothesis_table)=c("Normality p_value","Normal distribution","Homogeneity p_values","Variances homogeneous","Variance_test","Is there Mean difference?","Between")

  

  for (current_variable in variable_list){
    current_data=full_dataset
    #test normality of the data
    current_shapiro_test=shapiro_test(data=current_data[,current_variable])$p.value
    Hypothesis_table["Normality p_value",current_variable]=formatC(current_shapiro_test,digits=3,format='e')

    if (current_shapiro_test>0.05){
      Hypothesis_table["Normal distribution",current_variable]="Yes"
    }
    else{
      Hypothesis_table["Normal distribution",current_variable]="No"
      Hypothesis_table["Variance_test",current_variable]="KW"
    }

    #test variance homogeneity

    current_formula=as.formula(paste0(current_variable," ~ ",myfactor))
    current_levene_test=levene_test(data=current_data,formula=current_formula)$p
    Hypothesis_table["Homogeneity p_values",current_variable]=formatC(current_levene_test,digits=3,format='e')

    if (current_levene_test>0.05){
      Hypothesis_table["Variances homogeneous",current_variable]="Yes"
    }

    else{
      Hypothesis_table["Variances homogeneous",current_variable]="No"
      Hypothesis_table["Variance_test",current_variable]="KW"
    }

   
    #Perform Mean difference test (KW or ANOVA)
    if (Hypothesis_table["Variance_test",current_variable]=="KW"){
      variable_test=kruskal_test(current_data,formula = current_formula)
      
    }
    else{
      anova_test(current_data,formula = current_formula)
      variable_test=anova_test(full_dataset,formula = current_formula)
    }

    # If applicable, perform pair-wise comparison
    if (variable_test$p<0.05){
      if (Hypothesis_table["Variance_test",current_variable]=="KW"){
      Hypothesis_table["Is there Mean difference?",current_variable]="Yes"
      current_dunn_test=dunn_test(current_data,formula=current_formula,p.adjust.method = "bonferroni")
      

    # Get the group name and significance level
      all_pair=""
    for (pwc in seq(nrow(current_dunn_test))){
      
      if (current_dunn_test[pwc,"p.adj"]<0.05){
        current_pair=as.character(paste0(current_dunn_test[pwc,"group1"],current_dunn_test[pwc,"p.adj.signif"],current_dunn_test[pwc,"group2"]))
        all_pair=paste(all_pair,current_pair,sep='\n')
        
      }
    
    
    }
      Hypothesis_table["Between",current_variable]=all_pair
      }
      
      if (Hypothesis_table["Variance_test",current_variable]=="Anova"){
        Hypothesis_table["Is there Mean difference?",current_variable]="Yes"
       # current_Tukey_test=tukey_hsd(current_data,formula=current_formula,p.adjust.method = "bonferroni")
        current_Tukey_test=tukey_hsd(current_data,formula=current_formula)
        
        # Get the group name and significance level
        all_pair=""
        for (pwc in seq(nrow(current_Tukey_test))){
          
          if (current_Tukey_test[pwc,"p.adj"]<0.05){
            current_pair=as.character(paste0(current_Tukey_test[pwc,"group1"],current_Tukey_test[pwc,"p.adj.signif"],current_Tukey_test[pwc,"group2"]))
            all_pair=paste(all_pair,current_pair,sep='\n')
            
          }
          
          
        }
        Hypothesis_table["Between",current_variable]=all_pair
      }
    }
  }
  Hypothesis_table=data.frame(Hypothesis_table)
  return(Hypothesis_table)
}



#This function saves the plots selected by the user
saveallfigures <- function(Hypothesis_table, full_dataset,saving_path, file_name, nbfactors, myfactor, variable_to_save){
  
  
  #For each variable selected generate the plot
  
    formula=as.formula(paste0(variable_to_save," ~ ",myfactor))
    if (Hypothesis_table["Variance_test",variable_to_save]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    
    if (variable_test$p<0.05){
      if (Hypothesis_table["Variance_test",variable_to_save]=="KW"){
      current_dunn_test=dunn_test(full_dataset,formula=formula,p.adjust.method = "bonferroni")
      current_dunn_test=add_xy_position(current_dunn_test,x=myfactor)
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable_to_save]))+
        stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      if (Hypothesis_table["Variance_test",variable_to_save]=="Anova"){
        current_Tukey_test=tukey_hsd(current_data,formula=formula)
        current_Tukey_test=add_xy_position(current_Tukey_test,x=myfactor)
        variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable_to_save]))+
          stat_pvalue_manual(current_Tukey_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_Tukey_test))
      }
    }
    else{
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable_to_save]))+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    }
    
    myplot=ggarrange(variable_plot,ncol=1,nrow=1)
    #Save the plot in landscape format
    ggsave(filename = paste0(file_name,".pdf"),plot=myplot,path=saving_path,device = cairo_pdf,width=297,height = 210,units="mm")
    
  
  
  
  
}



count_samples <- function (full_dataset,nbfactors,myfactor,nbvariable){
  
  table_count=data.frame(matrix(0,nrow =length(levels(full_dataset[,myfactor])),ncol=nbvariable))
  
  colnames(table_count)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  rownames(table_count)=c(levels(full_dataset[,myfactor]))

  
  
    
    for (mylevel in levels(full_dataset[,myfactor])){
      current_data=full_dataset[-which(full_dataset[,myfactor]!=mylevel),]
      
      for (current_variable in colnames(table_count)){
        table_count[mylevel,current_variable]=round(nrow(current_data[-which(current_data[,current_variable]=="NaN"),]),digits = 1)
    
    }
    }
  
  return(table_count)
}

get_basic_stat <- function(full_dataset, nbfactors, myfactor){
 
  variable_list=colnames(full_dataset[(nbfactors+1):ncol(full_dataset)])
  levels_list=levels(full_dataset[,myfactor])
  mean_stat_table=data.frame(matrix(0,nrow =length(levels_list), ncol=length(variable_list)))
  
  
  colnames(mean_stat_table)=variable_list
  rownames(mean_stat_table)=c(levels_list)
  
  sd_stat_table=data.frame(matrix(0,nrow =length(levels_list), ncol=length(variable_list)))
  colnames(sd_stat_table)=variable_list
  rownames(sd_stat_table)=c(levels_list)
  for (variable in variable_list){
    
   
    current_data=full_dataset
    
    for (level in levels_list){
      
      
      mean_stat_table[level,variable]=mean(current_data[which(current_data[,myfactor]==level),variable])
      sd_stat_table[level,variable]=sd(current_data[which(current_data[,myfactor]==level),variable])
    }
  }
  full_stat_table=rbind(mean_stat_table,sd_stat_table)
  
  Stat=c(rep("Mean",length(levels_list)),rep("SD",length(levels_list)))
  
  stat_table=data.frame(cbind(Stat,full_stat_table))
  
  if(myfactor=="Firing_Type"){
    toremove=c("Neuron","Glia","Unspecified","Neuron1","Glia1","Unspecified1")
    
    stat_table=stat_table[!(rownames(stat_table) %in% toremove),]
    
    
    mean_stat_table=mean_stat_table[!(rownames(mean_stat_table) %in% toremove),]
    
    sd_stat_table=sd_stat_table[!(rownames(sd_stat_table) %in% toremove),]
    
  }
  
  mean_stat_table=data.frame(mean_stat_table)
  return(list("stat_table"=stat_table,
              "mean_table"=mean_stat_table,
              "sd_table"=sd_stat_table))
}

prepare_for_ggplot <- function(datatable,time_list,variable_to_analyse,nbfactors){
  ggdatatable=data.frame(matrix(0,nrow=(length(time_list)*nrow(datatable)),ncol=nbfactors))
  colname=c()
  time_col=c()
  data_col=c()
  for (elt in seq(nbfactors)){
    ggdatatable[,elt]=data.frame(rep(datatable[,elt]),
                                 length(time_list))
    colname=c(colname,colnames(datatable)[elt])
  }
  
  for (elt in seq((nbfactors+1),ncol(datatable))){
  time_point=str_remove(colnames(datatable)[elt],"ms")
  time_point=str_remove(time_point,"_spikes")
  time_point=as.numeric(time_point)
 
  current_time_col=rep(time_point,nrow(datatable))
  time_col=c(time_col,current_time_col)
  current_data_col=as.numeric(datatable[,elt])
  data_col=c(data_col,current_data_col) 
  }
  
  colname=c(colname,"Time",as.character(variable_to_analyse))
  ggdatatable=data.frame(cbind(data.frame(ggdatatable),
                               data.frame(time_col),
                               data.frame(data_col)))
  
  colnames(ggdatatable)=colname
  
  return(list("ggdatatable"=ggdatatable))
  
}

getsd <- function(ggdatatable,myfactor,variable_to_analyse,perTimeonly=FALSE){
  
  
  ggdatatable=data.frame(ggdatatable)
  ggdatatable[,4]=as.numeric(ggdatatable[,4])
  
  if (perTimeonly ==TRUE){
    sd_table <- ggdatatable %>%
      group_by(.data[["Time"]]) %>%
      summarise(SD=sd(.data[[variable_to_analyse]],na.rm=TRUE))
  }
  
  if (perTimeonly ==FALSE){
    
    sd_table <- ggdatatable %>%
      group_by(.data[["Time"]],.data[[myfactor]]) %>% 
    summarise(SD=sd(.data[[variable_to_analyse]],na.rm=TRUE))
    
    
  }
  
  return(list("sd_table"=sd_table))
}

getmean <- function(ggdatatable,myfactor,variable_to_analyse,perTimeonly=FALSE){
  if (perTimeonly ==TRUE){
    mean_table <- ggdatatable %>%
      group_by(.data[["Time"]]) %>%
      summarise(Mean=mean(.data[[variable_to_analyse]],na.rm=TRUE))
  }
  
  if (perTimeonly ==FALSE){
    mean_table <- ggdatatable %>%
      group_by(.data[["Time"]],.data[[myfactor]]) %>% 
      summarise(Mean=mean(.data[[variable_to_analyse]],na.rm=TRUE))
  }
  
  return(list("mean_table"=mean_table))
}

perform_t_test <- function(full_dataset,myfactor,time_list){
  
  mylevels=levels(full_dataset[,as.character(myfactor)])
  
  t_test_table=data.frame(matrix(0,
                                 nrow=length(time_list),
                                 ncol=(length(mylevels)+1)))
  rownames(t_test_table)=time_list
  colnames(t_test_table)=c(mylevels,"All Categories")
  full_dataset=data.frame(full_dataset)
  for(elt in time_list){
    full_dataset[,elt]=as.numeric(full_dataset[,elt])
  }
 
  for(current_time in time_list){
    for (current_level in mylevels){
     
      tryCatch(t_test_table[current_time,current_level] <- t.test(full_dataset[which(full_dataset[,myfactor]==current_level),current_time])$p.value,
               error=function(e){
                 
                 t_test_table[current_time,current_level]=-1
               })
      
      if (t_test_table[current_time,current_level]==0){
        t_test_table[current_time,current_level] <- "X"
      }
    }
    t_test_table[current_time,"All Categories"]=t.test(full_dataset[,current_time])$p.value
  }

  print(t_test_table)
}

overtime_basic_stat <- function(dataset,myfactor,time_list){
  mylevels=levels(dataset[,as.character(myfactor)])
  mean_table=data.frame(matrix(0,nrow=length(time_list),
                               ncol=(length(mylevels)+1)))
  rownames(mean_table)=time_list
  colnames(mean_table)=c(mylevels,"All Categories")
  dataset=data.frame(dataset)
  for(elt in time_list){
    dataset[,elt]=as.numeric(dataset[,elt])
  }
  
  sd_table=data.frame(matrix(0,nrow=length(time_list),
                             ncol=(length(mylevels)+1)))
  rownames(sd_table)=time_list
  colnames(sd_table)=c(mylevels,"All Categories")
  
  for(current_time in time_list){
    for (current_level in mylevels){
     
      mean_table[current_time,current_level] <- mean(dataset[which(dataset[,myfactor]==current_level),current_time],na.rm=TRUE)
      sd_table[current_time,current_level] <- sd(dataset[which(dataset[,myfactor]==current_level),current_time],na.rm=TRUE)
    }
    mean_table[current_time,"All Categories"] <- mean(dataset[,current_time],na.rm=TRUE)
    sd_table[current_time,"All Categories"] <- sd(dataset[,current_time],na.rm=TRUE)
  }
  stat_mean=c(rep("Mean",length(time_list)))
  
  stat_sd=c(rep("SD",length(time_list)))
  mean_table=data.frame(cbind(data.frame(stat_mean),data.frame(mean_table)))
  colnames(mean_table)[1] <- "Stat"
  sd_table=data.frame(cbind(data.frame(stat_sd),data.frame(sd_table)))
  colnames(sd_table)[1] <- "Stat"
  return(list("mean_table"=mean_table,
              "sd_table"=sd_table))
}