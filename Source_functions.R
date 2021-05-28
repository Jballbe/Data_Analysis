
#This function check if the user has the required packages, if not install them, and load them 
have_library <- function (required_packages){
  install.packages(setdiff(required_packages,rownames(installed.packages())))
  print ("All required packages installed")
  for (package_name in required_packages){
    library(package_name,character.only =TRUE);
  }
  print("All required packages loaded")
}

#This function create the full dataset table 
create_fulldataset <- function(population_class, data_file, nbfactors){
  #transform the factors columns into factor type
  for (elt in seq(1,nbfactors)){
    population_class[,elt]=as.factor(population_class[,elt])
  }
  #remove unit line from data_file
  data_file=data_file[2:nrow(data_file),]
  
  #create full dataset
  full_dataset=data.frame(merge(population_class,data_file,all=T))
  new_row_names=c(full_dataset[,1])
  full_dataset=full_dataset[,2:(ncol(full_dataset))]
  rownames(full_dataset)=new_row_names
  for (elt in seq((nbfactors+1),ncol(full_dataset))){
    full_dataset[,elt]=as.numeric(full_dataset[,elt])
  }
  #Remove unwanted data
  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Neuron"),]
  
  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Glia"),]
  
  full_dataset=full_dataset[-which(full_dataset[,nbfactors]=="Unspecified"),]
  
  factor_list=colnames(full_dataset[,1:nbfactors])
  variable_list=colnames(full_dataset[(nbfactors+1):ncol(full_dataset)])
  return (list("full_dataset"=full_dataset,
               "factor_list"=factor_list,
               "variable_list"=variable_list))
}


#Perform tests (data normality and variance homogeneity) to know which test is to be performed
parametric_test <- function(full_dataset, nbfactors, myfactor){
  
  #Create a table for normality test
  normality_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-(nbfactors))))
  normality_p_value_table=t(normality_p_value_table)
  normality_p_value_table=rbind(normality_p_value_table,c(rep(0,ncol(full_dataset)-(nbfactors))))
  colnames(normality_p_value_table)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  #Create a table for homogeneity test
  homogeneity_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-(nbfactors))))
  homogeneity_p_value_table=t(homogeneity_p_value_table)
  homogeneity_p_value_table=rbind(homogeneity_p_value_table,c(rep(0,ncol(full_dataset)-(nbfactors))))
  colnames(homogeneity_p_value_table)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  #Create a table to indicate the required test
  Variance_test=data.frame(c(rep("Anova",ncol(homogeneity_p_value_table))))
  Variance_test=t(Variance_test)
  colnames(Variance_test)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  #Gather all the table into Hypothesis_table
  Hypothesis_table=rbind(data.frame(normality_p_value_table),data.frame(homogeneity_p_value_table),data.frame(Variance_test))
  rownames(Hypothesis_table)=c("Normality p_value","Normal distribution","Homogeneity p_values","Variances homogeneous","Variance_test")
  
  for (elt in seq((nbfactors+1),ncol(full_dataset))){
    current_data=full_dataset
    #test normality of the data
    
    Hypothesis_table[1,elt-2]=round(shapiro_test(data=current_data[,elt])$p.value,digits=3)
    
    if (Hypothesis_table[1,elt-2]>0.05){
      Hypothesis_table[2,elt-2]="Yes"
    }
    else{
      Hypothesis_table[2,elt-2]="No"
      Hypothesis_table[5,elt-2]="KW"
    }
    
    #test variance homogeneity
    
    current_formula=as.formula(paste0(colnames(full_dataset[elt])," ~ ",myfactor))
    Hypothesis_table[3,elt-2]=round(levene_test(data=current_data,formula=current_formula)$p,digits=3)
    
    if (Hypothesis_table[3,elt-2]>0.05){
      Hypothesis_table[4,elt-2]="Yes"
    }
    
    else{
      Hypothesis_table[4,elt-2]="No"
      Hypothesis_table[5,elt-2]="KW"
    }
  }
  Hypothesis_table=data.frame(Hypothesis_table)
  return(Hypothesis_table)
}

#This function saves the plots selected by the user
saveallfigures <- function(Hypothesis_table, full_dataset,saving_path, file_name, nbfactors, myfactor, variable_to_save, which_plot){
  all_variables=colnames(full_dataset[(nbfactors+1):ncol(full_dataset)])
  
  if (which_plot != 'All'){
  variable_list=intersect(variable_to_save,all_variables)
  }
  else{
    variable_list=all_variables
  }
  #For each variable selected generate the plot
  for (variable in variable_list){
    formula=as.formula(paste0(variable," ~ ",myfactor))
    if (Hypothesis_table["Variance_test",variable]=="KW"){
      variable_test=kruskal_test(full_dataset,formula = formula)
    }
    else{
      variable_test=anova_test(full_dataset,formula = formula)
    }
    
    if (variable_test$p<0.05){
      current_dunn_test=dunn_test(full_dataset,formula=formula,p.adjust.method = "bonferroni")
      current_dunn_test=add_xy_position(current_dunn_test,x=myfactor)
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable]))+
        stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
    }
    else{
      variable_plot=ggboxplot(full_dataset,x=myfactor,y=colnames(full_dataset[variable]))+
        labs(subtitle=get_test_label(variable_test,detailed =TRUE))
    }
    myplot=ggarrange(variable_plot,ncol=1,nrow=1)
    #Save the plot in landscape format
    ggsave(filename = paste0(file_name,variable,".pdf"),plot=myplot,path=saving_path,device = cairo_pdf,width=297,height = 210,units="mm")
    
  }
  
  
  #ggsave(filename = paste0(file_name,".pdf"),plot=myplot,path=saving_path)
  #dev.off()
}

savethisfigure <- function(Hypothesis_table, full_dataset, file_name,variable, nbfactors, myfactor){
  
}

count_samples <- function (full_dataset,nbfactors,myfactor,nbvariable){
  
  table_count=data.frame(matrix(0,nrow =length(levels(full_dataset[,myfactor])),ncol=nbvariable))
  
  colnames(table_count)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  rownames(table_count)=c(levels(full_dataset[,myfactor]))

  
  
    
    for (mylevel in levels(full_dataset[,myfactor])){
      current_data=full_dataset[-which(full_dataset[,myfactor]!=mylevel),]
      
      for (current_variable in colnames(table_count)){
        table_count[mylevel,current_variable]=nrow(current_data[-which(current_data[,current_variable]=="NaN"),])
    
    }
    }
  
  return(table_count)
}



