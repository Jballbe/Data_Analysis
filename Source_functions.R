
#This function check if the user has the required packages, if not install them, and load them 
have_library <- function (required_packages){
  install.packages(setdiff(required_packages,rownames(installed.packages())))
  print ("All required packages installed")
  for (package_name in required_packages){
    library(package_name,character.only =TRUE);
  }
  print("All required packages loaded")
}
required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr","shinyFiles",'gghalves')
#Check if the user have all required libraries and if not, install them
have_library(required_packages = required_packages)

#This function create the full dataset table 
create_fulldataset <- function(population_class, data_file, nbfactors){
  #transform the factors columns into factor type
  for (elt in seq(1,(nbfactors+1))){
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
  
  
  
}



count_samples <- function (full_dataset,nbfactors,myfactor,nbvariable){
  
  table_count=data.frame(matrix(0,nrow =length(levels(full_dataset[,myfactor])),ncol=nbvariable))
  
  colnames(table_count)=colnames(full_dataset[,(nbfactors+1):ncol(full_dataset)])
  rownames(table_count)=c(levels(full_dataset[,myfactor]))

  
  
    
    for (mylevel in levels(full_dataset[,myfactor])){
      current_data=full_dataset[-which(full_dataset[,myfactor]!=mylevel),]
      
      for (current_variable in colnames(table_count)){
        table_count[mylevel,current_variable]=round(nrow(current_data[-which(current_data[,current_variable]=="NaN"),]),digits = 0)
    
    }
    }
  
  return(table_count)
}



