have_library <- function (required_packages){
  install.packages(setdiff(required_packages,rownames(installed.packages())))
  print ("All required packages installed")
  for (package_name in required_packages){
    library(package_name,character.only =TRUE);
  }
  print("All required packages loaded")
}

create_fulldataset <- function(population_class, data_file, nbfactors){
  #transform the factors columns into factor type
  for (elt in seq(1,nbfactor)){
    population_class[,elt]=as.factor(population_class)
  }
  #remove unit line from data_file
  data_file=data_file[2:nrow(data_file),]
  
  #create full dataset
  full_dataset=data.frame(merge(population_class,data_file,all=T))
  
  for (elt in seq(nbfactors+2,ncol(full_dataset))){
    full_dataset[,elt]=as.numeric(full_dataset[,elt])
  }
  factor_list=list(colnames(full_dataset)[1:nbfactors])
  return (list("full_dataset"=full_dataset,
               "factor_list"=factor_list))
}


#Perform tests (data normality and variance homogeneity) to know which test is to be performed
parametric_test <- function(full_dataset, factor_list){
  nbfactors=length(factor_list)
  normality_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-(nbfactors+1))))
  normality_p_value_table=t(normality_p_value_table)
  normality_p_value_table=rbind(normality_p_value_table,c(rep(0,ncol(full_dataset)-(nbfactors+1))))
  colnames(normality_p_value_table)=colnames(full_dataset[,(nbfactors+2):ncol(full_dataset)])
  
  homogeneity_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-(nbfactors+1))))
  homogeneity_p_value_table=t(homogeneity_p_value_table)
  homogeneity_p_value_table=rbind(homogeneity_p_value_table,c(rep(0,ncol(full_dataset)-(nbfactors+1))))
  colnames(homogeneity_p_value_table)=colnames(full_dataset[,(nbfactors+2):ncol(full_dataset)])
  Variance_test=data.frame(c(rep("Anova",ncol(homogeneity_p_value_table))))
  Variance_test=t(Variance_test)
  colnames(Variance_test)=colnames(full_dataset[,(nbfactors+2):ncol(full_dataset)])
  
  Hypothesis_table=rbind(data.frame(normality_p_value_table),data.frame(homogeneity_p_value_table),data.frame(Variance_test))
  rownames(Hypothesis_table)=c("Normality p_value","Normal distribution","Homogeneity p_values","Variances homogeneous","Variance_test")
  
  for (elt in seq((nbfactors+2),ncol(full_dataset))){
    current_data=full_dataset[-which(full_dataset[,elt]=="NaN"),]
    current_data=current_data[-which(current_data[,(nbfactors+1)]=="Neuron"),]
    current_data=current_data[-which(current_data[,(nbfactors+1)]=="Glia"),]
    current_data=current_data[-which(current_data[,(nbfactors+1)]=="Unspecified"),]
    
    current_formula=as.formula(paste0(colnames(current_data[elt])," ~ ",factor))
    #test normality of the data
    Hypothesis_table[1,elt-3]=shapiro_test(data=current_data[,elt])$p.value
    
    if (Hypothesis_table[1,elt-3]>0.05){
      Hypothesis_table[2,elt-3]="Yes"
    }
    else{
      Hypothesis_table[2,elt-3]="No"
      Hypothesis_table[5,elt-3]="KW"
    }
    
    #test variance homogeneity
    Hypothesis_table[3,elt-3]=levene_test(data=current_data,formula=current_formula)$p
    
    if (Hypothesis_table[3,elt-3]>0.05){
      Hypothesis_table[4,elt-3]="Yes"
    }
    
    else{
      Hypothesis_table[4,elt-3]="No"
      Hypothesis_table[5,elt-3]="KW"
    }
  }
  return(Hypothesis_table)
}