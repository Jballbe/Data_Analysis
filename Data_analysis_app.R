library(plyr)
library(shiny)
library(ggplot2)
library(GGally)
library(plotly)
library(tidyverse)
library(pracma)
library(gghighlight)
library(rstatix)
library(ggpubr)

ui <- fluidPage(
  #Application title
  titlePanel("Application Mean variance analysis"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      fileInput("Pop_class_file","Choose Population Class file"),
      fileInput("My_data","Choose data file (csv) to analyse"),
      fileInput("Source_functions","Choose the source function file"),
      numericInput("nbfactors","How many possible factors are they?"),
      uiOutput("factors")),
      
    
    mainPanel(
      #Display results, plots...
      tabsetPanel(
        tabPanel("Parametric Hypothesis",tableOutput("Parametric_hypothesis")),
        tabPanel("G_Input_Gain_Slope",plotOutput("G_Input_Gain_Slope"),plotlyOutput("G_Input_Gain_Slope_ly")),
        tabPanel("G_Input_Gain_Intercept",plotOutput("G_Input_Gain_Intercept"),plotlyOutput("G_Input_Gain_Intercept_ly")),
        tabPanel("G_Input_Gain_Slp.Int",plotOutput("G_Input_Gain_Slp.Int"),plotlyOutput("G_Input_Gain_Slp.Int_ly")),
        tabPanel("G_Input_Threshold_Slope",plotOutput("G_Input_Threshold_Slope"),plotlyOutput("G_Input_Threshold_Slope_ly")),
        tabPanel("G_Input_Threshold_Intercept",plotOutput("G_Input_Threshold_Intercept"),plotlyOutput("G_Input_Threshold_Intercept_ly")),
        tabPanel("G_Input_Threshold_Slp.Int",plotOutput("G_Input_Threshold_Slp.Int"),plotlyOutput("G_Input_Threshold_Slp.Int_ly")),
        tabPanel("G_Input_Saturation_Slope",plotOutput("G_Input_Saturation_Slope"),plotlyOutput("G_Input_Saturation_Slope_ly")),
        tabPanel("G_Input_Saturation_Intercept",plotOutput("G_Input_Saturation_Intercept"),plotlyOutput("G_Input_Saturation_Intercept_ly")),
        tabPanel("G_Input_Saturation_Slp.Int",plotOutput("G_Input_Saturation_Slp.Int"),plotlyOutput("G_Input_Saturation_Slp.Int_ly")),
        tabPanel("V_subthreshold_Input_Gain_Slope",plotOutput("V_subthreshold_Input_Gain_Slope"),plotlyOutput("V_subthreshold_Input_Gain_Slope_ly")),
        tabPanel("V_subthreshold_Input_Gain_Intercept",plotOutput("V_subthreshold_Input_Gain_Intercept"),plotlyOutput("V_subthreshold_Input_Gain_Intercept_ly")),
        tabPanel("V_subthreshold_Input_Gain_Slp.Int",plotOutput("V_subthreshold_Input_Gain_Slp.Int"),plotlyOutput("V_subthreshold_Input_Gain_Slp.Int_ly")),
        tabPanel("V_subthreshold_Input_Threshold_Slope",plotOutput("V_subthreshold_Input_Threshold_Slope"),plotlyOutput("V_subthreshold_Input_Threshold_Slope_ly")),
        tabPanel("V_subthreshold_Input_Threshold_Intercept",plotOutput("V_subthreshold_Input_Threshold_Intercept"),plotlyOutput("V_subthreshold_Input_Threshold_Intercept_ly")),
        tabPanel("V_subthreshold_Input_Threshold_Slp.Int",plotOutput("V_subthreshold_Input_Threshold_Slp.Int"),plotlyOutput("V_subthreshold_Input_Threshold_Slp.Int_ly")),
        tabPanel("V_subthreshold_Input_Saturation_Slope",plotOutput("V_subthreshold_Input_Saturation_Slope"),plotlyOutput("V_subthreshold_Input_Saturation_Slope_ly")),
        tabPanel("V_subthreshold_Input_Saturation_Intercept",plotOutput("V_subthreshold_Input_Saturation_Intercept"),plotlyOutput("V_subthreshold_Input_Saturation_Intercept_ly")),
        tabPanel("V_subthreshold_Input_Saturation_Slp.Int",plotOutput("V_subthreshold_Input_Saturation_Slp.Int"),plotlyOutput("V_subthreshold_Input_Saturation_Slp.Int_ly"))
      ))
    
    )
)


server <- function(input, output){
  solveSimulator <- reactive({
    source(file=input$Source_functions$input)
    required_packages=c("plyr","shiny","ggplot2","GGally","plotly","tidyverse","pracma","gghighlight","rstatix","ggpubr")
    have_library(required_packages = required_packages)
    #Download the datasets
    
    population_class=read.csv(file=input$Pop_class_file$name,header=T)
    data_file=read.csv(input$My_data$name,header=T)
    
    nbfactors=input$nbfactors
    full_dataset=create_fulldataset(population_class,data_file,nbfactors)$full_dataset
    factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
    output$factors <- renderUI({
      factor_list=create_fulldataset(population_class,data_file,nbfactors)$factor_list
      selectInput("factor","Based on which factor do the analysis",factor_list)
    })
    # population_class$Species=as.factor(population_class$Species)
    # population_class$Firing_Type=as.factor((population_class$Firing_Type))
    # 
    # data_file=read.csv(input$My_data$name,header=T)
    # data_file=data_file[2:nrow(data_file),]
    
    #Create a single table containing both the population class and the data
    # full_dataset=data.frame(cbind(population_class[,2:ncol(population_class)],data_file[,2:ncol(data_file)]))
    # column_names=c(colnames(population_class[,2:ncol(population_class)]),
    #               colnames(data_file[,2:ncol(data_file)]))
    # line_names=c(population_class[,1])
    # 
    # colnames(full_dataset) <- column_names
    # rownames(full_dataset) <- line_names
    # 
    # for (col in seq(3,ncol(full_dataset))){
    #   full_dataset[,col]=as.numeric(full_dataset[,col])
    # }
    # 
    # factor=input$factor
    # 
    #Before performing the analysis, we need to verify if the hypothesis are checked for parametric analysis or not, in our case the normal distribution of the data, and the homogeneity of the variances among the groups
    #check normal distribution of the data
    # normality_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-2)))
    # normality_p_value_table=t(normality_p_value_table)
    # normality_p_value_table=rbind(normality_p_value_table,c(rep(0,ncol(full_dataset)-2)))
    # colnames(normality_p_value_table)=colnames(full_dataset[,3:ncol(full_dataset)])
    # 
    # homogeneity_p_value_table=data.frame(c(rep(0,ncol(full_dataset)-2)))
    # homogeneity_p_value_table=t(homogeneity_p_value_table)
    # homogeneity_p_value_table=rbind(homogeneity_p_value_table,c(rep(0,ncol(full_dataset)-2)))
    # colnames(homogeneity_p_value_table)=colnames(full_dataset[,3:ncol(full_dataset)])
    # Variance_test=data.frame(c(rep("Anova",ncol(homogeneity_p_value_table))))
    # Variance_test=t(Variance_test)
    # colnames(Variance_test)=colnames(full_dataset[,3:ncol(full_dataset)])
    # 
    # 
    # Hypothesis_table=rbind(data.frame(normality_p_value_table),data.frame(homogeneity_p_value_table),data.frame(Variance_test))
    # 
    # rownames(Hypothesis_table)=c("Normality p_value","Normal distribution","Homogeneity p_values","Variances homogeneous","Variance_test")
    # 
    # for (elt in seq(3,ncol(full_dataset))){
    #   current_data=full_dataset[-which(full_dataset[,elt]=="NaN"),]
    #   current_formula=as.formula(paste0(colnames(current_data[elt])," ~ ",factor))
    #   Hypothesis_table[1,elt-2]=shapiro_test(data=current_data[,elt])$p.value
    #   
    #   if (Hypothesis_table[1,elt-2]>0.05){
    #     Hypothesis_table[2,elt-2]="Yes"
    #   }
    #   else{
    #     Hypothesis_table[2,elt-2]="No"
    #     Hypothesis_table[5,elt-2]="KW"
    #   }
    #   current_data=full_dataset[-which(full_dataset[,2]=="Neuron"),]
    #   current_data=current_data[-which(current_data[,2]=="Glia"),]
    #   current_data=current_data[-which(current_data[,2]=="Unspecified"),]
    #   
    #   Hypothesis_table[3,elt-2]=levene_test(data=current_data,formula=current_formula)$p
    #   
    #   if (Hypothesis_table[3,elt-2]>0.05){
    #     Hypothesis_table[4,elt-2]="Yes"
    #   }
    #   
    #   else{
    #     Hypothesis_table[4,elt-2]="No"
    #     Hypothesis_table[5,elt-2]="KW"
    #   }
    # }
    # FT_dataset=full_dataset[-which(full_dataset[,2]=="Neuron"),]
    # FT_dataset=FT_dataset[-which(FT_dataset[,2]=="Glia"),]
    # FT_dataset=FT_dataset[-which(FT_dataset[,2]=="Unspecified"),]
  
    
    return(list('full_dataset'=full_dataset,
                'factor'=factor,
                'normality_table'=normality_p_value_table,
                'hypothesis_table'=Hypothesis_table,
                'FT_dataset'=FT_dataset
                ))
  })
  
  
#outputs
  
  
  output$Parametric_hypothesis <- renderTable({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    data.frame(Hypothesis_table)
  },rownames = TRUE)
  
  
  output$G_Input_Gain_Slope_ly <- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Gain_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slope"]=="KW"){
        G_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slope"]=="KW"){
        G_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE))
      
    }
    
    G_Input_Gain_Slope_plot
  })
  output$G_Input_Gain_Intercept_ly<- renderPlotly({
  
  sol=solveSimulator()
  Hypothesis_table=sol$hypothesis_table
  factor=sol$factor
  formula=as.formula(paste0("G_Input_Gain_Intercept"," ~ ",factor))
  full_dataset=sol$full_dataset
  
  if (factor=="Firing_Type"){
    FT_dataset=sol$FT_dataset
    current_dataset=FT_dataset
    if (Hypothesis_table["Variance_test","G_Input_Gain_Intercept"]=="KW"){
      G_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
    }
    else{
      G_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
    }
    G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
      labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE))
    
  }
  
  if(factor=="Species"){
    current_dataset=full_dataset
    if (Hypothesis_table["Variance_test","G_Input_Gain_Intercept"]=="KW"){
      G_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
    }
    else{
      G_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
    }
    G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
      labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE))
    
  }
  
  G_Input_Gain_Intercept_plot
  })
  output$G_Input_Gain_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Gain_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slp.Int"]=="KW"){
        G_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slp.Int"]=="KW"){
        G_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE))
      
    }
    
    G_Input_Gain_Slp.Int_plot
  })
  output$G_Input_Threshold_Slope_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slope"]=="KW"){
        G_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slope"]=="KW"){
        G_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE))
      
    }
    
    G_Input_Threshold_Slope_plot
  })
  output$G_Input_Threshold_Intercept_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Intercept"]=="KW"){
        G_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Intercept"]=="KW"){
        G_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE))
      
    }
    
    G_Input_Threshold_Intercept_plot
  })
  output$G_Input_Threshold_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slp.Int"]=="KW"){
        G_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slp.Int"]=="KW"){
        G_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE))
      
    }
    
    G_Input_Threshold_Slp.Int_plot
  })
  output$G_Input_Saturation_Slope_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slope"]=="KW"){
        G_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slope"]=="KW"){
        G_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE))
      
    }
    
    G_Input_Saturation_Slope_plot
  })
  output$G_Input_Saturation_Intercept_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Intercept"]=="KW"){
        G_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Intercept"]=="KW"){
        G_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE))
      
    }
    
    G_Input_Saturation_Intercept_plot
  })
  output$G_Input_Saturation_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slp.Int"]=="KW"){
        G_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slp.Int"]=="KW"){
        G_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
        labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE))
      
    }
    
    G_Input_Saturation_Slp.Int_plot
  })
  output$V_subthreshold_Input_Gain_Slope_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slope"]=="KW"){
        V_subthreshold_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slope"]=="KW"){
        V_subthreshold_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE))
     
    }
    
    V_subthreshold_Input_Gain_Slope_plot
  })
  output$V_subthreshold_Input_Gain_Intercept_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Intercept"]=="KW"){
        V_subthreshold_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Intercept"]=="KW"){
        V_subthreshold_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Gain_Intercept_plot
  })
  output$V_subthreshold_Input_Gain_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slp.Int"]=="KW"){
        V_subthreshold_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slp.Int"]=="KW"){
        V_subthreshold_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Gain_Slp.Int_plot
  })
  output$V_subthreshold_Input_Threshold_Slope_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slope"]=="KW"){
        V_subthreshold_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE))
     
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slope"]=="KW"){
        V_subthreshold_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Threshold_Slope_plot
  })
  output$V_subthreshold_Input_Threshold_Intercept_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Intercept"]=="KW"){
        V_subthreshold_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE))
     
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Intercept"]=="KW"){
        V_subthreshold_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Threshold_Intercept_plot
  })
  output$V_subthreshold_Input_Threshold_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slp.Int"]=="KW"){
        V_subthreshold_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE))
     
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slp.Int"]=="KW"){
        V_subthreshold_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE))
     }
    
    
    V_subthreshold_Input_Threshold_Slp.Int_plot
  })
  output$V_subthreshold_Input_Saturation_Slope_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slope"]=="KW"){
        V_subthreshold_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE))
     
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slope"]=="KW"){
        V_subthreshold_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Saturation_Slope_plot
  })
  output$V_subthreshold_Input_Saturation_Intercept_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Intercept"]=="KW"){
        V_subthreshold_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE))
     
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Intercept"]=="KW"){
        V_subthreshold_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Saturation_Intercept_plot
  })
  output$V_subthreshold_Input_Saturation_Slp.Int_ly<- renderPlotly({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slp.Int"]=="KW"){
        V_subthreshold_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE))
      
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slp.Int"]=="KW"){
        V_subthreshold_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
        labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE))
      
    }
    
    V_subthreshold_Input_Saturation_Slp.Int_plot
  })
  
  output$G_Input_Gain_Slope<- renderPlot({
    
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Gain_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slope"]=="KW"){
        G_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slope"]=="KW"){
        G_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Slope_test$p<0.05){
        print("right")
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        View(current_dunn_test)
        G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        print("lololo")
        G_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Slope_test,detailed =TRUE))
      }
    }
    
    G_Input_Gain_Slope_plot
  })
  output$G_Input_Gain_Intercept<- renderPlot({
    
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Gain_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Intercept"]=="KW"){
        G_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Intercept"]=="KW"){
        G_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Intercept_test$p<0.05){
        print("right")
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        View(current_dunn_test)
        G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        print("lololo")
        G_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Intercept_test,detailed =TRUE))
      }
    }
    
    G_Input_Gain_Intercept_plot
  })
  output$G_Input_Gain_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Gain_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slp.Int"]=="KW"){
        G_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Gain_Slp.Int"]=="KW"){
        G_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Gain_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Gain_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Gain_Slp.Int_test,detailed =TRUE))
      }
    }
    
    G_Input_Gain_Slp.Int_plot
  })
  output$G_Input_Threshold_Slope<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slope"]=="KW"){
        G_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slope"]=="KW"){
        G_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Slope_test,detailed =TRUE))
      }
    }
    
    G_Input_Threshold_Slope_plot
  })
  output$G_Input_Threshold_Intercept<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Intercept"]=="KW"){
        G_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Intercept"]=="KW"){
        G_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Intercept_test,detailed =TRUE))
      }
    }
    
    G_Input_Threshold_Intercept_plot
  })
  output$G_Input_Threshold_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Threshold_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slp.Int"]=="KW"){
        G_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Threshold_Slp.Int"]=="KW"){
        G_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Threshold_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Threshold_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Threshold_Slp.Int_test,detailed =TRUE))
      }
    }
    
    G_Input_Threshold_Slp.Int_plot
  })
  output$G_Input_Saturation_Slope<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slope"]=="KW"){
        G_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slope"]=="KW"){
        G_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slope"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Slope_test,detailed =TRUE))
      }
    }
    
    G_Input_Saturation_Slope_plot
  })
  output$G_Input_Saturation_Intercept<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Intercept"]=="KW"){
        G_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Intercept"]=="KW"){
        G_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Intercept"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Intercept_test,detailed =TRUE))
      }
    }
    
    G_Input_Saturation_Intercept_plot
  })
  output$G_Input_Saturation_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("G_Input_Saturation_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slp.Int"]=="KW"){
        G_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","G_Input_Saturation_Slp.Int"]=="KW"){
        G_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        G_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (G_Input_Saturation_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        G_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["G_Input_Saturation_Slp.Int"]))+
          labs(subtitle=get_test_label(G_Input_Saturation_Slp.Int_test,detailed =TRUE))
      }
    }
    
    G_Input_Saturation_Slp.Int_plot
  })
  output$V_subthreshold_Input_Gain_Slope<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slope"]=="KW"){
        V_subthreshold_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slope"]=="KW"){
        V_subthreshold_Input_Gain_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slope_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Gain_Slope_plot
  })
  output$V_subthreshold_Input_Gain_Intercept<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Intercept"]=="KW"){
        V_subthreshold_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Intercept"]=="KW"){
        V_subthreshold_Input_Gain_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Intercept_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Gain_Intercept_plot
  })
  output$V_subthreshold_Input_Gain_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Gain_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slp.Int"]=="KW"){
        V_subthreshold_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Gain_Slp.Int"]=="KW"){
        V_subthreshold_Input_Gain_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Gain_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Gain_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Gain_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Gain_Slp.Int_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Gain_Slp.Int_plot
  })
  output$V_subthreshold_Input_Threshold_Slope<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slope"]=="KW"){
        V_subthreshold_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slope"]=="KW"){
        V_subthreshold_Input_Threshold_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slope_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Threshold_Slope_plot
  })
  output$V_subthreshold_Input_Threshold_Intercept<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Intercept"]=="KW"){
        V_subthreshold_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Intercept"]=="KW"){
        V_subthreshold_Input_Threshold_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Intercept_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Threshold_Intercept_plot
  })
  output$V_subthreshold_Input_Threshold_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Threshold_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slp.Int"]=="KW"){
        V_subthreshold_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Threshold_Slp.Int"]=="KW"){
        V_subthreshold_Input_Threshold_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Threshold_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Threshold_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Threshold_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Threshold_Slp.Int_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Threshold_Slp.Int_plot
  })
  output$V_subthreshold_Input_Saturation_Slope<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Slope"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slope"]=="KW"){
        V_subthreshold_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slope"]=="KW"){
        V_subthreshold_Input_Saturation_Slope_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slope_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Slope_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Slope_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slope"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slope_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Saturation_Slope_plot
  })
  output$V_subthreshold_Input_Saturation_Intercept<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Intercept"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Intercept"]=="KW"){
        V_subthreshold_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Intercept"]=="KW"){
        V_subthreshold_Input_Saturation_Intercept_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Intercept_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Intercept_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Intercept"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Intercept_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Saturation_Intercept_plot
  })
  output$V_subthreshold_Input_Saturation_Slp.Int<- renderPlot({
    sol=solveSimulator()
    Hypothesis_table=sol$hypothesis_table
    factor=sol$factor
    formula=as.formula(paste0("V_subthreshold_Input_Saturation_Slp.Int"," ~ ",factor))
    full_dataset=sol$full_dataset
    
    if (factor=="Firing_Type"){
      FT_dataset=sol$FT_dataset
      current_dataset=FT_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slp.Int"]=="KW"){
        V_subthreshold_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE))
      }
    }
    
    if(factor=="Species"){
      current_dataset=full_dataset
      if (Hypothesis_table["Variance_test","V_subthreshold_Input_Saturation_Slp.Int"]=="KW"){
        V_subthreshold_Input_Saturation_Slp.Int_test=kruskal_test(current_dataset,formula = formula)
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_test=anova_test(current_dataset,formula = formula)
      }
      
      if (V_subthreshold_Input_Saturation_Slp.Int_test$p<0.05){
        current_dunn_test=dunn_test(current_dataset,formula=formula,p.adjust.method = "bonferroni")
        current_dunn_test=add_xy_position(current_dunn_test,x=factor)
        V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
          stat_pvalue_manual(current_dunn_test,hide.ns = TRUE)+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE),caption=get_pwc_label(current_dunn_test))
      }
      else{
        V_subthreshold_Input_Saturation_Slp.Int_plot=ggboxplot(current_dataset,x=factor,y=colnames(current_dataset["V_subthreshold_Input_Saturation_Slp.Int"]))+
          labs(subtitle=get_test_label(V_subthreshold_Input_Saturation_Slp.Int_test,detailed =TRUE))
      }
    }
    
    V_subthreshold_Input_Saturation_Slp.Int_plot
  })
  
}


shinyApp(ui=ui,server = server)