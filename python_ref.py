#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri May 12 12:15:01 2023

@author: julienballbe
"""
import numpy as np
from scipy.special import erf
import scipy

from scipy import stats

import pandas as pd
from lmfit.models import LinearModel, StepModel, ExpressionModel, Model,ExponentialModel,ConstantModel,GaussianModel,QuadraticModel,PolynomialModel,SkewedGaussianModel
from lmfit import Parameters, Minimizer,fit_report
# from plotnine import ggplot, geom_line, aes, geom_abline, geom_point, geom_text, labels, geom_histogram, ggtitle, geom_vline,geom_hline
# from plotnine.scales import scale_y_continuous, ylim, xlim, scale_color_manual
# from plotnine.labels import xlab
# from plotnine.stats import stat_bin
# import matplotlib.pyplot as plt
# from matplotlib.colors import LogNorm
def testMethod(bins): 
  string = "I came from a Python Function" 
  noOfBins = str(bins) 
  return "You have selected " +noOfBins+ " bins and " +string

def skewedgaussian(x, A, gamma, mu, sigma):

    
    y = (A / (sigma * np.sqrt(2 * np.pi))) * np.exp(-(x - mu) ** 2 / (2 * sigma ** 2)) * (
                 1 + erf((gamma * (x - mu)) / (sigma * np.sqrt(2))))
    return (y)

def sk_gauss_to_minimize(params,x_data,y_data):
    best_A=params['A']
    best_gamma=params['gamma']
    best_mu=params['mu']
    best_sigma=params['sigma']   
    
    model=skewedgaussian(x_data,best_A,best_gamma,best_mu,best_sigma)
    return model-y_data


def reticulate_data_distribution(feature_array,n_bins):
    bin_edges = np.linspace(np.nanmin(feature_array), np.nanmax(feature_array), n_bins + 1) # includes right edge, so adding one to desired bin number
    bin_width = bin_edges[1] - bin_edges[0]

    current_output,current_bin_edges,current_bins_number = scipy.stats.binned_statistic(feature_array,
                                    feature_array,
                                    statistic='count',
                                    bins=bin_edges)
    
    bin_center_array=current_bin_edges[1:]-(current_bin_edges[1:]-current_bin_edges[:-1])/2
    bin_df=pd.DataFrame({"Feature":bin_center_array,"Count":current_output})
    
    
    return bin_df
    
    
    
def fit_distribution(feature_array,n_bins):
    
    
    fit_table= reticulate_data_distribution(feature_array, n_bins)
    
    fit_df=pd.DataFrame(columns=["Fit",'Parameters','RMSE'])
    x_data=np.array(fit_table.iloc[:,0])
    y_data=np.array(fit_table.iloc[:,1])
    

    sk_gauss_params=Parameters()
    sk_gauss_params.add('A',value=max(y_data))
    sk_gauss_params.add('gamma',value=0)
    sk_gauss_params.add('mu',value=x_data[np.nanargmax(y_data)])
    sk_gauss_params.add('sigma',value=np.std(y_data)**2)  
    
    sk_gauss_params['A'].set(brute_step=5)
    sk_gauss_params['gamma'].set(brute_step=.1)
    sk_gauss_params['mu'].set(brute_step=1)
    sk_gauss_params['sigma'].set(brute_step=0.1)    
        
           
    #data=my_exponential_decay(x=median_table["interval"], A=0.5, B=2, C=0.1)
    fitter=Minimizer(sk_gauss_to_minimize,sk_gauss_params,fcn_args=(x_data,y_data))
    result_brute=fitter.minimize(method='brute',Ns=10,keep=10)
    best_chi=None
    
    for current_result in result_brute.candidates:
            current_A=current_result.params['A'].value
            current_gamma=current_result.params['gamma'].value
            current_mu=current_result.params['mu'].value
            current_sigma=current_result.params['sigma'].value
            
            current_sk_gaus_model = Model(skewedgaussian)
            current_sk_gauss_params=Parameters()
            current_sk_gauss_params.add('A',value=current_A)
            current_sk_gauss_params.add('gamma',value=current_gamma)
            current_sk_gauss_params.add('mu',value=current_mu)
            current_sk_gauss_params.add('sigma',value=current_sigma) 
            
            sk_gauss_out=current_sk_gaus_model.fit(y_data, current_sk_gauss_params, x=x_data)        
            
            if best_chi==None:
                best_chi=sk_gauss_out.chisqr
                best_A=sk_gauss_out.best_values['A']
                best_gamma=sk_gauss_out.best_values['gamma']
                best_mu=sk_gauss_out.best_values['mu']
                best_sigma=sk_gauss_out.best_values['sigma']   
                best_params_dict=sk_gauss_out.best_values
                
            elif best_chi>sk_gauss_out.chisqr:
                best_chi=sk_gauss_out.chisqr
                best_A=sk_gauss_out.best_values['A']
                best_gamma=sk_gauss_out.best_values['gamma']
                best_mu=sk_gauss_out.best_values['mu']
                best_sigma=sk_gauss_out.best_values['sigma']   
                best_params_dict=sk_gauss_out.best_values
    
    
    
    
    
    

    best_params=[best_A,best_gamma,best_mu,best_sigma]
    #return(best_params_dict)
    best_params_df=pd.DataFrame([best_params_dict])
    
    pred = skewedgaussian(x_data,best_A,best_gamma,best_mu,best_sigma)
    squared_error = np.square((y_data - pred))
    sum_squared_error = np.sum(squared_error)
    RMSE = np.sqrt(sum_squared_error / y_data.size)
    fit_table['Data']='Original_Data'
    simulation_table=pd.DataFrame(np.column_stack((x_data,pred)),columns=["Gain","Count"])
    simulation_table['Data']='Fit_Data'
    fit_table=pd.concat([fit_table, simulation_table], axis=0)
    new_line_test=pd.Series(["Skewed_Gaussian",*best_params_df.loc[0,:],RMSE],index=["Fit",*best_params_df.columns,'RMSE'])
    fit_df=pd.DataFrame(columns=["Fit",*best_params_df.columns,'RMSE'])
    
    fit_df=fit_df.append(new_line_test,ignore_index=True)
    
        
        
    return fit_df

    
    
    