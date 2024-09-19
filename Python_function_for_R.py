#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Apr 16 10:56:56 2024

@author: julienballbe
"""

import scipy
from scipy.special import erfc, erf
from scipy import stats
import numpy as np
import pandas as pd
from lmfit.models import LinearModel, StepModel, ExpressionModel, Model,ExponentialModel,ConstantModel,GaussianModel,QuadraticModel,PolynomialModel,SkewedGaussianModel
from lmfit import Parameters, Minimizer,fit_report
import plotnine as p9
from scipy.stats import iqr
import matplotlib.pyplot as plt
from sklearn.mixture import GaussianMixture


# Function to compute AIC
def compute_aic(log_likelihood, num_params):
    return 2 * num_params - 2 * log_likelihood

# Function to compute KS test statistic and p-value
def compute_ks_test(data, distribution, params):
    return stats.kstest(data, distribution, args=params)

# 1. Exponential Fit Function
def fit_distribution_to_exponential_new(data):
    exp_params = stats.expon.fit(data)
    exp_ll = np.sum(stats.expon.logpdf(data, *exp_params))
    exp_aic = compute_aic(exp_ll, len(exp_params))
    ks_stat, ks_p_val = compute_ks_test(data, 'expon', exp_params)
    
    return pd.DataFrame({
        'Fit': ['Exponential'],

        'mu': [np.nan],
        'sigma': [np.nan],
        'gamma': [np.nan],
        'tau': [exp_params[1]],  # Scale parameter

        'AIC': [exp_aic],
        'KS_p_val': [ks_p_val]
    })

# 2. LogNormal Fit Function
def fit_distribution_log_norm_new(data):
    lognorm_params = stats.lognorm.fit(data, floc=0)
    lognorm_ll = np.sum(stats.lognorm.logpdf(data, *lognorm_params))
    lognorm_aic = compute_aic(lognorm_ll, len(lognorm_params))
    ks_stat, ks_p_val = compute_ks_test(data, 'lognorm', lognorm_params)
    
    return pd.DataFrame({
        'Fit': ['LogNormal'],

        'mu': [lognorm_params[1]],  # Mean of the lognormal distribution
        'sigma': [lognorm_params[0]],  # Standard deviation of the lognormal distribution
        'gamma': [np.nan],
        'tau': [np.nan],

        'AIC': [lognorm_aic],
        'KS_p_val': [ks_p_val]
    })

# 3. Gaussian Fit Function
def fit_distribution_to_gaussian_new(data):
    norm_params = stats.norm.fit(data)
    norm_ll = np.sum(stats.norm.logpdf(data, *norm_params))
    norm_aic = compute_aic(norm_ll, len(norm_params))
    ks_stat, ks_p_val = compute_ks_test(data, 'norm', norm_params)
    
    return pd.DataFrame({
        'Fit': ['Gaussian'],

        'mu': [norm_params[0]],  # Mean
        'sigma': [norm_params[1]],  # Standard deviation
        'gamma': [np.nan],
        'tau': [np.nan],

        'AIC': [norm_aic],
        'KS_p_val': [ks_p_val]
    })

# 4. Skewed Gaussian (Skew Normal) Fit Function
def fit_distribution_to_skewed_gaussian_new(data):
    skewnorm_params = stats.skewnorm.fit(data)
    skewnorm_ll = np.sum(stats.skewnorm.logpdf(data, *skewnorm_params))
    skewnorm_aic = compute_aic(skewnorm_ll, len(skewnorm_params))
    ks_stat, ks_p_val = compute_ks_test(data, 'skewnorm', skewnorm_params)
    
    return pd.DataFrame({
        'Fit': ['Skewed Gaussian'],

        'mu': [skewnorm_params[1]],  # Location
        'sigma': [skewnorm_params[2]],  # Scale
        'gamma': [skewnorm_params[0]],  # Skewness
        'tau': [np.nan],

        'AIC': [skewnorm_aic],
        'KS_p_val': [ks_p_val]
    })

# General Function to Fit Distributions
def fit_distribution_new(value_array, distribution_list):
    fit_table = pd.DataFrame(columns=['Fit', "mu", 'sigma', "gamma", "tau", "AIC", "KS_p_val"])
    print(type(value_array))
    print(type(value_array[0]))
    for distrib in distribution_list:


        try:
            if distrib == "Exponential":
                fit_result_table = fit_distribution_to_exponential_new(value_array)
            elif distrib == "LogNormal":
                fit_result_table = fit_distribution_log_norm_new(value_array)
            elif distrib == "Gaussian":
                fit_result_table = fit_distribution_to_gaussian_new(value_array)
            elif distrib == "Skewed Gaussian":
                fit_result_table = fit_distribution_to_skewed_gaussian_new(value_array)

            fit_table = pd.concat([fit_table, fit_result_table], ignore_index=True)
    
        except:
            fit_result_table = pd.DataFrame([[distrib, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan]],
                                            columns=fit_table.columns)
            fit_table = pd.concat([fit_table, fit_result_table], ignore_index=True)
    
    return fit_table





def get_data_bin_df(value_array, bin_width_or_nb, bin_def = "Width"):
    
    if bin_def == 'Number':
        bin_edges = np.linspace(np.nanmin(value_array), np.nanmax(value_array), bin_width_or_nb + 1) # includes right edge, so adding one to desired bin number
        bin_width = bin_edges[1] - bin_edges[0]
        bin_edges = np.linspace((np.nanmin(value_array)-bin_width), (np.nanmax(value_array)+bin_width),bin_width_or_nb)
        
    else:

        bin_nb = int(((np.nanmax(value_array)+bin_width_or_nb) - (np.nanmin(value_array)-bin_width_or_nb))//bin_width_or_nb)

        bin_edges = np.linspace((np.nanmin(value_array)-bin_width_or_nb), (np.nanmax(value_array)+bin_width_or_nb),bin_nb) # includes right edge, so adding one to desired bin number
        
        
    current_output,current_bin_edges,current_bins_number = scipy.stats.binned_statistic(value_array,
                                    value_array,
                                    statistic='count',
                                    bins=bin_edges)

    bin_center_array=current_bin_edges[1:]-(current_bin_edges[1:]-current_bin_edges[:-1])/2
    bin_count_df=pd.DataFrame({"Feature":bin_center_array,"Count":current_output})
    
    return bin_count_df
    


def fit_distribution(distribution_binned_df_original, distribution_list):
    
    fit_table = pd.DataFrame(columns=['Fit', 'A', "mu", 'sigma', "gamma", "tau", "C", "RMSE"])
    
    for distrib in distribution_list:
        distribution_binned_df = distribution_binned_df_original.copy()
        try : 
            if distrib == "Exponential":
                fit_result_table = fit_distribution_to_exponential(distribution_binned_df, False)
            elif distrib == "LogNormal":
                fit_result_table = fit_distribution_log_norm(distribution_binned_df, False)
            elif distrib == "Gaussian":
                fit_result_table = fit_distribution_to_gaussian(distribution_binned_df, False)   
            elif distrib == "Skewed Gaussian":
                fit_result_table = fit_distribution_to_skewed_gaussian(distribution_binned_df, False)
                
            fit_table = pd.concat([fit_table, fit_result_table], ignore_index = True)
        
        except:
            
            fit_result_table = pd.DataFrame([distrib, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan, np.nan]).T
            fit_result_table.columns = fit_table.columns
            fit_table = pd.concat([fit_table, fit_result_table], ignore_index = True)
    return fit_table
            

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

def fit_distribution_to_skewed_gaussian(distribution_df,do_plot):
    fit_table=distribution_df.copy()
    
    x_data=np.array(distribution_df.iloc[:,0])
    y_data=np.array(distribution_df.iloc[:,1])
    
    
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

    best_RMSE = None
    
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
            
            _A=sk_gauss_out.best_values['A']
            _gamma=sk_gauss_out.best_values['gamma']
            _mu=sk_gauss_out.best_values['mu']
            _sigma=sk_gauss_out.best_values['sigma']   
            
            pred = skewedgaussian(x_data, _A, _gamma, _mu, _sigma)
            
            
            current_table = pd.DataFrame({'Original':y_data, 'Pred':pred})

            current_table_without_zero = current_table.loc[current_table['Original']!=0,:]

            squared_error = np.square((np.array(current_table_without_zero.loc[:,'Original']) - np.array(current_table_without_zero.loc[:,'Pred'])))
            sum_squared_error = np.sum(squared_error)
            current_RMSE = np.sqrt(sum_squared_error / len(np.array(current_table_without_zero.loc[:,'Original'])))
            
            
            if best_RMSE==None:

                best_params_dict=sk_gauss_out.best_values
                best_RMSE = current_RMSE
                
            elif best_RMSE>current_RMSE:
                
                best_params_dict=sk_gauss_out.best_values
                best_RMSE = current_RMSE
    
    
    
    
    
    

   
    best_params_df=pd.DataFrame([best_params_dict])
    
    
    fit_table['Data']='Original_Data'
    simulation_table=pd.DataFrame(np.column_stack((x_data,pred)),columns=distribution_df.columns)
    simulation_table['Data']='Fit_Data'
    fit_table=pd.concat([fit_table, simulation_table], axis=0)
    
    if do_plot:
        fit_plot = p9.ggplot()+p9.geom_point(fit_table.loc[fit_table['Data']=="Original_Data",:], p9.aes(x="Feature", y='Count'),color = 'black') + p9.geom_line(fit_table.loc[fit_table['Data']=="Fit_Data",:], p9.aes(x="Feature", y='Count'),color='red')
        #fit_plot+=p9.xlim(-.5,5)
        fit_plot.show()
    
    fit_df=pd.DataFrame(columns=["Fit",*best_params_df.columns,'RMSE'])
    new_line_test=pd.DataFrame(["Skewed Gaussian",*best_params_df.loc[0,:],best_RMSE]).T
    new_line_test.columns = fit_df.columns
    
    fit_df = pd.concat([fit_df, new_line_test],ignore_index=True)
    
    

    return fit_df



def gaussian(x, A, mu, sigma):
    y = (A / (sigma * np.sqrt(2 * np.pi))) * np.exp(-(x - mu) ** 2 / (2 * sigma ** 2))
    return (y)

def gauss_to_minimize(params,x_data,y_data):
    best_A=params['A']
    best_mu=params['mu']
    best_sigma=params['sigma']   
    
    model=gaussian(x_data,best_A,best_mu,best_sigma)
    return model-y_data

def fit_distribution_to_gaussian(distribution_df,do_plot):
    fit_table=distribution_df.copy()
    
    x_data=np.array(distribution_df.iloc[:,0])
    y_data=np.array(distribution_df.iloc[:,1])
    

    gauss_params=Parameters()
    gauss_params.add('A',value=max(y_data))

    gauss_params.add('mu',value=x_data[np.nanargmax(y_data)])
    gauss_params.add('sigma',value=np.std(y_data)**2)  
    
    gauss_params['A'].set(brute_step=5)
    
    gauss_params['mu'].set(brute_step=1)
    gauss_params['sigma'].set(brute_step=0.1)    

        
           
    #data=my_exponential_decay(x=median_table["interval"], A=0.5, B=2, C=0.1)
    fitter=Minimizer(gauss_to_minimize ,gauss_params,fcn_args=(x_data,y_data))
    result_brute=fitter.minimize(method='brute',Ns=10,keep=10)

    best_RMSE = None
    for current_result in result_brute.candidates:
            current_A=current_result.params['A'].value
            
            current_mu=current_result.params['mu'].value
            current_sigma=current_result.params['sigma'].value
            
            current_gaus_model = Model(gaussian)
            current_gauss_params=Parameters()
            current_gauss_params.add('A',value=current_A)

            current_gauss_params.add('mu',value=current_mu)
            current_gauss_params.add('sigma',value=current_sigma) 
            
            gauss_out=current_gaus_model.fit(y_data, current_gauss_params, x=x_data)        
            
            _A=gauss_out.best_values['A']
            
            _mu=gauss_out.best_values['mu']
            _sigma=gauss_out.best_values['sigma']   
            pred = gaussian(x_data,_A,_mu,_sigma)
           
            
            current_table = pd.DataFrame({'Original':y_data, 'Pred':pred})

            current_table_without_zero = current_table.loc[current_table['Original']!=0,:]

            squared_error = np.square((np.array(current_table_without_zero.loc[:,'Original']) - np.array(current_table_without_zero.loc[:,'Pred'])))
            sum_squared_error = np.sum(squared_error)
            current_RMSE = np.sqrt(sum_squared_error / len(np.array(current_table_without_zero.loc[:,'Original'])))
                        
            
            if best_RMSE==None:
                
                best_params_dict=gauss_out.best_values
                best_RMSE = current_RMSE
                
            elif best_RMSE>current_RMSE:
                
                best_params_dict=gauss_out.best_values
                best_RMSE = current_RMSE
    
    
    

   
    best_params_df=pd.DataFrame([best_params_dict])
    
    
    
    fit_table['Data']='Original_Data'
    simulation_table=pd.DataFrame(np.column_stack((x_data,pred)),columns=distribution_df.columns)
    simulation_table['Data']='Fit_Data'
    fit_table=pd.concat([fit_table, simulation_table], axis=0)
    
    if do_plot:
        fit_plot = p9.ggplot()+p9.geom_point(fit_table.loc[fit_table['Data']=="Original_Data",:], p9.aes(x="Feature", y='Count'),color = 'black') + p9.geom_line(fit_table.loc[fit_table['Data']=="Fit_Data",:], p9.aes(x="Feature", y='Count'),color='red')
        #fit_plot+=p9.xlim(-.5,5)
        fit_plot.show()
    
    fit_df=pd.DataFrame(columns=["Fit",*best_params_df.columns,'RMSE'])
    new_line_test=pd.DataFrame(["Gaussian",*best_params_df.loc[0,:],best_RMSE]).T
    new_line_test.columns = fit_df.columns
    
    fit_df = pd.concat([fit_df, new_line_test],ignore_index=True)
    
    

    return fit_df


def exponential(x, A, tau,C):
    y = (A * np.exp((-x)/tau))+C
    return (y)

def exponential_to_minimize(params,x_data,y_data):
    best_A=params['A']
    best_tau=params['tau']
    best_C=params['C']
    

    model=exponential(x_data,best_A,best_tau,best_C)
    return model-y_data

def fit_distribution_to_exponential(distribution_df,do_plot):
    fit_table=distribution_df.copy()
    
    x_data=np.array(distribution_df.iloc[:,0])
    y_data=np.array(distribution_df.iloc[:,1])
    
    
    expo_params=Parameters()
    expo_params.add('A',value=max(y_data))
    expo_params.add('tau',value=2,min=1e-4)
    expo_params.add('C',value=0)
    
    expo_params['A'].set(brute_step=5)
    expo_params['tau'].set(brute_step=1)
    expo_params['C'].set(brute_step=1)

        
           
    #data=my_exponential_decay(x=median_table["interval"], A=0.5, B=2, C=0.1)
    fitter=Minimizer(exponential_to_minimize,expo_params,fcn_args=(x_data,y_data))
    result_brute=fitter.minimize(method='brute',Ns=10,keep=10)

    best_RMSE = None
    for current_result in result_brute.candidates:
            current_A=current_result.params['A'].value
            current_tau=current_result.params['tau'].value
            current_C=current_result.params['C'].value
            
            
            current_expo_model = Model(exponential)
            current_expo_params=Parameters()
            current_expo_params.add('A',value=current_A)
            current_expo_params.add('tau',value=current_tau)
            current_expo_params.add('C',value=current_C)
            
            
            expo_out=current_expo_model.fit(y_data, current_expo_params, x=x_data)        
            
            _A=expo_out.best_values['A']
            _tau=expo_out.best_values['tau']
            _C=expo_out.best_values['C']
            
            pred = exponential(x_data, _A, _tau, _C)
            
            
            current_table = pd.DataFrame({'Original':y_data, 'Pred':pred})

            current_table_without_zero = current_table.loc[current_table['Original']!=0,:]

            squared_error = np.square((np.array(current_table_without_zero.loc[:,'Original']) - np.array(current_table_without_zero.loc[:,'Pred'])))
            sum_squared_error = np.sum(squared_error)
            current_RMSE = np.sqrt(sum_squared_error / len(np.array(current_table_without_zero.loc[:,'Original'])))
            
            if best_RMSE==None:
                
                best_params_dict=expo_out.best_values
                best_RMSE = current_RMSE
                
            elif best_RMSE>current_RMSE:
                
                best_params_dict=expo_out.best_values
                best_RMSE = current_RMSE
    
    
    

    
    #return(best_params_dict)
    best_params_df=pd.DataFrame([best_params_dict])
    
   
    
    fit_table['Data']='Original_Data'
    simulation_table=pd.DataFrame(np.column_stack((x_data,pred)),columns=distribution_df.columns)
    simulation_table['Data']='Fit_Data'
    fit_table=pd.concat([fit_table, simulation_table], axis=0)
    
    if do_plot:
        fit_plot = p9.ggplot()+p9.geom_point(fit_table.loc[fit_table['Data']=="Original_Data",:], p9.aes(x="Feature", y='Count'),color = 'black') + p9.geom_line(fit_table.loc[fit_table['Data']=="Fit_Data",:], p9.aes(x="Feature", y='Count'),color='red')
        #fit_plot+=p9.xlim(-.5,5)
        fit_plot.show()
    
    fit_df=pd.DataFrame(columns=["Fit",*best_params_df.columns,'RMSE'])
    new_line_test=pd.DataFrame(["Exponenial",*best_params_df.loc[0,:],best_RMSE]).T
    new_line_test.columns = fit_df.columns
    
    fit_df = pd.concat([fit_df, new_line_test],ignore_index=True)
    
    

    return fit_df





def Log_normal(x, A, mu, sigma):
    y = (A / (sigma * np.sqrt(2 * np.pi))) * (np.exp(-((np.log(x) - mu) ** 2) / (2 * (sigma ** 2))) / x)
   
    return (y)

def Log_normal_to_minimize(params,x_data,y_data):
    best_A=params['A']
    best_mu=params['mu']
    best_sigma=params['sigma']   
    
    model=Log_normal(x_data,best_A,best_mu,best_sigma)

    return model-y_data


def fit_distribution_log_norm(distribution_df,do_plot):
    fit_table=distribution_df.copy()
    
    if np.nanmin(fit_table.iloc[:,0]) <=0:
        shift = np.nanmin(fit_table.iloc[:,0])
        fit_table.iloc[:,0] += np.abs(shift)+1
        
    
    x_data=np.array(fit_table.iloc[:,0])
    y_data=np.array(fit_table.iloc[:,1])
    
    
    log_norm_params=Parameters()
    log_norm_params.add('A',value=max(y_data))

    log_norm_params.add('mu',value=x_data[np.nanargmax(y_data)])
    log_norm_params.add('sigma',value=np.std(y_data)**2)  
    
    log_norm_params['A'].set(brute_step=5)
    
    log_norm_params['mu'].set(brute_step=1)
    log_norm_params['sigma'].set(brute_step=0.1)   
    
    fitter=Minimizer(Log_normal_to_minimize,log_norm_params,fcn_args=(x_data,y_data))
    result_brute=fitter.minimize(method='brute',Ns=10,keep=10)

    best_RMSE = None
    for current_result in result_brute.candidates:
            current_A=current_result.params['A'].value
            
            current_mu=current_result.params['mu'].value
            current_sigma=current_result.params['sigma'].value
            
            current_log_norm_model = Model(Log_normal)
            current_log_norm_params=Parameters()
            current_log_norm_params.add('A',value=current_A)

            current_log_norm_params.add('mu',value=current_mu)
            current_log_norm_params.add('sigma',value=current_sigma) 
            
            log_norm_out=current_log_norm_model.fit(y_data, current_log_norm_params, x=x_data)        
            
            _A=log_norm_out.best_values['A']

            _mu=log_norm_out.best_values['mu']
            _sigma=log_norm_out.best_values['sigma']  
            
            pred = Log_normal(x_data,_A,_mu,_sigma)
            
            
            current_table = pd.DataFrame({'Original':y_data, 'Pred':pred})
            current_table_without_zero = current_table.loc[current_table['Original']!=0,:]
            
            squared_error = np.square((np.array(current_table_without_zero.loc[:,'Original']) - np.array(current_table_without_zero.loc[:,'Pred'])))
            sum_squared_error = np.sum(squared_error)
            current_RMSE = np.sqrt(sum_squared_error / len(np.array(current_table_without_zero.loc[:,'Original'])))
                        
            if best_RMSE==None:

                
                
                best_params_dict=log_norm_out.best_values
                best_RMSE = current_RMSE
                
            elif best_RMSE>current_RMSE:
                
                
                best_params_dict=log_norm_out.best_values
                best_RMSE = current_RMSE
    
    
    
    
    
    

    

    best_params_df=pd.DataFrame([best_params_dict])
    
    
    fit_table['Data']='Original_Data'
    simulation_table=pd.DataFrame(np.column_stack((x_data,pred)),columns=distribution_df.columns)
    
    simulation_table['Data']='Fit_Data'
    
    
    fit_table=pd.concat([fit_table, simulation_table], axis=0)
    
    if do_plot:
        fit_plot = p9.ggplot()+p9.geom_point(fit_table.loc[fit_table['Data']=="Original_Data",:], p9.aes(x="Feature", y='Count'),color = 'black') + p9.geom_line(fit_table.loc[fit_table['Data']=="Fit_Data",:], p9.aes(x="Feature", y='Count'),color='red')
        #fit_plot+=p9.xlim(-.5,5)
        fit_plot.show()
    
    fit_df=pd.DataFrame(columns=["Fit",*best_params_df.columns,'RMSE'])
    
    new_line_test=pd.DataFrame(["LogNormal",*best_params_df.loc[0,:],best_RMSE]).T
    new_line_test.columns = fit_df.columns
    
    fit_df=pd.concat([fit_df,new_line_test],ignore_index=True)
   

    return fit_df




def determine_Full_best_fit_table(Full_value_table,Full_dict):
    
    Full_fit_table = pd.DataFrame()
    population_done = []
    for column, columns_dict in Full_dict.items():
        for current_population, current_population_dict in columns_dict.items():
            
            if current_population in population_done:
                continue
            Sub_table = Full_value_table.loc[Full_value_table[column]==current_population, :].copy()
            Sub_table_fit_table = determine_best_fit(Sub_table, current_population_dict)
            

            Sub_table_fit_table.loc[:,'Population']=current_population
            Full_fit_table = pd.concat([Full_fit_table, Sub_table_fit_table],ignore_index=True )
            population_done.append(current_population)
    return Full_fit_table
        

def determine_best_fit(Full_value_table, fit_dict):
    
    Full_fit_table = pd.DataFrame()
    value_table = Full_value_table.loc[Full_value_table['General_area']=='VIS',:]
    for feature in fit_dict.keys():
        layer = fit_dict[feature]
        value_table = value_table.astype({feature : 'float'})
        value_array = np.array(value_table.loc[:, feature].dropna())
        
            

        bin_width = Freedman_Diaconis_bin_width(value_array)
        for current_layer in value_table.loc[:,'Layer_second'].unique():
            
            if layer == 'Per_layer':
                current_value_array = np.array(value_table.loc[value_table['Layer_second']==current_layer, feature].dropna())
                if len(current_value_array)<5:
                    current_value_array = np.array(value_table.loc[:, feature].dropna()) # if less than 10 value for a given layer, then take all layers
                    layer = "Not enough value in population, take all layers"
            else:
                current_value_array = np.array(value_table.loc[:, feature].dropna()) # Take all layers
            
            if feature == 'Adaptation_index':
                current_value_array = current_value_array[current_value_array<=1000]
            #bin_width = Freedman_Diaconis_bin_width(current_value_array)
            if len(current_value_array)<5:
                current_value_array = np.array(value_table.loc[:, feature].dropna()) # if less than 10 value for a given layer, then take all layers
                layer = "Not enough value in population, take all layers"
            bin_df = get_data_bin_df(current_value_array, bin_width,'Width')
            current_fit_df = fit_distribution(bin_df, ['LogNormal', 'Gaussian', 'Skewed Gaussian'])
            current_fit_df.loc[:,"Nb_obs"]=len(current_value_array)
            current_fit_df.loc[:,'Feature']=feature
            current_fit_df.loc[:,'Layer_second']=current_layer
            current_fit_df.loc[:,'Bin_width']=bin_width
            current_fit_df.loc[:,"Population_used"] = layer
            current_fit_df=current_fit_df.astype({'RMSE':'float'})
            current_fit_df= current_fit_df.drop(columns=['C', 'tau'])
            best_current_fit_df = current_fit_df.loc[current_fit_df['RMSE']==np.nanmin(current_fit_df['RMSE'])]

            Full_fit_table = pd.concat([Full_fit_table, best_current_fit_df],ignore_index=True)
    
    return Full_fit_table
                
    


def Freedman_Diaconis_bin_width(value_array):
    
    bin_width = np.round((2*iqr(value_array))/((len(value_array))**(1/3)),2)
    return bin_width

def establish_new_class(population_class_table, upstroke_Downstroke_table):
    New_population_class_table = population_class_table.copy()
    Exc_Inh_dict = {"Excitatory":["Glt25d2-Cre_NF107", "Ctgf-T2A-dgCre", "Nr5a1-Cre", "Rbp4-Cre_KL100", "Rorb-IRES2-Cre", "Scnn1a-Tg3-Cre", "Scnn1a-Tg2-Cre", "Tlx3-Cre_PL56", "Cux2-CreERT2"],
                    "Inhibitory":["Chat-IRES-Cre-neo", "Vip-IRES-Cre", "Vipr2-IRES2-Cre", "Chrna2-Cre_OE25", "Nos1-CreERT2", "Sst-IRES-Cre", "Nkx2-1-CreERT2", 'Pvalb-IRES-Cre',"Gad2-IRES-Cre", "Ndnf-IRES2-dgCre", "Htr3a-Cre_NO152"]}

    cell_type_dict = {'Excitatory' : ["Glt25d2-Cre_NF107", "Ctgf-T2A-dgCre", "Nr5a1-Cre", "Rbp4-Cre_KL100", "Rorb-IRES2-Cre", "Scnn1a-Tg3-Cre", "Scnn1a-Tg2-Cre", "Tlx3-Cre_PL56", "Cux2-CreERT2"],
                      'Vip' : ["Chat-IRES-Cre-neo", "Vip-IRES-Cre", "Vipr2-IRES2-Cre"],
                      'Sst' : ["Chrna2-Cre_OE25", "Nos1-CreERT2", "Sst-IRES-Cre"],
                      'Pvalb' : ["Nkx2-1-CreERT2", 'Pvalb-IRES-Cre'],
                      "Htr3a" : ['Htr3a-Cre_NO152']}
    
    New_population_class_table.loc[:,'Exc_Inh']='--'
    for key, line_list in Exc_Inh_dict.items():
        
        New_population_class_table.loc[(New_population_class_table['line_name'].isin(line_list)) & (New_population_class_table['cell_reporter_status']=='positive'),'Exc_Inh']=key
        
    
    
    
    
    Full_gaussian_table = pd.DataFrame()

    gmm = GaussianMixture(n_components=2)
    up_down_array = np.array(upstroke_Downstroke_table.loc[:,"Upstroke_to_downstroke"].dropna())
    gmm.fit(np.array(up_down_array).reshape(-1, 1))
    
    means = gmm.means_

    # Conver covariance into Standard Deviation
    standard_deviations = gmm.covariances_**0.5  

    # Useful when plotting the distributions later
    weights = gmm.weights_  
    
    
    
    extended_x_array=np.arange(np.nanmin(up_down_array), np.nanmax(up_down_array),.01)
    if means[0][0] < means[1][0]:
        pop_list = ['FS', 'NFS']
    else:
        pop_list = ['NFS', 'FS']
    
    for amp, mu, sigma, pop in zip (weights, means, standard_deviations, pop_list):
        current_y = gaussian(extended_x_array, amp, mu[0], sigma[0][0])
        current_table = pd.DataFrame({'Up_Dowtroke_ratio':list(extended_x_array), 'Count':list(current_y)})
        current_table.loc[:,'Population']=pop
        Full_gaussian_table = pd.concat([Full_gaussian_table, current_table],ignore_index=True)
    
    Full_gaussian_table.loc[:,"Count"]*=70
    NFS_table = Full_gaussian_table.loc[Full_gaussian_table['Population']=='NFS',['Count','Up_Dowtroke_ratio']]
    FS_table = Full_gaussian_table.loc[Full_gaussian_table['Population']=='FS',['Count','Up_Dowtroke_ratio']]
    
    NFS_FS_table = pd.merge(NFS_table,FS_table, on='Up_Dowtroke_ratio')
    NFS_FS_table.columns = ['NFS', 'Up_Dowtroke_ratio', 'FS']
    break_value = None
    for index, row in NFS_FS_table.iterrows():
        if row['NFS'] > row['FS']:
            break_value = row['Up_Dowtroke_ratio']
            break
    up_down_plot = p9.ggplot(upstroke_Downstroke_table, p9.aes(x='Upstroke_to_downstroke'))+p9.geom_histogram(bins=100)+p9.geom_line(Full_gaussian_table,p9.aes(x='Up_Dowtroke_ratio',y='Count',color='Population'),size = 1.)+p9.geom_vline(p9.aes(xintercept=break_value))
    up_down_plot.show()
    

    New_population_class_table.loc[:,'Exc_Inh_FT']='--'
    New_population_class_table = pd.merge(New_population_class_table, upstroke_Downstroke_table, on='Cell_id', how='outer')

    New_population_class_table.loc[New_population_class_table['Exc_Inh'] == 'Excitatory' ,'Exc_Inh_FT']="Excitatory"
    New_population_class_table.loc[(New_population_class_table['Exc_Inh'] == 'Inhibitory') & (New_population_class_table['Upstroke_to_downstroke']<=break_value) &(np.isnan(New_population_class_table['Upstroke_to_downstroke']) == False) ,'Exc_Inh_FT']="Inhibitory_Fast_spiking"
    New_population_class_table.loc[(New_population_class_table['Exc_Inh'] == 'Inhibitory') & (New_population_class_table['Upstroke_to_downstroke']>break_value) &(np.isnan(New_population_class_table['Upstroke_to_downstroke']) == False),'Exc_Inh_FT']="Inhibitory_Non_Fast_spiking"
    
    
    

    New_population_class_table.loc[:,'Cell_type']='--'
    
    for key, line_list in cell_type_dict.items():
        New_population_class_table.loc[(New_population_class_table['line_name'].isin(line_list)) & (New_population_class_table['cell_reporter_status']=="positive"),'Cell_type']=key
    
    #New_population_class_table=New_population_class_table.drop(columns=['Upstroke_to_downstroke'])

    return New_population_class_table
    
    for elt in New_population_class_table.index:
        if New_population_class_table.loc[elt,"cell_reporter_status"]=="positive":
            if New_population_class_table.loc[elt, "line_name"] in Exc_Inh_dict['Exc']:
                New_population_class_table.loc[elt, 'Exc_Inh'] = "Excitatory"
                New_population_class_table.loc[elt, 'Exc_Inh_FT'] = "Excitatory"
                New_population_class_table.loc[elt, 'Cell_type'] = "Excitatory"
                
            elif New_population_class_table.loc[elt, "line_name"] in Exc_Inh_dict['Inh']:
                New_population_class_table.loc[elt, 'Exc_Inh'] = "Inhibitory"
                if New_population_class_table.loc[elt, 'Upstroke_to_downstroke']<=break_value and np.isnan(New_population_class_table.loc[elt, 'Upstroke_to_downstroke']) == False:
                    New_population_class_table.loc[elt, 'Exc_Inh_FT'] = "Fast-Spiking"
                elif New_population_class_table.loc[elt, 'Upstroke_to_downstroke']>break_value and np.isnan(New_population_class_table.loc[elt, 'Upstroke_to_downstroke']) == False:
                    New_population_class_table.loc[elt, 'Exc_Inh_FT'] = "Non Fast-Spiking"
                
                if New_population_class_table.loc[elt, 'line_name'] in cell_type_dict['Htr3a']:
                    New_population_class_table.loc[elt, 'Cell_type'] = "Htr3a"
                elif New_population_class_table.loc[elt, 'line_name'] in cell_type_dict['Pvalb']:
                    New_population_class_table.loc[elt, 'Cell_type'] = "Pvalb"
                elif New_population_class_table.loc[elt, 'line_name'] in cell_type_dict['Sst']:
                    New_population_class_table.loc[elt, 'Cell_type'] = "Sst"
                elif New_population_class_table.loc[elt, 'line_name'] in cell_type_dict['Vip']:
                    New_population_class_table.loc[elt, 'Cell_type'] = "Vip"

Exc_dict={'Gain':'Per_layer','Threshold':'All_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'Per_layer'}
Inh_dict={'Gain':'All_layer','Threshold':'Per_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'Per_layer'}

FS_dict={'Gain':'All_layer','Threshold':'All_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'Per_layer'}
NFS_dict={'Gain':'Per_layer','Threshold':'Per_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'Per_layer'}

Htr3a_dict={'Gain':'All_layer','Threshold':'All_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'All_layer'}
Pvalb_dict={'Gain':'Per_layer','Threshold':'Per_layer','Input_Resistance_GOhms':'Per_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'All_layer'}
Sst_dict={'Gain':'All_layer','Threshold':'All_layer','Input_Resistance_GOhms':'All_layer', 'Time_constant_ms':'Per_layer', 'Adaptation_index':'Per_layer'}
Vip_dict={'Gain':'Per_layer','Threshold':'All_layer','Input_Resistance_GOhms':'All_layer', 'Time_constant_ms':'All_layer', 'Adaptation_index':'All_layer'}


# Exc_Inh_dict = {"Exc":["Glt25d2-Cre_NF107", "Ctgf-T2A-dgCre", "Nr5a1-Cre", "Rbp4-Cre_KL100", "Rorb-IRES2-Cre", "Scnn1a-Tg3-Cre", "Scnn1a-Tg2-Cre", "Tlx3-Cre_PL56", "Cux2-CreERT2"],
#                 "Inh":["Chat-IRES-Cre-neo", "Vip-IRES-Cre", "Vipr2-IRES2-Cre", "Chrna2-Cre_OE25", "Nos1-CreERT2", "Sst-IRES-Cre", "Nkx2-1-CreERT2", 'Pvalb-IRES-Cre',"Gad2-IRES-Cre", "Ndnf-IRES2-dgCre", "Htr3a-Cre_NO152"]}

# cell_type_dict = {'Exc' : ["Glt25d2-Cre_NF107", "Ctgf-T2A-dgCre", "Nr5a1-Cre", "Rbp4-Cre_KL100", "Rorb-IRES2-Cre", "Scnn1a-Tg3-Cre", "Scnn1a-Tg2-Cre", "Tlx3-Cre_PL56", "Cux2-CreERT2"],
#                   'Vip' : ["Chat-IRES-Cre-neo", "Vip-IRES-Cre", "Vipr2-IRES2-Cre"],
#                   'Sst' : ["Chrna2-Cre_OE25", "Nos1-CreERT2", "Sst-IRES-Cre"],
#                   'Pvalb' : ["Nkx2-1-CreERT2", 'Pvalb-IRES-Cre'],
#                   "Htr3a" : ['Htr3a-Cre_NO152']}

full_dict = {"Exc_Inh":{'Excitatory':Exc_dict, 
                        'Inhibitory':Inh_dict},
             "Exc_Inh_FT":{'Excitatory':Exc_dict,
                           'Fast-Spiking':FS_dict,
                           'Non Fast-Spiking':NFS_dict},
             "Cell_type":{'Excitatory':Exc_dict,
                          'Htr3a':Htr3a_dict,
                          'Pvalb':Pvalb_dict,
                          'Sst':Sst_dict,
                          'Vip':Vip_dict}}







