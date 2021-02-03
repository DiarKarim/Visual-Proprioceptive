import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import os
import random
from scipy.optimize import curve_fit
from scipy.stats import norm

# Function Definitions
# Curve fitting part 
# define the true objective function ----------------------------------------------
def psyFunction(x,mu,sd,k,offset): #Sigmoid function 
    yhat = norm.cdf(x,mu, sd) * k + offset
    return yhat 

def exponentialFunction(x, a,b,c):
    return a*pow(x,2)+b*x+c
# --------------------------------------------------------------------------------------

def Create2DList(rows,cols,initVal):
    answrs=[]
    for j in range(rows):
        column = []
        for i in range(cols):
            column.append(initVal)
        answrs.append(column)
    return answrs


def Average(lst): 
    
    # Make sure no number 2s are included in the average
    if 2 in lst:
        lst.remove(2.0) 
    
    avrg = 0.0
    try:
        avrg = np.round(sum(lst) / len(lst),3)
    except Exception as e:
#         print(e)
        avrg = np.nan
    
    return avrg

def find_nearest(array, value):
    array = np.asarray(array)
    idx = (np.abs(array - value)).argmin()
    return idx, array[idx]