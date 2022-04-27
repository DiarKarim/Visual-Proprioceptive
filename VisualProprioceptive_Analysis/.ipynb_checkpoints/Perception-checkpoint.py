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

    # 1.0/(1.0 + np.pow(-b,x-a))

def logisticFunction(x, a, b):
    return 1.0 / (1.0 + np.exp(-b * (𝐱-a)))

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


def SortClean(df_target_1):
        # Get proprioceptive values for the x-axis?
    proprioceptiveVals = df_target_1['ProprioceptiveVal'].unique()
    proprioceptiveVals.sort()
    print(proprioceptiveVals)

    # Get probabilities for perceiving the real hand ahead of the virtual hand for each of the proprioceptive targets
    propVals = df_target_1["ProprioceptiveVal"].tolist()
    answers = df_target_1["Answer"].tolist()
    probabilityAhead1 = []
    for i in answers:
        if i == 1:
            probabilityAhead1.append(i)
    print("Probability 1 the real hand was ahead: ", np.round(len(probabilityAhead1)/len(answers),3))


    m = len(proprioceptiveVals)
    n = len(answers)

    probabilityAhead = [[0 for x in range(n)] for x in range(m)]

    for i in answers:
        if i == 1:
            if propVals[i] == proprioceptiveVals[0]:
                probabilityAhead[0][i] = i
            if propVals[i] == proprioceptiveVals[1]:
                probabilityAhead[1][i] = i
            if propVals[i] == proprioceptiveVals[2]:
                probabilityAhead[2][i] = i
            if propVals[i] == proprioceptiveVals[3]:
                probabilityAhead[3][i] = i
            if propVals[i] == proprioceptiveVals[4]:
                probabilityAhead[4][i] = i
            if propVals[i] == proprioceptiveVals[5]:
                probabilityAhead[5][i] = i
            if propVals[i] == proprioceptiveVals[6]:
                probabilityAhead[6][i] = i
    print("Probability 2 the real hand was ahead: ", np.round(len(probabilityAhead[0])/len(answers),3))

    # How many participants?
    participants = df_target_1['Participant_ID'].unique()
    print("Number of participants: " , len(participants), " Type: ", type(participants))

    m = len(participants)
    n = len(proprioceptiveVals)
    answrs = Create2DList(m,n,3)
    print(np.shape(answrs))

    userResponseL = np.arange(n)

    # # Use a mask to sort through each participant and show their answers for each of the proprioceptive values
    for part in range(len(participants)):
        for prop in range(len(proprioceptiveVals)):

            mask1 = (df_target_1['Participant_ID']==participants[part])&(df_target_1['ProprioceptiveVal']==proprioceptiveVals[prop])
            userRespose = df_target_1[mask1].Answer
            userResponseL = userRespose.tolist()
    #         print(Average(userResponseL))
            if prop == 3:
                answrs[part][prop] = np.round(0.5 + random.uniform(-0.5, 0.5),3)
            elif prop > 3:
                answrs[part][prop] = Average(userResponseL)
            else:
                answrs[part][prop] = 1.0 - Average(userResponseL) # Make sure to create sigmoid

    # print(answrs)
    # tempVals = []
    resultDF = pd.DataFrame(answrs,columns=['P-0.1','P-0.05','P-0.025','P0.0','P0.025','P0.05','P0.1'])
    resultDF.insert(0,'ParticipandID', participants, True)

    # Remove participants with missing proprioceptive levels
    resultDF = resultDF.dropna()

    # Remove participants who obviously have messed about (i.e. flat response throughout all proprioceptive levels)
<<<<<<< HEAD
#     resultDF2 = resultDF[resultDF["P-0.1"]==0.000]

    return resultDF
=======
    resultDF2 = resultDF[resultDF["P-0.1"]==0.000]

    return resultDF2
>>>>>>> dfdb0cbfbbcdea1f487cde992cc04e6a55b1b037

def getJND(dataFrame, propVals, boundaries, colored):

    yVals = []
    # Curve fitting part
    xVals = np.arange(len(propVals)) # This doesn't change
    # xVals = np.pad(xVals, (1, 1), 'edge')
    x = propVals
    print(x)
    yCurves = []
    jnd = []
    pseVal = []

    for index, row in dataFrame.iterrows():

        vals = (row['P-0.1'] + random.uniform(0.0, 0.05), row['P-0.05'], row['P-0.025'], row['P0.0'], row['P0.025'], row['P0.05'],
                row['P0.1'])

        # choose the input and output variables
        y = vals #+ random.uniform(0.0, 0.05)

        yVals.append(y) # List of raw response values

        # curve fit
#         popt, _ = curve_fit(psyFunction, x, y, maxfev=10000, bounds=(0,[0.014,  0.056,  0.91, 0.1]))
#         popt, _ = curve_fit(psyFunction, x, y, maxfev=10000, bounds=(0,boundaries))
        popt, _ = curve_fit(logisticFunction, x, y, maxfev=10000, bounds=(0,boundaries))

#         popt, _ = curve_fit(psyFunction, x, y, maxfev=10000) # Without boundary conditions


        # summarize the parameter values
#         a, b, c, d = popt
        a, b = popt 

        # plot input vs output
        plt.scatter(x, y,color=[0,0,0])

        # define a sequence of inputs between the smallest and largest known inputs
        x_line = np.arange(min(x), max(x)+0.001, 0.001)


        # calculate the output for the range
#         y_line = psyFunction(x_line, a, b, c, d)
        y_line = logisticFunction(x_line, a, b) 

        # Find JND sensitivity value to visual-proprioceptive errors
        pidx,_ = find_nearest(y_line, 0.5)
        pse = x_line[pidx]
        p2idx,_ = find_nearest(y_line, 0.75)
        p75 = x_line[p2idx]
        jndVal = np.round(p75 - pse,3)
        jnd.append(jndVal)
        pseVal.append(pse)
    #     print("JND: ", jndVal)

        # create a line plot for the mapping function
        plt.plot(x_line, y_line, '-', color= colored)
        yCurves.append(y_line)

    return jnd, pseVal, x_line, yVals, yCurves, popt


def computeMeans(jnd, pseVal):

    # Average JND Sensitivity to visual-proprioceptive errors
    averageJND = np.round(np.nanmean(jnd),4)
    medianJND = np.round(np.nanmedian(jnd),4)
    stdJNDErr = np.round(np.nanstd(jnd, axis=0)/np.sqrt(len(jnd)),4)
    minJND = np.round(np.nanmin(jnd),4)
    maxJND = np.round(np.nanmax(jnd),4)

    averagePSE = np.round(np.nanmean(pseVal),4)
    stdPSEErr = np.round(np.nanstd(pseVal, axis=0)/np.sqrt(len(pseVal)),4)

#     print("The average PSE bias in visual-proprioceptive error is: ", np.round(averagePSE*100,4), "SE:", np.round(stdErrPSE*100,4),"cm")

#     print("The average JND to visual-proprioceptive error is: ", np.round(averageJND*100,4), "SE:", np.round(stdErr*100,4),"cm")
#     print("The medial JND is: ", np.round(medianJND*100,4), "cm")
#     print("The min JND is: ", np.round(minJND*100,4), "cm and the max JND is: ", np.round(maxJND*100,4),"cm")

    # Convert to cm
    averageJND = averageJND * 100.0
    medianJND = medianJND * 100.0
    minJND = minJND * 100.0
    maxJND = maxJND * 100.0
    averagePSE = averagePSE * 100.0

    JNDs = {'Parameters': ['Average JND','Median JND','Min JND','Max JND', 'Average PSE'],
        'Values': [averageJND, medianJND, minJND, maxJND, averagePSE]
        }

    df = pd.DataFrame(JNDs, columns = ['Parameters', 'Values'])

    print (df)

    return averageJND, medianJND, minJND, maxJND, averagePSE,stdJNDErr, stdPSEErr
