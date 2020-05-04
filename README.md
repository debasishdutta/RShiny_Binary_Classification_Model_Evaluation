# RShiny_Binary_Classification_Model_Evaluation
Binary Classification Model Performance Evaluation Using RShiny Application

Disclaimer: 
This code snippet is developed with an intension for generic use only. I hereby declare that the code was not part of any of my professional development work. Also, no sensitive or confidential data sources are used for this development. 

Description: 
This interactive R-Shiny app specifically designed for Binary Classification problem. The app can be used in following two circumstances: 
1. When a user has a pool of predictors and wants to develop a binary classification model from the scratch using best possible combination of predictors. 
2. When a user has already developed multiple binary classification models but wants to observe the various performance and accuracy metrices across these models. 

Function Definition: 
1. shinyPerfMeasures : This function evaluates newly built or already existing multiple models based on following performance measures: 
a. Hosmer Lemeshow Test 
b. Calibration Plot 
c. Lift 
d. Concordance, Discordance & Tie. 
2. shinyConfMatrix : This function evaluates newly built or already existing multiple models based on following performance measures: 
a. Confusion Matrix 
b. Accuracy, True Positive Rate, False Positive Rate, Precision Plots at various probability cutoffs. 

Function Inputs: 
Both the functions mentioned above accepts below inputs: 
1. list_models: It should be used only when the user wants to compare already built multiple models. This is a list of one or more data frames for each model whose performance is to be evaluated. Each data frame should comprise of two columns; the first column indicating the actual class labels (0 or 1) and the second column providing the raw predicted probabilities (in the range of 0 to 1). The model_function parameter should not be included while function calling if list_models is used. 
2. sample_size_concord: For computing concordance-discordance measures (and c-statistic) a random sample is drawn from each dataset (if nrow(dataset) > 5000). Default sample size of 5000 can be adjusted by changing the value of this argument. 
3. model_function: It should be used only when new models has to be built interactively. For this option to work, a model function should be passed as an argument. The model function should take a formula as an argument, and return a data frame as output (data frame should comprise of two columns with the first column indicating the class labels (0 or 1) and the second column providing the raw predicted probabilities). The list_models parameter should not be included while function calling if model_function is used. 
4. Data: The name of the data-set. The Independent Variable (IV) names, for interactive model building, is picked up from this data set. 
5. y: The column name of the Dependent Variable (DV), for interactive model building. 

Execution Overview: 

These functions while called will launch a RShiny app. Input parameters such as the number of bins or the probability threshold can be adjusted through app widgets. 

For interactive Model building, a model function, data set and the dependent variable name should be passed as arguments. Interactive model building option creates additional input widgets in the app. This includes: 

1. A drop down to select independent variables. The names of the variables will be picked up from the data argument. Kindly exclude dependent variable name while choosing the predictor names. 

2. An input slider to include additional models. Currently up to 10 additional models can be created. Each additional model updates the original model created. For e.g. consider the dataset has 10 predictors: x1-x10. Original model was created by selecting x1-x4 from the drop-down list. If we need to create a second model, by including x5 and excluding x3 simply type, “+ x5 - x3" in the input text box. 

When multiple already built models needs to be compared then a list of one or more data frames each having two columns; actual class labels (0 or 1) and raw predicted probabilities (in the range of 0 to 1) from each model needs to be passed as an argument along with actual data set and the dependent variable name. 

The 'Run-Analysis' button in the app, will generate model performance output basis selected input parameters.

Compatibility:
The code is developed and tested on RStudio (Version 1.0.44) using R-3.3.2
