#Execution Sample:

#############When New Models Needs To Be Built Interactively:
####### Model Definition
glm_model <- function(formula) {
  glm_model <- glm(formula, data = iris, family = "binomial")
  out <- data.frame(glm_model$y, fitted(glm_model)) #### First Argument should be 0/1, 
                                                    #### Second Argument should be prob value ranging 0-1
  return(out)
}
######### Shiny App Calling
source("Model Performance RShiny App.R")
shinyPerfMeasures(model_function = glm_model, data = iris,y = "Species")
shinyConfMatrix(model_function = glm_model, data = iris,y = "Species")

##########When Multiple Models Needs To Be Compared:
############# Model Development
model_1 <- glm(Species ~ ., data = iris, family = binomial)
df1 <- data.frame(model_1$y, fitted(model_1)) #### First Argument should be 0/1, 
                                              #### Second Argument should be prob value ranging 0-1
model_2 <- glm(Species ~ Sepal.Length + Sepal.Width, data = iris, family = binomial)
df2 <- data.frame(model_2$y, fitted(model_2))  #### First Argument should be 0/1, 
                                               #### Second Argument should be prob value ranging 0-1
######### Shiny App Calling
source("Model Performance RShiny App.R")
shinyPerfMeasures(list_models = list(df1, df2),data = iris,y = "Species")
shinyConfMatrix(list_models = list(df1, df2),data = iris,y = "Species")
