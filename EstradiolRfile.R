#my project requires doing three different logistic regression analyses, one each for each hormone chemical as the IV
#the DV is the presence of acne for all three

#logistic regression analysis with Estradiol as IV
#load all packages needed for logistic regression
library("magrittr")
library("tidyr")
library("lmtest")
library("e1071")
library("caret")
library("dplyr")
library("popbio")
library("tidyverse")

#dependent variable is the presence of acne with options being present or not present, the dataset currently has
#several levels for the DV because it has levels of severity, so the DV needs to be recoded 0 (not present) and 1 (present)
#first rename Severity column to "Acne"
names(DATA)[names(DATA) == 'Severity'] <- 'Acne'
#recode to 0 and 1, in a text document that came with the dataset it was stated that 1, 2, and 3 severity meant acne was
#present and 4 meant there was no acne
DATA$AcnePresent <- NA
DATA$AcnePresent[DATA$Acne=='1'] <- 1
DATA$AcnePresent[DATA$Acne=='2'] <- 1
DATA$AcnePresent[DATA$Acne=='3'] <- 1
DATA$AcnePresent[DATA$Acne=='4'] <- 0

#test assumptions needed for logistic regression

#sample size assumption
#create regression model
logitmodel <- glm(AcnePresent ~ Estradiol, data=DATA, family="binomial")
#create predictions
probabilities <- predict(logitmodel, DATA, type="response")
DATA$Predicted <- ifelse(probabilities>.5, "pos", "neg")
#recode predicted variable
DATA$PredictedRecoded <- NA
DATA$PredictedRecoded[DATA$Predicted=='pos'] <- 1
DATA$PredictedRecoded[DATA$Predicted=='neg'] <- 0
#convert variables to factors for confusion matrix
DATA$PredictedRecoded <- as.factor(DATA$PredictedRecoded)
DATA$AcnePresent <- as.factor(DATA$AcnePresent)
#create confusion matrix
matrix <- caret::confusionMatrix(DATA$PredictedRecoded, DATA$AcnePresent)
matrix
#all values in Reference/Prediction table must be above five so assumption is not met
#will continue on for sake of learning
#accuracy rate is 82% which is good

#logit linearity assumption
#assessing this requires numeric variables
data1 <- DATA %>%
  dplyr::select_if(is.numeric)
#set column names of new dataframe
predictors <- colnames(data1)
#calculate the logit
data1 <- data1 %>%
  mutate(logit=log(probabilities/(1-probabilities))) %>%
  gather(key="predictors", value="predictor.value", -logit)
#graph the logit
ggplot(data1, aes(logit, predictor.value))+
  geom_point(size=.5, alpha=.5)+
  geom_smooth(method="loess")+
  theme_bw()+
  facet_wrap(~predictors, scales="free_y")
#estradiol graph shows line with slope so assumption is met

#don't need to test for multicollinearity assumption here because I only have one IV

#independent errors assumption
#graph errors
plot(logitmodel$residuals)
#graph doesn't entirely have even distribution of points across the x axis assumption not met
#double check using durbin-watson test
dwtest(logitmodel, alternative="two.sided")
#p value is < .05 so assumption is not met

#screen for outliers
infl <- influence.measures(logitmodel)
summary(infl)
#no dfb.1_ or dffit values > 1 or hat value > .3 so no outliers

#run regression
summary(logitmodel)
#p value is < .05 so amount of estradiol is a significant predictor of women having acne, however it is a very low significance
#in layman's terms, for every one unit decrease in estradiol, the log odds of having acne increases by 0.005
