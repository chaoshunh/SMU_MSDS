#Install Packages
library(dataMaid) #For quick data visualization
library(dplyr) #For data cleaning and summarizing
library(ggplot2) #For histograms and other plots
library(scales) # For formatting in data labels
library(qwraps2) #For tables and document markdown
library(car) #For vif
library(aod) #For wald.test
library(reshape2)
library(Hmisc) #For histogram of all variables in a data frame
library(tidyr)
library(MASS)
library(pscl) # For goodness of fit
library(survey) #For individual predictors 
library(lmtest)
library(caret)
library(pROC)
library(broom)
library(purrr) #For keep function
library(corrplot) # For correlation plot
library(epiDisplay) # To calculate C-statistic and ROC curve
library(rms)
#getSessionInfo
sessionInfo()

#Load the data

#talentManage <- read.delim('E:/Bibliotecas/Documents/Data Science/SMU/MSDS 6306 Doing Data Science/Case Study 2/SMU_MSDS/Doing_Data_Science/CaseStudy2/data/casestudy2.csv', header = TRUE, sep = ',')
talentManage <-read.csv("C:\\Users\\chux\\datascience\\MSDS6306CaseStudy2\\data\\casestudy2.csv")

#talentManage <-read.csv("E:\\Mahesh\\SMU\\GitHub\\SMU_MSDS\\Doing_Data_Science\\CaseStudy2\\data\\casestudy2.csv")


## Thanks to RPubs Raju Rimal for this function

diagPlot<-function(model){
    p1<-ggplot(model, aes(.fitted, .resid))+geom_point()
    p1<-p1+stat_smooth(method="loess")+geom_hline(yintercept=0, col="red", linetype="dashed")
    p1<-p1+xlab("Fitted values")+ylab("Residuals")
    p1<-p1+ggtitle("Residual vs Fitted Plot")+theme_bw()
    
    p2<-ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid))+geom_point(na.rm = TRUE)
    p2<-p2+geom_abline(aes(qqline(.stdresid)))+xlab("Theoretical Quantiles")+ylab("Standardized Residuals")
    p2<-p2+ggtitle("Normal Q-Q")+theme_bw()
    
    p3<-ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+geom_point(na.rm=TRUE)
    p3<-p3+stat_smooth(method="loess", na.rm = TRUE)+xlab("Fitted Value")
    p3<-p3+ylab(expression(sqrt("|Standardized residuals|")))
    p3<-p3+ggtitle("Scale-Location")+theme_bw()
    
    p4<-ggplot(model, aes(seq_along(.cooksd), .cooksd))+geom_bar(stat="identity", position="identity")
    p4<-p4+xlab("Obs. Number")+ylab("Cook's distance")
    p4<-p4+ggtitle("Cook's distance")+theme_bw()
    
    p5<-ggplot(model, aes(.hat, .stdresid))+geom_point(aes(size=.cooksd), na.rm=TRUE)
    p5<-p5+stat_smooth(method="loess", na.rm=TRUE)
    p5<-p5+xlab("Leverage")+ylab("Standardized Residuals")
    p5<-p5+ggtitle("Residual vs Leverage Plot")
    p5<-p5+scale_size_continuous("Cook's Distance", range=c(1,5))
    p5<-p5+theme_bw()+theme(legend.position="bottom")
    
    p6<-ggplot(model, aes(.hat, .cooksd))+geom_point(na.rm=TRUE)+stat_smooth(method="loess", na.rm=TRUE)
    p6<-p6+xlab("Leverage hii")+ylab("Cook's Distance")
    p6<-p6+ggtitle("Cook's dist vs Leverage hii/(1-hii)")
    p6<-p6+geom_abline(slope=seq(0,3,0.5), color="gray", linetype="dashed")
    p6<-p6+theme_bw()
    
    return(list(rvfPlot=p1, qqPlot=p2, sclLocPlot=p3, cdPlot=p4, rvlevPlot=p5, cvlPlot=p6))
}


### 2.	Loading and cleaning the data 

#List column names
colnames(talentManage)

#Abbreviate column names
colnames(talentManage)=abbreviate(colnames(talentManage), method=c("both.sides"),minlength = 11)

columns <- colnames(talentManage)
columns
sapply(columns, nchar)

#basic statistics (EDA)
visualize(talentManage)

#copy the data to a new dataset talentManage1 for analysis
talentManage1 <- talentManage

str(talentManage1)

#Check the 'class' of each variable column
lapply(talentManage, class)

#Recode variables from 'numeric' to 'factor'
talentManage[,7] <- factor(talentManage[,7], labels = c("Below College", "College", "Bachelor", "Master", "Doctor"), ordered = TRUE)
talentManage[,11] <- factor(talentManage[,11], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,14] <- factor(talentManage[,14], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,17] <- factor(talentManage[,17], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,25] <- factor(talentManage[,25], labels = c('Excellent', 'Outstanding'), ordered = TRUE)
talentManage[,26] <- factor(talentManage[,26], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,31] <- factor(talentManage[,31], labels = c('Bad', 'Good', 'Better', 'Best'), ordered = TRUE)
talentManage[,15] <- factor(talentManage[,15], ordered = TRUE)
talentManage[,28] <- factor(talentManage[,28])

#Perform an additional recoding: divide the "Years with Current Manager" variable into three groups
talentManage[,35] <- cut(talentManage[,35], breaks = c(0,5,10,17), labels = c("0-5", "6-10", "More than 10"), right = TRUE, include.lowest = TRUE, ordered_result = TRUE)

# remove employeeCount, employeeNumber, Over18 from talentManage
talentManage[, c("Over18", "EmployeeCnt", "EmployeNmbr", "StandardHrs")] <- list(NULL)
talentManage1[, c("Over18", "EmployeeCnt", "EmployeNmbr", "StandardHrs")] <- list(NULL)

#store and remove monthly Income as we are using log Monthly Income
MonthlyIncome <- talentManage[,c("MonthlyIncm")]

# Convert Monthly income to log Monthly Income
talentManage$logMthlyInc <- log(talentManage$MonthlyIncm)
talentManage1$logMthlyInc <- log(talentManage$MonthlyIncm)

#Check that all variables have the correct 'class'
str(talentManage)

### 3.	Preliminary Analysis

#First we apply a function to check whether there are participants under age 18:

#Check every value of the 'Age' variable to see whether it is smaller than 18
summary(sapply(talentManage$Age, function(x) x < 18))

#Check every value of the 'Age' variable to see whether it is smaller than 18
summary(sapply(talentManage$Age, function(x) x >= 65))
retirement <- subset(talentManage$Age, talentManage$Age >=60)
retirement  

options(qwraps2_markup = "markdown")
summary_numeric <- with(talentManage,
                list("Age" = tab_summary(Age)[c(1,4,2,3)],
                     "Monthly Income" = tab_summary(MonthlyIncm)[c(1,4,2,3)],
                     "Distance From Home" = tab_summary(DistncFrmHm)[c(1,4,2,3)],
                     "Number of Companies Worked In" = tab_summary(NmCmpnsWrkd)[c(1,4,2,3)],
                     "Percent Salary Raise" = tab_summary(PrcntSlryHk)[c(1,4,2,3)],
                     "Total working Years" = tab_summary(TtlWrkngYrs)[c(1,4,2,3)],
                     "Years at Company" = tab_summary(YersAtCmpny)[c(1,4,2,3)]))
                     
table_numeric <- summary_table(talentManage, summary_numeric)
table_numeric

#Create the required histograms
ggplot(data = talentManage, mapping = aes(Age)) +
  geom_histogram(binwidth = 5, color = "gray", fill = "royalblue1" ) +
  labs(x = "Age", y = "Frequency", title = "Histogram of Age") + 
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = talentManage, mapping = aes(MonthlyIncm)) +
  geom_histogram(binwidth = 1000, color = "gray", fill = "orchid3" ) +
  labs(x = "Montly income (in US$)", y = "Frequency", title = "Histogram of Monthly Income") + 
  scale_x_continuous(label = comma) +
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(talentManage, aes(talentManage$logMthlyInc)) + 
  geom_histogram(binwidth=0.5, color="gray", fill="orchid3") +
  labs(x = "Lof of Montly income log(US$)", y = "Frequency", title = "Histogram of Log of Monthly Income") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Summarize categorical variables with counts

summary_factor <- with(talentManage,
                list("Gender" = tab_summary(factor(Gender)),
                     "Education" = tab_summary(factor(Education)),
                     "Ocupation" = tab_summary(factor(EducatinFld))))

table_factor <- summary_table(talentManage, summary_factor)
table_factor

#Management positions are 3 and above:
paste(talentManage$JobRole[110:125], talentManage$JobLevel[110:125], sep = ",")

summary_positions <- with(talentManage,
                     list("Management Positions" = tab_summary(factor(JobLevel[JobLevel == 3 | JobLevel ==4 | JobLevel ==5]))))

table_positions <- summary_table(talentManage, summary_positions)
table_positions

ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Attrition, fill = Attrition)) +
  annotate("text", label = "Yes = 233, or 15.9%", y=350, x=2) +
  annotate("text", label = "No = 1237, or 84.1%", y=1300, x=1) +
  labs(x = "Attrition", y = "Total", title = "Summary of Attrition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE)

#Graphing Age and Gender
ggplot((talentManage), aes(Age, as.numeric(Attrition)-1, color=Gender)) +
  stat_smooth(method="loess", formula=y~x, alpha=0.2, size=2, aes(fill=Gender)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  labs(title = "Attrition levels by age and gender", x= "Age", y = "Proportion of Attrition")

# Using distance from home and Marital Status:
ggplot((talentManage), aes(DistncFrmHm, as.numeric(Attrition)-1, color=MaritalStts)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=MaritalStts)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Distance from Home") + ylab("Attrition")

# Using Montly Income and Job Level:
ggplot((talentManage), aes(MonthlyIncm, as.numeric(Attrition)-1, color=JobLevel)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=JobLevel)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Monthly Income in US$") + ylab("Attrition")

# Using Years in the Company
ggplot((talentManage), aes(YersAtCmpny, as.numeric(Attrition)-1, color=StckOptnLvl)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=StckOptnLvl)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Years Worked at the Company") + ylab("Attrition")

#Using Salary Hike and Overtime
ggplot((talentManage), aes(PrcntSlryHk, as.numeric(Attrition)-1, color=OverTime)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=OverTime)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Percentage Salary Hike") + ylab("Attrition")


ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobSatsfctn, fill = Attrition), position = "fill") +
  labs(x = "Job Satisfaction (4 is higher)", y = "Proportion", title = "Attrition by Level of Job Satisfaction")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = EnvrnmntSts, fill = Attrition), position = "fill") +
  labs(x = "Environment Satisfaction (4 is higher)", y = "Proportion", title = "Attrition by Level of Environment Satisfaction")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = RltnshpStsf, fill = Attrition), position = "fill") +
  labs(x = "Relationship Satisfaction", y = "Proportion", title = "Attrition by Relationship Satisfaction")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobInvlvmnt, fill = Attrition), position = "fill") +
  labs(x = "Job Involvement (4 is higher)", y = "Proportion", title = "Attrition by Level of Job Involvement")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Department, fill = Attrition), position = "fill") +
  labs(x = "Department", y = "Proportion", title = "Attrition by Department")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = YrsWthCrrMn, fill = Attrition), position = "fill") +
  labs(x = "Years with Current Manager", y = "Proportion", title = "Attrition levels by Years with Current Manager")
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = BusinssTrvl, fill = Attrition), position = "fill") +
  labs(x = "Business Travel", y = "Proportion", title = "Attrition for Business Travel")

ggplot((talentManage), aes(NmCmpnsWrkd, as.numeric(Attrition)-1, color=EnvrnmntSts)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=EnvrnmntSts)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Number of previous Jobs") + ylab("Attrition")

ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobRole, fill = Attrition), position = "fill") +
  labs(x = "Job Role", y = "Proportion", title = "Attrition by Job Roles") +
  coord_flip()

talentManage$AggStsfctn = (as.numeric(talentManage$EnvrnmntSts) + as.numeric(talentManage$JobInvlvmnt) + as.numeric(talentManage$JobSatsfctn) + as.numeric(talentManage$RltnshpStsf)) /4

ggplot(data = talentManage, aes(x=JobRole, y = AggStsfctn, color=JobRole)) + 
  geom_boxplot() +
  labs(x = "Job Role", y = "Aggregate Satisfaction", title = "Aggregate Satisfaction by Job Role") +
  coord_flip() +
  guides(colour=FALSE)

### 4.	Deeper Analysis and Visualization 

talentManage1[, c("MonthlyIncm")] <- list(NULL)

#change variables to factors for talentManage1
talentManage1[,c("Education")] <- as.factor(talentManage1$Education)
talentManage1[,c("EnvrnmntSts")] <- as.factor(talentManage1$EnvrnmntSts)
talentManage1[,c("JobInvlvmnt")] <- as.factor(talentManage1$JobInvlvmnt)
talentManage1[,c("JobLevel")] <- as.factor(talentManage1$JobLevel)
talentManage1[,c("JobSatsfctn")] <- as.factor(talentManage1$JobSatsfctn)
talentManage1[,c("PrfrmncRtng")] <- as.factor(talentManage1$PrfrmncRtng)
talentManage1[,c("StckOptnLvl")] <- as.factor(talentManage1$StckOptnLvl)
talentManage1[,c("TrnngTmsLsY")] <- as.factor(talentManage1$TrnngTmsLsY)
talentManage1[,c("WorkLifBlnc")] <- as.factor(talentManage1$WorkLifBlnc)
talentManage1[,c("NmCmpnsWrkd")] <- as.factor(talentManage1$NmCmpnsWrkd)

str(talentManage1)
# Regression for full Model
fullModel <- glm(Attrition ~ Age + BusinssTrvl + DailyRate + Department + DistncFrmHm + 
    Education + EducatinFld + EnvrnmntSts + Gender + HourlyRate + 
    JobInvlvmnt + JobLevel + JobRole + JobSatsfctn + MaritalStts + 
    MonthlyRate + NmCmpnsWrkd + OverTime + PrcntSlryHk + PrfrmncRtng + 
    RltnshpStsf + StckOptnLvl + TtlWrkngYrs + TrnngTmsLsY + WorkLifBlnc + 
    YersAtCmpny + YrsInCrrntR + YrsSncLstPr + YrsWthCrrMn + logMthlyInc, data=talentManage1, family=binomial(link = logit))

summary(fullModel)

#anova(fullModel, test="Chisq")

#Get the confidence intervals for full Model
exp(cbind(coef(fullModel), confint(fullModel)))

#Model with only the intercept
nothing <- glm(Attrition ~ 1, data=talentManage1, family=binomial(link=logit))

summary(nothing)

#backwards = step(fullModel)

#backwards = step(fullModel, list(lower=formula(nothing),upper=formula(fullModel)), direction="backward",trace=0)
 
#formula(backwards)

#summary(backwards)

#back2 = glm(Attrition ~ Age + BusinssTrvl + DailyRate + DistncFrmHm + EducatinFld + 
#    EnvrnmntSts + Gender + JobInvlvmnt + JobRole + 
#    JobSatsfctn + NmCmpnsWrkd + OverTime + RltnshpStsf + StckOptnLvl + 
#    TtlWrkngYrs + TrnngTmsLsY + WorkLifBlnc + YrsInCrrntR + YrsSncLstPr + 
#    YrsWthCrrMn + logMthlyInc, data=talentManage, family=binomial(link = logit))

#summary(back2)

# Check overall effect of JobRole using aod library
#wald.test(b=coef(back2), Sigma=vcov(back2), Terms=18:25)

# Check overall effect of Education Field using aod library
#wald.test(b=coef(back2), Sigma=vcov(back2), Terms=6:10)

#car::vif(back2)

probabilities <- round(predict(fullModel, type = "response"), 2)

dataForAnalysis <- talentManage1 %>%
  dplyr::select_if(is.numeric) 

cor1 <- cor(data.matrix(talentManage1), method=c("kendall"))
corrplot(cor1)

#dataForAnalysis[, rm("log")]
predictors <- colnames(dataForAnalysis)

#Bind the logit and tidying the data for plot
dataForAnalysis <- dataForAnalysis %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  tidyr::gather(key = "predictors", value = "predictor.value", -logit)

ggplot(dataForAnalysis, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")+
  labs(title="Predicted values Vs Logit for Continuous variables", x="Logit Values", y="Predicted Values")+
  theme(plot.title = element_text(hjust = 0.5))

## forward selection

forwards = step(nothing, scope=list(lower=formula(nothing),upper=formula(fullModel)), direction="forward", trace=0)

formula(forwards)

#### Check the vif for the forwards model. We do find that JobLevel and JobRole are highly correlated so doing chi-square test of independence  
#### Chi- square test reveals that these two variables are independent   

# Variance inflation factor
car::vif(forwards)

#Check parameters selected are independent
cJob <- cbind(talentManage1$JobLevel, talentManage1$JobRole)

chisq.test(cJob)

### Removing JobLevel since JobRole and JobLevel are highly correlated

forward2 <- glm(Attrition ~ OverTime + JobRole + StckOptnLvl +  EnvrnmntSts +  
    BusinssTrvl + JobInvlvmnt + DistncFrmHm + JobSatsfctn + NmCmpnsWrkd + 
    TtlWrkngYrs + WorkLifBlnc + RltnshpStsf + YrsSncLstPr + logMthlyInc + 
    YrsInCrrntR + EducatinFld + TrnngTmsLsY + Gender + YrsWthCrrMn + 
    DailyRate + Age, data=talentManage1, family = binomial(link = logit))

summary(forward2)

#Get the correlation plot of the variables selected

#Get the confidence intervals
exp(cbind(coef(forward2), confint(forward2)))

# Variance inflation factor
car::vif(forward2)

# How accurate our test is
lroc(forward2)

# Check overall effect of JobRole using aod library
wald.test(b=coef(forward2), Sigma=vcov(forward2), Terms=2:9)

# Check overall effect of Stock Option Level using aod library
wald.test(b=coef(forward2), Sigma=vcov(forward2), Terms=10:12)

# Check overall effect of No of companies worked using aod library
wald.test(b=coef(forward2), Sigma=vcov(forward2), Terms=25:33)

# Check overall effect of Environment Status using aod library
wald.test(b=coef(forward2), Sigma=vcov(forward2), Terms=13:15)

# Stepwise method
bothways = step(nothing, list(lower=formula(nothing),upper=formula(fullModel)), direction="both",trace=0)

formula(bothways)

summary(bothways)

# Variance inflation factor
car::vif(bothways)

# get the variance of all the models
# backwards$deviance
forwards$deviance
bothways$deviance

#AIC values
cbind(forwards$aic, bothways$aic)

# Goodness of fit using LRT test
# Null hypothesis is that restricted model is statistically better than the unrestricted model
# nothing is the restricted model as it has only intercept whereas forward2 model has more variables
library(lmtest)
lrtest(forward2, nothing)

# using manual calculation for lrttest
#logLik(nothing)
#logLik(forward2)
# Calculate 2(loglikelihood of unrestricted model - loglikelihood of restricted model)
# gives computed value for LR
#2* (649.2914 - 409.8262)
# Get critcal value for LRTest, get df from logLik
#qchisq(0.95, 1)

# Same lrttest can be done using anova.
#anova(forward2, nothing, test="LRT")

# F test to test predictors after accounting for all others
drop1(forward2, test="F")

# Goodness of fit test using Pseudo R^2
pR2(forward2)

# Goodness of fit using the Hosmer-Lemeshow in R
#library(ResourceSelection)
#hl <- hoslem.test(forward2$model$Attrition, fitted(forward2), g=10)

## Test of individual predictors  
### The 3 best predictors for the model are OverTime, #StckOptnLvl, Number of companies worked

imp <- as.data.frame(varImp(forward2))
imp <- data.frame(overall = imp$Overall,
           names   = rownames(imp))
imp[order(imp$overall,decreasing = T),]

#print confusion Matrix to predict accuracy
pred <- as.factor(ifelse(predict(forward2, type="response") > 0.5, 1,0))
ref1 <- as.factor(ifelse(talentManage1$Attrition == "Yes", 1, 0))
confusionMatrix(data = pred, reference = ref1)


## Checking the model with Pearson Studentized residuals, cooksd, leverage and Standard Errors

#resid(forward2) 
#forward2$residuals

# partial residual plot against fitted values (explanatory variable)
plot(residuals(forward2, type="pearson") ~ talentManage1$Attrition, main="Partial residual Plot against fitted Values", xlab="Fitted Values", ylab="Partial Residual Plot") 

#Std.resid and cooks distance broom package augment
model.data <- augment(forward2) %>% 
  mutate(index = 1:n()) 

#Plot the standardized residuals:
ggplot(forward2, aes(.fitted, .resid)) + 
  geom_point(aes(color = Attrition), alpha = .5) +
  theme_bw()+
  labs(title="Studentized residuals vs Fitted values", x="Fitted values", y="Studentized residuals")+
  geom_smooth(se=F) +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(forward2, aes(seq_along(.cooksd), .cooksd)) + 
  geom_line(aes(color = Attrition), alpha = .5) +
  theme_bw()+
  labs(title="Cooks D vs observation number", x="Observation Nos", y="Cooks D values") +
  theme(plot.title = element_text(hjust = 0.5))

#The data for the top 3 largest values, according to the Cook's distance, can be displayed as follow
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .se.fit)) + 
  geom_point(aes(color = Attrition), alpha = .5) +
  theme_bw()+
  labs(title="Standard errors for Observation", x="Observations", y="Standard error") +
  theme(plot.title = element_text(hjust = 0.5))

# Residual Vs Leverage
ggplot(forward2, aes(.hat, .stdresid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point(aes(color = Attrition), alpha = .5) + geom_smooth(se = FALSE) +
  labs(title="Residual Vs Leverage", x="Leverage", y="Studentized residuals") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(forward2, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd, color = Attrition)) +
  geom_smooth(se = FALSE, size = 0.5) +
  labs(title="Residual Vs Leverage", x="Leverage", y="Studentized residuals") +
  theme(plot.title = element_text(hjust = 0.5))

# Cooks distance Vs Leverage

ggplot(forward2, aes(.hat, .cooksd)) +
  geom_vline(xintercept = 0, colour = NA) +
  geom_abline(slope = seq(0, 3, by = 0.5), colour = "white") +
  geom_smooth(se = FALSE) +
  geom_point(aes(color = Attrition), alpha = .5) +
  labs(title="Cooks D distance Vs Leverage", x="Leverage", y="Cooks D distance") +
    theme(plot.title = element_text(hjust = 0.5))


#age histogram
age_hist <- as.data.frame.table(table(talentManage$Age))

ggplot(data = age_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Age", y = "Frequency", title = "Histogram of Age") + 
  theme(plot.title = element_text(hjust = 0.5))

#Bsiness Travel histogram
BusinssTrvl_hist <- as.data.frame.table(table(talentManage$BusinssTrvl))

ggplot(data = BusinssTrvl_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Business Travel", y = "Frequency", title = "Business Travel Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Daily Rate histogram

DailyRate <- cut(talentManage[,c("DailyRate")], breaks = c(100,300,600,900,1200,1500), labels = c("100-300", "301-600", "601-900", "901-1200", "1201-1500"), right = TRUE, include.lowest = TRUE, ordered_result = TRUE)

DailyRate_hist <- as.data.frame.table(table(DailyRate))

ggplot(data = DailyRate_hist, mapping = aes(x=reorder(DailyRate, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Daily Rate", y = "Frequency", title = "Histogram of Daily Rate") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Department histogram
Department_hist <- as.data.frame.table(table(talentManage$Department))

ggplot(data = Department_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Department", y = "Frequency", title = "Department Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Distance From Home histogram
DistncFrmHm_hist <- as.data.frame.table(table(talentManage$DistncFrmHm))

ggplot(data = DistncFrmHm_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Distance From Home", y = "Frequency", title = "Distance From Home Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Education histogram
Education_hist <- as.data.frame.table(table(talentManage$Education))

ggplot(data = Education_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Education", y = "Frequency", title = "Education Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Education Field histogram
EducatinFld_hist <- as.data.frame.table(table(talentManage$EducatinFld))

ggplot(data = EducatinFld_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Education Field", y = "Frequency", title = "Education Field Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Environemnt Status histogram
EnvrnmntSts_hist <- as.data.frame.table(table(talentManage$EnvrnmntSts))

ggplot(data = EnvrnmntSts_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Environment Status", y = "Frequency", title = "Environment Status Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Gender histogram
Gender_hist <- as.data.frame.table(table(talentManage$Gender))

ggplot(data = Gender_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Gender", y = "Frequency", title = "Gender Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Hourly Rate histogram
HourlyRate_hist <- as.data.frame.table(table(talentManage$HourlyRate))

ggplot(data = HourlyRate_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Hourly Rate", y = "Frequency", title = "Hourly Rate Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Job Involvement histogram
JobInvlvmnt_hist <- as.data.frame.table(table(talentManage$JobInvlvmnt))

ggplot(data = JobInvlvmnt_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Job Involvement", y = "Frequency", title = "Job Involvement Histogram") + 
  theme(plot.title = element_text(hjust = 0.5))

#Job Level histogram
JobLevel_hist <- as.data.frame.table(table(talentManage$JobLevel))

ggplot(data = JobLevel_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Job Level", y = "Frequency", title = "Job Level Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Job Role histogram
JobRole_hist <- as.data.frame.table(table(talentManage$JobRole))

ggplot(data = JobRole_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Job Role", y = "Frequency", title = "Job Role Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Job Satisfaction histogram
JobSatsfctn_hist <- as.data.frame.table(table(talentManage$JobSatsfctn))

ggplot(data = JobSatsfctn_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Job satisfaction", y = "Frequency", title = "Job Satisfaction Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Marital Status histogram
MaritalStts_hist <- as.data.frame.table(table(talentManage$MaritalStts))

ggplot(data = MaritalStts_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Marital Status", y = "Frequency", title = "Maital Status Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Job Satisfaction histogram

MonthlyRate <- cut(talentManage[,c("MonthlyRate")], breaks = c(0,5000,10000,15000,20000,25000, 30000), labels = c("0-5000", "5001-10000", "10001-15000", "15001-20000", "20001-25000", "25001-30000"), right = TRUE, include.lowest = TRUE, ordered_result = TRUE)

MonthlyRate_hist <- as.data.frame.table(table(MonthlyRate))

ggplot(data = MonthlyRate_hist, mapping = aes(x=reorder(MonthlyRate, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Job satisfaction", y = "Frequency", title = "Job Satisfaction Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Number of Companies worked histogram
NmCmpnsWrkd_hist <- as.data.frame.table(table(talentManage$NmCmpnsWrkd))

ggplot(data = NmCmpnsWrkd_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Number of companies worked", y = "Frequency", title = "Number of companies worked Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Overtime histogram
OverTime_hist <- as.data.frame.table(table(talentManage$OverTime))

ggplot(data = OverTime_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Oertime", y = "Frequency", title = "Overtime Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Percent Salary Hike histogram
PrcntSlryHk_hist <- as.data.frame.table(table(talentManage$PrcntSlryHk))

ggplot(data = PrcntSlryHk_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Percent Salary", y = "Frequency", title = "Percent Salary Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Performance rating histogram
PrfrmncRtng_hist <- as.data.frame.table(table(talentManage$PrfrmncRtng))

ggplot(data = PrfrmncRtng_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Performance Rating", y = "Frequency", title = "Performance Rating Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Relationship Satisfaction histogram
RltnshpStsf_hist <- as.data.frame.table(table(talentManage$RltnshpStsf))

ggplot(data = RltnshpStsf_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Relationship Satisfaction", y = "Frequency", title = "Relationship Satisfaction Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Standard Hours histogram
#StandardHrs_hist <- as.data.frame.table(table(talentManage$StandardHrs))

#ggplot(data = StandardHrs_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
#  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
#  labs(x = "Standard Hours", y = "Frequency", title = "Standard Hours Histogram") + 
#  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Stock Option Level histogram
StckOptnLvl_hist <- as.data.frame.table(table(talentManage$StckOptnLvl))

ggplot(data = StckOptnLvl_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Stock Option Level", y = "Frequency", title = "Stock Option Level Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Total Working years histogram
TtlWrkngYrs_hist <- as.data.frame.table(table(talentManage$TtlWrkngYrs))

ggplot(data = TtlWrkngYrs_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Total Working Hours", y = "Frequency", title = "Total Working Hours Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Training Time Last Year histogram
TrnngTmsLsY_hist <- as.data.frame.table(table(talentManage$TrnngTmsLsY))

ggplot(data = TrnngTmsLsY_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Training Time Last Year", y = "Frequency", title = "Training Time Last Year Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Work Life Balance histogram
WorkLifBlnc_hist <- as.data.frame.table(table(talentManage$WorkLifBlnc))

ggplot(data = WorkLifBlnc_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Work Life Balance", y = "Frequency", title = "Work Life Balance Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Years at company histogram
YersAtCmpny_hist <- as.data.frame.table(table(talentManage$YersAtCmpny))

ggplot(data = YersAtCmpny_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Years at Company", y = "Frequency", title = "Years at Company Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Years in current role histogram
YrsInCrrntR_hist <- as.data.frame.table(table(talentManage$YrsInCrrntR))

ggplot(data = YrsInCrrntR_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Years in Current Role", y = "Frequency", title = "Years in Current Role Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Years since last promotion histogram
YrsSncLstPr_hist <- as.data.frame.table(table(talentManage$YrsSncLstPr))

ggplot(data = YrsSncLstPr_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Years Since Last Promotion", y = "Frequency", title = "Years Since Last Promotion Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))

#Years with Current Manager histogram
YrsWthCrrMn_hist <- as.data.frame.table(table(talentManage$YrsWthCrrMn))

ggplot(data = YrsWthCrrMn_hist, mapping = aes(x=reorder(Var1, -Freq), y=Freq)) +
  geom_bar(stat="identity", col = "gray", fill = "royalblue1" ) +
  labs(x = "Years with Current Manager", y = "Frequency", title = "Years with Current Manager Histogram") + 
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle=90, hjust=1))


ggplot(talentManage1, aes(Age, MonthlyIncome, color=Gender)) + 
  geom_point() +
  labs(title="Scatter plot of Monthly Income vs Age", xlab="Age", ylab="Monthly Income")+
  geom_smooth(method="lm", alpha=0.05)

ggplot(talentManage, aes(WorkLifBlnc, MonthlyIncome, color=Gender)) + 
  geom_point() +
  labs(title="Scatter plot of Monthly Income vs Life Satisfction", xlab="Life Satisfaction", ylab="Monthly Income")+
  geom_smooth(method="lm", alpha=0.05) +
  geom_abline()



