

#Install Packages
library(dataMaid) #For quick data visualization
library(dplyr) #For data cleaning
library(ggplot2) #For histograms and other plots
library(scales) # For formatting in data labels

#Load the data

talentManage <- read.delim('E:/Bibliotecas/Documents/Data Science/SMU/MSDS 6306 Doing Data Science/Case Study 2/SMU_MSDS/Doing_Data_Science/CaseStudy2/data/casestudy2.csv', header = TRUE, sep = ',')

#eXPLORE the data
dim(talentManage)

table(talentManage$HourlyRate, talentManage$JobLevel)

#Reduce the lenght of the variable names
colnames(talentManage)
colnames(talentManage)=abbreviate(colnames(talentManage), method=c("both.sides"),minlength = 11)
colnames(talentManage)


#Identify which variables have names longer than 12 characters
names <- names(data)
sapply(names, nchar)

#Changing the variable names
names(data) <- c("Age", "Attrition", "BusTravel", "DailyRate", "Department", "DistFromHome", "Education", "EducField", "EmplCount", "EmplNumber", "EnvSatisfact", "Gender", "HourlyRate", "JobInvolvmt", "JobLevel", "JobRole", "JobSatisfact", "MaritalStat", "MonthIncome", "MonthRate", "NumCoWork", "Over18", "OverTime", "PctSalryHike", "PerfRating", "RelSatisfact", "StdHours", "StockOptLvl", "TotWorkYrs", "TrngLstYr", "WorkLifeBal", "YrsAtCo", "YrsCurrrRole", "YrsScLstProm", "YrsWCurrMng")  
str(data)
lapply(talentManage, class)
class

#Recoding the variables from numeric to factor
talentManage[,7] <- factor(talentManage[,7], labels = c("Below College", "College", "Bachelor", "Master", "Doctor"), ordered = TRUE)
talentManage[,11] <- factor(talentManage[,11], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,14] <- factor(talentManage[,14], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,17] <- factor(talentManage[,17], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,25] <- factor(talentManage[,25], labels = c('Excellent', 'Outstanding'), ordered = TRUE)
talentManage[,26] <- factor(talentManage[,26], labels = c('Low', 'Medium', 'High', 'Very High'), ordered = TRUE)
talentManage[,31] <- factor(talentManage[,31], labels = c('Bad', 'Good', 'Better', 'Best'), ordered = TRUE)
talentManage[,15] <- factor(talentManage[,15], ordered = TRUE)
talentManage[,28] <- factor(talentManage[,28])
talentManage[,35] <- cut(talentManage[,35], breaks = c(0,5,10,17), labels = c("0-5", "6-10", "More than 10"), right = TRUE, include.lowest = TRUE, ordered_result = TRUE)


#Summary tables

summary_numeric <- with(talentManage,
                list("Age" = tab_summary(Age)[c(1,4,2,3)],
                     "Monthly Income" = tab_summary(MonthlyIncm)[c(1,4,2,3)],
                     "Distance From Home" = tab_summary(DistncFrmHm)[c(1,4,2,3)],
                     "Number of Companies Worked In" = tab_summary(NmCmpnsWrkd)[c(1,4,2,3)],
                     "Percent Salary Raise" = tab_summary(PrcntSlryHk)[c(1,4,2,3)],
                     "Total working Years" = tab_summary(TtlWrkngYrs)[c(1,4,2,3)],
                     "Years at Company" = tab_summary(YersAtCmpny)[c(1,4,2,3)]))
                     
summary(sapply(talentManage$Age, function(x) x < 18))
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

# Summarize categorical variables with counts

summary_factor <- with(talentManage,
                list("Gender" = tab_summary(factor(Gender)),
                     "Education" = tab_summary(factor(Education)),
                     "Ocupation" = tab_summary(factor(EducationFld))))

table_factor <- summary_table(talentManage, summary_factor)
table_factor

table(talentManage$JobLevel)
summary_positions <- with(talentManage,
                     list("Management Positions" = tab_summary(factor(JobLevel[JobLevel == 3 | JobLevel ==4 | JobLevel ==5]))))

table_positions <- summary_table(talentManage, summary_positions)
table_positions
# Examine proportion of Attrition vs. No Attrition
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Attrition, fill = Attrition)) +
  annotate("text", label = "Yes = 233, or 15.9%", y=350, x=2) +
  annotate("text", label = "No = 1237, or 84.1%", y=1300, x=1) +
  labs(x = "Attrition", y = "Total", title = "Summary of Attrition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE)

#Stacked bars with categories
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Department, fill = Attrition), position = "fill") +
  geom_text(aes(x = Department, y = Attrition, label = Attrition))

#Stacked bars with categories
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobRole, fill = Attrition), position = "fill") +
  coord_flip()



# Examine different variables
# Adding the Age (continuous) and Gender (binomial) variables:
ggplot((talentManage), aes(Age, as.numeric(Attrition)-1, color=Gender)) +
  stat_smooth(method="loess", formula=y~x, alpha=0.2, size=2, aes(fill=Gender)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  labs(title = "Attrition levels by age and gender", x= "Age", y = "Proportion of Attrition")

# Using distance from home and Level:
ggplot((talentManage), aes(DistncFrmHm, as.numeric(Attrition)-1, color=MaritalStts)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=MaritalStts)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Distance from Home") + ylab("Attrition")

#Analizing and choosing one oif the Income/Money variables




# Variables related to job satisfaction

ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = EnvrnmntSts, fill = Attrition), position = "fill") +
  coord_flip()

ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobSatsfctn, fill = Attrition), position = "fill") +
  coord_flip()

table(talentManage$JobSatsfctn, talentManage$EnvrnmntSts)


# Mosaic Plots

install.packages("vcd")
library(vcd)
mosaic(talentManage$JobSatsfctn ~ talentManage$Attrition, color = c("gray", "cyan", "magenta", "green3"))
mosaic(talentManage$JobInvlvmnt ~ talentManage$Attrition, shade = TRUE)
