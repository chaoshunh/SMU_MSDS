---
title: 'Analysis of Attrition Data: Main factors that affect Employee Turnover'
author: "Chaoshun Hu, Mahesh Kuklani, Rene Pineda"
date: "August 7, 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---



## Introduction

Voluntary attrition occurs when an employee leaves a company of his or her own accord. This can occur when employees leave their current positions for another job, leave the workforce entirely, or retire. The reasons for leaving a company can vary from personal reasons, such as desiring career advancement or moving to a different city, to company-based reasons, such as an unwanted change in company structure or management.

A high turnover rate typically means working conditions are not optimal, pay is below market average, or staffers are not well trained. Concurrently, a low turnover rate is indicative of a work environment where staffers feel appreciated, work as a team, have room to move up the corporate ladder, and are satisfied with their jobs.

Attrition rates vary widely accross industries. The following tables show attrition rates in the United States:

![](E:/Bibliotecas/Documents/Data Science/SMU/MSDS 6306 Doing Data Science/Project 2/Attrition Statistics.jpg) 

Employee attrition statistics have worsened in recent years, after years of recovery over the recesion of 2008:

![](E:/Bibliotecas/Documents/Data Science/SMU/MSDS 6306 Doing Data Science/Project 2/Historical Attrition.jpg) 

Source: CompData's 2016 edition of their annual BenchmarkPro Survey

Companies usually would like to decrease their voluntary turnover rates due to numerous unfavorable impacts:

* Overall poor company performance

* Low employee morale

* Operating and monetary costs (severance payments, cost of re-hiring and re-training)

* Money invested in the employees that leave the company can't be recovered

* The decrease in the workforce causes remaining employees to work on the slack left behind- mostly performing the task they are not completely trained to perform or are not the best suited

* The situation may get out of control and lead to mass exit of employees, disabling the ability of the company to perform at a high level.

In order to deal with employee attrition and deal with its negative impacts, it is important to know about the most important causes of Attrition. The purpose of this analysis is to try to dilucidate what are the most important factors that contribute to attrition, amongst the many factors that affect an employee's environment and satisfaction. Once these factors are determined, a company can take actions to control them. 


### 2.	Loading and cleaning the data 

**a	Read the csv into R and take a look at the data set.  Output how many rows and columns the data.frame is.**

```r
dim(talentManage)
```

```
## [1] 1470   35
```
The `talentManage` data frame has 1470 rows and 35 columns. 

**b	The column names are either too much or not enough.  Change the column names so that they do not have spaces, underscores, slashes, and the like. All column names should be under 12 characters. Make sure you’re updating your codebook with information on the tidied data set as well.**

We use an R function to automatically abbreviate the variable names:

```r
#List column names
colnames(talentManage)
```

```
##  [1] "Age"                      "Attrition"               
##  [3] "BusinessTravel"           "DailyRate"               
##  [5] "Department"               "DistanceFromHome"        
##  [7] "Education"                "EducationField"          
##  [9] "EmployeeCount"            "EmployeeNumber"          
## [11] "EnvironmentSatisfaction"  "Gender"                  
## [13] "HourlyRate"               "JobInvolvement"          
## [15] "JobLevel"                 "JobRole"                 
## [17] "JobSatisfaction"          "MaritalStatus"           
## [19] "MonthlyIncome"            "MonthlyRate"             
## [21] "NumCompaniesWorked"       "Over18"                  
## [23] "OverTime"                 "PercentSalaryHike"       
## [25] "PerformanceRating"        "RelationshipSatisfaction"
## [27] "StandardHours"            "StockOptionLevel"        
## [29] "TotalWorkingYears"        "TrainingTimesLastYear"   
## [31] "WorkLifeBalance"          "YearsAtCompany"          
## [33] "YearsInCurrentRole"       "YearsSinceLastPromotion" 
## [35] "YearsWithCurrManager"
```

```r
#Abbreviate column names
colnames(talentManage)=abbreviate(colnames(talentManage), method=c("both.sides"),minlength = 11)
```

We will now list the column names again and count the characters to confirm confirm that none exceeds the 12-character limit

```r
columns <- colnames(talentManage)
columns
```

```
##  [1] "Age"         "Attrition"   "BusinssTrvl" "DailyRate"   "Department" 
##  [6] "DistncFrmHm" "Education"   "EducatinFld" "EmployeeCnt" "EmployeNmbr"
## [11] "EnvrnmntSts" "Gender"      "HourlyRate"  "JobInvlvmnt" "JobLevel"   
## [16] "JobRole"     "JobSatsfctn" "MaritalStts" "MonthlyIncm" "MonthlyRate"
## [21] "NmCmpnsWrkd" "Over18"      "OverTime"    "PrcntSlryHk" "PrfrmncRtng"
## [26] "RltnshpStsf" "StandardHrs" "StckOptnLvl" "TtlWrkngYrs" "TrnngTmsLsY"
## [31] "WorkLifBlnc" "YersAtCmpny" "YrsInCrrntR" "YrsSncLstPr" "YrsWthCrrMn"
```

```r
sapply(columns, nchar)
```

```
##         Age   Attrition BusinssTrvl   DailyRate  Department DistncFrmHm 
##           3           9          11           9          10          11 
##   Education EducatinFld EmployeeCnt EmployeNmbr EnvrnmntSts      Gender 
##           9          11          11          11          11           6 
##  HourlyRate JobInvlvmnt    JobLevel     JobRole JobSatsfctn MaritalStts 
##          10          11           8           7          11          11 
## MonthlyIncm MonthlyRate NmCmpnsWrkd      Over18    OverTime PrcntSlryHk 
##          11          11          11           6           8          11 
## PrfrmncRtng RltnshpStsf StandardHrs StckOptnLvl TtlWrkngYrs TrnngTmsLsY 
##          11          11          11          11          11          11 
## WorkLifBlnc YersAtCmpny YrsInCrrntR YrsSncLstPr YrsWthCrrMn 
##          11          11          11          11          11
```

**c.	Some columns are, due to Qualtrics, malfunctioning. **

**d.	Make sure your columns are the proper data types (i.e., numeric, character, etc.).  If they are incorrect, convert them.**


```r
#Check the 'class' of each variable column
lapply(talentManage, class)
```

```
## $Age
## [1] "integer"
## 
## $Attrition
## [1] "factor"
## 
## $BusinssTrvl
## [1] "factor"
## 
## $DailyRate
## [1] "integer"
## 
## $Department
## [1] "factor"
## 
## $DistncFrmHm
## [1] "integer"
## 
## $Education
## [1] "integer"
## 
## $EducatinFld
## [1] "factor"
## 
## $EmployeeCnt
## [1] "integer"
## 
## $EmployeNmbr
## [1] "integer"
## 
## $EnvrnmntSts
## [1] "integer"
## 
## $Gender
## [1] "factor"
## 
## $HourlyRate
## [1] "integer"
## 
## $JobInvlvmnt
## [1] "integer"
## 
## $JobLevel
## [1] "integer"
## 
## $JobRole
## [1] "factor"
## 
## $JobSatsfctn
## [1] "integer"
## 
## $MaritalStts
## [1] "factor"
## 
## $MonthlyIncm
## [1] "integer"
## 
## $MonthlyRate
## [1] "integer"
## 
## $NmCmpnsWrkd
## [1] "integer"
## 
## $Over18
## [1] "factor"
## 
## $OverTime
## [1] "factor"
## 
## $PrcntSlryHk
## [1] "integer"
## 
## $PrfrmncRtng
## [1] "integer"
## 
## $RltnshpStsf
## [1] "integer"
## 
## $StandardHrs
## [1] "integer"
## 
## $StckOptnLvl
## [1] "integer"
## 
## $TtlWrkngYrs
## [1] "integer"
## 
## $TrnngTmsLsY
## [1] "integer"
## 
## $WorkLifBlnc
## [1] "integer"
## 
## $YersAtCmpny
## [1] "integer"
## 
## $YrsInCrrntR
## [1] "integer"
## 
## $YrsSncLstPr
## [1] "integer"
## 
## $YrsWthCrrMn
## [1] "integer"
```

```r
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

#Check that all variables have the correct 'class'
str(talentManage)
```

```
## 'data.frame':	1470 obs. of  35 variables:
##  $ Age        : int  41 49 37 33 27 32 59 30 38 36 ...
##  $ Attrition  : Factor w/ 2 levels "No","Yes": 2 1 2 1 1 1 1 1 1 1 ...
##  $ BusinssTrvl: Factor w/ 3 levels "Non-Travel","Travel_Frequently",..: 3 2 3 2 3 2 3 3 2 3 ...
##  $ DailyRate  : int  1102 279 1373 1392 591 1005 1324 1358 216 1299 ...
##  $ Department : Factor w/ 3 levels "Human Resources",..: 3 2 2 2 2 2 2 2 2 2 ...
##  $ DistncFrmHm: int  1 8 2 3 2 2 3 24 23 27 ...
##  $ Education  : Ord.factor w/ 5 levels "Below College"<..: 2 1 2 4 1 2 3 1 3 3 ...
##  $ EducatinFld: Factor w/ 6 levels "Human Resources",..: 2 2 5 2 4 2 4 2 2 4 ...
##  $ EmployeeCnt: int  1 1 1 1 1 1 1 1 1 1 ...
##  $ EmployeNmbr: int  1 2 4 5 7 8 10 11 12 13 ...
##  $ EnvrnmntSts: Ord.factor w/ 4 levels "Low"<"Medium"<..: 2 3 4 4 1 4 3 4 4 3 ...
##  $ Gender     : Factor w/ 2 levels "Female","Male": 1 2 2 1 2 2 1 2 2 2 ...
##  $ HourlyRate : int  94 61 92 56 40 79 81 67 44 94 ...
##  $ JobInvlvmnt: Ord.factor w/ 4 levels "Low"<"Medium"<..: 3 2 2 3 3 3 4 3 2 3 ...
##  $ JobLevel   : Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 2 2 1 1 1 1 1 1 3 2 ...
##  $ JobRole    : Factor w/ 9 levels "Healthcare Representative",..: 8 7 3 7 3 3 3 3 5 1 ...
##  $ JobSatsfctn: Ord.factor w/ 4 levels "Low"<"Medium"<..: 4 2 3 3 2 4 1 3 3 3 ...
##  $ MaritalStts: Factor w/ 3 levels "Divorced","Married",..: 3 2 3 2 2 3 2 1 3 2 ...
##  $ MonthlyIncm: int  5993 5130 2090 2909 3468 3068 2670 2693 9526 5237 ...
##  $ MonthlyRate: int  19479 24907 2396 23159 16632 11864 9964 13335 8787 16577 ...
##  $ NmCmpnsWrkd: int  8 1 6 1 9 0 4 1 0 6 ...
##  $ Over18     : Factor w/ 1 level "Y": 1 1 1 1 1 1 1 1 1 1 ...
##  $ OverTime   : Factor w/ 2 levels "No","Yes": 2 1 2 2 1 1 2 1 1 1 ...
##  $ PrcntSlryHk: int  11 23 15 11 12 13 20 22 21 13 ...
##  $ PrfrmncRtng: Ord.factor w/ 2 levels "Excellent"<"Outstanding": 1 2 1 1 1 1 2 2 2 1 ...
##  $ RltnshpStsf: Ord.factor w/ 4 levels "Low"<"Medium"<..: 1 4 2 3 4 3 1 2 2 2 ...
##  $ StandardHrs: int  80 80 80 80 80 80 80 80 80 80 ...
##  $ StckOptnLvl: Factor w/ 4 levels "0","1","2","3": 1 2 1 1 2 1 4 2 1 3 ...
##  $ TtlWrkngYrs: int  8 10 7 8 6 8 12 1 10 17 ...
##  $ TrnngTmsLsY: int  0 3 3 3 3 2 3 2 2 3 ...
##  $ WorkLifBlnc: Ord.factor w/ 4 levels "Bad"<"Good"<"Better"<..: 1 3 3 3 3 2 2 3 3 2 ...
##  $ YersAtCmpny: int  6 10 0 8 2 7 1 1 9 7 ...
##  $ YrsInCrrntR: int  4 7 0 7 2 7 0 0 7 7 ...
##  $ YrsSncLstPr: int  0 1 0 3 2 3 0 0 1 7 ...
##  $ YrsWthCrrMn: Ord.factor w/ 3 levels "0-5"<"6-10"<"More than 10": 1 2 1 1 1 2 1 1 2 2 ...
```

### 3.	Preliminary Analysis

**a	Remove all observations where the participant is under age 18.  No further analysis of underage individuals is permitted by your client.  Remove any other age outliers as you see fit, but be sure to tell what you’re doing and why.**

First we apply a function to check whether there are participants under age 18:

```r
#Check every value of the 'Age' variable to see whether it is smaller than 18
summary(sapply(talentManage$Age, function(x) x < 18))
```

```
##    Mode   FALSE    NA's 
## logical    1470       0
```
There are no participants under age 18.

Regarding outliers, we might want to remove participants who have passed the age of retirement (65 years old, although this has increased due to SS ammendments) and that might be having a negative impact in attrition rates, because attrition would be disproportionately high in this group. We can check this in the data

```r
#Check every value of the 'Age' variable to see whether it is smaller than 18
summary(sapply(talentManage$Age, function(x) x >= 65))
```

```
##    Mode   FALSE    NA's 
## logical    1470       0
```

```r
retirement <- subset(talentManage$Age, talentManage$Age >=60)
retirement  
```

```
## [1] 60 60 60 60 60
```
We do not have instances of people 65 years old or older, and only a handful of participants are 60 years old. We proceed without eliminating any data points. 

**b	Please provide (in table format or similar), descriptive statistics on at least 7 variables (age, Income, etc.).  Create a simple histogram for two of them.  Comment on the shape of the distribution in your markdown.**

```r
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
```



|                                  |talentManage (N = 1470)       |
|:---------------------------------|:-----------------------------|
|**Age**                           |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |18                            |
|&nbsp;&nbsp; max                  |60                            |
|&nbsp;&nbsp; median (IQR)         |36.00 (30.00, 43.00)          |
|&nbsp;&nbsp; mean (sd)            |36.92 &plusmn; 9.14           |
|**Monthly Income**                |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |1009                          |
|&nbsp;&nbsp; max                  |19999                         |
|&nbsp;&nbsp; median (IQR)         |4,919.00 (2,911.00, 8,379.00) |
|&nbsp;&nbsp; mean (sd)            |6,502.93 &plusmn; 4,707.96    |
|**Distance From Home**            |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |1                             |
|&nbsp;&nbsp; max                  |29                            |
|&nbsp;&nbsp; median (IQR)         |7.00 (2.00, 14.00)            |
|&nbsp;&nbsp; mean (sd)            |9.19 &plusmn; 8.11            |
|**Number of Companies Worked In** |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |0                             |
|&nbsp;&nbsp; max                  |9                             |
|&nbsp;&nbsp; median (IQR)         |2.00 (1.00, 4.00)             |
|&nbsp;&nbsp; mean (sd)            |2.69 &plusmn; 2.50            |
|**Percent Salary Raise**          |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |11                            |
|&nbsp;&nbsp; max                  |25                            |
|&nbsp;&nbsp; median (IQR)         |14.00 (12.00, 18.00)          |
|&nbsp;&nbsp; mean (sd)            |15.21 &plusmn; 3.66           |
|**Total working Years**           |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |0                             |
|&nbsp;&nbsp; max                  |40                            |
|&nbsp;&nbsp; median (IQR)         |10.00 (6.00, 15.00)           |
|&nbsp;&nbsp; mean (sd)            |11.28 &plusmn; 7.78           |
|**Years at Company**              |&nbsp;&nbsp;                  |
|&nbsp;&nbsp; min                  |0                             |
|&nbsp;&nbsp; max                  |40                            |
|&nbsp;&nbsp; median (IQR)         |5.00 (3.00, 9.00)             |
|&nbsp;&nbsp; mean (sd)            |7.01 &plusmn; 6.13            |

We want to see the histogram of the `Age` variable:
![](Attrition_Analysis_files/figure-html/histogram Age-1.png)<!-- -->

The shape of te histogram resembles a normal distribuiton, with a slight positive bias (mean > median)

And now the histogram of the `Income` variable:

```r
ggplot(data = talentManage, mapping = aes(MonthlyIncm)) +
  geom_histogram(binwidth = 1000, color = "gray", fill = "orchid3" ) +
  labs(x = "Montly income (in US$)", y = "Frequency", title = "Histogram of Monthly Income") + 
  scale_x_continuous(label = comma) +
  theme(plot.title = element_text(hjust = 0.5)) 
```

![](Attrition_Analysis_files/figure-html/histogram income-1.png)<!-- -->

The histogram shows a strong positive skewness, which is typical of Income distributions: a small group of people in the organization have very high salaries. 

**c	Give the frequencies (in table format or similar) for Gender, Education, and Occupation.  They can be separate tables, if that’s your choice.**

```r
# Summarize categorical variables with counts

summary_factor <- with(talentManage,
                list("Gender" = tab_summary(factor(Gender)),
                     "Education" = tab_summary(factor(Education)),
                     "Ocupation" = tab_summary(factor(EducatinFld))))

table_factor <- summary_table(talentManage, summary_factor)
table_factor
```



|                              |talentManage (N = 1470) |
|:-----------------------------|:-----------------------|
|**Gender**                    |&nbsp;&nbsp;            |
|&nbsp;&nbsp; Female           |588 (40)                |
|&nbsp;&nbsp; Male             |882 (60)                |
|**Education**                 |&nbsp;&nbsp;            |
|&nbsp;&nbsp; Below College    |170 (12)                |
|&nbsp;&nbsp; College          |282 (19)                |
|&nbsp;&nbsp; Bachelor         |572 (39)                |
|&nbsp;&nbsp; Master           |398 (27)                |
|&nbsp;&nbsp; Doctor           |48 (3)                  |
|**Ocupation**                 |&nbsp;&nbsp;            |
|&nbsp;&nbsp; Human Resources  |27 (2)                  |
|&nbsp;&nbsp; Life Sciences    |606 (41)                |
|&nbsp;&nbsp; Marketing        |159 (11)                |
|&nbsp;&nbsp; Medical          |464 (32)                |
|&nbsp;&nbsp; Other            |82 (6)                  |
|&nbsp;&nbsp; Technical Degree |132 (9)                 |

**d	Give the counts (again, table) of management positions.**

```r
summary_positions <- with(talentManage,
                     list("Management Positions" = tab_summary(factor(JobLevel[JobLevel == 3 | JobLevel ==4 | JobLevel ==5]))))

table_positions <- summary_table(talentManage, summary_positions)
table_positions
```



|                         |talentManage (N = 1470) |
|:------------------------|:-----------------------|
|**Management Positions** |&nbsp;&nbsp;            |
|&nbsp;&nbsp; 3           |218 (55)                |
|&nbsp;&nbsp; 4           |106 (27)                |
|&nbsp;&nbsp; 5           |69 (18)                 |

### Additional Analysis

Examine the proportion of attrition vs. No Attrition


```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Attrition, fill = Attrition)) +
  annotate("text", label = "Yes = 233, or 15.9%", y=350, x=2) +
  annotate("text", label = "No = 1237, or 84.1%", y=1300, x=1) +
  labs(x = "Attrition", y = "Total", title = "Summary of Attrition") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=FALSE)
```

![](Attrition_Analysis_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Investigate whether there are differences in attrition by continuous and categorical variables (Age and Gender, Marital Status and distance from home, Income and Job Level)

```r
#Graphing Age and Gender
ggplot((talentManage), aes(Age, as.numeric(Attrition)-1, color=Gender)) +
  stat_smooth(method="loess", formula=y~x, alpha=0.2, size=2, aes(fill=Gender)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  labs(title = "Attrition levels by age and gender", x= "Age", y = "Proportion of Attrition")
```

![](Attrition_Analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Using distance from home and Marital Status:
ggplot((talentManage), aes(DistncFrmHm, as.numeric(Attrition)-1, color=MaritalStts)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=MaritalStts)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Distance from Home") + ylab("Attrition")
```

![](Attrition_Analysis_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
# Using Montly Income and Job Level:
ggplot((talentManage), aes(MonthlyIncm, as.numeric(Attrition)-1, color=JobLevel)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=JobLevel)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Monthly Income in US$") + ylab("Attrition")
```

![](Attrition_Analysis_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
# Using Years in the Company
ggplot((talentManage), aes(YersAtCmpny, as.numeric(Attrition)-1, color=StckOptnLvl)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=StckOptnLvl)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Years Worked at the Company") + ylab("Attrition")
```

![](Attrition_Analysis_files/figure-html/unnamed-chunk-3-4.png)<!-- -->

Do people with a history of changing jobs have a larger tendency for Attrition (controlling by Environment Satisfaction)?

```r
ggplot((talentManage), aes(NmCmpnsWrkd, as.numeric(Attrition)-1, color=EnvrnmntSts)) +
  stat_smooth(method="loess", formula=y~x,
              alpha=0.2, size=2, aes(fill=EnvrnmntSts)) +
  geom_point(position=position_jitter(height=0.03, width=0)) +
  xlab("Number of previous Jobs") + ylab("Attrition")
```

![](Attrition_Analysis_files/figure-html/Number of jobs-1.png)<!-- -->

Analyze if the proportion of Attrition vs. No Attrition varies significantly among different categorical variables, related to Job Satisfaction and organizational factors


```r
par(mfrow = c(3,2))

ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobSatsfctn, fill = Attrition), position = "fill") +
  labs(x = "Job Satisfaction (4 is higher)", y = "Proportion", title = "Attrition by Level of Job Satisfaction")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-1.png)<!-- -->

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobInvlvmnt, fill = Attrition), position = "fill") +
  labs(x = "Job Involvement (4 is higher)", y = "Proportion", title = "Attrition by Level of Job Involvement")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-2.png)<!-- -->

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = Department, fill = Attrition), position = "fill") +
  labs(x = "Department", y = "Proportion", title = "Attrition by Department")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-3.png)<!-- -->

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = YrsWthCrrMn, fill = Attrition), position = "fill") +
  labs(x = "Years with Current Manager", y = "Proportion", title = "Attrition levels by Years with Current Manager")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-4.png)<!-- -->

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = RltnshpStsf, fill = Attrition), position = "fill") +
  labs(x = "Relationship Satisfaction", y = "Proportion", title = "Attrition by Relationship Satisfaction")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-5.png)<!-- -->

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = BusinssTrvl, fill = Attrition), position = "fill") +
  labs(x = "Business Travel", y = "Proportion", title = "Attrition for Business Travel")
```

![](Attrition_Analysis_files/figure-html/Additional bar graphs-6.png)<!-- -->

We see very clear differences in Attrition by Job Role:

```r
ggplot(data = talentManage) + 
  geom_bar(mapping = aes(x = JobRole, fill = Attrition), position = "fill") +
  labs(x = "Job Role", y = "Proportion", title = "Attrition by Job Roles") +
  coord_flip()
```

![](Attrition_Analysis_files/figure-html/Job Role chart-1.png)<!-- -->

### 4.	Deeper Analysis and Visualization 

a	Note: You should make all of these appealing looking.  Remember to include things like a clean, informative title, axis labels that are in plain English, and readable axis values that do not overlap.


b	Create bar charts in ggplot or similar. The bars should be in descending order, Use any color palette of your choice other than the default.


c	Is there a relationship between Age and Income?  Create a scatterplot and make an assessment of whether there is a relationship.  Color each point based on the Gender of the participant.  You’re welcome to use lm() or similar functions to back up your claims.


d	What about Life Satisfaction?  Create another scatterplot.  Is there a discernible relationship there to what?   



