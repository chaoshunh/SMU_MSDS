---
title: "casestudy2_codebook"
author: "Chaoshun Hu, Mahesh Kuklani, Rene Pineda"
date: "August 7, 2018"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## 1. Summary of the data

```
##  [1] "Age"         "Attrition"   "BusinssTrvl" "DailyRate"   "Department" 
##  [6] "DistncFrmHm" "Education"   "EducatinFld" "EmployeeCnt" "EmployeNmbr"
## [11] "EnvrnmntSts" "Gender"      "HourlyRate"  "JobInvlvmnt" "JobLevel"   
## [16] "JobRole"     "JobSatsfctn" "MaritalStts" "MonthlyIncm" "MonthlyRate"
## [21] "NmCmpnsWrkd" "Over18"      "OverTime"    "PrcntSlryHk" "PrfrmncRtng"
## [26] "RltnshpStsf" "StandardHrs" "StckOptnLvl" "TtlWrkngYrs" "TrnngTmsLsY"
## [31] "WorkLifBlnc" "YersAtCmpny" "YrsInCrrntR" "YrsSncLstPr" "YrsWthCrrMn"
```

```
##       Age        Attrition             BusinssTrvl     DailyRate     
##  Min.   :18.00   No :1233   Non-Travel       : 150   Min.   : 102.0  
##  1st Qu.:30.00   Yes: 237   Travel_Frequently: 277   1st Qu.: 465.0  
##  Median :36.00              Travel_Rarely    :1043   Median : 802.0  
##  Mean   :36.92                                       Mean   : 802.5  
##  3rd Qu.:43.00                                       3rd Qu.:1157.0  
##  Max.   :60.00                                       Max.   :1499.0  
##                                                                      
##                   Department   DistncFrmHm       Education    
##  Human Resources       : 63   Min.   : 1.000   Min.   :1.000  
##  Research & Development:961   1st Qu.: 2.000   1st Qu.:2.000  
##  Sales                 :446   Median : 7.000   Median :3.000  
##                               Mean   : 9.193   Mean   :2.913  
##                               3rd Qu.:14.000   3rd Qu.:4.000  
##                               Max.   :29.000   Max.   :5.000  
##                                                               
##            EducatinFld   EmployeeCnt  EmployeNmbr      EnvrnmntSts   
##  Human Resources : 27   Min.   :1    Min.   :   1.0   Min.   :1.000  
##  Life Sciences   :606   1st Qu.:1    1st Qu.: 491.2   1st Qu.:2.000  
##  Marketing       :159   Median :1    Median :1020.5   Median :3.000  
##  Medical         :464   Mean   :1    Mean   :1024.9   Mean   :2.722  
##  Other           : 82   3rd Qu.:1    3rd Qu.:1555.8   3rd Qu.:4.000  
##  Technical Degree:132   Max.   :1    Max.   :2068.0   Max.   :4.000  
##                                                                      
##     Gender      HourlyRate      JobInvlvmnt      JobLevel    
##  Female:588   Min.   : 30.00   Min.   :1.00   Min.   :1.000  
##  Male  :882   1st Qu.: 48.00   1st Qu.:2.00   1st Qu.:1.000  
##               Median : 66.00   Median :3.00   Median :2.000  
##               Mean   : 65.89   Mean   :2.73   Mean   :2.064  
##               3rd Qu.: 83.75   3rd Qu.:3.00   3rd Qu.:3.000  
##               Max.   :100.00   Max.   :4.00   Max.   :5.000  
##                                                              
##                       JobRole     JobSatsfctn      MaritalStts 
##  Sales Executive          :326   Min.   :1.000   Divorced:327  
##  Research Scientist       :292   1st Qu.:2.000   Married :673  
##  Laboratory Technician    :259   Median :3.000   Single  :470  
##  Manufacturing Director   :145   Mean   :2.729                 
##  Healthcare Representative:131   3rd Qu.:4.000                 
##  Manager                  :102   Max.   :4.000                 
##  (Other)                  :215                                 
##   MonthlyIncm     MonthlyRate     NmCmpnsWrkd    Over18   OverTime  
##  Min.   : 1009   Min.   : 2094   Min.   :0.000   Y:1470   No :1054  
##  1st Qu.: 2911   1st Qu.: 8047   1st Qu.:1.000            Yes: 416  
##  Median : 4919   Median :14236   Median :2.000                      
##  Mean   : 6503   Mean   :14313   Mean   :2.693                      
##  3rd Qu.: 8379   3rd Qu.:20462   3rd Qu.:4.000                      
##  Max.   :19999   Max.   :26999   Max.   :9.000                      
##                                                                     
##   PrcntSlryHk     PrfrmncRtng     RltnshpStsf     StandardHrs
##  Min.   :11.00   Min.   :3.000   Min.   :1.000   Min.   :80  
##  1st Qu.:12.00   1st Qu.:3.000   1st Qu.:2.000   1st Qu.:80  
##  Median :14.00   Median :3.000   Median :3.000   Median :80  
##  Mean   :15.21   Mean   :3.154   Mean   :2.712   Mean   :80  
##  3rd Qu.:18.00   3rd Qu.:3.000   3rd Qu.:4.000   3rd Qu.:80  
##  Max.   :25.00   Max.   :4.000   Max.   :4.000   Max.   :80  
##                                                              
##   StckOptnLvl      TtlWrkngYrs     TrnngTmsLsY     WorkLifBlnc   
##  Min.   :0.0000   Min.   : 0.00   Min.   :0.000   Min.   :1.000  
##  1st Qu.:0.0000   1st Qu.: 6.00   1st Qu.:2.000   1st Qu.:2.000  
##  Median :1.0000   Median :10.00   Median :3.000   Median :3.000  
##  Mean   :0.7939   Mean   :11.28   Mean   :2.799   Mean   :2.761  
##  3rd Qu.:1.0000   3rd Qu.:15.00   3rd Qu.:3.000   3rd Qu.:3.000  
##  Max.   :3.0000   Max.   :40.00   Max.   :6.000   Max.   :4.000  
##                                                                  
##   YersAtCmpny      YrsInCrrntR      YrsSncLstPr      YrsWthCrrMn    
##  Min.   : 0.000   Min.   : 0.000   Min.   : 0.000   Min.   : 0.000  
##  1st Qu.: 3.000   1st Qu.: 2.000   1st Qu.: 0.000   1st Qu.: 2.000  
##  Median : 5.000   Median : 3.000   Median : 1.000   Median : 3.000  
##  Mean   : 7.008   Mean   : 4.229   Mean   : 2.188   Mean   : 4.123  
##  3rd Qu.: 9.000   3rd Qu.: 7.000   3rd Qu.: 3.000   3rd Qu.: 7.000  
##  Max.   :40.000   Max.   :18.000   Max.   :15.000   Max.   :17.000  
## 
```

## 2. variable description

```
## Loading required package: dataMaid
```

```
## Loading required package: ggplot2
```

```
## $Age
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "43"     
## [4,] "Median"                  "36"     
## [5,] "1st and 3rd quartiles"   "30; 43" 
## [6,] "Min. and max."           "18; 60" 
## 
## $Attrition
##      Feature                   Result   
## [1,] "Variable type"           "factor" 
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "2"      
## [4,] "Mode"                    "\"No\"" 
## 
## $BusinssTrvl
##      Feature                   Result             
## [1,] "Variable type"           "factor"           
## [2,] "Number of missing obs."  "0 (0 %)"          
## [3,] "Number of unique values" "3"                
## [4,] "Mode"                    "\"Travel_Rarely\""
## 
## $DailyRate
##      Feature                   Result     
## [1,] "Variable type"           "integer"  
## [2,] "Number of missing obs."  "0 (0 %)"  
## [3,] "Number of unique values" "886"      
## [4,] "Median"                  "802"      
## [5,] "1st and 3rd quartiles"   "465; 1157"
## [6,] "Min. and max."           "102; 1499"
## 
## $Department
##      Feature                   Result                      
## [1,] "Variable type"           "factor"                    
## [2,] "Number of missing obs."  "0 (0 %)"                   
## [3,] "Number of unique values" "3"                         
## [4,] "Mode"                    "\"Research & Development\""
## 
## $DistncFrmHm
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "29"     
## [4,] "Median"                  "7"      
## [5,] "1st and 3rd quartiles"   "2; 14"  
## [6,] "Min. and max."           "1; 29"  
## 
## $Education
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "5"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 4"   
## [6,] "Min. and max."           "1; 5"   
## 
## $EducatinFld
##      Feature                   Result             
## [1,] "Variable type"           "factor"           
## [2,] "Number of missing obs."  "0 (0 %)"          
## [3,] "Number of unique values" "6"                
## [4,] "Mode"                    "\"Life Sciences\""
## 
## $EmployeeCnt
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "1"      
## [4,] "Median"                  "1"      
## [5,] "1st and 3rd quartiles"   "1; 1"   
## [6,] "Min. and max."           "1; 1"   
## 
## $EmployeNmbr
##      Feature                   Result           
## [1,] "Variable type"           "integer"        
## [2,] "Number of missing obs."  "0 (0 %)"        
## [3,] "Number of unique values" "1470"           
## [4,] "Median"                  "1020.5"         
## [5,] "1st and 3rd quartiles"   "491.25; 1555.75"
## [6,] "Min. and max."           "1; 2068"        
## 
## $EnvrnmntSts
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 4"   
## [6,] "Min. and max."           "1; 4"   
## 
## $Gender
##      Feature                   Result    
## [1,] "Variable type"           "factor"  
## [2,] "Number of missing obs."  "0 (0 %)" 
## [3,] "Number of unique values" "2"       
## [4,] "Mode"                    "\"Male\""
## 
## $HourlyRate
##      Feature                   Result     
## [1,] "Variable type"           "integer"  
## [2,] "Number of missing obs."  "0 (0 %)"  
## [3,] "Number of unique values" "71"       
## [4,] "Median"                  "66"       
## [5,] "1st and 3rd quartiles"   "48; 83.75"
## [6,] "Min. and max."           "30; 100"  
## 
## $JobInvlvmnt
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 3"   
## [6,] "Min. and max."           "1; 4"   
## 
## $JobLevel
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "5"      
## [4,] "Median"                  "2"      
## [5,] "1st and 3rd quartiles"   "1; 3"   
## [6,] "Min. and max."           "1; 5"   
## 
## $JobRole
##      Feature                   Result               
## [1,] "Variable type"           "factor"             
## [2,] "Number of missing obs."  "0 (0 %)"            
## [3,] "Number of unique values" "9"                  
## [4,] "Mode"                    "\"Sales Executive\""
## 
## $JobSatsfctn
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 4"   
## [6,] "Min. and max."           "1; 4"   
## 
## $MaritalStts
##      Feature                   Result       
## [1,] "Variable type"           "factor"     
## [2,] "Number of missing obs."  "0 (0 %)"    
## [3,] "Number of unique values" "3"          
## [4,] "Mode"                    "\"Married\""
## 
## $MonthlyIncm
##      Feature                   Result       
## [1,] "Variable type"           "integer"    
## [2,] "Number of missing obs."  "0 (0 %)"    
## [3,] "Number of unique values" "1349"       
## [4,] "Median"                  "4919"       
## [5,] "1st and 3rd quartiles"   "2911; 8379" 
## [6,] "Min. and max."           "1009; 19999"
## 
## $MonthlyRate
##      Feature                   Result         
## [1,] "Variable type"           "integer"      
## [2,] "Number of missing obs."  "0 (0 %)"      
## [3,] "Number of unique values" "1427"         
## [4,] "Median"                  "14235.5"      
## [5,] "1st and 3rd quartiles"   "8047; 20461.5"
## [6,] "Min. and max."           "2094; 26999"  
## 
## $NmCmpnsWrkd
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "10"     
## [4,] "Median"                  "2"      
## [5,] "1st and 3rd quartiles"   "1; 4"   
## [6,] "Min. and max."           "0; 9"   
## 
## $Over18
##      Feature                   Result   
## [1,] "Variable type"           "factor" 
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "1"      
## [4,] "Mode"                    "\"Y\""  
## 
## $OverTime
##      Feature                   Result   
## [1,] "Variable type"           "factor" 
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "2"      
## [4,] "Mode"                    "\"No\"" 
## 
## $PrcntSlryHk
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "15"     
## [4,] "Median"                  "14"     
## [5,] "1st and 3rd quartiles"   "12; 18" 
## [6,] "Min. and max."           "11; 25" 
## 
## $PrfrmncRtng
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "2"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "3; 3"   
## [6,] "Min. and max."           "3; 4"   
## 
## $RltnshpStsf
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 4"   
## [6,] "Min. and max."           "1; 4"   
## 
## $StandardHrs
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "1"      
## [4,] "Median"                  "80"     
## [5,] "1st and 3rd quartiles"   "80; 80" 
## [6,] "Min. and max."           "80; 80" 
## 
## $StckOptnLvl
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "1"      
## [5,] "1st and 3rd quartiles"   "0; 1"   
## [6,] "Min. and max."           "0; 3"   
## 
## $TtlWrkngYrs
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "40"     
## [4,] "Median"                  "10"     
## [5,] "1st and 3rd quartiles"   "6; 15"  
## [6,] "Min. and max."           "0; 40"  
## 
## $TrnngTmsLsY
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "7"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 3"   
## [6,] "Min. and max."           "0; 6"   
## 
## $WorkLifBlnc
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "4"      
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 3"   
## [6,] "Min. and max."           "1; 4"   
## 
## $YersAtCmpny
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "37"     
## [4,] "Median"                  "5"      
## [5,] "1st and 3rd quartiles"   "3; 9"   
## [6,] "Min. and max."           "0; 40"  
## 
## $YrsInCrrntR
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "19"     
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 7"   
## [6,] "Min. and max."           "0; 18"  
## 
## $YrsSncLstPr
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "16"     
## [4,] "Median"                  "1"      
## [5,] "1st and 3rd quartiles"   "0; 3"   
## [6,] "Min. and max."           "0; 15"  
## 
## $YrsWthCrrMn
##      Feature                   Result   
## [1,] "Variable type"           "integer"
## [2,] "Number of missing obs."  "0 (0 %)"
## [3,] "Number of unique values" "18"     
## [4,] "Median"                  "3"      
## [5,] "1st and 3rd quartiles"   "2; 7"   
## [6,] "Min. and max."           "0; 17"
```
