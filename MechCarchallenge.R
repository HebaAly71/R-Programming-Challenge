# MPG Regression
library(tidyverse)
# Read the mpg data
MechaCarMPG <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
#generate multiple linear regression model+summary statistics
summary(lm(mpg ~ `vehicle length` + `vehicle weight` + `spoiler angle` + `ground clearance`,data=MechaCarMPG)) #generate summary statistics

# Suspencion Coil Summary
library(tidyverse)
# Read the Coil data
Susp_Coil <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
#Create a summary table
summarize_susp_coil <- Susp_Coil %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),Stdev_PSI=sd(PSI)) #create summary table with multiple columns

# Suspension Coil T-Test
# Test normality for PSI
shapiro.test(Susp_Coil$PSI)
#randomly sample 50 data points
Susp_coil_sample_table <- Susp_Coil %>% sample_n(50) 
#compare sample versus population means (use log transformation since PSI data isn't normally distributed)
t.test((Susp_coil_sample_table$PSI),mu=mean(Susp_Coil$PSI)) 
