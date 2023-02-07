# Project Phase 3

# list of H0 to be tested:
# H0 - Null hypothesis 1: (H0-1)
#Auxin degradation gives same puncta area of GFP as wildtype

# H0 - Null hypothesis 2: (H0-2)
#Auxin degradation gives same puncta intensity of GFP as wildtype

# H0 - Null hypothesis 3: (H0-3)
#Auxin degradation gives same puncta intensity of RFP as wildtype

#### The H0-NUMBER codes are used in comments through out the code in comments ####


# loading libraries
library(readr)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(car)
library(MASS)
library(progress) 
library(FSA)

print("please ensure that all packages above are loaded")

####################### Fetching CSV files ######################

# GFP ONLY csv  
GFPAll <-read.csv(file.path("ProjectData", "FinalCSVfiles" , 
                            "GFPAll.csv"))

# GFP and mCherry (RFP) csv
GFPandmCherryAll <-read.csv(file.path("ProjectData", "FinalCSVfiles" , 
                                      "GFPandMCherryAll.csv"))


####################### H0-1 ####################### 
#Auxin degradation gives same puncta area of GFP as wild type.

############## Parameter Estimate H0-1 ##############

# Making a function to calculate the Confidence Interval for a Mean

CI95.for.mean <- function(passed_vector) {
  
  # the function calculates 95% CI for a mean 
  # input: A single vector containing values
  # output: "[ low , high ]"
  
  
  mean = mean(passed_vector, na.rm = TRUE)
  z = 1.96 # for confidence interval of 95%
  sd = sd(passed_vector, na.rm = TRUE)
  n = length(passed_vector)
  print(paste("[", mean - z * (sd/sqrt(n)),",", mean + z * (sd/sqrt(n)), "]"))
}

# overall mean of area of GFP 
CI95.for.mean(GFPAll$Area)

# Mean of GFP area per category
# Auxin
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Auxin")$Area)

# Wild type
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Wildtype")$Area)

# Loss of function
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Loss of function")$Area)

########################## END OF PARAMETER ESTIMATION

### ANOVA test is conducted for this hypothesis


# checking if the assumptions of the ANOVA test are met

#### Checking Assumptions of ANOVA: H0-1 ############


############################## Checking Assumption 1 of ANOVA: Independence 



# Given the nature of data collection and sampling the condition is met



###################################################  Assumption 1 Fulfilled





####################### Checking Assumption 2 of ANOVA: Normal distribution


# H0: data is normally distributed
shapiro.test(GFPAll$Area)
# p-value < 2.2e-16
# H0 Rejected

# H0: log of data is normally distributed
shapiro.test(log(GFPAll$Area))
# p-value < 2.2e-16
# H0 Rejected


# data is NOT normally distributed

####################################################### Assumption 2 FAILED


############################ Checking Assumption 3 of ANOVA: Equal Variance


# Grouping the data by treatment
GFPAll.by.treatment <- group_by(GFPAll, WT.or.LOF.or.Auxin)

summarise(GFPAll.by.treatment, mean = mean(Area), sd = sd(Area))
# the variance seem apart 

# conducting bartlett's test for better understanding of variance
# H0 for bartlett's test: Samples have equal variance 
bartlett.test(Area ~ WT.or.LOF.or.Auxin, data = GFPAll)
# p-value = 0.001749
# H0 is REJECTED


####################################################### Assumption 3 Failed 

###### ANOVA CANNOT BE CONDUCTED #####
# since assumptions 2 and 3 have failed


########### Kruskal-Wallis test for H0-1 ########### 
# according feedback



kruskal.test(data = GFPAll, Area ~ WT.or.LOF.or.Auxin)
# H0 / Null hypothesis: Equality of area between different categories 
# p-value = 6.483e-08
# H0 is REJECTED

# Therefore the data area of the GFP is NOT equal among different treatments




################################# End of kruskal test




########### Dunn's test as Post Hoc test ###########

# H0: There is no difference between each pair
dunnTest(Area ~ WT.or.LOF.or.Auxin, data=GFPAll, method="holm")
# OUTPUT of adjusted P values 
# Auxin - Loss of function: 5.231195e-05
# Auxin - Wildtype: 4.645097e-01
# Loss of function - Wildtype: 8.359581e-07

# We can reject the null hypothesis that there is no difference among 
#     Auxin - Loss of function and Loss of function - Wildtype.



# We cannot reject the null hypothesis of no difference for Auxin - Wildtype.


################################# End of Dunn's test 




############ BONUS MARKS ############
############ Permutation test for H0-1 #############

print("please make sure that the progress library at the top has been installed and imported")
# library(progress)


# Observed difference of means of Wild type (Control) and Auxin treatment
observed.difference.GFP.area.mean.WT.Auxin <- 
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Wildtype")$Area) -
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Auxin")$Area)

# Observed difference of means of Wild type (Control) and Loss of function
observed.difference.GFP.area.mean.WT.LOF <- 
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Wildtype")$Area) -
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Loss of function")$Area)

# Observed difference of means of d Auxin treatment and loss of function
observed.difference.GFP.area.mean.Auxin.LOF <-
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Auxin")$Area) - 
  mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Loss of function")$Area) 


# Vector to store the difference of means of permutation 
difference.of.GFP.area.WT.Auxin <- c()
difference.of.GFP.area.WT.LOF <- c()
difference.of.GFP.area.Auxin.LOF <- c()

# for easier reproducibility of results seed is set below
set.seed(837) 
number.of.permutation = 10000

######### NOTE ########
# if you want to run the for loop again make sure to 
# run the pb variable below prior to rerunning the loop

# progress bar for the loop
pb <- progress_bar$new(
  format = "  Calculating [:bar] :percent eta: :eta",
  total = number.of.permutation, clear = FALSE, width= 100)

for (i in 1:number.of.permutation){
  
  if (i == 1) {
  print("Commencing calculation for permutation test")
    if (number.of.permutation > 10000) {
      print("Calculation might take some time")
    }
  print("Progress report: Percentage and ETA are below at the right")
  }
  
   
  GFP.area.scrambled <- 
    transform(GFPAll, WT.or.LOF.or.Auxin = sample(WT.or.LOF.or.Auxin))
  
  grouped.GFP.area <- group_by(GFP.area.scrambled, WT.or.LOF.or.Auxin)
  
  mean.grouped.scrambled.GFP.area <- 
    summarise(grouped.GFP.area, mean_group = mean(Area))
  
  # Calculating mean difference between treatment and control
  difference.WT.Auxin <- 
    mean.grouped.scrambled.GFP.area$mean_group[3] - 
    mean.grouped.scrambled.GFP.area$mean_group[1]
  
  difference.WT.LOF <- 
    mean.grouped.scrambled.GFP.area$mean_group[3] - 
    mean.grouped.scrambled.GFP.area$mean_group[2]
  
  difference.Auxin.LOF <-
    mean.grouped.scrambled.GFP.area$mean_group[1] - 
    mean.grouped.scrambled.GFP.area$mean_group[2]
  
  
  
  # appending the difference 
  difference.of.GFP.area.WT.Auxin <- c(difference.of.GFP.area.WT.Auxin, 
                                       difference.WT.Auxin)
  
  difference.of.GFP.area.WT.LOF <- c(difference.of.GFP.area.WT.LOF,
                                     difference.WT.LOF)
  
  difference.of.GFP.area.Auxin.LOF <- c(difference.of.GFP.area.Auxin.LOF,
                                        difference.Auxin.LOF)
  
  
  pb$tick()
  Sys.sleep(1 / 100000)
  
  if (i == number.of.permutation) {
    print("Calculation completed successfully")
  }
  
}

##### BONUS MARKS: CHARTS FOR PERMUTATION TEST ##### 

# histogram of null distribution for mean of difference
# generated by the conducting the permutation on data 

# Null distribution of Wild type and Auxin (permuted data)
hist(difference.of.GFP.area.WT.Auxin,
     xlab = "Mean of differenece",
     col = "blue",
     main = "Mean of difference for Wild type and Auxin")

# Null distribution of Wild type and Loss of function (permuted data)
hist(difference.of.GFP.area.WT.LOF,
     xlab = "Mean of differenece",
     col = "green",
     main = "Mean of difference for Wild type and Loss of function")

# Null distribution of Auxin and Loss of function (permuted data)
hist(difference.of.GFP.area.Auxin.LOF,
     xlab = "Mean of differenece",
     col = "orange",
     main = "Mean of difference for Auxin and Loss of function")

# Histogram by category for original data
ggplot(GFPAll, aes(x = Area, 
                             fill = WT.or.LOF.or.Auxin)) +
  geom_histogram() +
  facet_wrap(~ WT.or.LOF.or.Auxin, ncol = 1) +
  theme_minimal() + 
  theme(legend.position = "none")

##################################### END OF CHARTS

# although the length of all permutations is the same the lines 
# the length function is done for each comparison for the sake of clarity

#################### Wild type and Auxin

permutation.test.h0.WT.Auxin.total <- length(difference.of.GFP.area.WT.Auxin)
permutation.test.h0.WT.Auxin.total



# finding values above observed mean difference
permutation.test.h0.WT.Auxin.filtered <- 
  length(difference.of.GFP.area.WT.Auxin[difference.of.GFP.area.WT.Auxin > 
                                  observed.difference.GFP.area.mean.WT.Auxin])
permutation.test.h0.WT.Auxin.filtered

# P value 
permutation.test.h0.WT.Auxin.filtered / permutation.test.h0.WT.Auxin.total
# since p > 0.05, we cannot reject the null hypothesis of equality 
# this conclusion is in agreement with the Dunn's test conducted previously

#################### END of wild type and Auxin


#################### Wildtype and loss of function
permutation.test.h0.WT.LOF.total <- length(difference.of.GFP.area.WT.LOF)


# finding values above observed mean difference
permutation.test.h0.WT.LOF.filtered <- 
  length(difference.of.GFP.area.WT.LOF[difference.of.GFP.area.WT.LOF >
                                       observed.difference.GFP.area.mean.WT.LOF])


# P value 
permutation.test.h0.WT.LOF.filtered / permutation.test.h0.WT.LOF.total
# P value is less than 0.05 therefore we reject the null hypothesis of equality
# this conclusion is in agreement with the Dunn's test conducted previously

############## End of Wildtype and loss of function


#################### Auxin and loss of function
permutation.test.h0.Auxin.LOF <- 
  length(difference.of.GFP.area.Auxin.LOF)

# finding values above observed mean difference
permutation.test.h0.Auxin.LOF.filtered <-
  length(difference.of.GFP.area.Auxin.LOF[difference.of.GFP.area.Auxin.LOF >
                                            observed.difference.GFP.area.mean.Auxin.LOF])


# P value 
permutation.test.h0.Auxin.LOF.filtered / permutation.test.h0.Auxin.LOF
# P value is less than 0.05, so we reject the null hypothesis of equal means
# this conclusion is in agreement with the Dunn's test conducted previously

############# END of Auxin and loss of function


############################ End of permutation test

######################################## END OF H0-1





####################### H0-2 ####################### 
#H0: Auxin degradation gives same puncta intensity of GFP as wild type.

############## Parameter Estimate H0-2 ##############

# note that mean is the column for light intensity of GFP

# Overall mean of GFP area per category
CI95.for.mean(GFPAll$Mean)

# Light intensity of GFP area per category
# Auxin 
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Auxin")$Mean)

# Loss of function
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Loss of function")$Mean)

# Wild type
CI95.for.mean(filter(GFPAll, WT.or.LOF.or.Auxin == "Wildtype")$Mean)

########################## END OF PARAMETER ESTIMATION



#### Checking Assumptions of ANOVA: H0-2 ############


############################## Checking Assumption 1 of ANOVA: Independence 



# Given the nature of data collection and sampling the condition is met



###################################################  Assumption 1 Fulfilled





####################### Checking Assumption 2 of ANOVA: Normal distribution


# Shapiro Test
# H0 / Null hypothesis: The data is normally distributed
shapiro.test(GFPAll$Mean)
# p-value = 3.419e-11
# we reject the null hypothesis of normal distribution for data

# log transformation 
shapiro.test(log(GFPAll$Mean))
# p-value = 9.562e-11
# we reject the null hypothesis of normal distribution for the transformed data

# 1/x inverse transformation 
shapiro.test(1/(GFPAll$Mean))
# we reject the null hypothesis of normal distribution for the transformed data

# Neither data nor its transformations are normally distributed.


####################################################### Assumption 2 FAILED




############################ Checking Assumption 3 of ANOVA: Equal Variance

# the group by part was used in the same part in H0-1
summarise(GFPAll.by.treatment, mean = mean(Mean), sd = sd(Mean))

# conducting bartlett's test 
# H0 for bartlett's test: Samples have equal variance 
bartlett.test(Mean ~ WT.or.LOF.or.Auxin, data = GFPAll)
# p-value = 1.43e-10
# we REJECT the null hypothesis

# So we can REJECT the null hypothesis that they have equal variance


####################################################### Assumption 3 Failed

###### ANOVA CANNOT BE CONDUCTED #####


########### Kruskal-Wallis test for H0-2 ########### 
# according to feedback


# H0 / null hypothesis: Equality of Mean between different categories 
kruskal.test(data = GFPAll, Mean ~ WT.or.LOF.or.Auxin)
# p-value < 2.2e-16
# WE REJECT the null hypothesis 


################################# End of kruskal test


########### Dunn's test as Post Hoc test ###########

# H0: There is no difference between each pair
dunnTest(Mean ~ WT.or.LOF.or.Auxin, data=GFPAll, method="holm")
# OUTPUT of adjusted P values 
# Auxin - Loss of function: 2.101243e-64
# Auxin - Wildtype: 5.047216e-08
# Loss of function - Wildtype: 3.950896e-28

# We can reject the null hypothesis that there is no difference among groups.



################################# End of Dunn's test 







################# Conclusion H0-2 ################# 


# Meaning that the treatments result in different 
# GFP light Intesity compared to each other.


####################################### END OF H0-2









####################### H0-3 ####################### 
#Auxin degradation gives same puncta intensity of RFP as wildtype

############## Parameter Estimate H0-3 ##############

# note that C1mean is the column that stores light intensity of RFP

# Overall Confidence Interval for a Mean of the Light intensity of RFP
CI95.for.mean(GFPandmCherryAll$C1mean)

# Light intensity of RFP per category
# Auxin
CI95.for.mean(filter(GFPandmCherryAll, WT.or.LOF.or.Auxin == "Auxin")$C1mean)

# Wild type
CI95.for.mean(filter(GFPandmCherryAll, WT.or.LOF.or.Auxin == "Wildtype")$C1mean)

# Loss of function
CI95.for.mean(filter(GFPandmCherryAll, WT.or.LOF.or.Auxin == "Loss of function")$C1mean)

########################## END OF PARAMETER ESTIMATION

#### Checking Assumptions of ANOVA: H0-3 ############

############################## Checking Assumption 1 of ANOVA: Independence 


# Given the nature of data collection and sampling the condition is met


###################################################  Assumption 1 Fulfilled



####################### Checking Assumption 2 of ANOVA: Normal distribution



# Shapiro Test
# H0: Data has normal distribution.
shapiro.test(GFPandmCherryAll$C1mean)
# p-value < 2.2e-16
# We REJECT null hypothesis of normal distribution.

# Shapiro Test on transformed data
# transformation : log
# H0: Data has normal distribution.
shapiro.test(log(GFPandmCherryAll$C1mean))
# p-value < 2.2e-16
# We REJECT null hypothesis of normal distribution.



####################################################### Assumption 2 FAILED





############################ Checking Assumption 3 of ANOVA: Equal Variance


GFPandmCherryAll.by.treatment <- group_by(GFPandmCherryAll, WT.or.LOF.or.Auxin)
summarise(GFPandmCherryAll.by.treatment, mean = mean(C1mean, na.rm = TRUE),
          sd = sd(C1mean, na.rm = TRUE))

# conducting bartlett's test for better understanding of variance
# H0 for bartlett's test: Samples have equal variance
bartlett.test(C1mean ~ WT.or.LOF.or.Auxin, data = GFPandmCherryAll)
# p-value = 4.759e-11
# We REJECT the null hypothesis. 

# bartlett's test for transformed data
# H0 for bartlett's test: Samples have equal variance
bartlett.test(log(C1mean) ~ WT.or.LOF.or.Auxin, data = GFPandmCherryAll)
# p-value = 1.203e-10
# We REJECT the null hypothesis. 


####################################################### Assumption 3 Failed

###### ANOVA CANNOT BE CONDUCTED #####


########### Kruskal-Wallis test for H0-3 ########### 
# according to feedback



kruskal.test(data = GFPandmCherryAll, C1mean ~ WT.or.LOF.or.Auxin)
# H0 / Null hypothesis: Equality of area between different categories 
# p-value < 2.2e-16
# H0 is REJECTED



########### Dunn's test as Post Hoc test ###########

# H0: There is no difference between each pair
dunnTest(C1mean ~ WT.or.LOF.or.Auxin, data=GFPandmCherryAll, method="holm")
# OUTPUT of adjusted P values
# Auxin - Loss of function: 1.431772e-63
# Auxin - Wildtype: 3.616009e-13
# Loss of function - Wildtype: 8.621272e-20

# We can reject the null hypothesis that there is no difference among groups.

################################# End of Dunn's test 


################# Conclusion H0-3 ################# 


# Meaning that the treatments result in different 
# RFP light Intensity compared to each other.


####################################### END OF H0-3




############################# THE END #############################

# please ignore any lines below here

# all lines below are commented out 






















########################################## DESIGNATION : progress report 
## STATUS : REPLACED
# PROGRESS 

# the code below is to make sure the code reports progress of the loop
# i.str <- toString(i)
# print(paste("Progress: ",i.str))

# Progress is reported by 5 percents

# progress.interval <- number.of.permutation / 20
# if (i%%progress.interval == 0) {
#   progress.number = i/number.of.permutation * 100
#   print(paste("Progress: ",progress.number, "%"))
# }
# 
# if (i == number.of.permutation) {
#   print("loop finished successfully")
#   print("Computation test for permutation test COMPLETED")
# }

##################################################### END OF DESIGNATION


########################################## DESIGNATION: POST HOC TESTS
## STATUS : REPLACED
# ONLY DUNN used after instructions provided by prof
# PROGRESS 


# coercing groups via factor

# typeV <- as.factor(GFPAll$WT.or.LOF.or.Auxin)
# 
# # games howell test
# games_howell_test(GFPAll, Mean ~ WT.or.LOF.or.Auxin, conf.level = 0.95, detailed = TRUE)
# 
# # Nemenyi-Damico-Wolfe Many-to-One Rank Comparison Test
# kwManyOneNdwTest(GFPAll$Mean, typeV)
# 
# # hartley Test
# hartleyTest(GFPAll$Mean, typeV)
# 
# # Dunn's Test 
# 
# dunnTest(Mean ~ WT.or.LOF.or.Auxin, data=GFPAll, method="bonferroni")



# "bonferroni"
# "sidak” (Sidak adjustment)
# “holm” (Holm Adjustment)
# “hs’ (Holm-Sidak Adjustment)
# “bs” (Bonferroni-Sidak Adjustment)
# “by” (Benjamini-Yekuteili Adjustment)
# “bh” (Benjamini-Hochberg procedure)

##################################################### END OF DESIGNATION

