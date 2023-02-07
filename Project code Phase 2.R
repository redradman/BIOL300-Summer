##################### PROJECT CODE ################################
# importing libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(readr)
library(RColorBrewer)

######### Phase 2 ############## 

# Please read before proceeding:
# The csv files collected by the team for this project was immense. The beginning
# of this R code is importing data, tagging them and then combining them together
# so that they can be used in R without losing track of which treatment each row
# has had
# Comments are added for readability of the code

# IF you want to see the code solely about the charts and importing the 
# finalized csv files after combining them please jump to line 112

################################

# the collected data for WT (Control) and Auxin treatment each have 2 files csv,
# one that has detailed information about GFP and one that 
# compares GFP and mCherry
# However the data for LOF (los function has only the necessary columns) and 
# it has combined the info about mCherry and GFP into a single csv file

#### Reading the csv files #####
# reading the csv files for control i.e. Wild type / WT
GFPandmCherryControl <- read_csv(file.path("ProjectData","Temporary csv files","ExpWT.csv"))
GFPControl <- read_csv(file.path("ProjectData","Temporary csv files","ResultWT.csv"))

# reading the csv files for Auxin treatment
GFPandmCherryAuxin <- read.csv(file.path("ProjectData","Temporary csv files","ExpAuxin.csv"))
GFPAuxin <- read.csv(file.path("ProjectData","Temporary csv files","ResultAuxin.csv"))

# reading the csv file for Loss of function (this dataset has only one file because
# it includes only the essential paratmeters)
GFPandmCherryLOF <- read.csv(file.path("ProjectData","Temporary csv files","AllLOF.csv"))

################################
##### the data / csv file has to be modified / Or a new one has to be created ######

# All the collected data have to be mutated such that they would have the 
# WT.or.LOF.or.Auxin.Enum attribute 
# It will be added via R's mutate() function

# the 3 type of the csv files:
# 1- Control
# 2- LOF
# 3- Auxin
# each will receive the tag by adding a new column to be tagged

# mutated means that a tag has been added to specify that each row 
# that specifies that its status i.e. WT.or.LOF.or.Auxin
# the csv files were outputted separately and this allows us to combine the csv files together
# without losing track of what treatment each row had

# NOTE : ORIGINAL DATA HAS NOT BEEN TAMPERED IN ANY WAY
WT.or.LOF.or.Auxin.Enum <- c("Wildtype", "Loss of function", "Auxin")

# adding the Control tag to its data and saving them to combine the csv files
##### the csv files will be combined using commands in terminal in to 2 files 
GFPandmCherryControlmutated <- mutate(GFPandmCherryControl, WT.or.LOF.or.Auxin = 
                                      WT.or.LOF.or.Auxin.Enum[1])
write_csv(x = GFPandmCherryControlmutated, file = "GFPandmCherryControlmutated.csv")

GFPControlmutated <- mutate(GFPControl, WT.or.LOF.or.Auxin = 
                                      WT.or.LOF.or.Auxin.Enum[1])
write_csv(x = GFPControlmutated, file = "GFPControlmutated.csv")

# adding the auxin tag to its data and saving the files
GFPandmCherryAuxinmutated <- mutate(GFPandmCherryAuxin, WT.or.LOF.or.Auxin = 
                                      WT.or.LOF.or.Auxin.Enum[3])
write_csv(x = GFPandmCherryAuxinmutated, file = "GFPandmCherryAuxinmutated.csv")
GFPAuxinmutated <- mutate(GFPAuxin, WT.or.LOF.or.Auxin = 
                            WT.or.LOF.or.Auxin.Enum[3])
write_csv(x = GFPAuxinmutated, file = "GFPAuxinmutated.csv")

# adding the LOF tag to its data and saving the file
GFPandmCherryLOFmutated <- mutate(GFPandmCherryLOF, WT.or.LOF.or.Auxin = 
                                    WT.or.LOF.or.Auxin.Enum[2])
write_csv(x = GFPandmCherryLOFmutated, file = "GFPandmCherryLOFmutated.csv")


##### List Of Hypothesis #######

#########
# there are 3 sets of hypothesis in our project:

### Set 1 ###########
# H0 : Auxin degradation gives different puncta area of GFP as wildtype
### Set 2 ###########
# H0 : Auxin degradation gives different puncta area of RFP as wildtype
### Set 3 ###########
# H0 : Auxin degradation gives different puncta intensity of RFP as wildtype

######################### FORMAL START ################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
#######################################################################
################################
##### IMPORTING THE COMBINED CSV FILES #####

#### 1st file for GFP ONLY #######

GFPAll <-read.csv(file.path("ProjectData", "FinalCSVfiles" , 
                            "GFPAll.csv"))

##################################

## 2nd file for GFP and mCherry ##
GFPandmCherryAll <-read.csv(file.path("ProjectData", "FinalCSVfiles" , 
                                      "GFPandMCherryAll.csv")) 

##################################

#################################################

##### CONSTANTS FOR ALL charts ########
xlabtag = "Treatment"
ylabtags = c("mCherry Light Intensity",
             "GFP light Intesity",
             "Area of the GFP (µm^2)")
#######################################



#################### Jitter Chart #######################
# this chart allows us to compare the distubution of data points 
# Code: JC
#### constants #####
positionJitterValue = 0.05
####################

# The mCherry (for hypothesis H0-1)
JC01 <- ggplot(GFPandmCherryAll, aes(x = WT.or.LOF.or.Auxin, y = C1mean)) + 
  geom_jitter(position = position_jitter(positionJitterValue)) + 
  xlab(xlabtag) + ylab(ylabtags[1]) + 
  theme_minimal()

JC01
ggsave("JC01.png", JC01, dpi = 300)
# ggsave function is used so that the resolution of the png files be higher

# The GFP (for hypothesis H0-2)
JC02 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin , y = Mean)) + 
  geom_jitter(position = position_jitter(positionJitterValue), show.legend = FALSE) +
  xlab(xlabtag) + ylab(ylabtags[2]) + theme_minimal() 
JC02
ggsave("JC02.png", JC02, dpi = 300)
# The Area (for hypothesis H0-3)
JC03 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin , y = Area)) + 
  geom_jitter(position = position_jitter(positionJitterValue), show.legend = FALSE) +
  xlab(xlabtag) + ylab(ylabtags[3]) + theme_minimal()
JC03
ggsave("JC03.png", JC03, dpi = 300)
#################### Violin chart #######################
# this chart allows us to see the distubution of the numbers in our data
# Code : VC
#### constants #####
colorsViolin = c("#386CB0", "#F0027F", "#BF5B17")
aspectRatio = 1
fillColor = "Black"
####################

# The mCherry (for hypothesis H0-1)
VC01 <- ggplot(GFPandmCherryAll, aes(x= WT.or.LOF.or.Auxin, y = C1mean, 
                             fill = WT.or.LOF.or.Auxin)) +
  geom_violin() +
  xlab(xlabtag) + 
  ylab(ylabtags[1]) + 
  theme_classic() + 
  scale_fill_manual(values= colorsViolin) +  
  stat_summary(fun=mean, geom="point", color="black") + 
  theme(legend.position="none")+
  theme(aspect.ratio=aspectRatio)
VC01
ggsave("VC01.png", VC01, dpi = 300)

# The GFP (for hypothesis H0-2)
VC02 <- ggplot(GFPAll, aes(x= WT.or.LOF.or.Auxin, y = Mean , fill = WT.or.LOF.or.Auxin)) +
  geom_violin() +
  xlab(xlabtag) + 
  ylab(ylabtags[2]) + 
  theme_classic() + 
  scale_fill_manual(values=colorsViolin) +  
  stat_summary(fun=mean, geom="point", color="black") + 
  theme(legend.position="none") +
  theme(aspect.ratio=aspectRatio)
VC02
ggsave("VC02.png", VC02, dpi = 300)

# The Area (for hypothesis H0-3)
VC03 <- ggplot(GFPAll, aes(x= WT.or.LOF.or.Auxin, y = Area , fill = WT.or.LOF.or.Auxin)) +
  geom_violin() +
  xlab(xlabtag) + 
  ylab(ylabtags[3]) + 
  theme_classic() + 
  scale_fill_manual(values=colorsViolin) + 
  stat_summary(fun=mean, geom="point", color="black") + 
  theme(legend.position="none") +
  theme(aspect.ratio=aspectRatio)
VC03
ggsave("VC03.png", VC03, dpi = 300)

#################### Box plot #######################
# a different way to observe the distubution of our data
# Code: BP
#### constants #####
fillValBoxPlot = "Categories"
colorsBoxPlot = brewer.pal(3, "Set3")
####################

# The mCherry (for hypothesis H0-1)
BP01 <- ggplot(GFPandmCherryAll, aes(x = WT.or.LOF.or.Auxin, y = C1mean, fill = WT.or.LOF.or.Auxin)) + 
  geom_boxplot() + xlab(xlabtag) + ylab(ylabtags[1]) + labs(fill = fillValBoxPlot) + 
  scale_fill_manual(values = colorsBoxPlot) + theme_classic()
BP01
ggsave("BP01.png",BP01, dpi = 300)
# The GFP (for hypothesis H0-2)
BP02 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin, y = Mean, fill = WT.or.LOF.or.Auxin)) + 
  geom_boxplot() + xlab(xlabtag) + ylab(ylabtags[2]) + labs(fill = fillValBoxPlot) + 
  scale_fill_manual(values = colorsBoxPlot) + theme_classic()
BP02
ggsave("BP02.png",BP02, dpi = 300)
# The Area (for hypothesis H0-3) +
BP03 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin, y = Area, fill = WT.or.LOF.or.Auxin)) + 
  geom_boxplot() + xlab(xlabtag) + ylab(ylabtags[3]) + labs(fill = fillValBoxPlot) + 
  scale_fill_manual(values = colorsBoxPlot) + theme_classic()
BP03
ggsave("BP03.png",BP03, dpi = 300)


#################### Dot plot #######################
# using dots to see where the majority of points lie in the y axis 
# Code : DP
#### constants #####
dotsizeArg = 0.3
binaxis  = "y"
stackdir = "center"
fill = "Categories"
####################

# The mCherry (for hypothesis H0-1)
DP01 <- ggplot(GFPandmCherryAll, aes(x = WT.or.LOF.or.Auxin, y = C1mean, 
                             fill = WT.or.LOF.or.Auxin)) +
  geom_dotplot(dotsize = dotsizeArg,binaxis = binaxis, stackdir = stackdir) +
  xlab(xlabtag) + 
  ylab(ylabtags[1]) + labs(fill = fill) + 
  theme_minimal()
DP01
ggsave("DP01.png", DP01, dpi = 300)
# The GFP (for hypothesis H0-2)
DP02 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin, y = Mean, 
                   fill = WT.or.LOF.or.Auxin)) +
  geom_dotplot(dotsize = dotsizeArg, binaxis = binaxis, stackdir = stackdir) +
  xlab(xlabtag) + 
  ylab(ylabtags[2]) + labs(fill = fill) + theme_minimal()
DP02
ggsave("DP02.png",DP02,dpi = 300)
# The Area (for hypothesis H0-3)
DP03 <- ggplot(GFPAll, aes(x = WT.or.LOF.or.Auxin, y = Area,
                   fill = WT.or.LOF.or.Auxin)) +
  geom_dotplot(dotsize= dotsizeArg, binaxis = binaxis, stackdir = stackdir) + 
  xlab(xlabtag) + 
  ylab(ylabtags[3]) + labs(fill = fill) + theme_minimal()
DP03
ggsave("DP03.png",DP03,dpi = 300)


######### END ######### 
#######################
#######################
#######################
#######################
#######################
#######################
#######################
#######################
#######################
#######################













