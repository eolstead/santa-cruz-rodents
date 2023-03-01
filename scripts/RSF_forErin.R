# Install devtools for MuMIn package
install.packages("devtools")

# Install MuMIn package from GitHub
devtools::install_github("cran/MuMIn")

install.packages("tidyverse")
install.packages("GGally")
install.packages("reshape2")
install.packages("lme4")

library(lme4)
library(MuMIn)
library(ggplot2)
library(GGally)
library(reshape2)
library(tidyverse)


###This code has two different ways to conduct a RSF - dredge method, or method using rsf package

##load in species v. available points, which should be compiled into a single dataset and labeled as 0,1 
#and include variables you'd like to include in RSF

rsf <- read.csv('data/Data_forErin_R.csv', header = T, sep = ",", stringsAsFactors = TRUE)
attach(data)

# Make veg category column

# Create groupings for vegetation data
microsite <- read_csv("data/microsite_raw.csv")

grass <- c("Bermuda grass", "Unknown grass", "Buffelgrass", "Unidentified grass", "Johnson grass", "Unidentified Grama grass", "Johnson grass, bermuda grass", "Bermuda grass / Johnson grass", "Johnson grass / bermuda grass" )
shrubs <- c("Arrowweed", "Cheese bush", "Cheesebush", "Desert broom", "Mesquite", "Salt cedar", "Desert broom/salt cedar")
forbs <- c("Cockleburr", "Cheeseweed burrobush")
sedge_typha <- c("Umbrella flatsedge", "Typha")
mixed <- c("Tall flatsedge / Bermuda mix", "Salt cedar / Typha", "Cheeseweed burrobush/Johnson grass", "Desert broom/Bermuda grass", "Johnson grass, desert broom", "unidentified aster/Bermuda grass", "Smartweed/Typha mix")

microsite <- microsite%>% 
  mutate(Grouped_Veg = case_when(veg_type %in% grass ~ 'grass', 
                                 veg_type %in% shrubs ~ 'shrubs',
                                 veg_type %in% forbs ~ 'forb',
                                 veg_type %in% sedge_typha ~ 'sedge_typha',
                                 TRUE ~ 'mixed'))

rsf_veg <- full_join(rsf, select(microsite, `Trap Location`, Grouped_Veg), by = c("trap_id" = "Trap Location"))


################################################################### RSF PACKAGE ###############################################################

#######easiest method is probably to try using RSF package in R

install.packages("ResourceSelection")
library(ResourceSelection)

rsf_veg <- as.data.frame(rsf_veg)

# scale continuous variables:
rsf_veg$veg_cover <- scale(rsf_veg$veg_cover)
rsf_veg$dist_water <- scale(rsf_veg$dist_water)

# run rspf & create object for each model of interest
# where "species" is your column of 0s and 1s associated with each species
# so your models will look like:
# rspf(sigmodon~dist_water_stand+dist_veg_stand+veg_cover_stand, data=data, B=99)
#or something like that

m1 <- rspf(sigmodon ~ veg_cover + dist_water + Grouped_Veg, m = 0, data = rsf_veg, B=99)
summary(m1)

m2<-rspf(sigmodon ~ veg_cover + Grouped_Veg, m = 0, data = rsf_veg, B=99)
summary(m2)

m3<-rspf(sigmodon ~ Grouped_Veg, m = 0, data = rsf_veg, B=99)
summary(m3)

m4<-rspf(sigmodon ~ veg_cover, m = 0, data = rsf_veg, B=99)
summary(m4)

m5<-rspf(sigmodon ~ veg_cover * Grouped_Veg, m = 0, data = rsf_veg, B=99)
summary(m5)

#model selection based on AIC

CAIC(m1,m2, m3, m4, m5)
AIC(m1, m2, m3, m4, m5)

#average models within 2 deltaAIC of lowest:
summary(model.avg(m1, m2))



##################################### DREDGE #####################################


#####Can also use dredge method: 


##Look at frequency histogram for original standardized distance variable, see whether it is normal/linear
windows(record=TRUE)
hist(data$var1)

##standardize variables
data$stand_var1<-(data$var1(data$var1))/sd(data$var1)
#note: the command "scale" does the same thing 

####May not need to do this if we don't have any continuous variables:
##Square the distance variable to prepare for the quadratic model
data$var1<-(data$var1)^2

##Standardize squared burrow distance variable
data$stand_burrsq<-(data$distburrsq-mean(data$distburrsq))/sd(data$distburrsq)

hist(data$stand_burrsq)

###Relevel the categorical variable so that the category that comprised the majority of the study area becomes the intercept
data$class<-relevel(data$class,"4")

###use AIC for model selection

##Run the first model with the original standardized distance
#add random effect for loc/species/id if necessary
datamod<-glmer(species~data$class+data$stand_dist_burr+data$stand_dist_wash+(1|id),family="binomial",data=data)

##Run the second model as quadratic using the squared distance variable, if needed
#if no need for a quadratic model, can skip to dredge step
datamodquad<-glmer(species~class+stand_dist_burr+stand_burrsq+stand_dist_wash+stand_dist_washsq+(1|id),family="binomial",data=data)

##Inspect individual model output
summary(datamod)
summary(datamodquad)

##use quadratic as dredge model if higher AIC

#Dredge:
options(na.action=na.fail)
a.dred<-dredge(datamodquad, trace=TRUE, rank="AICc", REML=FALSE)
a.dred
##this output tells you which variables are included in the models with the lowest AIC 

#if you have more than one model within 2 deltaAIC of lowest, can average them together:
summary(model.avg(a.dred, subset=delta<2))

#full v. conditional averages:
##the full average will input a "0" for beta values of variables not included in one model when calculating the model averages
#for example, if you're interested in the effects of a categorical variable, but only one of the top models includes that categorical variable, 
#a full average will average together the models' betas by inputing a "0" for the categorical variable's beta value associated with the model that doesn't have it, 
#a conditional average will calculate it as an "NA".
#in this example, you wouldn't want to input a "0" because it could underestimate the effects of that variable and bias it toward 0
#so you would take the conditional average
#that's my understanding, anyways!

######################################################## Plots ##############################################################

##Calculating and plotting predicted probability based on a variable, the following code uses standard distance to generate the probabilities
library(ggplot2)
library(GGally)
library(reshape2)
library(compiler)

##For additional information on this code, please refer to the web page: http://www.ats.ucla.edu/stat/r/dae/melogit.htm
#create dataframe with just the columns you need:
tmpdat<-data[, c("dist_burr", "stand_dist_burr", "dist_wash", "class", "species", "id", "stand_distsq")]
summary(tmpdat)
jvalues<-with(tmpdat, seq(from=min(stand_dist_burr), to=max(stand_dist_burr),length.out=1000))
pp<-lapply(jvalues, function(j) {
  tmpdat$stand_dist <- j
  predict(datamod1, newdata=tmpdat, type="response")})
sapply(pp[c(1)], mean)
plotdat<-t(sapply(pp, function(x) {
  c(M=mean(x), quantile(x, c(0.25, 0.75)))
}))

##The resulting figure will have standard distance as the x-axis, but can use original distance as well   
plotdat<-as.data.frame(cbind(plotdat, jvalues))
colnames(plotdat)<-c("PredictedProbability", "Lower", "Upper", "StanDistance")
summary(plotdat)
ggplot(plotdat, aes(x=StanDistance, y=PredictedProbability)) + geom_ridataon(aes(ymin=Lower, ymax=Upper), alpha=.15)+geom_line(size=2)+ylim(c(0,1))


