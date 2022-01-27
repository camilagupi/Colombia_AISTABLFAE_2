##############################################################################################################
###################### Logistic regression model for probability of deforestation ############################
##############################################################################################################

# Libraries
library(ggplot2)
library(lattice)
library(GGally)
library(corpcor)
library(mctest)
library(faraway)
library(nnet)
library(mlogit)
library(dfidx)
library(ncf)
library(plyr)
library(sampling)


# Import the data
def_data2<- # here the dataframe as a csv file is imported.
  # The columns in the file are:
  # Cover change type
  # Population density
  # Distance to roads
  # Distance to already deforested areas
  # FARC/No FARC presence
  # Protected Area/Not Protected Area
  # Distance to mines
  # Distance to oil wells
  # Slope in degrees
  # Elevation
  # y coordinate
  # x coordinate
  # distance to rivers
  # slop ein percentage

#############################################################################################################################
### Preparation of the data #################################################################################################
#############################################################################################################################



# Change variables value, FARC_raste and NP_raster_ to factors

def_data2$VALUE<-as.factor(def_data2$VALUE)
def_data2$FARC_raste<-as.factor(def_data2$FARC_raste)
def_data2$NP_raster_<-as.factor(def_data2$NP_raster_)
str(def_data2)

# Change the names of the variables

colnames(def_data2)<-c("cover_change",
                      "population_density",
                      "roads_dist",
                      "deforested_dist",
                      "FARC_presence",
                      "PA",
                      "mines_dist",
                      "wells_dist",
                      "slope",
                      "elevation",
                      "y_coordinate",
                      "x_coordinate",
                      "rivers_dist",
                      "slope_percentage")
summary(def_data2)


# Change the order of the variables in the data frame
def_data2<-def_data2[c(1,5,6,2,14,10,4,3,13,7,8,12,11)]
summary(def_data2)


# Delete the rows with NA's in population_density and rivers_dist

rows_NA_pop2<-which(is.na(def_data2$population_density), arr.ind=TRUE)  # Index of NA's in population_density 
def_data2<-def_data2[-rows_NA_pop2,]                                    # Remove nodata points
rows_NA_riv2<-which(is.na(def_data2$rivers_dist), arr.ind=TRUE)         # Index of NA's in rivers_dist
def_data2<-def_data2[-rows_NA_riv2,]                                    # Remove nodata points
summary(def_data2)



#############################################################################################################################
### Data exploration ########################################################################################################
#############################################################################################################################


# Number of points: total, forest to forest, to cattle, to coca and to other crops

# Histogram of cover_change
barplot(sort(table(def_data2$cover_change),decreasing=T))

#####################
# Check for outliers:
#####################

# Population density


# Slope
boxplot(def_data2$slope_percentage, main="Slope")

# Elevation
dotchart(def_data2$elevation)

# Distance to roads
png(file="Distance to roads.png")
par(mfrow=c(1,2))
boxplot(def_data2$roads_dist, main="Distance to roads")                    # Boxplot
dotchart(sort(def_dat2a$roads_dist),main="Distance to roads", pch=".")     # Cleveland dotplot 
dev.off()

# Distance to rivers
boxplot(def_data2$rivers_dist, main="Distance to rivers")

# Distance to mines
boxplot(def_data2$mines_dist, main="Distance to mines")

# Distance to oil wells
boxplot(def_data2$wells_dist, main="Distance to oil wells")



##############################
# Check for multicollinearity:
##############################

# Pairwise scatterplots comparing covariates

png(file="scatterplot_matrix2.png",               # Image with the scatterplot matrix
    width = 960, height = 960, units = "px")
pairs(~slope_percentage+elevation+deforested_dist+roads_dist+rivers_dist+mines_dist+wells_dist + x_coordinate+y_coordinate,    # The scatterplot matrix
      data = def_data2, 
      main="Scatterplot matrix",
      pch=".")
dev.off()


# Correlation coefficients matrix

matrix_corr2<-def_data2[,4:11]                      # Matrix with only the continuous predictors
cor(matrix_corr2)

png(file="correlation_matrix2.png",                # Image with correlation matrix and scatterplots
    width = 960, height = 960, units = "px")
ggpairs(matrix_corr2)
dev.off()


matrix_corr_xy2<-def_data2[,4:13]                   # Matrix with only the continuous predictors and spatial coordinates
cor(matrix_corr_xy2)

png(file="correlation_matrix_xy2.png",            # Image 
    width = 960, height = 960, units = "px")
ggpairs(matrix_corr_xy2)
dev.off()
                                  
# VIF
vif(matrix_corr2)   # VIF of the continuous predictors

matrix_no_mines<-matrix_corr2[,-7]               # Removing distance to mines
vif(matrix_no_mines)

cor(matrix_no_mines)




#############################################################################################################################
### Multinomial Logistic Regression Model ###################################################################################
#############################################################################################################################

# Indicate the reference level
def_data2$cover_change<-relevel(def_data2$cover_change, ref = "1")


# mod7: 2 binomial logistic regressions with FARC presence as random factor.
# mod71: conversion to agriculture or not.
# mod72: conversion to legal or illegal agricultural uses.

# Data:

# Regular, 20 km
data_reg_20km<-#here,the data with at least 20km distance is imported
summary(data_reg_20km)

data_reg_20km$cover_chan<-as.factor(data_reg_20km$cover_chan)
data_reg_20km$FARC_raste<-as.factor(data_reg_20km$FARC_raste)
data_reg_20km$NP_raster_<-as.factor(data_reg_20km$NP_raster_)
summary(data_reg_20km)

colnames(data_reg_20km)<-c("cover_change",
                               "population_density",
                               "roads_dist",
                               "deforested_dist",
                               "FARC_presence",
                               "PA",
                               "mines_dist",
                               "rivers_dist",
                               "wells_dist",
                               "slope_percentage",
                               "elevation",
                               "y_coordinate",
                               "x_coordinate"
)
summary(data_reg_20km)

# Change the order of the variables in the data frame
data_reg_20km<-data_reg_20km[c(1,5,6,2,10,11,4,3,8,7,9,12,13)]
summary(data_reg_20km)

# Delete the rows with NA's in population_density
rows_NA_pop_207<-which(is.na(data_reg_20km$population_density), arr.ind=TRUE)    # Index of NA's in population_density 
data_reg_20km<-data_reg_20km[-rows_NA_pop_207,]                                       # Remove nodata points
summary(data_reg_20km)

# Remove distance to mines
data_reg_20km<-data_reg_20km[,-10]
summary(data_reg_20km)

# Standardize all the numeric variables
data7_sc<-data_reg_20km                            
data7_sc$population_density<-scale(data_reg_20km$population_density)
data7_sc$slope_percentage<-scale(data_reg_20km$slope_percentage)
data7_sc$elevation<-scale(data_reg_20km$elevation)
data7_sc$deforested_dist<-scale(data_reg_20km$deforested_dist)
data7_sc$roads_dist<-scale(data_reg_20km$roads_dist)
data7_sc$rivers_dist<-scale(data_reg_20km$rivers_dist)
data7_sc$wells_dist<-scale(data_reg_20km$wells_dis)


# Data for mod71
# 0 forest, 1 agriculture

data71<-data7_sc
data71$cover_change<-revalue(x=data71$cover_change , replace = c("1"="0", "2"="1", "3"="1", "4"="1"))
summary(data71)

# Take a random sample of points that remained forested
data71<-data71[order(data71$cover_change),]    # Sort by cover_change

rand_ind7<-sample(x=1:sum(data71$cover_change==0),    # Indexes of random sample of points that remained forested
                   size=(sum(data71$cover_change==0)-sum(data71$cover_change==1)),   # The size of the sample will be equal to the number of pixels converted to agriculture
                   replace=FALSE)

data71<-data71[-rand_ind7,]
barplot(sort(table(data71$cover_change),decreasing=T))  # Now the 2 categories have the same size
summary(data71)


# Data for mod72
# Just point converted to agriculture
# 2 legal uses (cattle and other crops), 3 illegal (coca crops)

data72<-data7_sc[order(data7_sc$cover_change),]    # Sort by cover_change
forest_ind7<-1:sum(data7_sc$cover_change==1)       # index of points that remained forested
data72<-data72[-forest_ind7,]                      # remove those points
data72$cover_change<-as.numeric(data72$cover_change)
data72$cover_change<-as.factor(data72$cover_change)
barplot(sort(table(data72$cover_change),decreasing=T))  # check
summary(data72)                                         # 0 points that remained forested
data72$cover_change<-revalue(x=data72$cover_change , replace = c("2"="2", "3"="3", "4"="2"))
barplot(sort(table(data72$cover_change),decreasing=T))  # check
summary(data72)



# mod71
require(lme4)
mod71_r<-glmer(formula=cover_change~PA+population_density+slope_percentage+elevation+deforested_dist+
                                  roads_dist+rivers_dist+wells_dist+(1|FARC_presence),
             data=data71,
             family=binomial)  # boundary (singular) fit: see ?isSingular
isSingular(mod71_r)              # TRUE

require(stats)
mod71_glm<-glm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
             roads_dist+rivers_dist+wells_dist,
           data=data71, 
           family = binomial(link = "logit"))  # glm.fit: fitted probabilities numerically 0 or 1 occurred 

summary(mod71_glm)
e71_glm<-k_fold_cross_validation_bi(k=10, df=data71)
e71_glm

library(arm)
mod71_bayes <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                roads_dist+rivers_dist+wells_dist,
              data=data71, 
              family="binomial")

summary(mod71_bayes)
e71_bayes<-k_fold_cross_validation_bayes2(k=10, df=data71)
e71_bayes

# Removing distance to wells
mod71_bayes2 <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                          roads_dist+rivers_dist,
                        data=data71, 
                        family="binomial")

summary(mod71_bayes2)
e71_bayes2<-k_fold_cross_validation_bayes2(k=10, df=data71)
e71_bayes2

# Removing distance to rivers
mod71_bayes3 <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                           roads_dist,
                         data=data71, 
                         family="binomial")

summary(mod71_bayes3)
e71_bayes3<-k_fold_cross_validation_bayes3(k=10, df=data71)
e71_bayes3

# Removing PAs
mod71_bayes4 <- bayesglm(cover_change~FARC_presence+population_density+slope_percentage+elevation+deforested_dist+
                           roads_dist,
                         data=data71, 
                         family="binomial")

summary(mod71_bayes4)
e71_bayes4<-k_fold_cross_validation_bayes4(k=10, df=data71)
e71_bayes4

# Removing slope
mod71_bayes5 <- bayesglm(cover_change~FARC_presence+population_density+elevation+deforested_dist+
                           roads_dist,
                         data=data71, 
                         family="binomial")

summary(mod71_bayes5)
e71_bayes5<-k_fold_cross_validation_bayes5(k=10, df=data71)
e71_bayes5

# mod72
mod72_r<-glmer(formula=cover_change~PA+population_density+slope_percentage+elevation+deforested_dist+
                 roads_dist+rivers_dist+wells_dist+(1|FARC_presence),
               data=data72,
               family=binomial)  
summary(mod72_r)
e72_r<-k_fold_cross_validation_r(k=10, df=data72)
e72_r

mod72_bayes <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                          roads_dist+rivers_dist+wells_dist,
                        data=data72, 
                        family="binomial")
summary(mod72_bayes)
e72_bayes<-k_fold_cross_validation_bayes(k=10, df=data72)
e72_bayes

# Removing distance to wells
mod72_bayes2 <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                          roads_dist+rivers_dist,
                        data=data72, 
                        family="binomial")
summary(mod72_bayes2)
e72_bayes2<-k_fold_cross_validation_bayes2(k=10, df=data72)
e72_bayes2

# Removing distance to rivers
mod72_bayes3 <- bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                           roads_dist,
                         data=data72, 
                         family="binomial")
summary(mod72_bayes3)
e72_bayes3<-k_fold_cross_validation_bayes3(k=10, df=data72)
e72_bayes3

# Removing PA
mod72_bayes4 <- bayesglm(cover_change~FARC_presence+population_density+slope_percentage+elevation+deforested_dist+
                           roads_dist,
                         data=data72, 
                         family="binomial")
summary(mod72_bayes4)
e72_bayes4<-k_fold_cross_validation_bayes4(k=10, df=data72)
e72_bayes4

# Removing slope
mod72_bayes5 <- bayesglm(cover_change~FARC_presence+population_density+elevation+deforested_dist+
                           roads_dist,
                         data=data72, 
                         family="binomial")
summary(mod72_bayes5)
e72_bayes5<-k_fold_cross_validation_bayes5(k=10, df=data72)
e72_bayes5



mod72_bayes6 <- bayesglm(cover_change~FARC_presence+population_density+slope_percentage+elevation+deforested_dist+
                           roads_dist+FARC_presence*PA,
                         data=data72, 
                         family="binomial")


# Spatial autocorrelation, mod71
correlog_71_bayes<-correlog(x=data71$x_coordinate, 
                       y=data71$y_coordinate, 
                       z=mod71_bayes5$residuals,
                       increment=20,
                       latlon = TRUE)
correlog_71_bayes$n
correlog_71_bayes$p
correlog_71_bayes$correlation
correlog_71_bayes$x.intercept

plot(correlog_71_bayes, main="Correlogram mod71_bayes5-forest", xlab="Distance (Km)", ylab="Correlation (Moran's I)")    
abline(h=0, lty=5)


# Spatial autocorrelation, mod72
correlog_72_bayes<-correlog(x=data72$x_coordinate, 
                      y=data72$y_coordinate, 
                      z=mod72_bayes5$residuals,
                      increment=30,
                      latlon = TRUE)
correlog_72_bayes$n
correlog_72_bayes$p
correlog_72_bayes$correlation
correlog_72_bayes$x.intercept

plot(correlog_72_bayes, main="Correlogram mod72_bayes5-ag type", xlab="Distance (Km)", ylab="Correlation (Moran's I)")    
abline(h=0, lty=5)
#


# Predictions with updated data

# We need data about:
#  FARC presence                        ("FARC_presence")
#  Population density                   ("population_density") 
#  Elevation                            ("elevation")
#  Distance to already deforested areas ("deforested_dist")
#  Distance to roads                    ("roads_dist")
#  y coordinate                         ("y_coordinate")
#  x coordinate                         ("x_coordinate")

upd_data<-# here, data with updates values for predictions are imported
summary(upd_data)

upd_data$forest_201<-as.factor(upd_data$forest_201)
upd_data$FARC_prese<-as.factor(upd_data$FARC_prese)
summary(upd_data)

colnames(upd_data)<-c("forest",
                     "FARC_presence",
                     "population_density",
                     "elevation",
                     "roads_dist",
                     "deforested_dist",
                     "x_coordinate",
                     "y_coordinate")
summary(upd_data)

# Scale the numerical variables
upd_data_sc<-upd_data
upd_data_sc$population_density<-scale(upd_data$population_density)
upd_data_sc$elevation<-scale(upd_data$elevation)
upd_data_sc$roads_dist<-scale(upd_data$roads_dist)
upd_data_sc$deforested_dist<-scale(upd_data$deforested_dist)
summary(upd_data_sc)

# Probabilities of conversion to agriculture with mod71_bayes5
prob_def<-predict(mod71_bayes5, upd_data_sc, type = "response")
hist(prob_def) 
summary(prob_def)

# Probabilities of conversion to cattle and other crops or coca crops with mod72_bayes5
prob_coca<-predict(mod72_bayes5,upd_data_sc, type = "response")
hist(prob_coca) 
summary(prob_coca)

prob_cattle<-1-prob_coca
hist(prob_cattle) 
summary(prob_cattle)

# Add these three columns of probabilities to upd_data
upd_data_sc$prob_def<-prob_def
upd_data_sc$prob_cattle<-prob_cattle
upd_data_sc$prob_coca<-prob_coca
summary(upd_data_sc)

# Proportion of cattle-other crops in all the points from 2000-2012
w_cattle<-sum(def_data2$cover_change==2)/(sum(def_data2$cover_change==2)+sum(def_data2$cover_change==4))
w_other_crops<-sum(def_data2$cover_change==4)/(sum(def_data2$cover_change==2)+sum(def_data2$cover_change==4))
w_cattle+w_other_crops  # should =1

# Net rent for three types of agricultural uses (in USD/ha/year)
rent_cattle<-137.48
rent_coca<-952.23
rent_others<-1000.4

# Net rent for cattle and other crops (USD/ha/year)
rent_cat_other<-(w_cattle*rent_cattle)+(w_other_crops*rent_others)
rent_cat_other

# Expected return for cattler+others and for coca crops (USD/ha/year)
upd_data_sc$exp_cat_others<-upd_data_sc$prob_def*upd_data_sc$prob_cattle*rent_cat_other
upd_data_sc$exp_coca<-upd_data_sc$prob_def*upd_data_sc$prob_coca*rent_coca
summary(upd_data_sc)

# Total expected return for agricultural uses (USD/ha/year)
upd_data_sc$exp_total<-upd_data_sc$exp_cat_others+upd_data_sc$exp_coca
summary(upd_data_sc)

# Discount rate
disc_1<-0.05
disc_2<-0.1
disc_3<-0.2
  
# Divide total expected return by the discount rate
upd_data_sc$final_return1<-upd_data_sc$exp_total/disc_1
upd_data_sc$final_return2<-upd_data_sc$exp_total/disc_2
upd_data_sc$final_return3<-upd_data_sc$exp_total/disc_3
summary(upd_data_sc)
  
# Create the dataframe with x and y coordinates and final expected returns only
final_returns<-data.frame(upd_data_sc$prob_def,
                          upd_data_sc$final_return1,
                          upd_data_sc$final_return2,
                          upd_data_sc$final_return3,
                          upd_data_sc$x_coordinate,
                          upd_data_sc$y_coordinate)

colnames(final_returns)<-c("Prob_def",
                           "Return_1",
                           "Return_2",
                           "Return_3",
                           "x_coordinate",
                           "y_coordinate")

summary(final_returns)


# Export dataframe to .csv file
  write.csv(x=final_returns, file = "C:\\Users\\Usuario\\Dropbox\\Camila project\\data\\Returns.csv")
  


