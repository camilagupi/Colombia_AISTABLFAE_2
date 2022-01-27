#############################################################################################################################
### k-fold Cross-validation #################################################################################################
#############################################################################################################################

# Function to perform a k-fold cross validation on the multinomial logistic regression with 4 categories.
# As a score for the k models fit, it calculates the ....
# Parameters:
#  k   number of folds
#  df  data frame
# Returns:
#  Average score for the k models fit.

k_fold_cross_validation<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-multinom(formula= cover_change~., data=train_data)   
    
    # Evaluate the model on the validation data
    pred_k<-predict(mod_k,test_data)   # predictions with the validation data
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/sum(conf_matr_k[1:nrow(conf_matr_k)])
    NPV_1[i]<-sum(conf_matr_k[c(6,7,8,10,11,12,14,15,16)])/sum(conf_matr_k[5:16]) 
  } # End of for(i in 1:k)
  
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ  # Return the average of the k evaluation scores
} # End of k_fold_cross_validation<-function






# Function to perform a k-fold cross validation on the multinomial logistic regression with 3 categories.
# As a score for the k models fit, it calculates the ....
# Parameters:
#  k   number of folds
#  df  data frame
# Returns:
#  Average score for the k models fit.

k_fold_cross_validation_three<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-multinom(formula= cover_change~., data=train_data)   
    
    # Evaluate the model on the validation data
    pred_k<-predict(mod_k,test_data)   # predictions with the validation data
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
  } # End of for(i in 1:k)
  
  return<-mean(scores)  # Return the average of the k evaluation scores
} # End of k_fold_cross_validation:three<-function






# Function to perform a k-fold cross validation on the binomial logistic regression.
# As a score for the k models fit, it calculates the ....
# Parameters:
#  k   number of folds
#  df  data frame
# Returns:
#  Average score for the k models fit.

k_fold_cross_validation_bi<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-glm(cover_change~.,data=train_data, family = binomial(link = "logit"))  
    
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function



k_fold_cross_validation_bayes<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                      roads_dist+rivers_dist+wells_dist,
                    data=df, family="binomial")
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function




k_fold_cross_validation_bayes2<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                      roads_dist+rivers_dist,
                    data=df, family="binomial")
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function


k_fold_cross_validation_bayes3<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-bayesglm(cover_change~FARC_presence+PA+population_density+slope_percentage+elevation+deforested_dist+
                      roads_dist,
                    data=df, family="binomial")
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function







k_fold_cross_validation_bayes4<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-bayesglm(cover_change~FARC_presence+population_density+slope_percentage+elevation+deforested_dist+
                      roads_dist,
                    data=df, family="binomial")
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function






k_fold_cross_validation_bayes5<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-bayesglm(cover_change~FARC_presence+population_density+elevation+deforested_dist+
                      roads_dist,
                    data=df, family="binomial")
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function







k_fold_cross_validation_r<-function(k, df){
  
  data<-df[sample(nrow(df)),]    # randomly shuffle the data
  folds<- cut(seq(1,nrow(data)),breaks = k,labels = FALSE)  #create k equally size folds
  scores<-numeric(k)   # Vector to save the incorrec predictions of the k models
  PPV_1<-numeric(k)    # Positive Predictive Value of points that remained forested
  NPV_1<-numeric(k)    # Negative Predictive Value of points that remained forested
  
  for(i in 1:k){     # Perform k-fold cross validation
    
    # Get validation and training data sets 
    test_ind<-which(folds==i , arr.ind = TRUE)   # Indexes for validation data
    test_data<-data[test_ind,]   # Validation data
    train_data<-data[-test_ind,] # Training data
    
    # Fit the model on the training data
    mod_k<-glmer(formula=cover_change~PA+population_density+slope_percentage+elevation+deforested_dist+
                   roads_dist+rivers_dist+wells_dist+(1|FARC_presence),
                 data=df,
                 family=binomial)
    # Evaluate the model on the validation data
    prob_k<-predict(mod_k,test_data, type="response")   # predictions with the validation data
    pred_k<-ifelse(prob_k>0.5,1,0)
    conf_matr_k<-table(test_data$cover_change, pred_k) # Confusion matrix of validation data
    p_diag<-sum(diag(conf_matr_k))/sum(conf_matr_k)    # Proportion of points correctly predicted
    e_k<-1-p_diag                                      # Percentage of points uncorrectly predicted
    scores[i]<-e_k                                     # Save the error of this fold
    PPV_1[i]<-conf_matr_k[1]/(conf_matr_k[1]+conf_matr_k[2])
    NPV_1[i]<-conf_matr_k[4]/(conf_matr_k[3]+conf_matr_k[4])
  } # End of for(i in 1:k)
  answ<-c(mean(scores),mean(PPV_1),mean(NPV_1))
  return<-answ       # Return the average of the k evaluation scores
} # End of k_fold_cross_validation_bi<-function



