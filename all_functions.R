 ############################################
#              All Functions               #
############################################

source('K:/math400-02-f14/Common/R Functions/case_referent_functions.R')
source('K:/math400-02-f14/Common/R Functions/observational_functions.R')

install.packages("Matching")
install.packages("cem")
library(Matching)
library(survival)
library(foreign)
library(nnet)
library(cem)


###### Cleaning Data ######

clean.data <- function(data.set){
  # data set is a data frame containing the employment data from the study of interest
  
  data.set$u74 <- ifelse(data.set$re74 < 1, 1, 0)
  data.set$u75 <- ifelse(data.set$re75 < 1, 1, 0)
  data.set$employed <- ifelse(data.set$re78 < 1, 0, 1)
  data.set$case <- 1-(data.set$employed)
  
  return(data.set)
}

merge.null <- function(data.set.1, data.set.2, type){
  # data set 1 is a data frame of employment data.
  # data set 2 is also a data of more employment data.
  # type indicates whether we are merging "referent" or "control" observations
  
  if (type=="referent"){
    data.set <- merge.referent(data.set.1, data.set.2)
  } else if (type=="control"){
    data.set <- merge.controls(data.set.1, data.set.2)
  } else {
    print("Please specify type as either 'referent' or 'control'.")
    data.set <- NULL
  }
  
  return(data.set)
}

matched.pairs.data <- function(data.set, matched.pairs){
  # data.set is the data frame to be pared down
  # matched.pairs is the m-column matrix produced by match.function() with matches
  
  m <- ncol(matched.pairs)-1
  
  matched.data <- data.set[matched.pairs[,1],]
  
  for (i in 1:m){
    matched.data.control <- data.set[matched.pairs[,i+1],]
    matched.data <-rbind(matched.data, matched.data.control)
  }
  
  matched.data$pair.ID <- rep(c(1:length(matched.pairs[,1])), m+1)
  
  return(matched.data)
}

###### Simulating Case-Referent Study ######

study <- function(dataset, ncontrols=1, adjust = "unadjusted", numSims=1000){ 
  
  results <- rep(NA, numSims)
  
  set.seed(123)
  for(i in 1:numSims){
    print(i)
    referentindex <- sample((1:nrow(dataset))[dataset$case == 0],sum(dataset$case)*ncontrols)
    referents <- dataset[referentindex,]
    
    if(adjust=="unadjusted"){
      y1 <- mean(dataset$treat[dataset$case==1])
      y0 <- mean(referents$treat)
      results[i] <- 1/(y1/(1-y1)/(y0/(1-y0)))
    }
    else {
      controls2 <- rbind(dataset[dataset$case==1,],referents)
      model <- glm(case ~ treat + age + age2 + black + hisp + nodeg + ed + re74 + re75, data=controls2, family=binomial)
      results[i] <- 1/exp(model$coefficients[2])
    }
  }
  return(results)
}

###### Calculating Propensity Score ######

calculate.prop.score <- function(data.set, tail=F){
  # data.set is a data frame with cleaned treatment/control information
  # tail determines whether or not controls with extremely small propensity scores should be discarded (default)
  if(dim(data.set)[1]==2675|dim(data.set)[1]==2745){
    treat.glm <- glm(treat ~ age + age2 + ed + I(ed^2) + married + nodeg + black + hisp + re74 + re75 + I(re74^2) + I(re75^2) + u74*black, data=data.set, family=binomial)
  }
  else{
    treat.glm <- glm(treat ~ age + age2 +ed + I(ed^2) + nodeg + married + black + hisp + re74 + re75 + u74 + u75 + ed:re74 + I(age^3), data=data.set, family=binomial)
  }
  prop.score <- predict.glm(treat.glm, data.set, type="response")
  data.set$prop.score <- prop.score
  
  if (!tail){
    minimum <- min(data.set$prop.score[data.set$treat==1])
    index <- which(data.set$prop.score < minimum)
    
    data.set.2 <- data.set[-index,]
    if(dim(data.set)[1]==2675|dim(data.set)[1]==2745){
      treat.glm.2 <- glm(treat ~ age + age2 + ed + I(ed^2) + married + nodeg + black + hisp + re74 + re75 + I(re74^2) + I(re75^2) + u74*black, data=data.set.2, family=binomial)
    } else {
      treat.glm.2 <- glm(treat ~ age + age2 +ed + I(ed^2) + nodeg + married + black + hisp + re74 + re75 + u74 + u75 + ed:re74 + I(age^3), data=data.set.2, family=binomial)
      }
    prop.score.2 <- predict.glm(treat.glm.2, data.set.2, type="response")
    data.set.2$prop.score <- prop.score.2
  } else {
    data.set.2 <- data.set
  }
  
  return(data.set.2)
}

###### Finding Matches ######

match.function <- function(data.set, m=1, type){
  # data.set is a cleaned data frame containing treatment/control data
  # type indicates whether we are using an 'observational' or 'case-referent' study
  
  if (type=="observational"){
    matched.pairs <- match.r.obs(data.set)
  } else if (type=="referent"){
    matched.pairs <- match.r.referent(data.set, m)
  } else{
    print("Please specify type as either 'observational' or 'referent'.")
    matched.pairs <- NULL
  }
  
  return(matched.pairs)
}

###### Calculating estimands ######

estimate <- function(dataset, value, type, estimand="tau", population = "not tt", SE=F) {
  library("lmtest")
  library("sandwich")
  if(type=="unadjusted"){
    if(value=="employed"){
      y1 <- mean(dataset$employed[dataset$treat==1])
      y0 <- mean(dataset$employed[dataset$treat==0])
      if(estimand=="tau"){
        if(!SE){return(y1-y0)}
        else{
          return(sqrt(var(dataset$employed[dataset$treat==1])/sum(dataset$treat==1)+var(dataset$employed[dataset$treat==0])/sum(dataset$treat==0)))
        }
      }
      else{return(y1/(1-y1)/(y0/(1-y0)))}
    }
    else{
      if(!SE){
        return(mean(dataset$re78[dataset$treat==1])-mean(dataset$re78[dataset$treat==0], na.rm=T))
      }
      else{
        return(sqrt(var(dataset$re78[dataset$treat==1])/sum(dataset$treat==1)+var(dataset$re78[dataset$treat==0])/sum(dataset$treat==0)))
      }
    }
  }
  if(type=="adjusted"){
    if(value=="employed"){      
      if(estimand=="tau"){
        model <- lm(employed ~ treat + age + age2 + black + hisp + nodeg + ed + re74 + re75, data=dataset)
        if(!SE){
          return(model$coefficients[2])
        }
        else{
          return(coeftest(model,vcov = vcovHC(model, type = "HC0"))[2,2])
        }
      }
      else{
        model <- glm(employed ~ treat + age + age2 + black + hisp + nodeg + ed + re74 + re75, data=dataset, family=binomial)
        if(!SE){
          return(exp(model$coefficients[2]))
        }
        else {
          return(coef(summary(model))[2,2])
        }
      } 
    }
    else{
      model <- lm(re78 ~ treat + age + age2 + black + hisp + nodeg + ed + re74 + re75, data=dataset)
      if(SE){
        return(coeftest(model,vcov = vcovHC(model, type = "HC0"))[2,2])
      } else {
        return(model$coefficients[2])
      }
    }
  }
  if(type=="Freedman"){
    if(value=="employed"){
      model161 <- glm(employed ~ treat + age + age2 + black + hisp + nodeg + re74 + ed + re75, data=dataset, family=binomial)
      temp2 <- dataset
      temp2$treat <- 1
      y1hat <- mean(predict(model161, newdata=temp2, type="response"))
      temp2$treat <- 0
      y0hat <- mean(predict(model161, newdata=temp2, type="response"))
      
      if(population=="tt"){
        model161 <- glm(employed ~ age + age2 + black + hisp + nodeg + re74 + ed + re75, data=dataset[dataset$treat==0,], family=binomial)
        y1hat <- mean(dataset$employed[dataset$treat==1])
        y0hat <- mean(predict(model161, newdata=dataset[dataset$treat==1,], type="response"))
      }
      
      if(estimand=="tau"){
        return(y1hat-y0hat)
      }
      else{return(y1hat/(1-y1hat)/(y0hat/(1-y0hat)))}
    }
    else{
      model161 <- lm(re78 ~ age + age2 + black + hisp + nodeg + re74 + ed + re75, data=dataset[dataset$treat==1,])      
      model162 <- lm(re78 ~ age + age2 + black + hisp + nodeg + re74 + ed + re75, data=dataset[dataset$treat==0,])
      y1hat <- mean(predict(model161, newdata=dataset, type="response"))
      y0hat <- mean(predict(model162, newdata=dataset, type="response"))
      
      if(population=="tt"){
        model161 <- lm(re78 ~ age + age2 + black + hisp + nodeg + re74 + ed + re75, data=dataset[dataset$treat==0,])
        y1hat <- mean(dataset$re78[dataset$treat==1])
        y0hat <- mean(predict(model161, newdata=dataset[dataset$treat==1,], type="response"))
        
      }
      
      
      return(y1hat-y0hat)
    }    
  }
}

binned.odds.ratio.obs <- function(data.set, num.bins){
  # data.set is a cleaned data frame with matched treatment/control information
  # num.bins is a numerical value for the number of bins to be used
  
  data.treat <- data.set[which(data.set$treat==1),]
  data.control <- data.set[which(data.set$treat==0),]
  
  index <- order(data.treat$prop.score, decreasing=T)
  data.treat <- data.treat[index,]
  data.control <- data.control[index,]
  
  bin.width <- round(length(data.treat$treat)/num.bins)
  bin.designation <- c()
  for (i in 1:(num.bins-1)){
    bin.designation <- c(bin.designation, rep(i, bin.width))
  }
  bin.designation <- c(bin.designation, rep(num.bins, length(data.treat$treat)-(num.bins-1)*bin.width))
  
  odds.ratio <- mantelhaen.test(x=as.factor(data.treat$employed), y=as.factor(data.control$employed), z=as.factor(bin.designation))$estimate
  
  return(odds.ratio)
}

matched.odds.ratio.referent.2 <- function(data.set, n){
start <- proc.time()[3]
data.set <- matched.data
n <- length(which(data.set$case==1))
m <- length(data.set$case)/length(which(data.set$case==1)) - 1

u <- 0
v <- 0
for(i in 1:n){
  case <- data.set$treat[i]
  controls <- data.set$treat[(1:m)*n +i]
  u <- u + sum(case < controls)
  v <- v +sum(case > controls)
}
end<-proc.time()[3]
end - start

odds.ratio <- u/v

return (odds.ratio)
}


matched.odds.ratio <- function(data.set, type){
  # data.set is a cleaned data frame with matched information
  # type indicates whether we are using an 'observational' or 'case-referent' study
  
  if (type=="observational"){
    odds.ratio <- matched.odds.ratio.obs(data.set)
  } else if (type=="referent"){
    odds.ratio <- matched.odds.ratio.referent(data.set)
  } else{
    print("Please specify type as either 'observational' or 'referent'.")
    odds.ratio <- NULL
  }
  
  return (odds.ratio)   
}



