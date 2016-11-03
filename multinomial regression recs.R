#######################################
#                                     #
#  Multinomial Regression: REC score  #
#                                     #
#######################################

source('K:/math400-02-f14/Common/R Functions/all_functions.R')

# Load packages
library("foreign")
install.packages("Matching")
library("Matching")








cps<- read.dta("K:\\math400-02-f14\\CourseMaterials\\Data\\cps1re74.dta")
nsw<- read.dta("K:\\math400-02-f14\\CourseMaterials\\Data\\nswre74.dta")

cps <- clean.data(cps)
nsw <- clean.data(nsw)
controls <- merge.null(nsw,cps, type="control")

category <- numeric(nrow(cps))
table(category)
category[which(cps$treat==0 & cps$case==0)] <- 1
category[which(cps$treat==0 & cps$case==1)] <- 2
category[which(cps$treat==1 & cps$case==0)] <- 3
category[which(cps$treat==1 & cps$case==1)] <- 4
cps$category <- category
cps$category

category <- numeric(nrow(nsw))
category[which(nsw$treat==0 & nsw$case==0)] <- 1
category[which(nsw$treat==0 & nsw$case==1)] <- 2
category[which(nsw$treat==1 & nsw$case==0)] <- 3
category[which(nsw$treat==1 & nsw$case==1)] <- 4
nsw$category <- category
nsw$category

library(nnet)


# probs <- multinom(category ~ age + age2 + black + hisp + nodeg + re74 + ed + re75, data=cps)$fitted.values
# newestimate <- probs[,3] + probs[,4]
# splom(~ recs| treat * case, data=cps)
# cps$recs <- results
# propestimate <- calculate.prop.score(cps, tail=T)$prop.score

#plot(log(propestimate), log(newestimate))

# 
# results <- rep(NAz, nrow(cps))
# for(i in 1:nrow(cps)){
#   coefs <- coefficients(model)
#   observed <- c(cps$age[i],cps$age2[i], cps$black[i],cps$hisp[i], cps$nodeg[i],cps$re74[i], cps$ed[i],cps$re75[i])
# 
#   if(cps$treat[i]==0 & cps$case[i]==1){
#     results[i] <- sum(observed * coefs[1,-1])
#   }
#   if(cps$treat[i]==1 & cps$case[i]==0){
#     results[i] <- sum(observed * coefs[2,-1])
#   }
#   if(cps$treat[i]==1 & cps$case[i]==1){
#     results[i] <- sum(observed * coefs[3,-1])
#   }
# }
# hist(results)

# hispanic was removed from the following function
plotrecs <- function(dataset, ncontrols=1, pseudocount=T){
  set.seed(123)
  referentindex <- sample((1:nrow(dataset))[dataset$case == 0],sum(dataset$case)*ncontrols)
  referents <- dataset[referentindex,]
  controls2 <- rbind(dataset[dataset$case==1,],referents)
  if (pseudocount){
    pseudo <- apply(controls2, 2, mean)
    pseudo[16] <- 1
    pseudo[15] <- 0
    pseudo[1] <- 0
    controls2pseudo <- rbind(controls2, pseudo)
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    pseudo[16] <- 2
    pseudo[15] <- 1
    pseudo[1] <- 0
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    pseudo[16] <- 3
    pseudo[15] <- 0
    pseudo[1] <- 1
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    pseudo[16] <- 4
    pseudo[15] <- 1
    pseudo[1] <- 1
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    controls2pseudo <- rbind(controls2pseudo, pseudo)
    controls2 <- controls2pseudo
  }
  model <- multinom(category ~ age + age2 +ed + I(ed^2) + nodeg + married + black + re74 + re75 + u74 + u75 + ed:re74 + I(age^3), data=controls2, maxit=200)
  print(model)
  results01 <- rep(NA, nrow(controls2))
  results10 <- rep(NA, nrow(controls2))
  results11 <- rep(NA, nrow(controls2))
  for(i in 1:nrow(controls2)){
    coefs <- coefficients(model)
    observed <- c(controls2$age[i],controls2$age2[i], controls2$ed[i], controls2$ed[i]^2, controls2$nodeg[i], controls2$married[i], controls2$black[i], controls2$re74[i],controls2$re75[i], controls2$u74[i],controls2$u75[i], controls2$ed[i]*controls2$re74[i], controls2$age[i]^3)
    
    results01[i] <- sum(observed * coefs[1,-1])
    results10[i] <- sum(observed * coefs[2,-1])
    results11[i] <- sum(observed * coefs[3,-1])
  }
  cols <- ifelse(controls2$treat == 1, "dark green", "red")
  hist(results01[controls2$treat == 1], col=rgb(0,1,0,0.5), xlab="RECS Score (01) (green is treated, red is untreated)", xlim=c(-10,35), prob=T, main="Estimated RECS Score (01)")
  hist(results01[controls2$treat == 0], col=rgb(1,0,0,0.5), xlab="RECS Score (01) (green is treated, red is untreated)", prob=T, add=T)
  hist(results10[controls2$treat == 1], col=rgb(0,1,0,0.5), xlab="RECS Score (10) (green is treated, red is untreated)", xlim=c(-100,300), prob=T, main="Estimated RECS Score (10)")
  hist(results10[controls2$treat == 0], col=rgb(1,0,0,0.5), xlab="RECS Score (10) (green is treated, red is untreated)", prob=T, add=T)
  hist(results11[controls2$treat == 1], col=rgb(0,1,0,0.5), xlab="RECS Score (11) (green is treated, red is untreated)", xlim=c(-100,300), prob=T, main="Estimated RECS Score (11)")
  hist(results11[controls2$treat == 0], col=rgb(1,0,0,0.5), xlab="RECS Score (11) (green is treated, red is untreated)", prob=T, add=T)
  pairs(~ results01 + results10 + results11, col=cols)
  
  new.data <- data.frame(age=controls2$age, age2=controls2$age2, ed=controls2$ed, nodeg=controls2$nodeg, married=controls2$married, black=controls2$black, re74=controls2$re74, re75=controls2$re75, u74=controls2$u74, u75=controls2$u75)
  
  return(cbind(predict(model, type="probs", new.data),controls2))
}
#note that ive made ncontrols 2 now.
x <- plotrecs(cps, 2)
hist(x[x$treat == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x[x$treat == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", prob=T, add=T)
hist(x[x$treat == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x[x$treat == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", prob=T, add=T)
hist(x[x$treat == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x[x$treat == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", prob=T, add=T)
hist(x[x$treat == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x[x$treat == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", prob=T, add=T)

hist(x[x$case == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x[x$case == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", prob=T, add=T)
hist(x[x$case == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x[x$case == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", prob=T, add=T)
hist(x[x$case == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x[x$case == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", prob=T, add=T)
hist(x[x$case == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x[x$case == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", prob=T, add=T)





nohisp <- cps[cps$hisp==0,]
x2 <- plotrecs(nohisp, 2)
table(x2$category)

hist(x2[x2$treat == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x2[x2$treat == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", prob=T, add=T)
hist(x2[x2$treat == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x2[x2$treat == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", prob=T, add=T)
hist(x2[x2$treat == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x2[x2$treat == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", prob=T, add=T)
hist(x2[x2$treat == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x2[x2$treat == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", prob=T, add=T)

hist(x2[x2$case == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x2[x2$case == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", prob=T, add=T)
hist(x2[x2$case == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x2[x2$case == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", prob=T, add=T)
hist(x2[x2$case == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x2[x2$case == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", prob=T, add=T)
hist(x2[x2$case == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x2[x2$case == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", prob=T, add=T)


#let's try adding 8 pseudocount people
#predict probabilities from model: is there a predict function? otherwise, see picture from Friday
#KAITLYN I DONT KNOW HOW TO PREDICT PROBABILITIES, CAN YOU TAKE A LOOK? OTHERWISE WE CAN ASK DAVE BUT THIS IS A GOOD BASELINE
#add 8 pseudocounts. To add more include the following line more times between each group and rerun
#nohisppseudo <- rbind(nohisppseudo, pseudo)
pseudo <- apply(nohisp, 2, mean)
pseudo[16] <- 1
pseudo[15] <- 0
pseudo[1] <- 0
nohisppseudo <- rbind(nohisp, pseudo)
nohisppseudo <- rbind(nohisppseudo, pseudo)
pseudo[16] <- 2
pseudo[15] <- 1
pseudo[1] <- 0
nohisppseudo <- rbind(nohisppseudo, pseudo)
nohisppseudo <- rbind(nohisppseudo, pseudo)
pseudo[16] <- 3
pseudo[15] <- 0
pseudo[1] <- 1
nohisppseudo <- rbind(nohisppseudo, pseudo)
nohisppseudo <- rbind(nohisppseudo, pseudo)
pseudo[16] <- 4
pseudo[15] <- 1
pseudo[1] <- 1
nohisppseudo <- rbind(nohisppseudo, pseudo)
nohisppseudo <- rbind(nohisppseudo, pseudo)

#run analysis with the pseudocounts included
x3 <- plotrecs(nohisp, 2, T)
#seems to reverse the trends on several lines, unclear why

hist(x3[x3$treat == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x3[x3$treat == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is treated, red is untreated)", prob=T, add=T)
hist(x3[x3$treat == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x3[x3$treat == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is treated, red is untreated)", prob=T, add=T)
hist(x3[x3$treat == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x3[x3$treat == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is treated, red is untreated)", prob=T, add=T)
hist(x3[x3$treat == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x3[x3$treat == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is treated, red is untreated)", prob=T, add=T)

hist(x3[x3$case == 0,1], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 1")
hist(x3[x3$case == 1,1], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 1 (green is case, red is uncase)", prob=T, add=T)
hist(x3[x3$case == 0,2], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 2")
hist(x3[x3$case == 1,2], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 2 (green is case, red is uncase)", prob=T, add=T)
hist(x3[x3$case == 0,3], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 3")
hist(x3[x3$case == 1,3], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 3 (green is case, red is uncase)", prob=T, add=T)
hist(x3[x3$case == 0,4], col=rgb(1,0,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", xlim=c(0,1), prob=T, main="Predicted prob of cat 4")
hist(x3[x3$case == 1,4], col=rgb(0,1,0,0.5), xlab="Predicted prob of cat 4 (green is case, red is uncase)", prob=T, add=T)




# can we do three-dimensional object via match function?
# are there functions that do any sort of matching without subclassification?


cem(treatment=category, data = x3, datalist=NULL, cutpoints = NULL,  
    grouping = NULL, drop=NULL, eval.imbalance = FALSE, k2k=FALSE,  
    method=NULL, mpower=2, L1.breaks = NULL, L1.grouping = NULL, 
    verbose = 0, baseline.group="1",keep.all=FALSE)





#simpler model example
set.seed(123)
referentindex <- sample((1:nrow(temp))[temp$case == 0],sum(temp$case)*2)
referents <- temp[referentindex,]
controls2 <- rbind(temp[temp$case==1,],referents)
model <- multinom(category ~ age + age2 + black + nodeg + re74 + ed + re75, data=temp)
model
results01 <- rep(NA, nrow(controls2))
results10 <- rep(NA, nrow(controls2))
results11 <- rep(NA, nrow(controls2))
for(i in 1:nrow(controls2)){
  coefs <- coefficients(model)
  observed <- c(controls2$age[i],controls2$age2[i], controls2$black[i], controls2$nodeg[i],controls2$re74[i], controls2$ed[i],controls2$re75[i])
  
  results01[i] <- sum(observed * coefs[1,-1])
  results10[i] <- sum(observed * coefs[2,-1])
  results11[i] <- sum(observed * coefs[3,-1])
}
cols <- ifelse(controls2$treat == 1, "dark green", "red")
pairs(~ results01 + results10 + results11, col=cols)


table(temp$case[temp$treat==1], temp$hisp[temp$treat==1])
sum(temp$hisp)
sum(temp$hisp[temp$treat==1])










set.seed(123)
referentindex <- sample((1:nrow(cpspseudo))[cpspseudo$case == 0],sum(cpspseudo$case)*1)
referents <- cpspseudo[referentindex,]
controls2 <- rbind(cpspseudo[cpspseudo$case==1,],referents)
model <- multinom(category ~ age + age2 +ed + I(ed^2) + nodeg + married + black + hisp + re74 + re75 + u74 + u75 + ed:re74 + I(age^3), data=cps, maxit=200)
model
?multinom
results01 <- rep(NA, nrow(controls2))
results10 <- rep(NA, nrow(controls2))
results11 <- rep(NA, nrow(controls2))
for(i in 1:nrow(controls2)){
  coefs <- coefficients(model)
  observed <- c(controls2$age[i],controls2$age2[i], controls2$black[i],controls2$hisp[i], controls2$nodeg[i],controls2$re74[i], controls2$ed[i],controls2$re75[i])
  
  results01[i] <- sum(observed * coefs[1,-1])
  results10[i] <- sum(observed * coefs[2,-1])
  results11[i] <- sum(observed * coefs[3,-1])
}
cols <- ifelse(controls2$hisp == 1, "dark green", "red")
pairs(~ results01 + results10 + results11, col=cols)


table(cpspseudo$case[cpspseudo$treat==1], cpspseudo$hisp[cpspseudo$treat==1])
sum(cpspseudo$hisp)














studyrecs <- function(dataset, ncontrols=1){
  referentindex <- sample((1:nrow(dataset))[dataset$case == 0],sum(dataset$case)*ncontrols)
  referents <- dataset[referentindex,]
  controls2 <- rbind(dataset[dataset$case==1,],referents)
  prop <- multinom(category ~ treat + age + age2 + black + hisp + nodeg + ed + re74 + re75, data=controls2)$fitted.values
  
  p1 <- 1
  p0 <- sum(dataset$case)*ncontrols/sum(dataset$case==0)
  
  recs <- NULL
  
  for(i in 1:nrow(controls2)){
    if(controls2$treat[i]==0 & controls2$case[i]==0){
      piwy <- prop[i,1]
      py <- p0
    }
    if(controls2$treat[i]==0 & controls2$case[i]==1){
      piwy <- prop[i,2]
      py <- p1
    }
    if(controls2$treat[i]==1 & controls2$case[i]==0){
      piwy <- prop[i,3]
      py <- p0
    }
    if(controls2$treat[i]==1 & controls2$case[i]==1){
      piwy <- prop[i,4]
      py <- p1
    } 
    temp <- py*piwy/(p0*prop[i,1] + p1*prop[i,2] + p0*prop[i,3] + p1*prop[i,4])
    recs <- rbind(recs,temp)
    rownames(recs) <- NULL
  }    
  controls2$recs <- recs
  return(controls2)
}
recsdata <- studyrecs(cps)
hist(recsdata$recs)


# cem, treatment = case/noncase, create data set containing only case/noncase status and three rec scores