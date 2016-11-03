#######################################
# Variability in Odds Ratio Estimates #
#######################################

library(foreign)
library(xtable)

nsw<- read.dta("K:/math400-02-f14/CourseMaterials/Data/nswre74.dta")
nsw <- clean.data(nsw)
nsw <- calculate.prop.score(nsw, tail=T)

cps<- read.dta("K:/math400-02-f14/CourseMaterials/Data/cps1re74.dta")
cps <- clean.data(cps)
cps <- calculate.prop.score(cps,tail=T)

controls <- merge.controls(nsw,cps)
controls <- clean.data(controls)
controls <- calculate.prop.score(controls, tail=T)


## Case-Referent Scenario ##
##NSW##
# M = 1

odds.ratio.1.1 <- rep(NA, 10) #unadjusted odds ratio (u/v)
odds.ratio.2.1 <- rep(NA, 10) #conditional logistic regression odds ratio 

for (i in 1:100){
  matched.pairs <- match.function(nsw, 1, "referent")
  nsw.match <- matched.pairs.data(nsw, matched.pairs)
  odds.ratio.1.1[i] <- matched.odds.ratio(nsw.match, "referent")
  odds.ratio.2.1[i] <- clogit.referent(nsw.match) 
}

# Both methods of calculating the odds ratio are equivalent for m=1
hist(odds.ratio.1.1)
mean(odds.ratio.1.1) #1.465359 
hist(odds.ratio.2.1)
mean(odds.ratio.2.1) #1.465359

# M = 2

odds.ratio.1.2 <- rep(NA, 100)
odds.ratio.2.2 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(nsw, 2, "referent")
  nsw.match <- matched.pairs.data(nsw, matched.pairs)
  odds.ratio.1.2[i] <- matched.odds.ratio(nsw.match, "referent")
  odds.ratio.2.2[i] <- clogit.referent(nsw.match) 
}

hist(odds.ratio.1.2)
mean(odds.ratio.1.2)#1.702
hist(odds.ratio.2.2)
mean(odds.ratio.2.2)#1.693

#---------------------------------------------------------------------------------------------------
##CPS##
# M = 1

odds.ratio.1.1 <- rep(NA, 100)
odds.ratio.2.1 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, 1, "referent")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1.1[i] <- matched.odds.ratio(cps.match, "referent")
  odds.ratio.2.1[i] <- clogit.referent(cps.match) 
}

hist(odds.ratio.1.1)
mean(odds.ratio.1.1)#1.67405
hist(odds.ratio.2.1)
mean(odds.ratio.2.1)#1.67405 

# M = 2

odds.ratio.1.2 <- rep(NA, 100)
odds.ratio.2.2 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, 2, "referent")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1.2[i] <- matched.odds.ratio(cps.match, "referent")
  odds.ratio.2.2[i] <- clogit.referent(cps.match) 
}

hist(odds.ratio.1.2)
mean(odds.ratio.1.2)#1.7449
hist(odds.ratio.2.2)
mean(odds.ratio.2.2)#1.7326


# M = 3

odds.ratio.1.3 <- rep(NA, 100)
odds.ratio.2.3 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, 3, "referent")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1.3[i] <- matched.odds.ratio(cps.match, "referent")
  odds.ratio.2.3[i] <- clogit.referent(cps.match) 
}

hist(odds.ratio.1.3)
mean(odds.ratio.1.3)
hist(odds.ratio.2.3)
mean(odds.ratio.2.3) 

# M = 4

odds.ratio.1.4 <- rep(NA, 100)
odds.ratio.2.4 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, 4, "referent")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1.4[i] <- matched.odds.ratio(cps.match, "referent")
  odds.ratio.2.4[i] <- clogit.referent(cps.match) 
}

hist(odds.ratio.1.4)
mean(odds.ratio.1.4) 
hist(odds.ratio.2.4)
mean(odds.ratio.2.4) 
# M = 5

odds.ratio.1.5 <- rep(NA, 100)
odds.ratio.2.5 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, 5, "referent")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1.5[i] <- matched.odds.ratio(cps.match, "referent")
  odds.ratio.2.5[i] <- clogit.referent(cps.match) 
}

hist(odds.ratio.1.5)
mean(odds.ratio.1.5) 
hist(odds.ratio.2.5)
mean(odds.ratio.2.5)

##Creating a table with unadjusted odds ratio and clogit odds ratio ##
tab.1 <- c(mean(odds.ratio.1.1), mean(odds.ratio.1.2), mean(odds.ratio.1.3), mean(odds.ratio.1.4), mean(odds.ratio.1.5),
           mean(odds.ratio.2.1), mean(odds.ratio.2.2), mean(odds.ratio.2.3), mean(odds.ratio.2.4), mean(odds.ratio.2.5))
           
mat.1 <- t(matrix(tab.1, nrow=5))
mat.1
rownames(mat.1) <- c("Unadjusted OR","c.logit OR")
colnames(mat.1) <- c("m=1","m=2","m=3","m=4","m=5")
xtable(mat.1, caption="mean unadjusted and clogit odds ratio")
#-------------------------------------------------------------------------------------------------------
##CONTROLS ("Data Set 3")##
# M = 1

odds.ratio.1.1 <- rep(NA, 100) #unadjusted odds ratio (u/v)
odds.ratio.2.1 <- rep(NA, 100) #conditional logistic regression odds ratio 

for (i in 1:100){
  matched.pairs <- match.function(controls, 1, "referent")
  controls.match <- matched.pairs.data(controls, matched.pairs)
  odds.ratio.1.1[i] <- matched.odds.ratio(controls.match, "referent")
  odds.ratio.2.1[i] <- clogit.referent(controls.match) 
}

# Both methods of calculating the odds ratio are equivalent for m=1
hist(odds.ratio.1.1)
mean(odds.ratio.1.1) 
hist(odds.ratio.2.1)
mean(odds.ratio.2.1)

# M = 2

odds.ratio.1.2 <- rep(NA, 100)
odds.ratio.2.2 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(controls, 2, "referent")
  controls.match <- matched.pairs.data(controls, matched.pairs)
  odds.ratio.1.2[i] <- matched.odds.ratio(controls.match, "referent")
  odds.ratio.2.2[i] <- clogit.referent(controls.match) 
}

hist(odds.ratio.1.2)
mean(odds.ratio.1.2)
hist(odds.ratio.2.2)
mean(odds.ratio.2.2)

# M = 3

odds.ratio.1.3 <- rep(NA, 100)
odds.ratio.2.3 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(controls, 3, "referent")
  controls.match <- matched.pairs.data(controls, matched.pairs)
  odds.ratio.1.3[i] <- matched.odds.ratio(controls.match, "referent")
  odds.ratio.2.3[i] <- clogit.referent(controls.match) 
}

hist(odds.ratio.1.3)
mean(odds.ratio.1.3)
hist(odds.ratio.2.3)
mean(odds.ratio.2.3)

# M = 4

odds.ratio.1.4 <- rep(NA, 100)
odds.ratio.2.4 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(controls, 4, "referent")
  controls.match <- matched.pairs.data(controls, matched.pairs)
  odds.ratio.1.4[i] <- matched.odds.ratio(controls.match, "referent")
  odds.ratio.2.4[i] <- clogit.referent(controls.match) 
}

hist(odds.ratio.1.4)
mean(odds.ratio.1.4)
hist(odds.ratio.2.4)
mean(odds.ratio.2.4)

# M = 5

odds.ratio.1.5 <- rep(NA, 100)
odds.ratio.2.5 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(controls, 5, "referent")
  controls.match <- matched.pairs.data(controls, matched.pairs)
  odds.ratio.1.5[i] <- matched.odds.ratio(controls.match, "referent")
  odds.ratio.2.5[i] <- clogit.referent(controls.match) 
}

hist(odds.ratio.1.5)
mean(odds.ratio.1.5)
hist(odds.ratio.2.5)
mean(odds.ratio.2.5)
#-------------------------------------------------------------------------------------------------------

## Prospective Scenario ##

odds.ratio.1 <- rep(NA, 100)
odds.ratio.2 <- rep(NA, 100)

for (i in 1:100){
  matched.pairs <- match.function(cps, type="observational")
  cps.match <- matched.pairs.data(cps, matched.pairs)
  odds.ratio.1[i] <- matched.odds.ratio(cps.match, "observational")
  odds.ratio.2[i] <- binned.odds.ratio.obs(cps.match, 5) 
}

hist(odds.ratio.1)
mean(odds.ratio.1) # 1.51458 #1.523886 
hist(odds.ratio.2)
mean(odds.ratio.2) # 1.590737
