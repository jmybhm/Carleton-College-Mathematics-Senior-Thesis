source("K:/math400-02-f14/Common/R Functions/case_referent_functions.R")
source("K:/math400-02-f14/Common/R Functions/observational_functions.R")
cps<- read.dta("K:/math400-02-f14/CourseMaterials/Data/cps1re74.dta")
nsw<- read.dta("K:/math400-02-f14/CourseMaterials/Data/nswre74.dta")
psid <- read.csv("K:/math400-02-f14/Common/Data/new_psid.csv")
controls <- merge.controls(nsw,cps)
controls.psid <- merge.controls(nsw,psid)

#clean up the data  
nsw <- clean.data(nsw)
nsw <- calculate.prop.score(nsw)

cps <- clean.data(cps)
cps.no.tail <- calculate.prop.score(cps)
cps.tail <- calculate.prop.score(cps, tail=T)

controls <- clean.data(controls)
controls <- calculate.prop.score(controls)

psid <- clean.data(psid)
psid.no.tail <- calculate.prop.score(psid, tail = F)
psid.tail <- calculate.prop.score(psid, tail=T)

#controls without psid tail
controls.psid <- clean.data(controls.psid)
controls.psid <- calculate.prop.score(controls.psid)

#creating matched data set
nsw.matched.data <- match.function(nsw, m=1, "observational")
nsw.matched.data <- matched.pairs.data(nsw, nsw.matched.data)

cps.matched.data <- match.function(cps.no.tail, m=1, "observational")
cps.matched.data <- matched.pairs.data(cps.no.tail, cps.matched.data)

cps.matched.data.2 <- match.function(cps.tail, m=1, "observational")
cps.matched.data.2 <- matched.pairs.data(cps.tail, cps.matched.data.2)

controls.matched.data <-match.function(controls, m=1, "observational")
controls.matched.data <- matched.pairs.data(controls, controls.matched.data)

controls.matched.data2 <-match.function(controls2, m=1, "observational")
controls.matched.data2 <- matched.pairs.data(controls2, controls.matched.data2)

psid.matched.data <- match.function(psid.no.tail, m=1, "observational")
psid.matched.data <- matched.pairs.data(psid.no.tail, psid.matched.data)

psid.matched.data2 <- match.function(psid.tail, m=1, "observational")
psid.matched.data2 <- matched.pairs.data(psid.tail, psid.matched.data2)

controls.psid.matched.data <-match.function(controls.psid, m=1, "observational")
controls.psid.matched.data <- matched.pairs.data(controls.psid, controls.psid.matched.data)


########################PROSPECTIVE STUDY############################

#random experiment
estimate(nsw,"earnings","unadjusted")#1791~
estimate(nsw,"earnings","adjusted")#1639~

#prospective
#Unadjusted Estimates
estimate(cps.no.tail, "earnings", "unadjusted") #-1881.344
estimate(cps.tail, "earnings", "unadjusted") #-8497.516

estimate(psid.tail, "earnings","unadjusted") #-15204.78
estimate(controls, "earnings", "unadjusted") #-10291.86
estimate(controls.psid, "earnings", "unadjusted") #-7592.987

#Adjusted Estimates
estimate(cps.no.tail, "earnings","adjusted")#1192.758 
estimate(cps.tail, "earnings","adjusted") #738.8341 

estimate(psid.tail, "earnings","adjusted") #217.9423

estimate(controls, "earnings", "adjusted") #-1018.748 
estimate(controls.psid, "earnings", "adjusted") #-1148.888 

#Matched Unadjusted Estimates
estimate(cps.matched.data, "earnings", "unadjusted")  
estimate(cps.matched.data.2, "earnings", "unadjusted") 
estimate(psid.matched.data, "earnings","unadjusted")
estimate(psid.matched.data2, "earnings","unadjusted")
estimate(controls.matched.data, "earnings", "unadjusted") 
estimate(controls.matched.data2, "earnings","unadjusted")
estimate(controls.psid.matched.data, "earnings", "unadjusted") 



lm(re78 ~ treat + age +age2 + ed + I(ed^2) + married +nodeg + black + hisp + re74 + re75 + I(re74^2) + I(re75^2) + u74*black, data=psid)


######################NSWt v. PSID##################################
tab <- c( 1791, 1639, NA, 
          -8498, 739, 1608, 
          -15205, 218, 1378, 
         -10292, -1019, -562, 
          -7593, -1149, 76)

mat <- t(matrix(tab, nrow = 3))
rownames(mat) <- c("NSWt v. NSWc","NSWt v. CPS",
                   "NSWt v. PSID", "NSWc v. CPS",
                   "NSWc v. PSID")
colnames(mat) <- c("Unadjusted", "Adjusted", "Matched Unadjusted")
xtable(mat, caption="Wages Estimates")