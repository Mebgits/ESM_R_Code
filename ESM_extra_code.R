library(nlme)

## creating dichtomous variable home as 0


library(dplyr)


basic_temp <- basic %>%
  mutate(Location_multipleChoice_index = case_when(
    is.na(Location_multipleChoice_index) ~ Location_multipleChoice_index, # Keep NA as it is
    Location_multipleChoice_index == 1 ~ 0,
    TRUE ~ 1
  ))

basic_temp$Location_multipleChoice_index_temp <- basic$Location_multipleChoice_index

basic$Location_binary <- basic_temp$Location_multipleChoice_index_temp

#now social company


basic$social_company_string <- basic$`Social company_multipleChoice_string`
basic$social_company_string <- as.character(basic$social_company_string)

basic <- basic %>%
  mutate(social_company_numeric = case_when(
    is.na(social_company_string) ~ NA_real_,  
    social_company_string == "Niemand" ~ 0,
    TRUE ~ 1
  ))

model <- lme(DSST_CALC ~ social_company_numeric+Location_binary+ Age + beeplog,
          random = ~1 | alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
        na.action = na.omit)
summary(model)

## assumptions:
fitval <- fitted(model)
stdres <- resid(model, type="pearson")
plot(fitval, stdres, pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")
  
  
## jitter

plot(jitter(fitval, amount=0.2), jitter(stdres, amount=0.3), pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

round(100 * mean(abs(stdres) > 1.96), digits=2) ##5.23 seems ok 

##residuals: 
hist(stdres, main="", xlab="Standardized Residual") ## right skewed, but otherwise normal 

hist(stdres, breaks=50, main="", xlab="Standardized Residual", freq=FALSE)
curve(dnorm(x, mean=mean(stdres), sd=sd(stdres)), add=TRUE, lwd=2)

qqnorm(stdres, pch=19, cex=0.5)
qqline(stdres) ## it being skewed shows up here as well


#histograms and normal probability plots of the random effects for intercepts and slopes
par(mfrow=c(1,1))
hist(ranef(res)[,1], main="Histogram", xlab="Random Effects for Intercepts")
hist(ranef(res)[,2], main="Histogram", xlab="Random Effects for Slopes")
qqnorm(ranef(res)[,1], pch=19, cex=0.5)
qqline(ranef(res)[,1])
qqnorm(ranef(res)[,2], pch=19, cex=0.5)
qqline(ranef(res)[,2])

## might be a bit problematic, especially one value in the second figure(s), but I will leave that to you

# plot random effects for intercepts and slopes against each other
par(mfrow=c(1,1))
plot(ranef(res)[,1], ranef(res)[,2], xlab="Random Effects for Intercepts",
     ylab="Random Effects for Slopes", pch=19)



library(lme4)

model <- lmer(DSST_CALC ~ social_company_numeric + Location_binary + Age + beeplog + (1 | alias), 
              data = basic, na.action = na.omit)

inf <- influence(model)
cds <- cooks.distance(inf)
plot(cds, type="o", pch=19)
abline(h=0.07, lty="dotted")
thresh <- 4/1993
cdsthresh <- which(cds > thresh)

## adding the stdres 

stdres<- resid(res, type="pearson")

## usually standardized residuals are considered outliers if over 3

resvals <- which(stdres > 3)

common <- intersect(resvals, cdsthresh)

print(common)

## no outliers apparently

##final model: 

model <- lme(DSST_CALC ~ social_company_numeric+Location_binary+ Age + beeplog,
             random = ~1 | alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
              method="ML", na.action = na.omit)
summary(model)
## ML usually gives the best results, I think it is just a bit more computationally intensive 



## now running the same again but with fatigue 

basic <- basic %>%
  group_by(alias) %>%
  mutate(fat_pc = fatigue - mean(fatigue, na.rm = TRUE)) %>%
  ungroup()

model <- lme(DSST_CALC ~ fat_pc+ social_company_numeric+Location_binary+ Age + beeplog,
             random = ~fat_pc | alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit)
summary(model)

## assumptions:
fitval <- fitted(model)
stdres <- resid(model, type="pearson")
plot(fitval, stdres, pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")


## jitter

plot(jitter(fitval, amount=0.2), jitter(stdres, amount=0.3), pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

round(100 * mean(abs(stdres) > 1.96), digits=2) ##4.9 seems ok 

##residuals: 
hist(stdres, main="", xlab="Standardized Residual") ## right skewed, but otherwise normal 

hist(stdres, breaks=50, main="", xlab="Standardized Residual", freq=FALSE)
curve(dnorm(x, mean=mean(stdres), sd=sd(stdres)), add=TRUE, lwd=2)

qqnorm(stdres, pch=19, cex=0.5)
qqline(stdres) ## it being skewed shows up here as well


#histograms and normal probability plots of the random effects for intercepts and slopes
par(mfrow=c(1,1))
hist(ranef(res)[,1], main="Histogram", xlab="Random Effects for Intercepts")
hist(ranef(res)[,2], main="Histogram", xlab="Random Effects for Slopes")
qqnorm(ranef(res)[,1], pch=19, cex=0.5)
qqline(ranef(res)[,1])
qqnorm(ranef(res)[,2], pch=19, cex=0.5)
qqline(ranef(res)[,2])

## might be a bit problematic, especially one value in the second figure(s), but I will leave that to you

# plot random effects for intercepts and slopes against each other
par(mfrow=c(1,1))
plot(ranef(res)[,1], ranef(res)[,2], xlab="Random Effects for Intercepts",
     ylab="Random Effects for Slopes", pch=19)



library(lme4)

model <- lmer(DSST_CALC ~ fat_pc+ social_company_numeric + Location_binary + Age + beeplog + (fat_pc | alias), 
              data = basic, na.action = na.omit)

inf <- influence(model)
cds <- cooks.distance(inf)
plot(cds, type="o", pch=19)
abline(h=0.07, lty="dotted")
thresh <- 4/1993
cdsthresh <- which(cds > thresh)

## adding the stdres 

stdres<- resid(res, type="pearson")

## usually standardized residuals are considered outliers if over 3

resvals <- which(stdres > 3)

common <- intersect(resvals, cdsthresh)

print(common)

model <- lme(DSST_CALC ~ fat_pc+ social_company_numeric+Location_binary+ Age + beeplog,
             random = ~fat_pc | alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit, method = "ML")
summary(model)


## all the values from checking the assumptions are the same so I am assumming adding more factors doesn't change anything in that regard?
##actually nevermind its better to double check cause they are not actually the same
##model 3:

## 

basic <- basic %>%
  group_by(alias) %>%
  mutate(pos_pc = posaff - mean(posaff, na.rm = TRUE)) %>%
  ungroup()

basic <- basic %>%
  group_by(alias) %>%
  mutate(neg_pc = negaff - mean(negaff, na.rm = TRUE)) %>%
  ungroup()




model <- lme(DSST_CALC ~ fat_pc+ pos_pc+ neg_pc+social_company_numeric+Location_binary+ Age + beeplog,
             random = ~neg_pc +fat_pc| alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit, method = "ML",control=list(msMaxIter=1000))
summary(model)
fitval <- fitted(model)
stdres <- resid(model, type="pearson")
plot(fitval, stdres, pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")


## jitter

plot(jitter(fitval, amount=0.2), jitter(stdres, amount=0.3), pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

round(100 * mean(abs(stdres) > 1.96), digits=2) ##4.57 seems ok 

##residuals: 
hist(stdres, main="", xlab="Standardized Residual") ## right skewed, but otherwise normal 

hist(stdres, breaks=50, main="", xlab="Standardized Residual", freq=FALSE)
curve(dnorm(x, mean=mean(stdres), sd=sd(stdres)), add=TRUE, lwd=2)

qqnorm(stdres, pch=19, cex=0.5)
qqline(stdres) ## it being skewed shows up here as well


#histograms and normal probability plots of the random effects for intercepts and slopes
par(mfrow=c(1,1))
hist(ranef(res)[,1], main="Histogram", xlab="Random Effects for Intercepts")
hist(ranef(res)[,2], main="Histogram", xlab="Random Effects for Slopes")
qqnorm(ranef(res)[,1], pch=19, cex=0.5)
qqline(ranef(res)[,1])
qqnorm(ranef(res)[,2], pch=19, cex=0.5)
qqline(ranef(res)[,2])

## might be a bit problematic, especially one value in the second figure(s), but I will leave that to you

# plot random effects for intercepts and slopes against each other
par(mfrow=c(1,1))
plot(ranef(res)[,1], ranef(res)[,2], xlab="Random Effects for Intercepts",
     ylab="Random Effects for Slopes", pch=19)



library(lme4)

model <- lmer(DSST_CALC ~ pos_pc+neg_pc+fat_pc+ social_company_numeric + Location_binary + Age + beeplog + (fat_pc | alias), 
              data = basic, na.action = na.omit)

inf <- influence(model)
cds <- cooks.distance(inf)
plot(cds, type="o", pch=19)
abline(h=0.07, lty="dotted")
thresh <- 4/1993
cdsthresh <- which(cds > thresh)

## adding the stdres 

stdres<- resid(res, type="pearson")

## usually standardized residuals are considered outliers if over 3

resvals <- which(stdres > 3)

common <- intersect(resvals, cdsthresh)
print(common) ##it seems that cooks values and residuals dont change

## this isthe best i could find that converges 

model <- lme(DSST_CALC ~ fat_pc+ pos_pc+ neg_pc+social_company_numeric+Location_binary+ Age + beeplog,
             random = ~neg_pc +fat_pc| alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit, method = "ML",control=list(msMaxIter=1000))
summary(model)


##model 4:


basic <- basic %>%
  group_by(alias) %>%
  mutate(conc_pc = Concentratie_sliderNegPos - mean(Concentratie_sliderNegPos, na.rm = TRUE)) %>%
  ungroup()

model <- lme(DSST_CALC ~ conc_pc+fat_pc+ pos_pc+ neg_pc+social_company_numeric+Location_binary+ Age + beeplog,
             random = ~pos_pc +fat_pc| alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit, method = "ML",control=list(msMaxIter=1000))
summary(model)
fitval <- fitted(model)
stdres <- resid(model, type="pearson")
plot(fitval, stdres, pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")


## jitter

plot(jitter(fitval, amount=0.2), jitter(stdres, amount=0.3), pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

round(100 * mean(abs(stdres) > 1.96), digits=2) ##4.97 seems ok 

##residuals: 
hist(stdres, main="", xlab="Standardized Residual") ## right skewed, but otherwise normal 

hist(stdres, breaks=50, main="", xlab="Standardized Residual", freq=FALSE)
curve(dnorm(x, mean=mean(stdres), sd=sd(stdres)), add=TRUE, lwd=2)

qqnorm(stdres, pch=19, cex=0.5)
qqline(stdres) ## it being skewed shows up here as well


#histograms and normal probability plots of the random effects for intercepts and slopes
par(mfrow=c(1,1))
hist(ranef(res)[,1], main="Histogram", xlab="Random Effects for Intercepts")
hist(ranef(res)[,2], main="Histogram", xlab="Random Effects for Slopes")
qqnorm(ranef(res)[,1], pch=19, cex=0.5)
qqline(ranef(res)[,1])
qqnorm(ranef(res)[,2], pch=19, cex=0.5)
qqline(ranef(res)[,2])

## might be a bit problematic, especially one value in the second figure(s), but I will leave that to you

# plot random effects for intercepts and slopes against each other
par(mfrow=c(1,1))
plot(ranef(res)[,1], ranef(res)[,2], xlab="Random Effects for Intercepts",
     ylab="Random Effects for Slopes", pch=19)



library(lme4)

model <- lmer(DSST_CALC ~ conc_pc+fat_pc+ social_company_numeric + Location_binary + Age + beeplog + (fat_pc | alias), 
              data = basic, na.action = na.omit)

inf <- influence(model)
cds <- cooks.distance(inf)
plot(cds, type="o", pch=19)
abline(h=0.07, lty="dotted")
thresh <- 4/1993
cdsthresh <- which(cds > thresh)

## adding the stdres 

stdres<- resid(res, type="pearson")

## usually standardized residuals are considered outliers if over 3

resvals <- which(stdres > 3)

common <- intersect(resvals, cdsthresh)
print(common)
## no outliers 
##again best model i could find 

model <- lme(DSST_CALC ~ conc_pc+fat_pc+ pos_pc+ neg_pc+social_company_numeric+Location_binary+ Age + beeplog,
             random = ~pos_pc +fat_pc| alias,  correlation =corAR1(form = ~ beepsent | alias), data = basic,
             na.action = na.omit, method = "ML",control=list(msMaxIter=1000))



