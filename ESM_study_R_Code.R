## Step by step methods 
## before the beginning, as a side note, this makes r look a lot cooler ;) 
library(devtools)
rstudioapi::addTheme("https://raw.githubusercontent.com/dracula/rstudio/master/dracula.rstheme", apply = TRUE)

## start by initializing data 

basic <- read_excel("Dataset_mini_Valerie.xlsx")
Baseline_data_participantsrlc <- read_excel("Baseline data participantsrlc.xlsx")

## I had a lot of trouble with the csv files from mpath. They might have changed the formatting, i could not get it to work
## so i contacted valerie who apparently had a similar issue for her dataset

## i usually use the import function under "files", because it shows different
## options for dealign with the spaces in csv files (some use empty spaces,
## some use ; etc.)

View(basic)

## now to do some data cleaning: 

library(tidyverse) ## this package is the best for dealing with data in R

basic <- filter(basic, questionListName != "Morning questionnaire_RLC_2706")
basic <- filter(basic, questionListName != "Ochtend vragenlijst")
basic <- filter(basic, questionListName != "OchtendVragenlijst")


## now the (for me) unneccessary questionnaires are removed 

basic <- filter(basic, alias != "onyx") ## response rate too low <- I knew this

##before from looking at m-path, of course otherwise i would have calculated it 
basic <- filter(basic, alias != "Janne") ## experiment did not work

##removing unrealistic accuracies:
hist(basic$DSST_acc_computation)
which(basic$DSST_acc_computation > 100)
basic <- slice(basic, -154, -1314, -1315, -1577, -1578,-1579)




##calculating some relevant information:

basic$timeStampSent <- as.character.POSIXt(basic$timeStampSent)

## sorting data based on date, so beeps can be added (otherwise its not sorted properly and beeps will be assigned to random dates)

basic <- basic %>% arrange(alias, timeStampSent)



##before adding beeps, its important to remove the practice trials 
## how many rows per participant before?
participant_count <- basic %>% group_by(alias) %>% summarize(row_count = n())
print(n = 41, participant_count)

#two participants have values over 60, we talked about those and these were the 
##ones who got the wrong questions at first, and some duplicates

basic <- filter(basic, questionListName != "Momentary_RLC_0407.2")

basic <- basic %>% distinct(timeStampSent, .keep_all = TRUE)

##checking again: 
participant_count <- basic %>% group_by(alias) %>% summarize(row_count = n())
print(n = 41, participant_count)
##now removing the first datapoint per person(which is the practice one )

basic <- basic %>% group_by(alias) %>% slice(-1)

## now it should be okay, let's double check: 

participant_count <- basic %>% group_by(alias) %>% summarize(row_count = n())
print(n = 41, participant_count)

##some still have 50, lets double check for those:

which(participant_count$row_count > 49) # what happened in these cases?

basic <- basic %>% group_by(alias == "Marly 009") 
basic <- basic %>% group_by(alias == "nel") %>% slice(-1)
basic <- basic %>% group_by(alias == "rlc002") %>% slice(-3)

##in these rows additional practice questions were includes, now removed
## final check:
participant_count <- basic %>% group_by(alias) %>% summarize(row_count = n())
print(n = 41, participant_count)

##seems ok!

##adding the beeps:
##overall

basic <- basic %>%
  group_by(alias) %>%
  mutate(beepsent = row_number()) %>%
  ungroup()
##per day 
## adding a day variable 

basic$day <- as.Date(basic$timeStampSent)

##beep per day 
basic <- basic %>%
  group_by(alias, day) %>%
  mutate(beep_day = row_number()) %>%
  ungroup()

## now on to  calculate the DSST scores

basic$DSST_CALC <- (basic$DSST_acc_computation*basic$DSST_Ntotal_computation)/100
summary(basic$DSST_CALC) 

### low values(?):
hist(basic$DSST_CALC) ## pretty much a normal distribution, but low values more unrealistic than high ones
## theres a rather steep dropoff after 4, so thats what i would consider an unusually low score
low_vals <- which(basic$DSST_CALC <= 3)

basic_temp <- basic[low_vals, ]

aliases <- basic_temp$alias
print(low_vals)
print(aliases)

## [1] "RLC004"  "RLC013"  "RLC013"  "Jos"     "Peter"  
## [6] "Roel"    "Roel"    "Jolanda" "Martin" 


rlcsub004 <- subset(basic, alias == "RLC004")
summary(rlcsub004$DSST_CALC)
rlcsub013 <- subset(basic, alias == "RLC013")
summary(rlcsub013$DSST_CALC)
print(rlcsub013$DSST_CALC)
roelsub <- subset(basic, alias == "Roel")
summary(roelsub$DSST_CALC)
print(roelsub$DSST_CALC)
jolsub <- subset(basic, alias == "Jolanda")
summary(jolsub$DSST_CALC)
martsub <- subset(basic, alias == "Martin")
summary(martsub$DSST_CALC)

## for participants rlc013 and joel the 1 values are unrealistic, while the 3 values seem to be roughly in line with their performance
## for the other participants the values were not realistic
print(low_vals)
new_low_vals <- c(489, 544, 882, 962,991, 1087, 1172)
basic <- slice(basic, -new_low_vals)









## to do the correlational analysis, the dataframes need to be combined, which poses a bit of problem since the
## ESM dataframe of course has a lot more measurements and is sorted in a different way 

## first its important to make sure that the measurements match, that is that RLC
## of the Baseline dataset matches with the aliases of the basic dataset

basic <- basic %>% arrange(connectionId)## connectionId luckily has the same order 
## as the RLCs :)

##then we need to add the relevant scores of the RLC dataframe

##clce needs to be reverse coded

Baseline_data_participantsrlc <- Baseline_data_participantsrlc %>%
  mutate(across(c(23:44), ~ replace(., . == 2, 0)))

columns_to_sum_clce <- 23:35 ## these are the relevant CLCE columns 

Baseline_data_participantsrlc <- Baseline_data_participantsrlc %>%
  mutate(CLCE_cog = rowSums(select(., columns_to_sum_clce), na.rm = TRUE))

Baseline_data_participantsrlc$BDSST_ALL <- Baseline_data_participantsrlc$DSST_TOTAL - 
  Baseline_data_participantsrlc$DSST_mistake

## now creating a subset corelation matrix

cor_matrix <- subset(Baseline_data_participantsrlc, select = c("BDSST_ALL", "CLCE_cog", "Moca_total"))

## now summing up scores of the ESM dataset:

cor_matrix$MDSST <- basic %>% group_by(connectionId) %>%  summarize(MDSST = mean(DSST_CALC, na.rm = TRUE)) %>%
  ungroup() %>%
  select(MDSST) ## this is the average mDSST performance 

cor_matrix$CV <- basic %>% group_by(connectionId)%>% 
summarize(CV = (sd(DSST_CALC, na.rm = TRUE)/ (mean(DSST_CALC,na.rm = TRUE))))%>% 
  ungroup() %>% select(CV) ##this calculates the coefficient of variation (as i had to google again right now haha)

## now to the MSSD, which is a bit more tricky 

basic <- basic %>%  group_by(connectionId, day) %>% mutate(DSST_lag = lag(DSST_CALC)) #creates a timelagged variable of the DSST variable per participant per day

basic$distance <- basic$DSST_lag - basic$DSST_CALC ##distance between the points  

basic$distancesqr <- (basic$distance)**2 ## has to be squared 

cor_matrix$MSSD <- basic %>% group_by(connectionId) %>% summarize(MSSD = mean(distancesqr, na.rm = TRUE)) %>% 
  ungroup() %>% select(MSSD)
View(cor_matrix)


## now the added variables using dplyr are lists instead of vectors, I don't know how to prevent this,
## but I know how to fix it

cor_matrix$MDSST <- cor_matrix$MDSST$MDSST
cor_matrix$CV <- cor_matrix$CV$CV
cor_matrix$MSSD <- cor_matrix$MSSD$MSSD

## now since all the neccessary data is in the df, we can conduct the correlational analysis
## I wrote a small script to quickly check the assumptions of a correlational analysis with hypothesis testing:
library(mvnormalTest)

cor_testing <- function(old_df, column1, column2) {
  
  new_df <-  subset(old_df, select= c(column1, column2))
  column1 = as.numeric(new_df[[column1]])
  column2 = as.numeric(new_df[[column2]])
  normal1 <- shapiro.test(column1)
  normal2 <- shapiro.test(column2)
  normal3 <- mardia(new_df, std = TRUE)
  hist(column1)
  hist(column2)
  plot(new_df)
  abline(lm(column1 ~ column2, data=new_df))
  plot(lm(column1 ~ column2, data=new_df))
  cor <- cor.test(column1, column2)
  print(normal1)
  print(normal2)  
  print(normal3)
  print(cor)
}

## the logic is to put in the dataframe and two variables in it, for which it then automatically calculates the data
## now lets do it for all variables
cor_testing(cor_matrix, "BDSST_ALL", "CLCE_cog")
cor_testing(cor_matrix, "BDSST_ALL", "Moca_total")
cor_testing(cor_matrix, "BDSST_ALL", "MDSST")
cor_testing(cor_matrix, "BDSST_ALL", "CV")
cor_testing(cor_matrix, "BDSST_ALL", "MSSD")
cor_testing(cor_matrix, "CLCE_cog", "Moca_total")
cor_testing(cor_matrix, "CLCE_cog", "MDSST")
cor_testing(cor_matrix, "CLCE_cog", "CV")
cor_testing(cor_matrix, "CLCE_cog", "MSSD")
cor_testing(cor_matrix, "Moca_total", "MDSST")
cor_testing(cor_matrix, "Moca_total", "CV")
cor_testing(cor_matrix, "Moca_total", "MSSD")
cor_testing(cor_matrix, "MDSST", "CV")
cor_testing(cor_matrix, "MDSST", "MSSD")
cor_testing(cor_matrix, "CV", "MSSD")


## looking at this, it is obvious that the MSSD variable needs to be transformed, since bivariate
## normality cannot be met a lot of the times (which also makes sense since MSSD is not normal which might well be due to the fact this it is squared)

cor_matrix$MSSD <- sqrt(cor_matrix$MSSD)

cor_testing(cor_matrix, "BDSST_ALL", "MSSD")
cor_testing(cor_matrix, "CLCE_cog", "MSSD")
cor_testing(cor_matrix, "Moca_total", "MSSD")
cor_testing(cor_matrix, "MDSST", "MSSD")
cor_testing(cor_matrix, "CV", "MSSD")



##for a correlation matrix one can use apaTables:
library(apaTables)
cor_table_apa <- apa.cor.table(cor_matrix, "cor_table_script_4.doxc")
print(cor_table_apa)



##moving on to the multilevel model: 

## before the actual model the affect items need to be grouped together:


##PCA:
##positive
pcapos <- subset(basic, select = c("Zelfverzekerd_sliderNegPos", "Tevreden_sliderNegPos", "Relax_sliderNegPos", "Energetic_sliderNegPos"))
View(pcapos)
## following packages neccessary:
library(psych) 
library(nFactors)
library(reshape2)
library(ggplot2)
library(dplyr)
library(GPArotation)
library(corrplot)
library(Hmisc)
library(factoextra)
library(ggplot2)

#removing missing:
pcapos_nomiss <- na.omit(pcapos) ##PCA does not work well with missings
View(pcapos_nomiss)
## PCA:
pcaposres <- princomp(pcapos_nomiss)
eigen <- get_eigenvalue(pcaposres)
fviz_eig(pcaposres, addlabels = TRUE)

pcaposres$loadings[,1:4]



##eigenvalue variance.percent
##Dim.1  5.4055756        71.309113
##Dim.2  0.9527138        12.567981
##Dim.3  0.6896438         9.097622
##Dim.4  0.5325505         7.025283

##both scree and eigenvalues suggest 1 factor


##negative:
pca_neg <- subset(basic, select = c("Geïrriteerd _sliderNegPos","Angstig_sliderNegPos", "Verdrietig_sliderNegPos", "Eenzaam_sliderNegPos"))
pca_neg_nm <- na.omit(pca_neg)
View(pca_neg_nm)
## pca
pcanegres <- princomp(pca_neg_nm)
neg_eig <- get_eig(pcanegres)
fviz_eig(pcanegres, addlabels = TRUE)
screeplot(pcanegres, npcs = 4, type = "lines")
3
##loadings:

pcanegres$loadings[,1:4]


##eigenvalues 
##eigenvalue variance.percent
##Dim.1  3.8016935         57.55761
##Dim.2  1.3767095         20.84337
##Dim.3  0.8272611         12.52473
##Dim.4  0.5993589          9.07429


## looking at the loadings and scree plot, it seems okay to group them into one variable, even though eigenvalues suggest two factors

## grouping variables together
basic$irritated <- basic$`Geïrriteerd _sliderNegPos` ## r doesnt deal well with special characters so its better to rename columns with them
basic$fatigue <- basic$Moe_sliderNegPos ## this is just cause its easier to type :D

basic$negaff <- basic$irritated + basic$Angstig_sliderNegPos+ basic$Verdrietig_sliderNegPos+ basic$Eenzaam_sliderNegPos
basic$posaff <- basic$Zelfverzekerd_sliderNegPos + basic$Tevreden_sliderNegPos + basic$Relax_sliderNegPos+ basic$Energetic_sliderNegPos

## checking assumptions 
library(nlme)
# fit random intercepts and slopes model
res <- lme(DSST_CALC ~ posaff*negaff + fatigue, random = ~ posaff | alias, data=basic, na.action=na.omit)
summary(res)

# plot fitted values versus standardized residuals
fitval <- fitted(res)
stdres <- resid(res, type="pearson")
plot(fitval, stdres, pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

# plot fitted values versus standardized residuals with jitter
plot(jitter(fitval, amount=0.2), jitter(stdres, amount=0.3), pch=19, cex=0.5)
abline(h=0)
abline(h=c(-1.96,1.96), lty="dotted")

# what % of the standardized residuals is > than 1.96 in absolute value?
round(100 * mean(abs(stdres) > 1.96), digits=2) ## 4.88, which is pretty close to 5 which is how it should be 

# histogram of the standardized residuals
hist(stdres, main="", xlab="Standardized Residual")

# histogram of the standardized residuals (with normal distribution superimposed)
hist(stdres, breaks=50, main="", xlab="Standardized Residual", freq=FALSE)
curve(dnorm(x, mean=mean(stdres), sd=sd(stdres)), add=TRUE, lwd=2)

## a bit left skewed, but overall seems pretty normal 

# normal probability plot of the standardized residuals
qqnorm(stdres, pch=19, cex=0.5)
qqline(stdres)
## remove unneccessary variables 
rm(fitval, stdres)

# histograms and normal probability plots of the random effects for intercepts and slopes
par(mfrow=c(2,2))
hist(ranef(res)[,1], main="Histogram", xlab="Random Effects for Intercepts")
hist(ranef(res)[,2], main="Histogram", xlab="Random Effects for Slopes")
qqnorm(ranef(res)[,1], pch=19, cex=0.5)
qqline(ranef(res)[,1])
qqnorm(ranef(res)[,2], pch=19, cex=0.5)
qqline(ranef(res)[,2])

# plot random effects for intercepts and slopes against each other
par(mfrow=c(1,1))
plot(ranef(res)[,1], ranef(res)[,2], xlab="Random Effects for Intercepts",
     ylab="Random Effects for Slopes", pch=19)

# check ACF based on an AR(1) model
res <- lme(DSST_CALC ~ posaff*negaff + fatigue, random = ~ posaff | alias,
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit)
summary(res)
plot(ACF(res))

## cooks distances:
library(lme4)
res <- lmer(DSST_CALC ~ posaff * negaff + fatigue + (posaff | alias),
            data = basic,
            na.action = na.omit,
            control = lmerControl(optCtrl = list(maxfun = 10000)))
sav <- influence(res)
cds <- cooks.distance(sav)
plot(cds, type="o", pch=19)

##threshold for influential cases: 

thresh <- 4/1993 ## cooks distacne rule of thumb 4/n 

print(which(cds >= thresh)) ## for some reason duplicate values

cds <- unique(cds)

cdsvals <- which(cds >= thresh)

## now checking standardized residuals 
stdres <- resid(res, type="pearson")

## usually standardized residuals are considered outliers if over 3

resvals <- which(stdres > 3)

common <- intersect(resvals, cdsvals)
print(common)

## common value is row 1102, so this one can be removed

basic <- slice(basic, -1102)

##after looking at the ESM handbook provided by KU leuven, apparently it is best to include the time effects as a variable in the model to account for learning:

res <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit)
summary(res)

## different fitting algorithms:

res <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit, control=list(msMaxIter=1000))
summary(res)
res <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit, control=list(opt="optim"))
summary(res) ## this one does not work

# LRT for testing the variance component of the random slopes (can also compare AICs/BICs)

res0 <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ 1 | alias, 
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit, control=list(msMaxIter=1000))
res1 <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
           correlation=corAR1(form = ~ beepsent | alias),
           data=basic, na.action=na.omit, control=list(msMaxIter=1000))
anova(res0, res1)

#adding random slopes is good according to the anova for the model fit, 


## now to check interaction 
res0 <- lme(DSST_CALC ~ posaff*negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
            correlation=corAR1(form = ~ beepsent | alias),
            data=basic, na.action=na.omit, method="ML", control=list(msMaxIter=1000))
res1 <- lme(DSST_CALC ~ posaff+negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
            correlation=corAR1(form = ~ beepsent | alias),
            data=basic, na.action=na.omit, method="ML", control=list(msMaxIter=1000))
anova(res0,res1)

## res 1 without interaction has a slight but non-significant "advantage" over res 0 with interaction 

summary(res1)

# this is the best model i could find after trying around a bit, might be worth to experiment a bit more, however pretty much all models gaves me very similar conclusions
##
res2 <- lme(DSST_CALC ~ posaff+negaff + fatigue + beepsent ,random = ~ posaff+ beepsent | alias, 
            correlation=corAR1(form = ~ beepsqr | alias),
            data=basic, na.action=na.omit, method="ML", control=list(msMaxIter=1000))
summary(res2)


##The above model is without centering the data, however it does make sense to center the data within and between people in order to assess the respective effects 

##grand mean centering:

basic$posgmc <- basic$posaff - mean(basic$posaff, na.rm = TRUE)
basic$neggmc <- basic$negaff - mean(basic$negaff, na.rm = TRUE)
basic$fatgmc <- basic$fatigue - mean(basic$fatigue, na.rm = TRUE)

## person mean centering: 

basic <- basic %>% group_by(alias) %>%
  mutate(personposaff = mean(posaff, na.rm  = TRUE),
         pos_cwc = posaff-personposaff) %>%
  ungroup() %>%
  # Grand mean centering of the aggregated variable
  mutate(pos_cmc = personposaff-mean(personposaff, na.rm = TRUE)) 
  


basic <- basic %>% group_by(alias) %>%
  mutate(personnegaff = mean(negaff, na.rm  = TRUE),
         neg_cwc = negaff-personnegaff) %>%
  ungroup() %>%
  # Grand mean centering of the aggregated variable
  mutate(neg_cmc = personnegaff-mean(personnegaff, na.rm = TRUE)) 
  

basic <- basic %>% group_by(alias) %>%
  mutate(personfatigue = mean(fatigue, na.rm  = TRUE),
         fat_cwc = fatigue-personfatigue) %>%
  ungroup() %>%
  # Grand mean centering of the aggregated variable
  mutate(fat_cmc = personfatigue-mean(personfatigue, na.rm = TRUE))

##adding beep log

basic$beeplog <- log(basic$beepsent)

##lets see if improved model fit:

##age and gender as covariates
RLC_ID_MATCH <- subset(Baseline_data_participantsrlc, select = c("Age", "Gender"))
RLC_ID_MATCH$connectionId <- basic %>% group_by(connectionId) %>% summarize(connectionId = mean(connectionId))
RLC_ID_MATCH$connectionId <- RLC_ID_MATCH$connectionId$connectionId

basic <- right_join(RLC_ID_MATCH, basic, by= "connectionId")

res2 <- lme(DSST_CALC ~ pos_cwc+neg_cwc+ pos_cmc* neg_cmc + fat_cmc + fat_cwc + beeplog + Age , random = ~  neg_cwc | alias,
            data = basic,corr = corGaus (form = ~ beepsent |alias, nugget = TRUE),  na.action=na.omit, method="ML", control=list(msMaxIter=10000000)) 
summary(res2)
## in a previous version of the code i used a function to calculate CIs, however this is not accurate as it turns out, so now i wrote a function myself and used the output of the summary of the model

calc_stand <- function(value, stderr){
  std
  lower_bound <- value-1.96*stderr
  upper_bound <- value+1.96*stderr
  CI <- c(lower_bound, upper_bound)
  CI
}
calc_stand(0.034511,0.0185699)
calc_stand(-0.022421,0.0289852)
calc_stand(-0.010036,0.1362278)
calc_stand(-0.428860,0.1738391)
calc_stand(-0.092458,0.0443476)
calc_stand(-0.102570, 0.0363357)
calc_stand( -0.262739, 0.2652835)
calc_stand(0.976180, 0.0868751)
calc_stand(-0.165098, 0.0339573)

## calculating ICC 

# estimating the null model using lme4
m0 <- lmer(DSST_CALC ~ 1 + (1|alias), basic)

# Function to compute the intraclass correlation coefficient (ICC)
compute_icc <- function(lmer_object){
  var_dat <- lmer_object %>% VarCorr %>% as.data.frame
  icc <- var_dat$vcov[1]/(var_dat$vcov[1]+var_dat$vcov[2])
  return(icc)
}
compute_icc(m0) %>%
  round(2)
##0.67 


##: calculating some descriptives for the baseline data

View(Baseline_data_participantsrlc)

for (x in 1:length(Baseline_data_participantsrlc$DSST_TOTAL)) {
  age = Baseline_data_participantsrlc$Age[x]
  if (age %in% 20:24) {
    avg = 
    sd = 
  }
  
  else if (age %in% 25:30)
  else if (age %in% 31:35)
  else if (age %in% 36:45)
  else if (age %in% 45:55)
  else if (age %in% 55:65)
  else if (age %in% 66:75)
  else if (age %in% 76:85)
  
}
## this I still need to work on


## Moca:
  ## The cutoff for Moca is 23 (I explain this in the paper), so any Value below 23 can be considered outside the norm, which is easy to check

below_cutoff <- which(Baseline_data_participantsrlc$Moca_total < 23)
print(below_cutoff)
impaired_perc <- (length(below_cutoff)/41)
print(impaired_perc)
print(mean(Baseline_data_participantsrlc$Moca_total))
print(sd(Baseline_data_participantsrlc$Moca_total))






