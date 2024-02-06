setwd("~/Desktop/SS9055")
library(readxl) 
library(dplyr)
library(tidyverse)
library(broom)
library(MASS)

## merge two dataframes 
sheets <- readxl::excel_sheets('EbirdMonthlyUsers2021.xlsx')
tibble <- lapply(sheets, function(x) readxl::read_excel('EbirdMonthlyUsers2021.xlsx', sheet = x))
data_frame <- lapply(tibble, as.data.frame)
names(data_frame) <- sheets
print(data_frame)
names(data_frame$Data)[names(data_frame$Data)=="subnational2_code"] <- "Region"
data_frame$Data
Mergedata = merge(data_frame$Data, data_frame$Codes, by = 'Region')
Mergedata <- Mergedata[Mergedata['collection'] != "EBIRD-CA-SENS",]
index_active = which(Mergedata$"Start Year of Celebration" != 0)
Mergedata$aftercele = 0
for (i in index_active){
if((Mergedata$survey_year)[i] >= (Mergedata$'Start Year of Celebration')[i]){
  Mergedata$aftercele[i] = 1
}else{
  Mergedata$aftercele[i] = 0
}
}

Mergedata %>%
  arrange(desc(Mergedata$Region), Mergedata$survey_year)
Mergedata$day = 01
Mergedata$Date <- with(Mergedata, sprintf("%d%02d%02d", survey_year, survey_month, day))
Mergedata$Date <- as.Date(Mergedata$Date, '%Y%m%d')


#***** Exploratory data plot***********
### the bird activity during May is much higher than months without a celebration.
qplot(data=Mergedata,x=survey_month,y=numObs, xlab = 'survey month', ylab = 'numObs', main = 'bird activity in each month')

### bird activity grow quicker in areas with an active cele. The number of obs is larger.
### the slope is more steep in many areas with active cele.
lesscele = Mergedata[Mergedata$"Start Year of Celebration" == 0,]
activecele = Mergedata[Mergedata$"Start Year of Celebration" != 0,]
qplot(data=lesscele,x=survey_year,y=numObs, group = Region, colour = Region, main = 'bird activity for region with no celebration')
qplot(data=activecele,x=survey_year,y=numObs, group = Region, colour = Region, main = 'bird activity for region with active celebration')

### activity in rural area (CA.ON.ES) grow quicker than urban
rural = Mergedata[Mergedata$"Type (Rural/Urban)" == 'Rural',]
urban = Mergedata[Mergedata$"Type (Rural/Urban)" == 'Urban',]
qplot(data=rural,x=survey_year,y=numObs, group = Region, colour = survey_month, main = 'Rural area')
qplot(data=urban,x=survey_year,y=numObs, group = Region, colour = Region, main = 'Urban area')
qplot(data=Mergedata,x=survey_year,y=numObs, group = Region, colour = Region, main = 'bird activity in each region')

### two more plot
Mergedata = Mergedata[Mergedata$survey_year >= 1980,]
subsample <- Mergedata[Mergedata$Place %in% c('GTA', 'Waterloo', 'Durham'),]
May <- subsample[subsample$survey_month == 5,]
ggplot(subsample)+
  geom_line(aes(as.Date(Date), numObs, color = Place))+
  geom_point(data = May, aes(Date, numObs, color = Place))+
  geom_vline(xintercept = as.numeric(as.Date('20170101', '%Y%m%d')))+
  xlab('Survey date')+
  ylab('Number of observers')

Mergedata = Mergedata[Mergedata$survey_year >= 1980,]
subsample <- Mergedata[Mergedata$Place %in% c('GTA', 'Waterloo', 'Essex'),]
May <- subsample[subsample$survey_month == 5,]
ggplot(subsample)+
  geom_line(aes(as.Date(Date), numObs, color = Place))+
  geom_point(data = May, aes(Date, numObs, color = Place))+
  geom_vline(xintercept = as.numeric(as.Date('20170101', '%Y%m%d')))+
  geom_vline(xintercept = as.numeric(as.Date('20000101', '%Y%m%d')))+
  xlab('Survey date')+
  ylab('Number of observers')


#*************fit model****************
### screenreg/textreg/stargazer
Mergedata$Region = factor(Mergedata$Region)
Mergedata$collection = factor(Mergedata$collection)
Mergedata$Place = factor(Mergedata$Place)
Mergedata$'City/Town' = factor(Mergedata$'City/Town')
Mergedata$survey_month = factor(Mergedata$survey_month)
Mergedata$'Type (Rural/Urban)' = factor(Mergedata$'Type (Rural/Urban)')
#Mergedata = select(Mergedata, -c('Start Year of Celebration'))
Mergedata$May = ifelse(Mergedata$survey_month == 5, 1, 0)

# region, city/town is same as place, hence remove. Collection is not useful, removed.
# add other variable May
bird_poisson0 <- glm(numObs ~ survey_year + Mergedata$'Type (Rural/Urban)' + Place + Mergedata$'Celebration (Y/N)' + May + aftercele, data=Mergedata,family=poisson())
summary(bird_poisson0)
textreg(summary(bird_poisson0), labeling = 1)
bird_poisson1 <- glm(numObs ~ survey_year*aftercele + Mergedata$'Type (Rural/Urban)'*aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place, data=Mergedata,family=poisson())
summary(bird_poisson1)
bird_poisson2 <- glm(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place, data=Mergedata,family=poisson())
summary(bird_poisson2)


bird_poisson3 <- glm(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + I(survey_year^3)*aftercele + I(survey_year^4)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place, data=Mergedata,family=poisson())
summary(bird_poisson3)
#bird_poisson3 <- glm(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + survey_month*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele +survey_month*aftercele + survey_month*Place, data=Mergedata,family=poisson())
#summary(bird_poisson3)
# model comparison
AIC(bird_poisson0, bird_poisson1, bird_poisson2, bird_poisson3)
anova(bird_poisson0, bird_poisson1, test = "LRT")
anova(bird_poisson1, bird_poisson2, test = "LRT") 
anova(bird_poisson2, bird_poisson3, test = "LRT")

# Deviance goodness-of-fit test of best poisson model
round(pchisq(23629,6779,lower.tail = FALSE),3)


bird1 <- augment(bird_poisson2, type.predict = 'link')
# Plot variance as a function of mean (overdispersion)
# Under the Poisson assumption, the mean and variance should be
#approximately equal. This is not true!
bird_pn <- mutate(bird1, weightClass=cut(.fitted,seq(-3.83,6.97,0.5))) %>%
  group_by(weightClass) %>%
  summarise(Mean=mean(numObs),Var=var(numObs))

qplot(data=bird_pn,x=Mean,y=Var) +
  geom_abline(intercept=0,slope=1) +
  geom_smooth(method="lm",formula=y~ x - 1)

## Fit linear model
summary(lm(Var ~ Mean - 1,data=bird_pn))

## Fit quasi-Poisson model with weight (rejected)
qpoisson1 <- glm(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place , family=quasipoisson(), data=Mergedata)
summary(qpoisson1)
round(pchisq(23629/3.82, 6779,lower.tail = FALSE),3)

qpoisson2 <- glm(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + I(survey_year^3)*aftercele + I(survey_year^4)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place, family=quasipoisson(), data=Mergedata)
summary(qpoisson2)
round(pchisq(29314, 10920,lower.tail = FALSE),3)

# negative binomial
bird_nb1 <- glm.nb(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + May*aftercele + May*Place , data=Mergedata)
summary(bird_nb1)
round(pchisq(6810.9, 6779,lower.tail = FALSE),3)

bird_nb2 <- glm.nb(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + survey_month*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + survey_month*aftercele + survey_month*Place , data=Mergedata)
summary(bird_nb2)
round(pchisq(9170.8, 10804,lower.tail = FALSE),3)

bird_nb3 <- glm.nb(numObs ~ .+poly(survey_year,4)*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele  + May*Place - survey_year, data=Mergedata, control = glm.control(maxit = 1000))
summary(bird_nb3)
round(pchisq(9409.7, 10910,lower.tail = FALSE),3)

bird_nb4 <-  glm.nb(numObs ~ survey_year*aftercele + survey_month*aftercele, Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + survey_month*Place , data=Mergedata)
summary(bird_nb4)
#bird_nb4 <- glm.nb(numObs ~ survey_year*aftercele + I(survey_year^2)*aftercele + I(survey_year^3)*aftercele + I(survey_year^4)*aftercele + survey_month*aftercele + Mergedata$'Type (Rural/Urban)'* aftercele + Place*aftercele + Mergedata$'Celebration (Y/N)'*aftercele + survey_month*aftercele + survey_month*Place , data=Mergedata)
#summary(bird_nb4)
#round(pchisq(9470.2, 10800,lower.tail = FALSE),3)

BIC(bird_nb1, bird_nb2, bird_nb3)
bird0 = augment(bird_nb2, type.predict = 'link')

bird3 <- augment(bird_nb1, type.predict = 'link')
bird33 <- mutate(bird3, weightClass=cut(.fitted,seq(0.15,7.3,0.5))) %>%
  group_by(weightClass) %>%
  summarise(Mean=mean(numObs),Var=var(numObs))
bird2 <- augment(bird_nb1, type.predict = 'link')
bird22 <- mutate(bird2, weightClass=cut(.fitted,seq(-1.5,9.23,0.5))) %>%
  group_by(weightClass) %>%
  summarise(Mean=mean(numObs),Var=var(numObs))

#bird4 <- augment(bird_nb4, type.predict = 'link')
# mean-variance relationship for nb
qplot(data=bird33,x=Mean,y=Var) +
  geom_abline(intercept=0,slope=1) +
  geom_smooth(method="lm",formula=y~ x - 1) +
  stat_function(fun=function(x) x + (1/11.111) * x^2)

#qplot(data=bird3,x=Mean,y=Var) +
#  stat_function(fun=function(x) x + (1/8.455) * x^2)

qplot(data=bird22,x=Mean,y=Var) +
  geom_abline(intercept=0,slope=1) +
  geom_smooth(method="lm",formula=y~ x - 1) +
  stat_function(fun=function(x) x + (1/4.4258) * x^2)

# mean-variance relationship for quasi-Poisson
#qplot(data=bird_pn,x=Mean,y=Var) +
#  stat_function(fun=function(x) x*5.347662)

#*************Diagnostics****************
## Residual plot

bird0 %>%
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residual") + 
  xlab("Fitted Value")

bird2 %>%
  ggplot(aes(x=.fitted,y=.std.resid, color = Place )) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residual") + 
  xlab("Fitted Value")


bird3 %>%
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residual") + 
  xlab("Fitted Value")

bird4 %>%
  ggplot(aes(x=.fitted,y=.std.resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  ylab("Standardized Residual") + 
  xlab("Fitted Value")

## fitted value
fit.table.nb = augment(bird_nb1)
bird_nb.values <- bird_nb1 %>%
  augment(newdata = fit.table.nb,
          se_fit = TRUE , type.predict = "response") %>%
  mutate(Upper = .fitted + 1.96 * .se.fit,
         Lower = .fitted - 1.96 * .se.fit)

#pois.values <- bird_poisson3 %>%
#  augment(newdata = bird1,
#          se_fit = TRUE, type.predict = "response") %>%
#  mutate(Upper = .fitted + 1.96 * .se.fit,
#         Lower = .fitted - 1.96 * .se.fit)

fitted_pois = pois.values$.fitted
fitted_negbin = bird_nb.values$.fitted
fit.table_poi <- data.frame(cbind(fitted_pois, pois.values$Lower,pois.values$Upper
))
fit.table_neb <- data.frame(cbind(fitted_negbin, bird_nb.values$Lower,bird_nb.values$Upper
))

set.seed(123)
Index = sample(1:6812, 30)
#Index_fit.table_poi = fit.table_poi[Index,]
Index_fit.table_neb = fit.table_neb[Index,]
#sort_poi = Index_fit.table_poi[order(as.numeric(Index_fit.table_poi$fitted_pois)),]
sort_neg = Index_fit.table_neb[order(as.numeric(Index_fit.table_neb$fitted_negbin)),]
#sort_poi['Index'] = 1:500
sort_neg['Index'] = 1.5:30.5
#names(sort_poi) = c("FittedValue","Lower", "Upper", "Index")
names(sort_neg) = c("FittedValue","Lower", "Upper", "Index")
#fit.table = data.frame(rbind(sort_poi, sort_neg), c("FittedValue","Lower", "Upper", "Index"))
fit.table = data.frame(sort_neg, c("FittedValue","Lower", "Upper", "Index"))
#fit.table$Model <- rep(c("Poisson","Negative Binomial"),each=500)
#fit.table$Model <- rep("Negative Binomial",each=30)

#ggplot(sort_neg, aes(x = Index, y = FittedValue, color = Model)) +
# geom_point(size = 2) +
#  ylab("Fitted values") +
#  geom_errorbar(aes(ymax =Lower, ymin=Upper))

ggplot(sort_neg, aes(x = Index, y = FittedValue)) +
  geom_point(size = 2) +
  ylab("Fitted values") +
  geom_errorbar(aes(ymax =Lower, ymin=Upper))

## Plot leverage for nb
bird2 %>%
  ggplot(aes(x = .fitted, y = .hat, color = Place)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Leverage")

bird3 %>%
  ggplot(aes(x = .fitted, y = .hat)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Leverage")


## Plot Cook's distance for nb
bird2 %>%
  ggplot(aes(x = .fitted, y = .cooksd, color = Place)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Cook's Distance")

bird3 %>%
  ggplot(aes(x = .fitted, y = .cooksd)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  xlab("Fitted Value") +
  ylab("Cook's Distance")

