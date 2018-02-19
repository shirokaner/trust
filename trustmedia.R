###title: "Reproduction script for the paper 'Generalised trust and media consumption: 
###a multilevel comparison of democratic and nondemocratic societies' "
###author: "O Volchenko <ovolchenko@hse.ru> and A Shirokanova <a.shirokanova@hse.ru>, LCSR, HSE <lcsr.hse.ru/en>"
### date: "30 January 2018"

##1. Data preparation
library(haven); library(readr); library(plyr) #data import and management
library(lme4); library(arm); library(car) #modelling
library(stargazer); library(sjPlot) #visualization

#Download the individual-level data file from www.worldvalues.survey.org, Data and Documentation -> Wave 6
data <- read_sav("WV6_Data_spss_v_2016_01_01.sav")#if it fails, use:
#library(foreign)
#data <- read.spss("WV6_Data_spss_v_2016_01_01.sav", use.value.labels = T, to.data.frame = T)

#select the set of variables used in analysis
vars <- c(
  "V2",  #Country Code
  "V104",	#How much you trust: People you know personally
  "V105",	#How much you trust: People you meet for the first time
  "V217",	#Information source: Daily newspaper
  "V219",	#Information source: TV news
  "V220",	#Information source: Radio news
  "V223",	#Information source: Internet
  "V239",	#Scale of incomes
  "V240",	#Sex
  "V242",	#Age
  "V248" #Highest educational level attained
)
data<-data[vars]; rm(vars)

#Dependent variable: Trust to ppl met for the 1st time
#print_labels(data$V105)
data$trustunknown2 <- car::recode(data$V105, "1 = 1; 2 = 1; 3 = 0; 4 = 0") 
#'Trust completely' or 'Trust somewhat'= 1, don't trust = 0

#Predictors: 
data$int <- car::recode(as.numeric(data$V223), "1=1; 2=0; 3=0; 4=0; 5=0") 
#Internet as a source of news: create dummy for Daily = 1, others = 0
data$int <- factor(data$int, levels = c(0, 1), labels = c("n", "y")) #save news channel as factors

data$tvn <- car::recode(as.numeric(data$V219), "1=1; 2=0; 3=0; 4=0; 5=0") 
#dummy variable for TV as a daily source of information
data$tvn <- factor(data$tvn, levels = c(0, 1), labels = c("n", "y"))

data$radn <- car::recode(as.numeric(data$V220), "1=1; 2=0; 3=0; 4=0; 5=0") 
#dummy variable for radio as a daily source of information
data$radn <- factor(data$radn, levels = c(0, 1), labels = c("n", "y"))

data$nwsp <- car::recode(as.numeric(data$V217), "1=1; 2=0; 3=0; 4=0; 5=0") 
#dummy variable with 2 categories for news consumption: newspapers
data$nwsp <- factor(data$nwsp, levels = c(0, 1), labels = c("n", "y"))

data$trustknown2 <- car::recode(data$V104, "1 = 1; 2 = 1; 3 = 0; 4 = 0") 
#Trust to ppl you know personally: 'Trust completely' & 'Trust somewhat' = 1, others = 0

#Individual controls
data$income <- as.numeric(data$V239)	#Scale of incomes

data$gndr <- as.factor(data$V240)	#Sex

data$agenotscaled <- as.numeric(data$V242)	#Age
data$age <- scale(as.numeric(data$V242))	# Scaled Age

#if you used haven::read_sav()to import the file:
data$educ <- rep(NA, length(data$V248))#recode education intro three levels: primary, secondary, tertiary 
data$educ[data$V248 == "1" | #No formal education
            data$V248 == "2" | #Incomplete primary school
            data$V248 == "3"] <- 0 #Complete primary school
data$educ[data$V248 == "4" |
            #Incomplete secondary school: technical/ vocational type
            data$V248 == "5" |
            #Complete secondary school: university-preparatory type
            data$V248 == "6" |
            #Complete secondary school: technical/ vocational type
            data$V248 == "7"] <-  1 #Incomplete secondary school: university-preparatory type
data$educ[data$V248 == "8" |
            # Some university-level education, without degree
            data$V248 == "9"] <- 2 #University - level education, with degree
#if you used foreign::read.spss() to import the file:
# data$educ[data$V248 == "No formal education" |
#             data$V248 == "Incomplete primary school" |
#             data$V248 == "Complete primary school"]  <- 0
# data$educ[data$V248 == "Incomplete secondary school: technical/ vocational type" |
#             data$V248 == "Complete secondary school: university-preparatory type"|
#             data$V248 == "Complete secondary school: technical/ vocational type" |
#             data$V248 == "Incomplete secondary school: university-preparatory type"] <-  1
# data$educ[data$V248 == "Some university-level education, without degree" |
#             data$V248 == "University - level education, with degree"] <- 2
# 
data$educ <-  factor(data$educ,
                     levels = c(0, 1, 2),
                     labels = c("l", "m", "h"))
data$country <- data$V2

savevars <- c("country", "trustunknown2", "trustknown2", "int", "nwsp", "tvn", "radn", "income",
              "gndr", "age", "agenotscaled", "educ")
data1 <- data[savevars]; rm(savevars)

#country level data are: GDP PPP data for the year of fieldwork (World Bank, data.worldbank.org), 
#Freedom House country status (https://freedomhouse.org/report-types/freedom-world), and
#Dictatorship-Democracy index (https://www3.nd.edu/~ggoertz/qmir/cheibub_etal2010.pdf) 
#The resulting country-level data set is attached as "country_trust_data.csv"

###2. Merging the individual- and country- level data
country <- read_csv("country_data.csv", col_types = "ccidcc")
head(country)

country$GDP <- scale(log(country$GDP_ppp_pc_us))
country$Status <- as.factor(country$Status)
levels(country$Status) <- c("F", "NF", "PF")

data2 <- merge(data1, country, all.x = T, by = "country")#merge individual- and country-level data by country

data2$democracy <- factor(data2$democracy , levels = c(0, 1), labels = c("dict", "dem"))
data2$democracy <- relevel(data2$democracy, ref = "dict")
data2$Status <- relevel(as.factor(data2$Status), ref = "NF")

sjt.df(as.data.frame(data2))

data4 <- na.omit(data2)

sjt.df(as.data.frame(data4))

###3. Modelling
null.model <- glmer(trustunknown2 ~ (1 | country), data = data4, family = 'binomial') 
summary(null.model)
icc <- null.model@theta^2 / (null.model@theta^2 + (pi^2)/3) #icc = 11%

## Internet in nondemocracies
#internet * DD Index

m.intdem.1 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + 
                      (1 | country), data = data4, family = 'binomial')
m.intdem.2 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + 
                      (1 + int | country), data = data4, family = 'binomial')
m.intdem.3 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.intdem.4 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + democracy + 
                      (1 + int | country), data = data4, family = 'binomial')
m.intdem.5 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.intdem.6 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int * democracy + 
                      (1 + int | country), data = data4, family = 'binomial')
m.intdem.7 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + int * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.intdem.8 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + int * democracy + 
                      (1 + int | country), data = data4, family = 'binomial')
m.intdem.9 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + int * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.intdem.10 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + int * democracy + 
                       (1 + int | country), data = data4, family = 'binomial')
m.intdem.11 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int * democracy + 
                       (1 | country), data = data4, family = 'binomial')
m.intdem.12 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int + democracy + 
                       (1 + int  | country), data = data4,  family = 'binomial')
m.intdem.13 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int * democracy + 
                       (1 + int  | country), data = data4, family = 'binomial')

anova(m.intdem.12, m.intdem.13)
sjt.glmer(m.intdem.12, m.intdem.13,
          #file = "modeling.intdem.htm",
          separate.ci.col = FALSE, 
          show.ci = F)


#internet * FH status

m.intdem.Status.1 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + 
                             (1 | country), data = data4, family = 'binomial')
m.intdem.Status.2 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + 
                             (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.3 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + Status + 
                             (1 | country), data = data4, family = 'binomial')
m.intdem.Status.4 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + Status + 
                             (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.5 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int * Status + 
                             (1 | country), data = data4, family = 'binomial')
m.intdem.Status.6 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int * Status + 
                             (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.7 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + int * Status + 
                             (1 | country), data = data4, family = 'binomial')
m.intdem.Status.8 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + int * Status + 
                             (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.9 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + int * Status + 
                             (1 | country), data = data4, family = 'binomial')
m.intdem.Status.10 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + int * Status + 
                              (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.11 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int * Status + 
                              (1 | country), data = data4, family = 'binomial')
m.intdem.Status.12 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int + Status + 
                              (1 + int | country), data = data4, family = 'binomial')
m.intdem.Status.13 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + nwsp + radn + int * Status + 
                              (1 + int | country), data = data4, family = 'binomial')

anova(m.intdem.Status.12, m.intdem.Status.13)
sjt.glmer(m.intdem.Status.12, m.intdem.Status.13,
          #file = "modeling.intdem.Status.htm",
          separate.ci.col = FALSE, 
          show.ci = F)

##Television in nondemocracies
#television * DD Index
m.tvndem.1 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + 
                      (1 | country), data = data4, family = 'binomial')
m.tvndem.2 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + 
                      (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.3 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.tvndem.4 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + democracy + 
                      (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.5 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.tvndem.6 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn * democracy + 
                      (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.7 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + tvn * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.tvndem.8 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + tvn * democracy + 
                      (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.9 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + tvn * democracy + 
                      (1 | country), data = data4, family = 'binomial')
m.tvndem.10 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + tvn * democracy + 
                       (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.11 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn * democracy + 
                       (1 | country), data = data4, family = 'binomial')
m.tvndem.12 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn + democracy + 
                       (1 + tvn | country), data = data4, family = 'binomial') 
m.tvndem.13 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn * democracy + 
                       (1 + tvn  | country), data = data4, family = 'binomial') 

anova(m.tvndem.12, m.tvndem.13)
sjt.glmer(m.tvndem.12, m.tvndem.13,
          #file = "modeling.tvndem.htm",
          separate.ci.col = FALSE, 
          show.ci = F)


#television * FH status
m.tvndem.Status.1 <- glmer(trustunknown2 ~ trustknown2 +  income + gndr + age + educ + GDP + tvn + 
                             (1 | country), data = data4, family = 'binomial')
m.tvndem.Status.2 <- glmer(trustunknown2 ~ trustknown2 +  income + gndr + age + educ + GDP + tvn + 
                             (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.Status.3 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + Status + 
                             (1 | country), data = data4, family = 'binomial')
m.tvndem.Status.4 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn + Status + 
                             (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.Status.5 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn * Status + 
                             (1 + tvn| country), data = data4, family = 'binomial')
m.tvndem.Status.6 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + tvn * Status + 
                             (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.Status.7 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + tvn * Status + 
                             (1 | country), data = data4, family = 'binomial')
m.tvndem.Status.8 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + tvn * Status + 
                             (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.Status.9 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + tvn * Status + 
                             (1 | country), data = data4, family = 'binomial')
m.tvndem.Status.10 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + tvn * Status + 
                              (1 + tvn | country), data = data4, family = 'binomial')
m.tvndem.Status.11 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn * Status + 
                              (1 | country), data = data4, family = 'binomial')
m.tvndem.Status.12 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn + Status + 
                              (1 + tvn  | country), data = data4, family = 'binomial') 
m.tvndem.Status.13 <- glmer(trustunknown2 ~ trustknown2 + income + gndr + age + educ + GDP + int + nwsp + radn + tvn * Status + 
                              (1 + tvn  | country), data = data4, family = 'binomial') 

anova(m.tvndem.Status.12, m.tvndem.Status.13)
sjt.glmer(m.tvndem.Status.12, m.tvndem.Status.13,
          #file = "modeling.tvndem.s.html"
          separate.ci.col = FALSE, 
          show.ci = F)
summary(m.tvndem.Status.13)

##Summary table of final models
sjt.glmer(m.intdem.Status.12, m.intdem.Status.13, m.tvndem.Status.12, m.tvndem.Status.13,
          separate.ci.col = FALSE,
          show.ci = F,
          #file = "allunknown.htm", 
          show.dev = TRUE, 
          show.loglik = TRUE, show.aic = TRUE)

##Plotting the significant interactions
#effects::allEffects(modeling.intdem.Status.13)
sjPlot::plot_model(modeling.intdem.Status.13, terms = c("Status", "int"), type = "pred")
#effects::allEffects(modeling.tvndem.Status.13)
sjPlot::plot_model(modeling.tvndem.Status.13, terms = c("Status", "tvn"), type = "pred")

###End.