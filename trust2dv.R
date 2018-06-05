###title: "Reproduction script for the paper 'Trusting Under What Conditions? The Links between Internet and
#Television News Consumption and Generalised Trust'"
###author: "O Volchenko <ovolchenko@hse.ru> and A Shirokanova <a.shirokanova@hse.ru>, LCSR, HSE <lcsr.hse.ru/en>"
### date: "05 June 2018"

##1. Data preparation
library(foreign); library(readr); library(plyr) #data import and management
library(lme4); library(arm); library(car) #modelling
library(stargazer); library(sjPlot) #visualization


#Download the individual-level data file from www.worldvalues.survey.org, Data and Documentation -> Wave 6
data <- read.spss("WV6_Data_spss_v_2016_01_01.sav", use.value.labels = T, to.data.frame = T)

#select the set of variables used in analysis
vars <- c(
  "V2",  #Country Code
  "V24",	#Most people can be trusted 
  "V105",	#How much you trust: People you meet for the first time
  "V217",	#Information source: Daily newspaper
  "V219",	#Information source: TV news
  "V239",	#Scale of incomes
  "V240",	#Sex
  "V242",	#Age
  "V248" #Highest educational level attained
)
data<-data[vars]; rm(vars)

#Dependent variable: Trust to ppl met for the 1st time
#print_labels(data$V105)
data$trustunknown2 <- car::recode(data$V105, "'Trust completely' = 1; 'Trust somewhat' = 1; 'Do not trust very much' = 0; 'Do not trust at all' = 0") 
#'Trust completely' or 'Trust somewhat'= 1, don't trust = 0
data$trustknown2 <- car::recode(data$V104, "'Trust completely' = 1; 'Trust somewhat' = 1; 'Do not trust very much' = 0; 'Do not trust at all' = 0") 
#Trust to ppl you know personally: 'Trust completely' & 'Trust somewhat' = 1, others = 0

#Predictors: 
data$int <- car::recode(as.numeric(data$V223), "1=1; 2=0; 3=0; 4=0; 5=0") 
#Internet as a source of news: create dummy for Daily = 1, others = 0
data$int <- factor(data$int, levels = c(0, 1), labels = c("n", "y")) #save news channel as factors

data$tvn <- car::recode(as.numeric(data$V219), "1=1; 2=0; 3=0; 4=0; 5=0") 
#dummy variable for TV as a daily source of information
data$tvn <- factor(data$tvn, levels = c(0, 1), labels = c("n", "y"))

data$trust <- car::recode(data$V24, "'Most people can be trusted' = 1; 
                                     'Need to be very careful' = 0") 
data$tvint <- rep(NA, nrow(data))
data$tvint[which(data$int == "n" & data$tvn == "y")] <- "0tvonly"
data$tvint[which(data$int == "y" & data$tvn == "n")] <- "1intonly"
data$tvint[which(data$int == "y" & data$tvn == "y")] <- "2both"
data$tvint[which(data$int == "n" & data$tvn == "n")] <- "3none"

data$tvint <- relevel(as.factor(data$tvint), ref = "0tvonly")
  
#Individual controls
data$income <- as.numeric(data$V239)	#Scale of incomes

data$gndr <- as.factor(data$V240)	#Sex

data$agenotscaled <- as.numeric(data$V242)	#Age
data$age <- scale(as.numeric(data$V242))	# Scaled Age

data$educ <- rep(NA, length(data$V248))#recode education intro three levels: primary, secondary, tertiary 
data$V248 <- as.numeric(data$V248)
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
data$educ <-  factor(data$educ,
                     levels = c(0, 1, 2),
                     labels = c("l", "m", "h"))

data$country <- data$V2
savevars <- c("country", "trustunknown2", "trustknown2",  "int", "tvn", "radn", "nwsp", "trust", "tvint",  
              "income", "gndr", "age", "agenotscaled", "educ")
data1 <- data[savevars]; rm(savevars)

#country level data are: GDP PPP data for the year of fieldwork (World Bank, data.worldbank.org), 
#Freedom House country status (https://freedomhouse.org/report-types/freedom-world), and
#Dictatorship-Democracy index (https://www3.nd.edu/~ggoertz/qmir/cheibub_etal2010.pdf) 
#The resulting country-level data set is attached as "country_trust_data.csv"

###2. Merging the individual- and country- level data
country <- read.csv("country_data.csv")
head(country)
str(country)
#country$GDP_ppp_pc_us <- gsub(",", ".", country$GDP_ppp_pc_us)
country$GDP <- scale(log(as.numeric(country$GDP_ppp_pc_us)))
country$Status <- as.factor(country$Status)

levels(country$Status) <- c("2 - F", "0 - NF", "1 - PF")
names(country) <- c("countryN", "country", "year", "GDP_ppp_pc_us", "Status", 
                    "democracy", "GDP")
data2 <- merge(data10, country, all.x = T, by = "country")#merge individual- and country-level data by country

data2$democracy <- factor(data2$democracy , levels = c(0, 1), labels = c("1 - dict", "2 - dem"))
data2$democracy <- relevel(data2$democracy, ref = "1 - dict")
data2$Status <- relevel(as.factor(data2$Status), ref = "0 - NF")
data2$tvint <- relevel(as.factor(data2$tvint), ref = "0tvonly")

data4 <- na.omit(data2)

###3. Modelling
null.model <- glmer(trustunknown2 ~ (1 | country), data = data4, family = 'binomial') 
summary(null.model)
icc <- null.model@theta^2 / (null.model@theta^2 + (pi^2)/3) #icc = 11%
sjstats::icc(null.model)

null.model1 <- glmer(trust ~ (1 | country), data = data4, family = 'binomial') 
summary(null.model1)
icc <- null.model1@theta^2 / (null.model1@theta^2 + (pi^2)/3) #icc = 23%
sjstats::icc(null.model1)


#Trust to most people as DV
trustmodel1 <- glmer(trust ~  tvint +
                       (1 | country), data = data4, family = 'binomial')
trustmodel2 <- glmer(trust ~  income + gndr + age + educ + GDP + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustmodel3 <- glmer(trust ~  income + gndr + age + educ + GDP + democracy + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustmodel4 <- glmer(trust ~  income + gndr + age + educ + GDP + Status + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustmodel5 <- glmer(trust ~  income + gndr + age + educ + GDP + democracy * tvint +
                          (1 | country), data = data4, family = 'binomial')
trustmodel6 <- glmer(trust ~  income + gndr + age + educ + GDP + Status * tvint +
                          (1 | country), data = data4, family = 'binomial')
trustmodel7 <- glmer(trust ~  income + gndr + age + educ + GDP + democracy * tvint +
                       (1 + tvint | country), data = data4, family = 'binomial')
trustmodel8 <- glmer(trust ~  income + gndr + age + educ + GDP + Status * tvint + 
                       (1 + tvint | country), data = data4, family = 'binomial')
#Trust to people one meets for the first time (unknown people) as DV
trustuknmodel1 <- glmer(trustunknown2 ~  tvint +
                       (1 | country), data = data4, family = 'binomial')
trustuknmodel2 <- glmer(trustunknown2 ~  income + gndr + age + educ + GDP + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustuknmodel3 <- glmer(trustunknown2 ~  income + gndr + age + educ + GDP + democracy + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustuknmodel4 <- glmer(trustunknown2 ~  income + gndr + age + educ + GDP + Status + tvint +
                       (1 | country), data = data4, family = 'binomial')
trustuknmodel5 <- glmer(trustunknown2 ~ income + gndr + age + educ + GDP + democracy * tvint + 
                          (1 | country), data = data4, family = 'binomial')
trustuknmodel6 <- glmer(trustunknown2 ~ income + gndr + age + educ + GDP + Status * tvint + 
                          (1 | country), data = data4, family = 'binomial')
trustuknmodel7 <- glmer(trustunknown2 ~ income + gndr + age + educ + GDP + democracy * tvint + 
                          (1 + tvint | country), data = data4, family = 'binomial')
trustuknmodel8 <- glmer(trustunknown2 ~ income + gndr + age + educ + GDP + Status * tvint + 
                          (1 + tvint | country), data = data4, family = 'binomial')

#regression table of final models
sjt.glmer(trustmodel7, trustmodel8, trustuknmodel5, trustuknmodel8,
          separate.ci.col = FALSE, emph.p = T,
          show.ci = F, sep.column = F, show.se = T, p.numeric = F)


#Final Plots
sjPlot::plot_model(trustmodel7, terms = c("democracy", "tvint"), type = "eff", axis.lim = c(0.10, 0.35))
sjPlot::plot_model(trustuknmodel5, terms = c("democracy", "tvint"), type = "eff", axis.lim = c(0.1, 0.35))
sjPlot::plot_model(trustmodel8, terms = c("Status", "tvint"), type = "eff", axis.lim = c(0.1, 0.35))
sjPlot::plot_model(trustuknmodel8, terms = c("Status", "tvint"), type = "eff", axis.lim = c(0.1, 0.35))

###FIN
