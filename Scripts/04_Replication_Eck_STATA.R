#Replikation von Tabelle 1 des Papers von Kristine Eck zur Konfliktintensivierung


# Setup -------------------------------------------------------------------
library(RStata)
library(survival)
library(survminer)
library(tidyverse)
library(haven)
library(foreign)
library(stargazer)
library(finalfit)

#Datensatz laden
eck_data <- read_dta(file = "Data/Eck Replication Data/ISQ.Eck.replication.dta")

# Teil RSTATA-------------------------------------------------------------
options("RStata.StataPath" = "\"C:\\Program Files\\Stata16\\StataSE-64\"")
options("RStata.StataVersion" = 16)
#chooseStataBin()


# Ausprobieren mit STATA --------------------------------------------------
stata('di "Hello World"')
stata('di 2+2')
stata('clear all')

command <- "sysuse auto, clear
  sum mpg, d"
stata(command)

#Daten von R zu STATA
test <- data.frame("column_1" = rnorm(100))
stata("sum column_1, d", data.in = df)



# STATA Code Replikation --------------------------------------------------

stata("ISQ.Eck.replication.do", 
      data.out = T,
      stata.echo = T) #geht theoretisch auch ohne weitere Argumente


# Datenimport und Tidying -------------------------------------------------




#?berblick ?ber den Datensatz verschaffen
str(eck_data)
class(eck_data$war) #Abh?ngige Variable
class(eck_data$ethnic) #Unabh?ngige Variable
summary(eck_data$ethnic)
#df$war <- as.factor(df$war)
#df$ethnic <- as.factor(df$ethnic)
#levels(df$ethnic)


#Zeitvariable unbenennen in sprechende Namen ohne underscore
#In Stata ist analysis time _t = end_of_segment - origin (hier benennen wir _t und die weiteren 
#zur Einfachkeit in time um)
#mit origin: time first_year
names(eck_data)[42] <- "analysis_time"
names(eck_data)[41] <- "origin"
names(eck_data)[40] <- "failure"
names(eck_data)

count(eck_data, war)
count(eck_data, failure) #Jetzt komme ich auf dieselbe Anzahl fehlgeschlagener Konflikte, 
#d.h. diejenigen die in Krieg eskaliert sind


#Vergleich Variablen War und Failure im Datensatz
eck_data <- eck_data %>% 
  relocate(war, .after = year) %>% 
  relocate(failure, .after = war)


#Analyse der Zeitvariablen
eck_times <- eck_data %>% 
  select(id, location, year, first_year, origin, start_of_segment, end_of_segment,
         analysis_time, failure, war, ethnic, plural)



# Replikation Tabelle 1 Modell 1-5 ----------------------------------------

##Modell 1, Tabelle 1
model1 <- coxph(Surv(time = analysis_time, event = failure) ~ 
                  ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, cluster = location, ties = "efron", robust = T)

#funktioniert nur, wenn ich die dummy variablen nicht in kategoriale umwandle
summary(model1)

#exp(coef) gibt uns die Hazard ratio zur?ck: Die interpretieren wir so:
#At a given instant in time, conflicts that are ethnically mobilized are 28% as likely to escalate to war 
#as conflicts that are not mobilized along ethnic lines adjusted for all the control variables
str(model1)

#Jetzt stimmt die Anzahl der Observationen und die Anzahl failures und die Richtung der Koeffizienten
#ABER die Werte und die Signifikanz unterscheiden sich. WAS mache ich falsch?

#Vergleich zum STATA Code von Modell 1: Auch ohne die zus?tzlichen Argumente im STATA Call gibt es
#dieselben Koeffizienten bzw. Hazard Ratios + p-Values aus. WO liegt das problem?
stata("Modell1_Tab1_Full_Code.do", 
      data.out = TRUE,
      stata.echo = TRUE)

stata("Modell1_Tab1_Min_Code.do",
      data.out = TRUE,
      stata.echo = TRUE)


##Modell 2, Tabelle 1
model2 <- coxph(Surv(time = analysis_time, event = failure) ~ 
                  ethnic + ethfrac + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, cluster = location, ties = "efron", robust = T)
summary(model2)


##Modell 3, Tabelle 1
model3 <- coxph(Surv(time = analysis_time, event = failure) ~ 
                  ethnic + ethfrac + ethfracsq + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, cluster = location, ties = "efron", robust = T)
summary(model3)


##Modell 4, Tabelle 1
model4 <- coxph(Surv(time = analysis_time, event = failure) ~ 
                  ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw + milper,
                data = eck_data, cluster = location, ties = "efron", robust = T)
summary(model4)


##Modell 5, Tabelle 1
model5 <- coxph(Surv(time = analysis_time, event = failure) ~ 
                  ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw + sideb2nd,
                data = eck_data, cluster = location, ties = "efron", robust = T)
summary(model5)
broom::tidy(model5) #gibt nur normale Koeffizienten aus nicht hazard ratio


#Tabelle machem mit Modell 1 bis 5
stargazer(model1, model2, model3, model4, model5, type = "text", dep.var.labels = "failure",
          title = "Table 1: Risk of Conflict Intensification 1946-2004",
          digits = 2, out = "models.txt", model.names = T)
          #coef = exp(c(model1$coefficients, model2$coefficients, model3$coefficients, 
          #             model4$coefficients, model5$coefficients))
#hier muss ich noch etwas ?ndern, damit die exponentiellen Koeffizienten abgedruckt werden


exp(model1$coefficients)

# Cox proportional hazards model Output interpretieren --------------------

conflict_survival <- survfit(Surv(time = analysis_time, event = failure) ~ ethnic, data = eck_data,
                      cluster = location)
base_plot <- ggsurvplot(conflict_survival, xlab = "Time in years", risk.table = T, pval = T)
base_plot
#Logranktest p-value = ndicates that there is an overall difference between the two groups
#Difference in overall survival
#lines do not cross each other = no violation of proportional hazards assumption

#Jetzt k?nnen wir auch ein Univariates Modell erstellen
base_model <- coxph(Surv(time = analysis_time, event = failure) ~ ethnic, data = eck_data,
                    cluster = location)
summary(base_model)

#Assumptions testen: Proportional Hazard = hazard is proportionally similar among the groups
cox.zph(model1)
#Wie interpretiere ich das?


# finalfit ----------------------------------------------------------------

dependent_var <- "Surv(time = analysis_time, event = failure)"
explanatory_1 <- c("ethnic", "plural", "gdp2", "lnpop1", "demo", "auto", "incompatibility", "cw",
                   "cluster(location)")

eck_data %>% 
  finalfit.coxph(dependent = dependent_var,explanatory = explanatory_1, add_dependent_label = FALSE) %>% 
  rename("Overall survival" = label)

#Testing for proportional hazards
eck_data %>% 
  coxphmulti(dependent_var, explanatory_1) %>% 
  cox.zph() %>% 
  {zph_result <<- .} %>% 
  plot(var = 8) #plot geht irgendwie nicht

zph_result

eck_data %>% 
  hr_plot(dependent_var, explanatory_1)

eck_data %>% 
  coxphmulti(dependent_var, explanatory_1) %>% 
  fit2df()

#Visualizing the estimated distribution of survival times
ggsurvplot(survfit(model1), color = "#2E9FDF", ggtheme = theme_minimal(), data = eck_data)



# Replikation Eck mit logarithmischem Modell ------------------------------

eck_log1 <- glm(war ~ ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, family = "binomial")
summary(eck_log1)

eck_log2 <- glm(war ~ ethnic + ethfrac + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, family = "binomial")
summary(eck_log2)

eck_log3 <- glm(war ~ ethnic + ethfrac + ethfracsq + gdp2 + lnpop1 + demo + auto + incompatibility + cw,
                data = eck_data, family = "binomial")
summary(eck_log3)
broom::tidy(eck_log3)

eck_log4 <- glm(war ~ ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw + milper,
                data = eck_data, family = "binomial")
summary(eck_log4)

eck_log5 <- glm(war ~ ethnic + plural + gdp2 + lnpop1 + demo + auto + incompatibility + cw + sideb2nd,
                data = eck_data, family = "binomial")
summary(eck_log5)

stargazer(eck_log1, eck_log2, eck_log3, eck_log4, eck_log5, type = "text",
          dep.var.labels = "war",
          title = "Table 1: Risk of Conflict Intensification 1946-2004",
          digits = 2, out = "models.txt", model.names = T)
#WARUM ist keine meiner ethnischen Mobilisierung Variable signifikant?

