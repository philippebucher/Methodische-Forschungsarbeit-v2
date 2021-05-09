#Logarithmische Modelle von Konfliktintensivierung und ethnischer Mobilisierung

#Vorgehen:
#- Pre-step: Grundmodelle fitten und interpretieren anhand unterschiedlicher Vorgehensweisen
#- 1. Schritt: Modelle erstellen, deren Variablen möglichst nah an der Originalstudie von Eck kodiert sind,
#mit 1946-2019 Daten und Interpretation der Modelle, die auf Tabelle 1 und Tabelle 3 basieren.
#- 2. Schritt: Dieselben Modelle wie in Schritt 1 erstellen, aber mit demselben Zeitraum, wie in der 
#Originalstudie (1946-2004) und Interpretation der Modelle.
#- 3. Schritt: Modelle mit eigenen Variablen (selbe Konzepte, anders kodiert) erstellen mit den 1946-2019
#Daten und dann mit denn Modellen aus Schritt 1 vergleichen.
#- 4. Schritt: Erweiterte Modelle mit 2019 Daten erstellen, siehe dazu u.A. STATA .do file von der 
#Originalstudie.


# Setup -------------------------------------------------------------------

library(stargazer)
library(tidyverse)
library(caret)
library(caTools)
library(descr)
library(tidymodels)
library(vip)

#import data
UCDP <- readRDS(file = "Data/UCDP_data.rds")

# Kleine Data Preparation Änderungen --------------------------------------

#Alle Variablen aus dem ACD2EPR Datensatz, welche NA's sind in 0 codieren. 0 heisst laut Guy, dass
#". Die EPR-Daten wurden so kodiert, dass bei einem fehlen einer ethnischen Verbindung alle Variablen 
#den Wert NA bekommen."

count(UCDP, ethnic_conflict)
UCDP$ethnic_conflict[is.na(UCDP$ethnic_conflict)] <- 0

count(UCDP, recruitment)
UCDP$recruitment[is.na(UCDP$recruitment)] <- 0

count(UCDP, claim)
UCDP$claim[is.na(UCDP$claim)] <- 0

count(UCDP, support)
UCDP$support[is.na(UCDP$support)] <- 0
UCDP <- UCDP %>% 
  mutate(support = if_else(support > 0, 1, 0))

# Dummy Variablen in Faktoren umwandeln -----------------------------------
str(UCDP)
UCDP$war <- as_factor(UCDP$war)
UCDP$recruitment <- as_factor(UCDP$recruitment)
UCDP$ethnic_conflict <- as_factor(UCDP$ethnic_conflict)
UCDP$claim <- as_factor(UCDP$claim)
UCDP$support <- as_factor(UCDP$support)
UCDP$active <- as_factor(UCDP$active)
UCDP$incompatibility <- as_factor(UCDP$incompatibility)
UCDP$cumulative_intensity <- as_factor(UCDP$cumulative_intensity)
UCDP$side_b_2nd_dummy <- as_factor(UCDP$side_b_2nd_dummy)
UCDP$region <- as_factor(UCDP$region)
UCDP$middle_east <- as_factor(UCDP$middle_east)
UCDP$asia <- as_factor(UCDP$asia)
UCDP$africa <- as_factor(UCDP$africa)
UCDP$americas <- as_factor(UCDP$americas)
UCDP$democ_dummy <- as_factor(UCDP$democ_dummy)
UCDP$autoc_dummy <- as_factor(UCDP$autoc_dummy)
UCDP$Cold_War <- as_factor(UCDP$Cold_War)
summary(UCDP)

# Test und Train Data erstellen -------------------------------------------

#Version 1: Train / Test splits mit tidymodels Ansatz
set.seed(123)
splits <- UCDP %>% 
  initial_split(prop = 0.80)
splits

#Version 2: Train und Test Data (nach diesem Tutorial: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function)
## 75% of the sample size
smp_size <- floor(0.8 * nrow(UCDP))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(UCDP)), size = smp_size)

train <- UCDP[train_ind, ]
test <- UCDP[-train_ind, ]

#Version 3:
#Tutorial UC Business Analytics (https://uc-r.github.io/logistic_regression)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(UCDP), replace = T, prob = c(0.8,0.2))
train <- UCDP[sample, ]
test <- UCDP[!sample, ]


# Erste bivariate und simple Modelle interpretieren ---------------------------------

#Visualisierungen
UCDP %>% 
  ggplot(aes(ln_lag_gdp_per_cap, as.numeric(as.character(war))))+
  geom_jitter(alpha = 0.15, height = 0.03)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs(title = "Logistic Regression model fit")+
  xlab("Conflict Duration")+
  ylab("Probability of War")

UCDP %>% 
  ggplot(aes(as.numeric(as.character(recruitment)), as.numeric(as.character(war))))+
  geom_jitter(alpha = 0.15, height = 0.03)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs(title = "Logistic Regression model fit")+
  xlab("Ethnic Recruitment")+
  ylab("Probability of War")
#sieht komisch aus mit gerader Linie statt S-Kurve

#Diese beiden Visualisierungen beunruhigen mich. Für die gesamte range von Conflict Duration gibt es
#Konflikte, die eskalieren und solche die auf niedrigem Niveau verbleiben.Offenbar ist die Trennleistung
#sehr gering denn die Kurve sieht eher wie eine Gerade aus als eine typische S-Kurve wie wir sie aus 
#der logistischen Regression kennen.

#Bivariates Grundmodell nur mit der unabhängigen Variable ethn. Mobilisierung
fit <- glm(war ~ recruitment, data = train, family = "binomial")
summary(fit)
coef(fit)
#Bei einem Wechsel von nicht ethnisch mobilisierten Konflikten zu ethnischer Mobilisierung nehmen
#die Log(Odds) eines Kriegs um 0.49 zu.

exp(coef(fit))
#Bei einem Wechsel von nicht ethnisch mobilisierten zu ethnisch mobilisierten Konflikten, nehmen
#die Log(odds) um den Faktor 1.63 zu.

#Konfidenzintervalle
confint(fit)
exp(confint(fit))

#Base R plot not working properly
plot(as.numeric(as.character(train$recruitment)), as.numeric(as.character(train$war)))
lines(as.numeric(as.character(train$recruitment)[1:length(fit$fitted.values)]), fit$fitted.values)

#Bivariates Modell mit der numerischen Variable Konfliktdauer als UV
fit0 <- glm(war ~ conflict_duration, data = train, family = "binomial")
summary(fit0)
#We can further interpret the conflict duration coefficient as - for every one conflict duration increase 
#the odds of the conflict escalating to war increases by a factor of 1.63.

#Grundmodell mit 2 Prädikatoren
fit1 <- glm(war ~ recruitment + conflict_duration, data = train, family = "binomial")
summary(fit1)

#Vorhersagen machen für bivariates Modell fit
predict(fit, data.frame(recruitment = factor(c(0,1))), type = "response")
#Spricht gegen meine Hypothese, weil Verringerung in der Wahrscheinlichkeit in Krieg zu eskalieren, 
#wenn ethnisch mobilisiert wird.

#Vorhersagen machen für bivariates Modell fit0
predict(fit0, data.frame(conflict_duration = c(10,20)), type = "response")
#As you can see as the conflict_duration moves from 10 to 20 the probability of 
#war increases only slightly, from 26% to 31%

#Vorhersagen machen für multivariates Modell fit1
new.df <- tibble(recruitment = as_factor(c(0,1)), ethfrac = 0.8, conflict_duration = 20)
predict(fit1, new.df, type = "response")
#In case of a given ethfrac and conflict duration recruitment has about 6% lower probability
#of war than non-recruitment in conflicts.

#Predictions fit1 (Tutorial https://www.youtube.com/watch?v=XycruVLySDg)
res <- predict(object = fit1, train, type = "response")
res

#Validate the Model fit1 - Confusion Matrix
confmatrix <- table(Actual_Value = train$war, Predicted_Value = res > 0.5)
confmatrix #Kein 1 wurde effektiv wahrheitsgemäss mit 1 vorausgesagt, sehr ungenügend

#(confmatrix[[1,1]] + confmatrix[[2,2]]/ sum(confmatrix))

#fitted probabilities for y = 1
head(fit1$fitted.values)
yHat <- fit1$fitted.values > 0.5

#Hit Matrix
#tab <- table(train$war, yHat)
#What should I do in order for them to have the same length?

#Which Variable in a multivariate model is the most important/ most influential in 
#predicting the response Variable War?
summary(fit1)
varImp(fit1) #recruitment ist nicht die grösste, sondern conflict_duration

#Look at the Likelihood Ratio Test and compare two models
anova(fit, fit1, test = "Chisq")
#fit1 verringert die Residual Devianz und das ist gut. Wir sehen am wichtigsten, dass diese
#Anpassungsverbesserung signifikant ist. Das heisst für uns, dass das multivariate Modell 
#ein besserer Modellfit ist.

#Weitere Goodness of Fit masse: Pseudo R^2
list(fit = pscl::pR2(fit)["McFadden"],
     fit0 = pscl::pR2(fit0)["McFadden"],
     fit1 = pscl::pR2(fit1)["McFadden"])
#Alle drei Modelle sind ziemlich schlecht. McFadden ist in Ordnung ab 0.2.

#Residual Assessment
library(broom)
library(modelr)
fit1_data <- broom::augment(fit1) %>% 
  mutate(index = 1:n())

ggplot(fit1_data, aes(index, .std.resid, color = war)) +
  geom_point(alpha = .5) +
  geom_ref_line(h = 3)

fit1_data %>% 
  filter(abs(.std.resid) > 3)

#influential observation with Cook's distance values
plot(fit1, which = 4, id.n = 5)

fit1_data %>% 
  top_n(5, .cooksd)
#This means if we were to remove these observations (not recommended), the shape, 
#location, and confidence interval of our logistic regression S-curve would likely shift.

#Validation of our Predicted Values, again with a Confusion Matrix
test.predicted.fit <- predict(fit, newdata = test, type = "response")
test.predicted.fit1 <- predict(fit1, newdata = test, type = "response")

list(
  fit = table(test$war, test.predicted.fit > 0.5) %>% prop.table(),
  fit1 = table(test$war, test.predicted.fit1 > 0.5) %>% prop.table()
)
#Mega schlecht, kein Modell hat Krieg vorausgesagt wenn es tatsächlich Krieg gab.

#Hier nochmals das Ergebnis für unser bivariates Modell mit den absoluten Häufigkeiten
table(test$war, test.predicted.fit > 0.5)

test %>%
  mutate(f.pred = ifelse(test.predicted.fit > 0.5, 1,0),
         f1.pred = ifelse(test.predicted.fit1 > 0.5, 1,0)) %>%
  summarise(f.error = mean(war != f.pred),
            f1.error = mean(war != f1.pred))
#?

#ROC and AUC
library(ROCR)
par(mfrow=c(1, 2))

prediction(test.predicted.fit, test$war) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.fit1, test$war) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()
#Wie kann ich hier die NA's umgehen?

#AUC von Hand berechnen
prediction(test.predicted.fit, test$war) %>%
  performance(measure = "auc") %>%
  .@y.values
#Nur leichte Verbesserung im Vergleich zu random guessing

#Tutorial Datacamp (https://www.datacamp.com/community/tutorials/logistic-regression-R)
fit1 <- glm(war ~ recruitment + ethfrac + conflict_duration, data = train, 
            family = "binomial")
summary(fit1)

glm.probs <- predict(fit1, type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "War", "No War")

#Confusion Matrix
noNA<-((is.na(train$war)+is.na(train$recruitment)+ is.na(train$ethfrac) + 
          +           is.na(train$conflict_duration))==0)
table(train$war[noNA], glm.probs > 0.5)
#Jetzt habe ich es behoben, indem ich alle NA's in den verwendeten Variablen herausgefiltert habe.
#Gibt doch sicher einen einfacheren Weg oder nicht?

#Tutorial Datacamp (https://www.datacamp.com/community/tutorials/confusion-matrix-calculation-r)
p <- predict(fit1, train, type = "response")
summary(p)

p_war <- ifelse(p > 0.5, "War", "No War")
table(p_war)

table(train$war[noNA], p_war)
#Warum jetzt?

confusionMatrix(factor(p_war), train$war)

#Jetzt können wir einen Graph plotten
predicted.data <- data.frame(probability.of.war = fit1$fitted.values,
                             war = UCDP$war)
#Was kann ich hier machen?

# tidymodels Ansatz -------------------------------------------------------

#Kleines Modell mit 3 Predictoren (und tidymodels Ansatz https://www.youtube.com/watch?v=Qi-sVE0SWFc)
fit1 <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(war ~ recruitment + ethfrac + conflict_duration, data = training(splits))
fit1

#Interpretation

#Predictions fit1 (tidymodels approach)
prediction_class_test <- predict(fit1, new_data = testing(splits), type = "class")
prediction_prob_test <- predict(fit1, new_data = testing(splits), type = "prob")

results_tbl <- bind_cols(
  prediction_class_test,
  prediction_prob_test,
  testing(splits)) %>% 
  select(c(1:3, 7, 10, 14, 64))
#wegen den vielen NA's in recruitment und ethfrac sind viele der Predictions auch NA das ist die grösste
#Einschränkung in meinem Datensatz.

#How accurate is this Model? Evaluate with AUC
results_tbl %>% 
  roc_auc(war, .pred_0)
#Es ist nur ein wenig besser als ein random guess (das wär 0.5, maximum wäre 1)

results_tbl %>% 
  roc_curve(war, .pred_0) %>%
  autoplot(
    options = list(
      smooth = TRUE
    )
  )
#we are predicting better than the random line (which would be 0.5)

#Visualize it
fit1 %>% 
  vip(
    num_features = 20,
    geom = "point",
    aesthetics = list(size = 4, color = "#18bc9c")
  )+
  theme_minimal(base_size = 18)+
  labs(title = "Logistic Regression: Feature Importance")
#We can see more and less important features.

#Visualize the most important features
UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(recruitment, conflict_duration, color = war))+
  geom_boxplot()+
  geom_jitter(alpha = 0.25)+
  theme_minimal(base_size = 18)+
  scale_colour_viridis_d(end = 0.4)+
  labs(title = "Ethnic Mobilization and Conflict Intensification")

# Tutorial 4 Youtube ------------------------------------------------------

#Assess Model using Cross Validation (ab Min 30: https://www.youtube.com/watch?v=D1xVEi8PU-A)
library(caret)
crossValSettings <- trainControl(method = "repeatedcv", number = 10, savePredictions = T)
crossVal <- train(as.factor(war) ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
                    democ + autoc + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                  data = UCDP, family = "binomial", method = "glm", trControl = crossValSettings, 
                  na.action = na.omit)
crossVal
pred <- predict(crossVal, newdata = UCDP, na.action = na.pass)

confusionMatrix(data = pred, reference = as_factor(UCDP$war))
###!!!!!!

#Producing ROC Curve of Model (ab Minute 46 im Tutorial)
probFull <- predict(fit1, UCDP, type = "response", na.action = na.omit)
probFull
predictFull <- prediction(probFull, as_factor(UCDP$war))
#hier mache ich einen fehler!


# Schritt 1: Replikation Eck Modelle mit 2019 Daten -----------------------------------
#Hier repliziere ich die Modelle aus Tabelle 3 mit Variablen, die möglichst nah an der Originalstudie 
#von Eck kodiert wurden einfach mit den aktualisierten UCDP Daten von 1946-2019 Daten statt 1946-2004

#Table 3 Model 2 mit 2019 Daten
Eck_fit2_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                       autoc_dummy + incompatibility + Cold_War + conflict_duration,
                     data = UCDP, family = "binomial")
summary(Eck_fit2_2019)

#Table 3 Model 3 mit 2019 Daten
Eck_fit3_2019 <- glm(war ~ recruitment + ethfrac + ethfracsq + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                       autoc_dummy + incompatibility + Cold_War + conflict_duration,
                     data = UCDP, family = "binomial")
summary(Eck_fit3_2019)

#Table 3 Model 4 (mit ethnic fractionalization statt ethnic pluralism) mit 2019 Daten
Eck_fit4_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                       autoc_dummy + incompatibility + Cold_War + lag_mil_pers_t + conflict_duration,
                     data = UCDP, family = "binomial")
summary(Eck_fit4_2019) #komisch offenbar hat die Variable Militärpersonen einen grossen Einfluss auf das
#Modell. Warum gibt es hier so viel weniger Observationen als in den anderen Modellen?

#Table 3 Model 5 (ohne Duration aber mit ethfrac statt ethpluralism) mit 2019 Daten
Eck_fit5_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                       autoc_dummy + incompatibility + Cold_War,
                     data = UCDP, family = "binomial")
summary(Eck_fit5_2019)

#Table 1 Model 5 mit 2019 Daten und mit conflict duration wie bei der OLS Regression in Tabelle 3
Eck_fit6_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                       autoc_dummy + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                     data = UCDP, family = "binomial")
summary(Eck_fit6_2019)

stargazer(Eck_fit2_2019, Eck_fit3_2019, Eck_fit4_2019, 
          Eck_fit5_2019, Eck_fit6_2019, type = "text", dep.var.labels = "War",
          title = "Table 3: Severity of Armed Conflict, 1946-2019",
          digits = 2, out = "models_eck2.txt", model.names = T, 
          column.labels = c("M2 2019", "M3 2019",
                            "M4 2019", "M5 2019", "M6 2019"))

# Interpretation Eck Modelle 2019 -----------------------------------------
res2 <- predict(object = Eck_fit2_2019, UCDP, type = "response")
res2

#Validate the Model 2 - Confusion Matrix
confmatrix <- table(Predicted_Value = res2 > 0.5, Actual_Value = UCDP$war)
confmatrix
#wir haben viele (199) False Negative: Das Modell 2 hat keinen Krieg vorausgesagt, aber in Wirklichkeit
#war es Krieg. False Positives sind, wenn Krieg vorausgesagt wurde aber in Wirklichkeit kein Krieg
#stattfand.

#Modelle 2, 3, 5 und 6 vergleichen
anova(Eck_fit2_2019, Eck_fit3_2019, Eck_fit5_2019, Eck_fit6_2019, test = "Chisq")
anova(Eck_fit2_2019, Eck_fit3_2019, Eck_fit5_2019, Eck_fit6_2019, test = "LR")
#Wenn wir von Modell 2 zu Modell 6 verringert sich die Residual Devianz und das ist gut. 
#Wir sehen am wichtigsten, dass diese Anpassungsverbesserung signifikant ist. Das heisst für uns, 
#dass das multivariate Modell 6 der beste Modellfit ist.

#Überprüfen mit einem Direktvergleich von Modell 3 und Modell 6
anova(Eck_fit3_2019, Eck_fit6_2019, test = "LR")

#Weitere Goodness of Fit masse: Pseudo R^2
list(Model_2 = pscl::pR2(Eck_fit2_2019)["McFadden"],
     Model_3 = pscl::pR2(Eck_fit3_2019)["McFadden"],
     Model_4 = pscl::pR2(Eck_fit4_2019)["McFadden"],
     Model_5 = pscl::pR2(Eck_fit5_2019)["McFadden"],
     Model_6 = pscl::pR2(Eck_fit6_2019)["McFadden"])
#Alle fünf Modelle sind ziemlich schlecht. Es bestätigt sich, dass Modell 6 am besten ist und das
#höchste McFadden Pseudo R^2 besitzt. Trotzdem ist das mit einem Wert von 0.1 ziemlich schlecht.


# Schritt 2: Replikation Tabelle 1 & 3 Eck mit logarithmischen Modell UCDP Daten 2004 --------------------
#Hier nehme ich die möglichst gleiche Konstellation an Variablen wie Eck, die genau so kodiert sind.

UCDP_Eck <- UCDP %>% 
  filter(year <= 2004)

#Table 3 Model 2 mit 2004 Daten
Eck_fit2_2004 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit2_2004)

#Table 3 Model 3 mit 2004 Daten
Eck_fit3_2004 <- glm(war ~ recruitment + ethfrac + ethfracsq + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit3_2004)

#Table 3 Model 4 (mit ethnic fractionalization statt ethnic pluralism)
Eck_fit4_2004 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + lag_mil_pers_t + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit4_2004) #komisch

#Table 3 Model 5 (ohne Duration aber mit ethfrac statt ethpluralism)
Eck_fit5_2004 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit5_2004)

#Table 1 Model 5
Eck_fit6_2004 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit6_2004)

stargazer(Eck_fit2_2004, Eck_fit3_2004, Eck_fit4_2004, Eck_fit5_2004, Eck_fit6_2004, type = "text",
          dep.var.labels = "war",
          title = "Table 3: Severity of Armed Conflict, 1946-2004",
          digits = 2, out = "models_eck.txt", model.names = T)

# Interpretation Eck Modelle 2004 -----------------------------------------

list(Model_2 = pscl::pR2(Eck_fit2_2004)["McFadden"],
     Model_3 = pscl::pR2(Eck_fit3_2004)["McFadden"],
     Model_4 = pscl::pR2(Eck_fit4_2004)["McFadden"],
     Model_5 = pscl::pR2(Eck_fit5_2004)["McFadden"],
     Model_6 = pscl::pR2(Eck_fit6_2004)["McFadden"])
#Die Modelle sind noch schlechter bzw. gleich schlecht wie die 2019 Modelle.

#Jetzt kann ich eine Regressionstabelle machen, welche die 2004 Modelle mit denjenigen von 2019 vergleicht.
#Um den Vergleich zu erleichtern wurden die Modelle möglichst so gefitted wie in der Originalstudie. Ohne
#ethnische Pluralisierung

#Vergleich der Modelle
stargazer(Eck_fit2_2004, Eck_fit2_2019, Eck_fit3_2004, Eck_fit3_2019, Eck_fit4_2004, Eck_fit4_2019, 
          Eck_fit5_2004, Eck_fit5_2019, type = "text", dep.var.labels = "War",
          title = "Table 3: Severity of Armed Conflict, 1946-2019",
          digits = 2, out = "models_eck2.txt", model.names = T, 
          column.labels = c("M2 2004", "M2 2019", "M3 2004", "M3 2019",
                            "M4 2004", "M4 2019", "M5 2004", "M5 2019"))

#HTML für Word erstellen
stargazer(Eck_fit2_2004, Eck_fit2_2019, Eck_fit3_2004, Eck_fit3_2019, Eck_fit4_2004, Eck_fit4_2019, 
          Eck_fit5_2004, Eck_fit5_2019, type = "html", dep.var.labels = "War",
          title = "Tabelle 2: Replikation von Eck 2009. Konfliktintensität und ethnische Mobilisierung 1946-2019",
          digits = 2, out = "Tabelle2.html", model.names = T, style = "apsr",
          column.labels = c("M2 2004", "M2 2019", "M3 2004", "M3 2019",
                            "M4 2004", "M4 2019", "M5 2004", "M5 2019"))


# Schritt 3: Replikation Tabelle 1 & 3 mit logarithmisches Modell und 2019 Daten ----------------

#Zusätzlich zur Replikation mit denselben Variablen fitte ich hier die Modelle nochmals mit Variablen,
#die ähnlich sind aber nicht exakt gleich wie in der Originalstudie.

#Entspricht konzeptuell Modell 2 in Tabelle 3 auf Seite 383
fit2 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
            democ + autoc + incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit2)

#Entspricht konzeptuell Modell 3 in Tabelle 3
fit3 <- glm(war ~ recruitment + ethfrac + ethfracsq + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
            democ + autoc + incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit3)

#Entspricht konzeptuell Modell 4 in Tabelle 3 obwohl ich keine Variable für ethnische Pluralisierung habe
fit4 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
            incompatibility + Cold_War + ln_lag_mil_pers_tot + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit4)

#Entspricht Modell 5 in Tabelle 3 (ohne Konfliktdauer)
fit5 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
              democ + autoc + incompatibility + Cold_War,
            data = UCDP, family = "binomial")
summary(fit2)

#Entspricht Modell 5 aus Tabelle 1 (mit externer Intervention)
fit6 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
            incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit6)

#Regressionstabelle erstellen: diese kommt in die Arbeit
stargazer(fit2, fit3, fit4, fit5, fit6, 
          type = "text", dep.var.labels = "War",
          title = "Tabelle 3: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe.txt", model.names = T)

#Jetzt noch HTML Output für Word Dokument
stargazer(fit2, fit3, fit4, fit5, fit6, 
          type = "html", dep.var.labels = "War",
          title = "Tabelle 3: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "Tabelle3.html", model.names = T, style = "apsr",
          column.labels = c("Modell 2", "Modell 3", "Modell 4", "Modell 5", "Modell 6"))

# Interpretation der eigenen Modelle aus Tabelle 1 & 3 mit 2019 Daten ----------------------------

#Wie wähle ich das beste Modell aus?
list(Model_2 = pscl::pR2(fit2)["McFadden"],
     Model_3 = pscl::pR2(fit3)["McFadden"],
     Model_4 = pscl::pR2(fit4)["McFadden"],
     Model_5 = pscl::pR2(fit5)["McFadden"],
     Model_6 = pscl::pR2(fit6)["McFadden"])
#Auch wenn ich die Variablen ein wenig anders Messe (z.B. GDP per Capita oder Demokratie + Autokratie)
#verbessert sich das McFadden Pseudo R^2 nicht. Die Modelle bleiben immer noch sehr schwach. Wäre nicht
#ein Wert von 0.4 sehr gut bzw. ab 0.2 akzeptabel?

#Achtung fit4 = Modell 4 aus der Tabelle 3 hat weniger Observationen als die anderen drei Modelle
#und es ist deshalb schwierig diese Modelle zu vergleichen. Modellvergleiche kann nur Modelle vergleichen 
#mit der gleichen Anzahl Observation, deshalb fällt hier Modell 4 raus und ich fokussiere mich auf die 
#anderen Modelle der Tabellen 3 und 1 aus der Originalstudie
anova(fit5, fit6, test = "Chisq") 

#Likelihood-Ratio Test
library(lmtest)
lrtest(fit5, fit6) #gleiches Resultat, nur andere Funktion

#Offenbar ist das Modell 6 das beste, weshalb ich das genauer untersuche:
broom::tidy(fit6)
logLik(fit6) #Log Likelihood
LogRegR2(fit6) #Chi^2 = L^2 Prüfgrösse 0.09 McFadden ist nicht sehr nice
fit6$aic

confint(fit5) #Wenn es den Wert 0 beinhaltet hat die entsprechende UV in der GG keinen Effekt
coef(fit5) #Logits
exp(coef(fit5)) #Effektkoeffizienten: Sind das die Odds Ratio wie (https://www.youtube.com/watch?v=D1xVEi8PU-A) sagt?
exp(confint(fit5)) #Konfidenzintervalle Odds Ratio, wenn 1 drin dann nicht signifikant

#Hosmer-Lemeshow Test
# library(ResourceSelection)
# hoslem.test(UCDP$war, fitted(fit6), g= 10)

#Wald Test
library(survey)
regTermTest(fit6, "recruitment")
#Die erklärende Variable ethnische Mobilisierung ist signifikant.

#Variable Importance
varImp(fit6)
#Interessant ist, dass recruitment nicht die wichtigste Variable im Modell ist!

#Herausfinden, welches X ist am wichtigsten um Y vorherzusagen
list(fit2 = varImp(fit2),
     fit3 = varImp(fit3),
     fit4 = varImp(fit4),
     fit5 = varImp(fit5),
     fit6 = varImp(fit6))
#Bei den anderen Modellen das gleiche: ethnische Mobilierung ist nicht der wichtigste Prädikator.


#Validation of Predicted Values
#Classification Rate

fit6_train <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
                    incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                  data = train, family = "binomial")
pred <- predict(object = fit6_train, newdata = test, type = "response", na.action = na.exclude)
accuracy = table(pred, test$war[1:length(pred)])
sum(diag(accuracy)) / sum(accuracy)

pred <- predict(fit6_train, newdata = test, type = "response", na.action = na.pass)
pred
#confusionMatrix(table(data = pred, test$war))
str(pred)

confmatrix <- table(Predicted_Value = pred > 0.5, Actual_Value = train$war[1:length(pred)])
confmatrix
sensitivity <- confmatrix[[1,1]] / (confmatrix[[1,1]] + confmatrix[[2,1]])
sensitivity #sensitivity tells us that 95% of the conflicts where war occured were correctly identified by the Model
specificity <- confmatrix[[2,2]] / (confmatrix[[2,2]] + confmatrix[[1,2]])
specificity #this tells us that 6% of the conflicts where war didn't occur were correctly identified
#by the Logistic Regression Model

#Ich bin nicht sicher, ob ich das richtig von Hand berechnet habe.

#ROC-Curve
library(pROC)
f1 <- roc(war ~ as.numeric(as.character(recruitment)) + ethfrac + ln_lag_gdp_per_cap + 
            ln_lag_pop_tot + democ + autoc + as.numeric(as.character(incompatibility)) + 
            as.numeric(as.character(Cold_War)) + as.numeric(as.character(side_b_2nd_dummy)) + 
            conflict_duration, data = train) #geht nicht
f1 <- roc(war ~ conflict_duration, data = train)
plot(f1, col = "red")

library(ROCR)
prob <- predict(fit6_train, newdata = test, type = "response", na.action = na.exclude)
prob #Das sind die Wahrscheinlichkeiten für jede Observation, dass y = 1
pred <- prediction(prob, test$war[1:length(prob)])
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf)

auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
auc
#Das kann doch nicht sein, dass die AUC Kurve kleiner als 0.5 ist oder schon?

#K-Fold Cross Validation
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod_fit <- train(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
                   incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration, data = UCDP,
                 method="glm", family="binomial",
                 trControl = ctrl, tuneLength = 5, na.action = na.exclude)
pred = predict(mod_fit, newdata=test)
confusionMatrix(data=pred, test$war[1:length(pred)])
#Specificity = 0, wow

# Schritt 4: Weitere Modellspezifikationen -------------------------------------------

#Hier kontrolliere ich für ethnischen Konflikt = Endogenität?
fit7 <- glm(war ~ recruitment + ethnic_conflict + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
            democ + autoc + incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit7)

#Hier kontrolliere ich auf die Region: Baseline = Europa
fit8 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot +
            democ + autoc + incompatibility + Cold_War +
            middle_east + asia + africa + americas + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit8)

#Hier messe ich Demokratie nur anhand des polity2 scores und ohne die Variablen Demokratie & Autokratie
fit9 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + polity2 +
            incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit9)

#Hier messe ich die ökonomischen Effekte mit dem logarithmierten BIP Total, gelaggt um 1 Jahr
fit10 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_tot + ln_lag_pop_tot + democ + autoc +
            incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit10)

#Hier messe ich die ökonomischen Effekte nach Hegre und Sambanis (2006) mit dem jährlichen Wirtschafts-
#wachstum in Prozent, gelaggt um 1 Jahr
fit11 <- glm(war ~ recruitment + ethfrac + lag_gdp_growth + ln_lag_pop_tot + democ + autoc +
            incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit11)

#Hier messe ich die militärische Stärke statt in Militärpersonal mit den GDP Ausgaben in Prozent
fit12 <- glm(war ~ recruitment + ethfrac + lag_gdp_growth + ln_lag_pop_tot + democ + autoc +
            incompatibility + Cold_War + lag_mil_exp_gdp + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit12)

#Regressionstabelle mit den erweiterten Modellspezifikationen (andere Operationalisierung)
stargazer(fit5, fit7, fit8, fit9, fit10, fit11, fit12,
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe3.txt", model.names = T)

#Jetzt noch mit HTML für Word Dokument
stargazer(fit5, fit7, fit8, fit9, fit10, fit11, fit12,
          type = "html", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "Tabelle4.html", model.names = T, style = "apsr",
          column.labels = c("Modell 5", "Modell 7", "Modell 8", "Modell 9", "Modell 10",
                            "Modell 11", "Modell 12"))

# Interpretation der erweiterten Modelle -----------------------

list(Model_6 = pscl::pR2(fit6)["McFadden"],
     Model_7 = pscl::pR2(fit7)["McFadden"],
     Model_8 = pscl::pR2(fit8)["McFadden"],
     Model_9 = pscl::pR2(fit9)["McFadden"],
     Model_10 = pscl::pR2(fit10)["McFadden"],
     Model_11 = pscl::pR2(fit11)["McFadden"],
     Model_12 = pscl::pR2(fit12)["McFadden"])
#McFadden wird eifach nicht besser.

