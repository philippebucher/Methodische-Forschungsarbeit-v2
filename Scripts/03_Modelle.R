#Logarithmische Modelle von Konfliktintensivierung und ethnischer Mobilisierung


# Setup -------------------------------------------------------------------

library(stargazer)
library(tidyverse)
library(caret)
library(caTools)
library(descr)


#import data
UCDP <- readRDS(file = "Data/UCDP_data.rds")

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

split_dummy <- sample(c(rep(0, 0.8 * nrow(UCDP)),  # Create dummy for splitting
                        rep(1, 0.2 * nrow(UCDP))))
table(split_dummy)
train <- UCDP[split_dummy == 0,]
test <- UCDP[split_dummy == 1,]

# Erste Modelle erstellen, vergleichen und interpretieren---------------------------------
names(UCDP)

#Bivariates Grundmodell nur mit der unabhängigen Variable ethn. Mobilisierung
fit_bivariate <- glm(war ~ recruitment, data = train, family = "binomial")
summary(fit_bivariate)
#Bei einem Wechsel von nicht ethnische mobilisierten Konflikten zu ethnischer Mobilisierung nehmen
#die Log(Odds) eines Kriegs um 0.45 zu.

#Grundmodell mit 2 Predictoren
fit0 <- glm(war ~ recruitment + conflict_duration, data = train, family = "binomial")
summary(fit0)

#Kleines Modell mit 3 Predictoren
fit1 <- glm(war ~ recruitment + ethfrac + conflict_duration, data = train, family = "binomial")
summary(fit1)


# Tutorial 1 Youtube ------------------------------------------------------

#Run the test data through the model (Tutorial https://www.youtube.com/watch?v=XycruVLySDg)
res <- predict(object = fit0, newdata = test, type = "response")
res

res <- predict(object = fit0, newdata = train, type = "response")
res

#Validate the Model - Confusion Matrix
confmatrix <- table(Actual_Value = train$war, Predicted_Value = res > 0.5)
confmatrix #Kein 1 wurde effektiv wahrheitsgemäss mit 1 vorausgesagt

(confmatrix[[1,1]] + confmatrix[[2,2]]/ sum(confmatrix))


#Tutorial Datacamp (https://www.datacamp.com/community/tutorials/logistic-regression-R)
fit1 <- glm(war ~ recruitment + ethfrac + conflict_duration, data = UCDP, 
            family = "binomial")
summary(fit1)

glm.probs <- predict(fit1, type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "War", "No War")

table(glm.pred, UCDP$war)
#Was mache ich falsch? Das wäre wieder die Klassifikationstabelle von oben.


# Tutorial 2 UC Business Analytics ----------------------------------------

#Tutorial UC Business Analytics (https://uc-r.github.io/logistic_regression)
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(UCDP), replace = T, prob = c(0.8,0.2))
train <- UCDP[sample, ]
test <- UCDP[!sample, ]

fit <- glm(war ~ conflict_duration, data = train, family = "binomial")
summary(fit)
#To be precise, a one-unit increase in conflict duration is associated with an increase 
#in the log odds of war by 0.02 units.

exp(coef(fit))
#We can further interpret the conflict duration coefficient as - for every one conflict duration increase 
#the odds of the conflict escalating to war increases by a factor of 1.02.

UCDP %>% 
  ggplot(aes(ln_lag_gdp_per_cap, as.numeric(as.character(war))))+
  geom_point(alpha = 0.15)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs(title = "Logistic Regression model fit")+
  xlab("Conflict Duration")+
  ylab("Probability of War")
#Wo ist die Kurve?

UCDP %>% 
  ggplot(aes(as.numeric(as.character(recruitment)), as.numeric(as.character(war))))+
  geom_jitter(alpha = 0.15)+
  geom_smooth(method = "glm", method.args = list(family = "binomial"))+
  labs(title = "Logistic Regression model fit")+
  xlab("Ethnic Recruitment")+
  ylab("Probability of War")

# (default <- as_tibble(ISLR::Default))
# default %>%
#   mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
#   ggplot(aes(balance, prob)) +
#   geom_point(alpha = .15) +
#   geom_smooth(method = "glm", method.args = list(family = "binomial")) +
#   ggtitle("Logistic regression model fit") +
#   xlab("Balance") +
#   ylab("Probability of Default")

confint(fit)

#Vorhersagen machen
predict(fit, data.frame(conflict_duration = c(10,20)), type = "response")
#As you can see as the conflict_duration moves from 10 to 20 the probability of 
#war increases only slightly, from 26% to 31%!

#Dasselbe mit dem bivariaten Grundmodell und ethn. Mobilisierung
fit0 <- glm(war ~ recruitment, data = train, family = "binomial")
summary(fit0)

predict(fit0, data.frame(recruitment = factor(c(0,1))), type = "response")
#Nur kleine Erhöhung in der Wahrscheinlichkeit in Krieg zu eskalieren, wenn ethnisch mobilisiert
#wird.

#Which Variable in a multivariate model is the most important/ most influential in 
#predicting the response Variable War?
fit1 <- glm(war ~ recruitment + conflict_duration, data = train, family = "binomial")
summary(fit1)

varImp(fit1) #recruitment ist nicht die grösste, sondern conflict_duration

#Predictions for multivariate models
new.df <- tibble(recruitment = as_factor(c(0,1)), ethfrac = 0.8, conflict_duration = 20)
predict(fit1, new.df, type = "response")
#In case of a given ethfrac and conflict duration recruitment has about a third higher probability
#of war than non-recruitment in conflicts

#Look at the Likelihood Ratio Test and compare two models
summary(fit0)
summary(fit1)

anova(fit0, fit1, test = "Chisq")
#fit1 verringert die Residual Devianz und das ist gut. Wir sehen am wichtigsten, dass diese
#Anpassungsverbesserung signifikant ist. Das heisst für uns, dass Modell 3 ein besserer Modellfit ist.

#Weitere Goodness of Fit masse: Pseudo R^2
list(fit = pscl::pR2(fit)["McFadden"],
     fit0 = pscl::pR2(fit0)["McFadden"],
     fit1 = pscl::pR2(fit1)["McFadden"])
#Mega schlechte Modelle: poor fit.

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


#Validation of Predicted Values
test.predicted.fit0 <- predict(fit0, newdata = test, type = "response")
test.predicted.fit1 <- predict(fit1, newdata = test, type = "response")

list(
  fit0 = table(test$war, test.predicted.fit0 > 0.5) %>% prop.table(),
  fit1 = table(test$war, test.predicted.fit1 > 0.5) %>% prop.table()
)
#Mega schlecht, kein Modell hat Krieg vorausgesagt wenn es tatsächlich Krieg gab.

test %>%
  mutate(f0.pred = ifelse(test.predicted.fit0 > 0.5, 1,0),
         f1.pred = ifelse(test.predicted.fit1 > 0.5, 1,0)) %>%
  summarise(f0.error = mean(war != f0.pred),
            f1.error = mean(war != f1.pred))


table(test$war, test.predicted.fit0 > 0.5) #Raw values not percentages of the Confusion Matrix

#ROC
library(ROCR)
par(mfrow=c(1, 2))

prediction(test.predicted.fit0, test$war) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()

prediction(test.predicted.fit1, test$war) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot()
#Habe wieder einmal so viele NA's dass es alles versaut

#AUC von Hand berechnen
prediction(test.predicted.fit0, test$war) %>%
  performance(measure = "auc") %>%
  .@y.values


# Tutorial 3 Youtube ------------------------------------------------------

#Tutorial Logistic Regression in R dplyr Ansatz (https://www.youtube.com/watch?v=Qi-sVE0SWFc)
library(tidymodels)
library(vip)

#Train / Test splits
set.seed(123)
splits <- UCDP %>% 
  initial_split(prop = 0.80)
splits

#Modelling
model_fit_glm <- logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(war ~ recruitment + ethfrac + conflict_duration, data = training(splits))

model_fit_glm

#Predictions
prediction_class_test <- predict(model_fit_glm, new_data = testing(splits), type = "class")
prediction_prob_test <- predict(model_fit_glm, new_data = testing(splits), type = "prob")

results_tbl <- bind_cols(
  prediction_class_test,
  prediction_prob_test,
  testing(splits)) %>% 
  select(c(1:3, 7, 10, 14, 64))


#How accurate is this Model? Evaluate with AUC
results_tbl %>% 
  roc_auc(war, .pred_0)
#Lol es ist nur ein wenig mehr als random guess (das wär 0.5, maximum wäre 1)

results_tbl %>% 
  roc_curve(war, .pred_0) %>%
  autoplot(
    options = list(
      smooth = TRUE
    )
  )
#we are predicting better than the random line


#Visualize it
model_fit_glm %>% 
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
  labs(title = "Pew Pew Pew")



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

#Producing ROC Curve of Model (ab Minute 46 im Tutorial)
library(ROCR)
probFull <- predict(fit5, UCDP, type = "response",na.action = na.omit)
probFull
predictFull <- prediction(probFull, as_factor(UCDP$war))
#hier mache ich einen fehler!

# Tutorial 5 Youtube Statquest --------------------------------------------

#Statquest Tutorial:
#McFadden Pseudo R^2 können wir auch wie bei Statquest berechnen (https://www.youtube.com/watch?v=C4N3_XJJ-jU)
ll.null <- fit4$null.deviance/-2
ll.proposed <- fit4$deviance/-2
(ll.null-ll.proposed)/ ll.null #Poor fit

#damit können wir den p-Wert für dieses R^2 berechnen
1- pchisq(2*(ll.proposed-ll.null), df = (length(fit4$coefficients)-1))
#In this case the p-Value is tiny, so the R^2 value isn't due to dumb luck

#Jetzt können wir einen Graph plotten
predicted.data <- data.frame(probability.of.war = fit4$fitted.values,
                             war = UCDP$war)
#Was kann ich hier machen???

# Schritt 1: Replikation Tabelle 1 & 3 mit logarithmisches Modell und 2019 Daten ----------------

#Neu habe ich noch die Variable conflict_duration eingefügt, dann verschwinden viele Zusammenhänge!
#Hier in diesem Abschnitt kann ich verschiedene Variablen und Modelle ausprobieren wie z.B. Konzepte
#unterschiedlich messen (z.B. GDP in drei Formen)

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

#Entspricht Modell 5 aus Tabelle 1 (mit externer Intervention)
fit5 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit5)

#Regressionstabelle erstellen: diese kommt in die Arbeit
stargazer(fit2, fit3, fit4, fit5, 
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe.txt", model.names = T)

#Acht geben auf fit4 = Modell4 aus der Tabelle3 hat weniger Observationen als die anderen drei Modelle
#und es ist deshalb schwierig diese Modelle zu vergleichen

# Schritt 2: Interpretation der Modelle aus Tabelle 1 & 3 mit 2019 Daten ----------------------------

#Wie wähle ich das beste Modell aus?

#Analyse des Modells 5 (weil kleinste Log-Likelihood)
broom::tidy(fit5)
logLik(fit5) #Log Likelihood
fit5$aic

confint(fit5) #Wenn es den Wert 0 beinhaltet hat die entsprechende UV in der GG keinen Effekt
coef(fit5) #Logits
exp(coef(fit5)) #Effektkoeffizienten: Sind das die Odds Ratio wie (https://www.youtube.com/watch?v=D1xVEi8PU-A) sagt?
exp(confint(fit5)) #Konfidenzintervalle Odds Ratio, wenn 1 drin dann nicht signifikant

LogRegR2(fit5) #Chi^2 = L^2 Prüfgrösse 0.09 McFadden ist nicht sehr nice
fit5$null.deviance - fit4$deviance

#Modellvergleiche kann nur Modelle vergleichen mit der gleichen Anzahl Observation, deshalb fällt
#hier Modell 4 raus und ich fokussiere mich auf die anderen Modelle der Tabelle aus der Originalstudie
anova(fit2, fit3, fit5, test = "Chi")
#Die Residualdevianz des dritten Modells (fit5) ist kleiner als bei fit2 und fit3 und 
#somit bessere Anpassungleistung.
#Die Verbesserung von fit2 auf fit3 ist aber NICHT signifikant

#Plotten
ggplot(data = UCDP, aes(x = as.numeric(as.character(recruitment)), y = as.numeric(as.character(war)))) +
  geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Conflict Intensity")+
  xlab("Recruitment")
#sieht komisch aus mit gerader Linie statt S-Kurve

#Herausfinden, welches X ist am wichtigsten um Y vorherzusagen
list(fit2 = varImp(fit2),
     fit3 = varImp(fit3),
     fit4 = varImp(fit4),
     fit5 = varImp(fit5))
#Wir sehen, dass recruitment, d.h. unsere primäre erklärende Variable in diesen Modellen
#nicht am wichtigsten ist.

#Goodness of Fit: Pseudo R^2 mit McFadden
list(fit2 = pscl::pR2(fit2)["McFadden"],
     fit3 = pscl::pR2(fit3)["McFadden"],
     fit4 = pscl::pR2(fit4)["McFadden"],
     fit5 = pscl::pR2(fit5)["McFadden"])
#0.4 wäre ein guter Wert offenbar sind die Modelle gar nicht gut! Poor fit


# Schritt 3: Weitere Modellspezifikationen -------------------------------------------

#Hier kontrolliere ich für ethnischen Konflikt = Endogenität?
fit6 <- glm(war ~ recruitment + ethnic_conflict + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
              democ + autoc + incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit6)

#Hier kontrolliere ich auf die Region: Baseline = Europa
fit7 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot +
              democ + autoc + incompatibility + Cold_War +
              middle_east + asia + africa + americas + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit7)

#Hier messe ich Demokratie nur anhand des polity2 scores und ohne die Variablen Demokratie & Autokratie
fit8 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + polity2 +
              incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit8)

#Hier messe ich die ökonomischen Effekte mit dem logarithmierten BIP Total, gelaggt um 1 Jahr
fit9 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_tot + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit9)

#Hier messe ich die ökonomischen Effekte nach Hegre und Sambanis (2006) mit dem jährlichen Wirtschafts-
#wachstum in Prozent, gelaggt um 1 Jahr
fit10 <- glm(war ~ recruitment + ethfrac + lag_gdp_growth + ln_lag_pop_tot + democ + autoc +
               incompatibility + Cold_War + conflict_duration,
             data = UCDP, family = "binomial")
summary(fit10)

#Hier messe ich die militärische Stärke statt in Militärpersonal mit den GDP Ausgaben in Prozent
fit11 <- glm(war ~ recruitment + ethfrac + lag_gdp_growth + ln_lag_pop_tot + democ + autoc +
               incompatibility + Cold_War + lag_mil_exp_gdp + conflict_duration,
             data = UCDP, family = "binomial")
summary(fit11)

#Regressionstabelle mit den erweiterten Modellspezifikationen (andere Operationalisierung)
stargazer(fit5, fit6, fit7, fit8, fit9, fit10, fit11,
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe3.txt", model.names = T)


# Schritt 4: Interpretation der erweiterten Modelle -----------------------


# Schritt 5: Replikation Tabelle 1 & 3 Eck mit logarithmischen Modell UCDP Daten 2004 --------------------
#Hier nehme ich die möglichst gleiche Konstellation an Variablen wie Eck, die genau so kodiert sind.

UCDP_Eck <- UCDP %>% 
  filter(year <= 2004)
names(UCDP_Eck)

#Table 3 Model 2 mit 2004 Daten
Eck_fit2 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit2)

#Table 3 Model 3 mit 2004 Daten
Eck_fit3 <- glm(war ~ recruitment + ethfrac + ethfracsq + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit3)

#Table 3 Model 4 (mit ethnic fractionalization statt ethnic pluralism)
Eck_fit4 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + lag_mil_pers_t + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit4) #weird

#Table 3 Model 5 (ohne Duration aber mit ethfrac statt ethpluralism)
Eck_fit5 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit5)

#Table 1 Model 5
Eck_fit6 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit6)

stargazer(Eck_fit2, Eck_fit3, Eck_fit4, Eck_fit5, type = "text", dep.var.labels = "war",
          title = "Table 3: Severity of Armed Conflict, 1946-2004",
          digits = 2, out = "models_eck.txt", model.names = T)


# Schritt 6: Interpretation der Eck Modelle mit 2004 Daten --------------------------------------------------------------




# Schritt 7: Eck Modelle mit 2019 Daten -----------------------------------
#Hier nehme ich wiederum die Modelle aus Tabelle 3, die möglichst nah an der Originalstudie von Eck
#kodiert wurden einfach mit 2019 Daten statt 2004


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
summary(Eck_fit4_2019) #weird

#Table 3 Model 5 (ohne Duration aber mit ethfrac statt ethpluralism) mit 2019 Daten
Eck_fit5_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War,
                data = UCDP, family = "binomial")
summary(Eck_fit5_2019)

#Table 1 Model 5 mit 2019 Daten
Eck_fit6_2019 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                data = UCDP, family = "binomial")
summary(Eck_fit6_2019)

#Jetzt kann ich eine Regressionstabelle machen, welche die 2004 Modelle mit denjenigen von 2019 vergleicht.
#Um den Vergleich zu erleichtern wurden die Modelle möglichst so gefitted wie in der Originalstudie. Ohne
#ethnische Pluralisierung

stargazer(Eck_fit2, Eck_fit2_2019, Eck_fit3, Eck_fit3_2019, Eck_fit4, Eck_fit4_2019, 
          Eck_fit5, Eck_fit5_2019, type = "text", dep.var.labels = "War",
          title = "Table 3: Severity of Armed Conflict, 1946-2019",
          digits = 2, out = "models_eck2.txt", model.names = T, 
          column.labels = c("M2 2004", "M2 2019", "M3 2004", "M3 2019",
                            "M4 2004", "M4 2019", "M5 2004", "M5 2019"))



