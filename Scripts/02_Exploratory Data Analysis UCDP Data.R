# Datawrangling, Analyse und Visualisierungen für Kapitel 5 der Methodischen Forschungsarbeit

# Setup -------------------------------------------------------------------

#load packages
library(tidyverse)
library(devtools)
#install_github("guyschvitz/dprtools")
library(dprtools) #package Guy

#import data
UCDP <- readRDS(file = "Data/UCDP_data.rds")

# Übersicht über das UCDP Dataset ----------------------

summary(UCDP$year)

UCDP %>% 
  summarise(conflicts = n_distinct(conflict_id),
            locations = n_distinct(location),
            governments = n_distinct(side_a)) #die location und government stimmen überein
#In unserem Datensatz haben wir 221 Konflikte, die in 106 Ländern ausgebrochen sind.

#In welchem Land sind wieviele Konflikte ausgebrochen?
UCDP %>% 
  group_by(location) %>%
  summarise(number_conflicts = n_distinct(conflict_id)) %>% 
  arrange(desc(number_conflicts)) %>% 
  top_n(5)

#Anzahl Konflikte pro Jahr zwischen 1946 und 2019
UCDP %>% 
  group_by(year) %>% 
  summarise(count_conflict = n()) %>% 
  ggplot()+
  geom_line(aes(x = year, y = count_conflict), color = "black", lty = 1)

#Welche Konflikte dauern besonders lang?
longest_conflicts <- UCDP %>% 
  group_by(conflict_id) %>%
  summarise(
    first_year = first(year),
    last_year = last(year),
    duration = last_year-first_year,
  ) %>% 
  arrange(desc(duration)) %>% 
  top_n(10)
longest_conflicts
#Wir sehen ein paar Konflikte, welche sich über den ganzen Datensatz ziehen (209 ist z.B Philippinen). 
#Ist aber eine andere Messung als die Variable conflict_duration in unserem Datensatz, welche dem 
#Vorgehen von Eck folgt. Z.B. bei einer inaktiven Zeit von 10 Jahren wird die Konfliktddauer zurückgesetzt und startet von neuem.

# Missings ----------------------------------------------------------------

#Überblick über die Missings verschaffen
dprtools::plotNAs(data = UCDP)
dprtools::plotGroupedNAs(data = UCDP, groupvar = year)
dprtools::plotGroupedNAs(data = UCDP, groupvar = location)

#Andere Art Missings anzuschauen
Amelia::missmap(UCDP, col = c("blue4", "red4"), legend = T)

# Verteilung der abhängigen Variable Krieg --------------------------------

UCDP %>% 
  group_by(conflict_id, location) %>% 
  count(war)
#Das kann ein Problem sein, wenn ein Konflikt wie Nr 209 mehrmals in Krieg eskaliert.
#Die ethnische Mobilisierung bleibt über den ganzen Konflikt gleich, unser Modell erklärt ja dann
#nicht ob ethnische Mobilisierung nur für die erste Eskalation (Stufe Krieg) verantwortlich ist oder
#für spätere Intensivierungen auch.

UCDP %>% 
  count(location, wt = war)

UCDP %>% 
  group_by(conflict_id, location) %>% 
  summarise(
    count = n(),
    total_wars = sum(war),
    share_wars = total_wars/count) %>% 
  arrange(desc(total_wars))
#Jetzt sehen wir, wieviele Observationen es pro Konflikt gibt und wieviele davon eskaliert in
#Krieg eskaliert sind. Daraus können wir den Anteil berechnen. 
#Beispiel: 93% aller Observationen von Afghanistan (ID = 333) betreffen Krieg.

#Wieviele Konflikte sind während ihrer ganzen Dauer in Krieg eskaliert und wieviele nicht?
UCDP %>% 
  group_by(conflict_id, location) %>% 
  summarise(war = max(war)) %>% 
  ggplot()+
  geom_bar(aes(x = war))
#Die Mehrheit der 221 Konflikte zwischen 1946 und 2019 ist auf niedrigem Intensitätsniveau verblieben

UCDP %>% 
  ggplot(aes(x = war))+
  geom_bar()
#Wenn wir uns dazu nur die Observationen anschauen, dann sehen wir, dass die überwiegende Mehrheit der
#Observationen auf niedrigem Intensitätsniveau sind und nur eine kleine Anzahl der Observationen Krieg
#bezeichnen

UCDP %>% 
  count(war)
#Die Kategorien der abhängigen Variable sind ungleich verteilt, wie sieht es mit der unabhängigen 
#Variable aus?

# Verteilung unabhängige Variable ethnische Mobilisierung -----------------

#Wieviele Konflikte haben entlang ethnischen Linien mobilisiert/ rekrutiert?
UCDP %>% 
  group_by(conflict_id, location) %>% 
  summarise(recruitment = max(recruitment)) %>% 
  ggplot()+
  geom_bar(aes(x = recruitment))
#Eine deutliche Mehrheit der Konflikte hat entlang ethnischen Linien mobilisiert.

UCDP %>% 
  group_by(conflict_id) %>% 
  ggplot(aes(x = recruitment))+
  geom_bar()
count(UCDP, recruitment)
#Schauen wir uns nur die Observationen an, dann sehen wir, dass sie sehr ungleich verteilt sind.
#Auch fallen uns die vielen fehlenden Werte auf!

# Kontrollvariablen untersuchen -------------------------------------------

#Besonders Häufigkeiten anschauen, um zu sehen ob die Variable im Datensatz etwa gleich verteilt ist.

#Was ist mit unserer ethnic_conflict variable?
UCDP %>% 
  count(ethnic_conflict) #ungleich verteilt

UCDP %>% 
  count(incompatibility) #gleich verteilt

UCDP %>% 
  count(Cold_War) #gleich verteilt

UCDP %>% 
  count(region)

#ethnic fractionalization
summary(UCDP$ethfrac)
range(UCDP$ethfrac, na.rm = T)

ggplot(data = UCDP)+
  geom_histogram(aes(x = ethfrac), bins = 10, 
                 fill="#69b3a2", color="#e9ecef", alpha=0.9)

ggplot(data = UCDP, mapping = aes(x = ethfrac, y = ..density..))+
  geom_freqpoly(aes(color = as_factor(war)))
#Wenn wirs so machen, dann sehen wir dass es offenbar keinen Unterschied macht wie hoch die
#ethnische Fragmentierung ist, ob ein Konflikt intensiviert oder nicht.

#Andere Möglichkeit für um die Kovariation zu untersuchen sind boxplots
#Hier der Zusammenhang zwischen ethnischer Fragmentierung und Krieg
ggplot(data = UCDP)+
  geom_boxplot(aes(x = ethfrac, fill = as_factor(war)))+
  coord_flip()
#Die gruppierten Boxplots zeigen, dass Konflikte, die in Krieg eskaliert sind einen höheren
#Median haben.

#Zusammenhang von ethnischer Fragmentierung und ethnischer Rekrutierung
UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot()+
  geom_boxplot(aes(x = ethfrac, fill = as_factor(recruitment)))+
  coord_flip()
#Wie interpretiere ich die ethnische Fragmentierung?

# Korrelationen -------------------------
#(Tutorial: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software)
cor(UCDP$war, UCDP$recruitment, use = "complete.obs") #minimale Korrelation

correlations <- UCDP %>% 
  select_if(is.numeric) %>% 
  select(c(3,6,10,18:21,24,27,31,33,38,42,48)) #nur numerisch relevante Variablen auswählen

res <- Hmisc::rcorr(as.matrix(correlations))

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(res$r, res$P) %>% 
  filter(row == c("war", "recruitment")) %>% 
  arrange(desc(cor))
#Wir sehen hier, dass die Korrelationen zwischen war und recruitment und den anderen Variablen
#extrem klein sind.

#Korrelationen visuell darstellen
corrplot::corrplot(res$r, order="hclust", p.mat = res2$P, 
                   sig.level = 0.05, insig = "blank") 
#Viele der oben genannten Korrelationen sind gar nicht überhaupt nicht signifikant

#Dasselbe als Heatmap
heatmap(x = res$r, col = c("blue", "white", "red"), symm = T)


#Was mache ich, wenn die Variablen überhaupt nicht korrelieren? Der Grund dafür könnte in den vielen
#Missings liegen.




# Explorative Datenanalyse Bivariate Verteilungen: Visualisierungen -------
UCDP %>% 
  select_if(is.numeric) %>% 
  select(c(10:11, 18:21, 29, 30, 32, 35, 40, 43)) %>% 
  pairs(.)
#Übersicht über die Beziehungen zwischen allen numerischen Variablen, it's a mess!

#Bivariate Visualisierungen mit generic plot funciton von ggplot
qplot(x = ln_lag_gdp_per_cap, y = as_factor(war), data = UCDP) #offenbar teilt das Modell nicht gut,
#weil auf allen Ebenen von GDP kommen Kriege und Nicht-Kriege vor.
qplot(x = ethfrac, y = as_factor(war), data = UCDP)
qplot(x = ethfrac, y = as_factor(war), data = UCDP)
qplot(x = conflict_duration, y = as_factor(war), data = UCDP)
qplot(x = conflict_duration, y = as_factor(war), data = UCDP)
qplot(x = as_factor(region), y = as_factor(war), data = UCDP)
qplot(x = polity2, y = as_factor(war), data = UCDP)
qplot(x = ln_lag_pop_tot, y = as_factor(war), data = UCDP)
qplot(x = conflict_duration, y = ethfrac, data = UCDP)

ggplot(data = UCDP, aes(x = conflict_duration, y = ethfrac))+
  geom_jitter(aes(color = as_factor(war)))
#Was sehen wir hier? Nicht viel, weil kein klares Muster

ggplot(data = UCDP, aes(x = ethfrac))+
  geom_histogram(aes(fill = as_factor(war)))
#Ziemlich gleichverteilt oder wie?

UCDP %>% 
  ggplot(aes(x = conflict_duration, y = ethfrac))+
  geom_point(position = "jitter")+
  geom_smooth(aes(color = as_factor(war)))
#nicht sehr aussagekräftig

ggplot(data = UCDP, aes(x = conflict_duration, y = ln_lag_gdp_per_cap))+
  geom_jitter()+
  facet_wrap(~ as_factor(war), ncol = 2)
#Die Beziehung zwischen GDP und Konfliktddauer sieht gleich aus wenn wir die Grafik nach
#Krieg aufsplitten.

UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(x = conflict_duration, y = ln_lag_gdp_per_cap))+
  geom_jitter()+
  facet_grid(as_factor(recruitment) ~ as_factor(war))

#Jetzt noch dasselbe mit facet_grid und der UV und der AV.

UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(x = as_factor(recruitment)))+
  geom_bar(aes(fill = as_factor(war)), stat = "count")

UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(x = as_factor(recruitment)))+
  geom_bar(aes(fill = as_factor(war)), position = "dodge")

UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(x = as_factor(recruitment)))+
  geom_bar(aes(fill = as_factor(war)), position = "fill")
#Das macht es einfacher Verhältnisse über Gruppen besser zu verstehen. Konflikte, die entlang
#ethnischen Linien mobilisieren sind häufiger in Krieg eskaliert, als solche die nicht entlang
#ethnischen Linien mobilisiert haben. 

ggplot(data = UCDP)+
  geom_count(mapping = aes(x = as_factor(recruitment), y = as_factor(war)))

UCDP %>% 
  drop_na(recruitment) %>% 
  count(recruitment, war)
#Das spricht aber überhaupt nicht für meine These, dass ethnische Mobilisierung einen Einfluss auf
#die Konfliktintensität hat.

UCDP %>% 
  drop_na(recruitment) %>% 
  count(recruitment, war) %>% 
  ggplot(aes(x = recruitment, y = war))+
  geom_tile(aes(fill = n))
#super aussagekräftig, wowowow

UCDP %>% 
  count(region, war) %>% 
  ggplot(aes(x = region, y = war))+
  geom_tile(aes(fill = n))






# Schritt 1: Erweiterte Analyse logarithmisches Modell mit 2019 Daten ----------------
names(UCDP)
#Neu habe ich noch die Variable conflict_duration eingefügt, dann verschwinden viele Zusammenhänge!
#Hier in diesem Abschnitt kann ich verschiedene Variablen und Modelle ausprobieren wie z.B. Konzepte
#unterschiedlich messen (z.B. GDP in drei Formen)

#Grundmodell nur mit der UV und der Konfliktdauer
fit_base <- glm(war ~ recruitment + conflict_duration, data = UCDP, family = "binomial")
summary(fit_base)
#Bei einem Wechsel von nicht ethnische mobilisierten Konflikten zu ethnischer Mobilisierung nehmen
#die Log(Odds) eines Kriegs um 0.76 zu.

#Bivariates Modell
fit0 <- glm(war ~ recruitment, data = UCDP, family = "binomial")
summary(fit0)

#Kleines Modell
fit1 <- glm(war ~ recruitment + ethfrac + conflict_duration, data = UCDP, family = "binomial")
summary(fit1)

#Entspricht konzeptuell Modell 2 in Tabelle 3 auf Seite 383
fit2 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
              democ + autoc + incompatibility + Cold_War + conflict_duration,
            data = UCDP, family = "binomial")
summary(fit2)
broom::tidy(fit2)

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

#Vergleich des Modells 4 aus Tabelle 3 mit den 2004 Daten von Eck
fit4_2004 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
                  incompatibility + Cold_War + ln_lag_mil_pers_tot + conflict_duration,
                  data = UCDP_Eck, family = "binomial")
summary(fit4_2004)

#Entspricht Modell 5 aus Tabelle 1 (mit externer Intervention)
fit5 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
              data = UCDP, family = "binomial")
summary(fit5)

#Weitere Modellspezifikationen
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

#Aufbauend auf Modell4, das die kleinste LogLikelihood hat: Wird das Modell noch besser?
#polity2 score hinzugefügt: WARUM gibt es NA's?
fit4_1 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
                polity2 + incompatibility + Cold_War + ln_lag_mil_pers_tot + 
                conflict_duration, data = UCDP, family = "binomial")
summary(fit4_1)
summary(UCDP$polity2)

#polity2 score und Regionen hinzugefügt
fit4_2 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
                UCDP$polity2 + incompatibility + Cold_War + ln_lag_mil_pers_tot + 
                middle_east + asia + africa + americas + conflict_duration,
              data = UCDP, family = "binomial")
summary(fit4_2)

#Regressionstabelle erstellen
stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, 
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe.txt", model.names = T)


#Zweite Regressionstabelle mit weiteren Modellspezifikationen erstellen
stargazer(fit4, fit8, fit9, fit10, fit11, 
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe2.txt", model.names = T)

#Dritte Regressionstabelle mit Modell 4 und weiteren Spezifikationen erstellen
stargazer(fit4, fit4_1, fit4_2, 
          type = "text", dep.var.labels = "War",
          title = "Tabelle 1: Risiko der Konfliktintensivierung 1946-2019",
          digits = 2, out = "models_philippe3.txt", model.names = T)


# Schritt 2: Interpretation der eigenen Modelle mit 1946-2019 Daten ----------------------------
#Analyse des Modells 4 (weil kleinste Log-Likelihood) Vielleicht ist das doch nicht gut
#Weil Modell 4 hat mit 669 Observationen, weniger als die anderen Modelle mit 1274. Evt.
#beeinflusst das die Modellgüte
broom::tidy(fit4)
logLik(fit4) #Log Likelihood
fit4$aic

confint(fit4) #Wenn es den Wert 0 beinhaltet hat die entsprechende UV in der GG keinen Effekt
coef(fit4) #Logits
exp(coef(fit4)) #Effektkoeffizienten: Sind das die Odds Ratio wie (https://www.youtube.com/watch?v=D1xVEi8PU-A) sagt?
exp(confint(fit4)) #Konfidenzintervalle Odds Ratio, wenn 1 drin dann nicht signifikant

LogRegR2(fit4) #Chi^2 = L^2 Prüfgrösse 0.16 McFadden ist nicht sehr nice
fit4$null.deviance - fit4$deviance

#Modellvergleiche kann nur Modelle vergleichen mit der gleichen Anzahl Observation, deshalb fällt
#hier Modell 4 auf
anova(fit5, fit6, fit7, test = "Chi")
anova(fit5, fit6, fit7, test = "Chisq")
#Die Residualdevianz des dritten Modells (fit7) ist kleiner als bei fit5 und fit6 und 
#somit bessere Anpassungleistung.
#Die Verbesserung von fit5 auf fit7 ist aber nur auf dem 0.1 Level signifikant. D.h. kleine Anpassung?

#Plotten
ggplot(data = UCDP, aes(x = recruitment, y = war)) +
  geom_point()+
  # geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Conflict Intensity")+
  xlab("Recruitment")
#sieht komisch aus mit gerader Linie statt S-Kurve

ggplot(data = UCDP, aes(x = recruitment, y = war)) +
  geom_point()+
  # geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "lm", se = FALSE) +
  ylab("Conflict Intensity")+
  xlab("Recruitment")

#Test stattdessen mit ethnischer Fragmentierung
ggplot(data = UCDP, aes(x = ethfrac, y = war)) +
  geom_point()+
  # geom_jitter(alpha = 0.1, height = 0.05) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Conflict Intensity")+
  xlab("Ethnic Fractionalization")

#Test stattdessen mit Polity score
ggplot(data = UCDP, aes(x = polity2, y = war)) +
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ylab("Conflict Intensity")+
  xlab("Polity2 Score")+
  ylim(c(0,1))
#Wir sehen, dass die Trennung nicht scharf ist und es für die gesamte range von polity2
#Konflikte gibt, die Eskalieren und solche die auf einer niedrigen Intensität bleiben.

#Jetzt kann ich mal in einem bivariaten Modell mit einer dichotomen UV die predict Funktion verwenden
summary(fit0)
predict(fit0, data.frame(recruitment = c(0,1)), type = "response")
#Wenn wir von nicht ethnisch mobilisierten Konflikten zu ethnisch mobilisierten Konflikten springen, 
#erhöht sich die Wahrscheinlichkeit eines Kriegs von 14% auf 18%. Ist diese Interpretation richtig?
#Die Rekrutierung entlang ethnischen Linien hat die doppelten Odds für Krieg als nicht ethnisch
#rekrutierte Konflikte.
#Beispiel: "As you can see as the balance moves from $1000 to $2000 the probability of defaulting 
#increases signficantly, from 0.5% to 58%!"

#Mit predict könnte ich auch multivariate Modelle vorhersagen, aber dazu müsste ich für jede unabhängige
#Variable einen Wert definieren
summary(fit2)
new.df <- tibble(recruitment = c(0,1), ethfrac = 0.7, ln_lag_gdp_per_cap = 3, ln_lag_pop_tot = 20, 
                 democ = 0, autoc = 10, incompatibility = 1, Cold_War = 0, conflict_duration = 10.99)
predict(fit2, new.df, type = "response")
#Wie interpretiere ich jetzt das Resultat? Die Wahrscheinlichkeit eines Krieges erhöht sich von 3% auf
#15%?


#Herausfinden, welches X ist am wichtigsten um Y vorherzusagen
caret::varImp(fit4)
#Wir sehen, dass recruitment, d.h. unsere primäre erklärende Variable in diesem Modell
#nicht am wichtigsten ist.


#Goodness of Fit: Pseudo R^2 mit McFadden
list(fit2 = pscl::pR2(fit2)["McFadden"],
     fit3 = pscl::pR2(fit3)["McFadden"],
     fit4 = pscl::pR2(fit4)["McFadden"])
#0.4 wäre ein guter Wert offenbar sind die Modelle gar nicht gut! Poor fit


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


#gleich wie bei linearer Regression können wir wichtige Observationen identifizieren mit
#Cook's distance values. Hier identifizieren wir die 5 grössten Werte.
plot(fit4, which = 4, id.n = 5)











# Replikation Resultate Eck mit logarithmischen Modell UCDP Daten 2004 --------------------

#Hier nehme ich die möglichst gleiche Konstellation an Variablen wie Eck, die genau so kodiert sind.
#Erst im zweiten Schritt, dem Vergleich mit meinen 2019 Daten nehme ich meine eigens kodierten Variablen
#und Präferenzen.
names(UCDP_Eck)

#Table 1 Model 2 mit 2004 Daten
Eck_fit2 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit2)

#Table 1 Model 3 mit 2004 Daten
Eck_fit3 <- glm(war ~ recruitment + ethfrac + ethfracsq + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit3)

#Table 1 Model 4 
Eck_fit4 <- glm(war ~ recruitment + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + lag_mil_pers_t + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit4) #weird

#Table 1 Model 5
Eck_fit5 <- glm(war ~ recruitment + ethfrac + lag_gdp_per_cap_t + ln_lag_pop_t + democ_dummy +
                  autoc_dummy + incompatibility + Cold_War + side_b_2nd_dummy + conflict_duration,
                data = UCDP_Eck, family = "binomial")
summary(Eck_fit5)

stargazer(Eck_fit2, Eck_fit3, Eck_fit4, Eck_fit5, type = "text", dep.var.labels = "war",
          title = "Table 1: Risk of Conflict Intensification 1946-2019",
          digits = 2, out = "models_eck.txt", model.names = T)



# Erweiterte Analyse logarithmisches Modell mit 2019 Daten ----------------
names(UCDP)
#Jetzt auch noch ohne conflict_duration und dann die Modelle vergleichen

fit1 <- glm(war ~ recruitment, data = UCDP, family = "binomial")
summary(fit1)

fit2 <- glm(war ~ recruitment + ethfrac, data = UCDP, family = "binomial")
summary(fit2)

fit3 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War,
            data = UCDP, family = "binomial")
summary(fit3)
broom::tidy(fit3)

fit4 <- glm(war ~ recruitment + ethfrac + ethfracsq + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War,
            data = UCDP, family = "binomial")
summary(fit4)

fit5 <- glm(war ~ recruitment + ethnic_conflict + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + 
              democ + autoc + incompatibility + Cold_War,
            data = UCDP, family = "binomial")
summary(fit5)

fit6 <- glm(war ~ recruitment + ethnic_conflict + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot +
              democ + autoc + incompatibility + Cold_War +
              middle_east + asia + africa + americas,
            data = UCDP, family = "binomial")
summary(fit6)

fit7 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War + ln_lag_mil_pers_tot,
            data = UCDP, family = "binomial")
summary(fit7)

fit8 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + democ + autoc +
              incompatibility + Cold_War + side_b_2nd_dummy,
            data = UCDP, family = "binomial")
summary(fit8)

fit9 <- glm(war ~ recruitment + ethfrac + ln_lag_gdp_per_cap + ln_lag_pop_tot + polity2 +
              incompatibility + Cold_War,
            data = UCDP, family = "binomial")
summary(fit9)

stargazer(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, 
          type = "text", dep.var.labels = "war",
          title = "Table 1: Risk of Conflict Intensification 1946-2019",
          digits = 2, out = "models_no_duration.txt", model.names = T)



# Jetzt kann ich die Modelle mit und ohne Dauer des Konflikts verg --------

stargazer(fit7, fit7_d, fit8, fit8_d, fit9, fit9_d,
          type = "text", dep.var.labels = "war",
          title = "Table 1: Risk of Conflict Intensification 1946-2019",
          digits = 2, out = "models_comparison.txt", model.names = T)


