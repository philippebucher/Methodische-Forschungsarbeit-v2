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


# Bivariate Verteilungen: Visualisierungen -------
correlations %>% 
  pairs(.)
#Übersicht über die Beziehungen zwischen allen numerischen Variablen, It's a mess!


#Bivariate Visualisierungen mit generic plot funciton von ggplot
qplot(x = ln_lag_gdp_per_cap, y = as_factor(war), data = UCDP)
qplot(x = ethfrac, y = as_factor(war), data = UCDP)
qplot(x = ethfrac, y = as_factor(war), data = UCDP)
qplot(x = conflict_duration, y = as_factor(war), data = UCDP)
qplot(x = conflict_duration, y = as_factor(war), data = UCDP)
qplot(x = as_factor(region), y = as_factor(war), data = UCDP)
qplot(x = polity2, y = as_factor(war), data = UCDP)
qplot(x = ln_lag_pop_tot, y = as_factor(war), data = UCDP)
#Meine erste Überlegung ist, dass unsere Daten überhaupt nicht gut klassifizieren weil es z.B.
#Grössen von GDP per capital Konflikte gab die in Kriege eskaliert sind und solche, die auf niedrigem
#Niveau verharrt sind.

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
#Jetzt noch dasselbe mit facet_grid und der unabhängigen Variable und der abhängige Variable.

#Verteilung von recruitment und war anschauen
UCDP %>% 
  drop_na(recruitment) %>% 
  ggplot(aes(x = as_factor(recruitment)))+
  geom_bar(aes(fill = as_factor(war)), position = "fill")
#Das macht es einfacher Verhältnisse über Gruppen besser zu verstehen. Konflikte, die entlang
#ethnischen Linien mobilisieren sind häufiger in Krieg eskaliert, als solche die nicht entlang
#ethnischen Linien mobilisiert haben.Wenn auch diese Unterschiede nur minimal sind.

UCDP %>% 
  drop_na(recruitment) %>% 
  count(recruitment, war)
#Das spricht aber überhaupt nicht für meine These, dass ethnische Mobilisierung einen Einfluss auf
#die Konfliktintensität hat. Denn über 1000 ethnisch mobilisierte Observationen sind auf niedrigem
#Niveau verblieben.


#Zwischenfazit bevor ich die Replikation der Resultate von Eck in Angriff nehme: Im Datensatz hat es sehr
#Viele Missings und die Korrelationen innerhalb des Datensatzes sind oft schwach und unklar.
