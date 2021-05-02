
# Extended Analysis -------------------------------------------------------

#Pakete laden
library(tidyverse)

#Daten laden
#Eck_data <- haven::read_stata(file = "Data/Eck Replication Data/ISQ.Eck.replication.dta")
ACD2EPR_2019 <- read_csv(file = "Data/ACD2EPR ETH Data/ACD2EPR-2019.csv", col_names = T)
UCDP_Armed_Conflict_2019 <- read_csv(file = "Data/UCDP-PRIO_2019/ucdp-prio-acd-201.csv", col_names = T)
UCDP_Dyadic_2019 <- read_csv(file = "Data/UCDP-PRIO_2019/ucdp-dyadic-201.csv", col_names = T)
#EPR_2019 <- read_csv(file = "Data/EPR-2019.csv")

# Joining the Datasets ----------------------------------------------------

#Convert ACD2EPR Variables as character
ACD2EPR_2019$sidea_id <- as.character(ACD2EPR_2019$sidea_id)
ACD2EPR_2019$sideb_id <- as.character(ACD2EPR_2019$sideb_id)

#Code Guy ethnic conflict variables
ACD2EPR_2019$claim <- ifelse(ACD2EPR_2019$claim > 0, 1, 0)
ACD2EPR_2019$recruitment <- ifelse(ACD2EPR_2019$recruitment > 0, 1, 0)
ACD2EPR_2019$ethnic_conflict <- ifelse(ACD2EPR_2019$claim + ACD2EPR_2019$recruitment == 2, 1, 0)

#Philippe
ACD2EPR_2019 %>% 
  count(ethnic_conflict)

ACD2EPR_2019 %>% 
  count(recruitment)

#Code Guy
ACD2EPR_2019$ethnic_conflict[is.na(ACD2EPR_2019$ethnic_conflict)] <- 0
#Wie Guy kann ich die NA's bei der recruitment Variable in 0 umwandeln
#ACD2EPR_2019$recruitment[is.na(ACD2EPR_2019$recruitment)] <- 0
#Darf ich das?


#Join ACD2EPR with the UCDP Dyadic Dataset
UCDP_Joined <- UCDP_Dyadic_2019 %>%
  filter(type_of_conflict %in% c(3,4)) %>%
  left_join(ACD2EPR_2019, by = c("side_a_id" = "sidea_id", "side_b_id" = "sideb_id"))

distinct(UCDP_Joined)

UCDP_Joined %>% 
  group_by(conflict_id) %>% 
  summarise(n = n()) #Jetzt haben wir 221 Konflikte

UCDP_Joined %>% 
  count(recruitment)

UCDP_Joined %>% 
  select(conflict_id, dyad_id, year, location, recruitment, side_a, side_b) %>% 
  View()

UCDP_Joined %>%
  group_by(conflict_id) %>%
  count(dyad_id)

#Weitere Vorbereitung
df <- UCDP_Joined %>% 
  select(conflict_id, location, year, recruitment, ethnic_conflict)
#Warum hat es hier mehrmals das selbe Jahr beim selben Konflikt (ID)? 

df2 <- UCDP_Joined %>% 
  filter(conflict_id == "11345", year == "2011") %>% 
  arrange(dyad_id)
#Weil es in einem Konflikt (hier Südsudan) mehrere Dyaden, das heisst unterschiedliche Konfliktparteien
#bzw. Rebellengruppen geben kann

#Wir können das umgehen, wenn wir nur schauen ob ein Konflikt in einem Jahr ein ethnischer Konflikt war
#oder nicht
#Code Guy:
UCDP_Joined %>% 
  group_by(conflict_id, year) %>%
  dplyr::summarise(ethnic_conflict = max(ethnic_conflict, na.rm=T))

df3 <- UCDP_Joined %>%
  select(conflict_id, year, location, ethnic_conflict) %>% 
  group_by(conflict_id, year) %>%
  summarise(ethnic_conflict = max(ethnic_conflict, na.rm=T))
summary(df3$ethnic_conflict)
df3$ethnic_conflict
#Jetzt sehen wir, ob ein Konflikt generell ethnisch geprägt war oder nicht, nicht nur 

#BZW neu mache ich es nur mit recruitment statt ethnischem Konflikt
UCDP_Joined %>%
  select(conflict_id, year, location, recruitment) %>% 
  group_by(conflict_id, year)

df4 <- UCDP_Joined %>%
  select(conflict_id, year, location, recruitment) %>% 
  group_by(conflict_id, year) %>%
  summarise(recruitment = max(recruitment, na.rm=T))

range(df4$recruitment)
summary(df4$recruitment)
which(is.na(df4$recruitment))
df4$recruitment[df4$recruitment == -Inf] <- NA
as_factor(df4$recruitment)

df4 %>% 
  count(recruitment)

x <- c(0,1,2,3, NA)
is.na(x)
which(is.na(x))

df4
#Jetzt haben wir das country level und sehen ob ein Konflikt mit mehreren Dyaden pro Jahr ethnisch
#rekrutiert war oder nicht. Das ist dasselbe wie Guy für die ethnic_conflict Variable macht.


# df <- UCDP_AC_Joined %>%
#   select(conflict_id, year, location, ethnic_conflict) %>% 
#   group_by(conflict_id, year, location) %>%
#   summarise(ethnic_conflict = max(ethnic_conflict, na.rm=T))
# 
# summary(df$ethnic_conflict)



#Jetzt haben wir das country level statt nicht mehr das Dyadische Level und können die Daten zu den
#ethnischen Konflikten mit den UCDP Armed Conflict Daten joinen
UCDP <- inner_join(df3, UCDP_Armed_Conflict_2019, 
                                   by = c("conflict_id", "year"))

UCDP <- left_join(df3, UCDP_Armed_Conflict_2019, 
                   by = c("conflict_id", "year"))
which(duplicated(UCDP))
distinct(UCDP)


#Dasselbe geht auch für die recruitment Variable
UCDP <- left_join(df4, UCDP_Armed_Conflict_2019,
                  by = c("conflict_id", "year"))

