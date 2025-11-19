# ==============================================================================
# Projet Trafic Aerien - Mission 1 : Se familiariser avec les donnees
# Consultant Data - Aeroports de Paris (ADP)
# ==============================================================================

# ------------------------------------------------------------------------------
# Chargement des bibliotheques
# ------------------------------------------------------------------------------
library(nycflights13)
library(dplyr)
library(ggplot2)
library(tidyr)

# Chargement des donnees
data(flights)
data(airports)
data(airlines)
data(planes)
data(weather)

cat("\n========== MISSION 1 : SE FAMILIARISER AVEC LES DONNEES ==========\n\n")

# ==============================================================================
# QUESTION 1 : Combien y-a-t-il de...
# ==============================================================================
cat("========== QUESTION 1 ==========\n\n")

# Nombre total d'aeroports (origine + destination)
aeroports_depart <- flights %>%
  distinct(origin) %>%
  nrow()

aeroports_destination <- flights %>%
  distinct(dest) %>%
  nrow()

aeroports_total <- flights %>%
  select(origin, dest) %>%
  pivot_longer(cols = everything(), values_to = "airport") %>%
  distinct(airport) %>%
  nrow()

cat("Nombre d'aeroports de depart:", aeroports_depart, "\n")
cat("Nombre d'aeroports de destination:", aeroports_destination, "\n")
cat("Nombre total d'aeroports (depart + destination):", aeroports_total, "\n\n")

# Aeroports ou on ne passe pas a l'heure d'ete (dst != "A")
aeroports_no_dst <- airports %>%
  filter(dst != "A") %>%
  nrow()

cat("Nombre d'aeroports sans passage a l'heure d'ete (dst != 'A'):", aeroports_no_dst, "\n\n")

# Nombre de fuseaux horaires
fuseaux_horaires <- airports %>%
  distinct(tzone) %>%
  nrow()

cat("Nombre de fuseaux horaires distincts:", fuseaux_horaires, "\n")
cat("Liste des fuseaux horaires:\n")
print(airports %>% distinct(tzone) %>% arrange(tzone))
cat("\n")

# Nombre de compagnies
nb_compagnies <- airlines %>%
  nrow()

cat("Nombre de compagnies:", nb_compagnies, "\n\n")

# Nombre d'avions
nb_avions <- planes %>%
  nrow()

cat("Nombre d'avions:", nb_avions, "\n\n")

# Nombre de vols annules
vols_annules <- flights %>%
  filter(is.na(dep_time) | is.na(arr_time)) %>%
  nrow()

cat("Nombre de vols annules:", vols_annules, "\n\n")


# ==============================================================================
# QUESTION 2 : Aeroport le plus emprunte et top 10 destinations
# ==============================================================================
cat("========== QUESTION 2 ==========\n\n")

# Aeroport de depart le plus emprunte
aeroport_plus_emprunte <- flights %>%
  group_by(origin) %>%
  summarise(nb_vols = n()) %>%
  arrange(desc(nb_vols)) %>%
  left_join(airports, by = c("origin" = "faa")) %>%
  select(origin, name, nb_vols)

cat("Aeroport de depart le plus emprunte:\n")
print(aeroport_plus_emprunte %>% head(1))
cat("\n")

# Top 10 destinations les plus prisees
top10_destinations_plus <- flights %>%
  group_by(dest) %>%
  summarise(nb_vols = n()) %>%
  mutate(pourcentage = round(nb_vols / sum(nb_vols) * 100, 2)) %>%
  arrange(desc(nb_vols)) %>%
  head(10) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(dest, name, nb_vols, pourcentage)

cat("Top 10 des destinations les PLUS prisees:\n")
print(top10_destinations_plus)
cat("\n")

# Top 10 destinations les moins prisees
top10_destinations_moins <- flights %>%
  group_by(dest) %>%
  summarise(nb_vols = n()) %>%
  mutate(pourcentage = round(nb_vols / sum(nb_vols) * 100, 2)) %>%
  arrange(nb_vols) %>%
  head(10) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  select(dest, name, nb_vols, pourcentage)

cat("Top 10 des destinations les MOINS prisees:\n")
print(top10_destinations_moins)
cat("\n")

# Top 10 avions qui ont le plus decolle
top10_avions_plus <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  summarise(nb_vols = n()) %>%
  arrange(desc(nb_vols)) %>%
  head(10)

cat("Top 10 des avions qui ont le PLUS decolle:\n")
print(top10_avions_plus)
cat("\n")

# Top 10 avions qui ont le moins decolle
top10_avions_moins <- flights %>%
  filter(!is.na(tailnum)) %>%
  group_by(tailnum) %>%
  summarise(nb_vols = n()) %>%
  arrange(nb_vols) %>%
  head(10)

cat("Top 10 des avions qui ont le MOINS decolle:\n")
print(top10_avions_moins)
cat("\n")


# ==============================================================================
# QUESTION 3 : Destinations par compagnie et graphiques
# ==============================================================================
cat("========== QUESTION 3 ==========\n\n")

# Nombre de destinations par compagnie
dest_par_compagnie <- flights %>%
  group_by(carrier) %>%
  summarise(nb_destinations = n_distinct(dest)) %>%
  left_join(airlines, by = "carrier") %>%
  select(carrier, name, nb_destinations) %>%
  arrange(desc(nb_destinations))

cat("Nombre de destinations desservies par compagnie:\n")
print(dest_par_compagnie)
cat("\n")

# Nombre de destinations par compagnie et par aeroport d'origine
dest_par_compagnie_origin <- flights %>%
  group_by(carrier, origin) %>%
  summarise(nb_destinations = n_distinct(dest), .groups = "drop") %>%
  left_join(airlines, by = "carrier") %>%
  select(carrier, name, origin, nb_destinations) %>%
  arrange(carrier, origin)

cat("Nombre de destinations par compagnie et par aeroport d'origine:\n")
print(dest_par_compagnie_origin)
cat("\n")

# Graphique 1 : Nombre de destinations par compagnie
g1 <- ggplot(dest_par_compagnie, aes(x = reorder(carrier, -nb_destinations), y = nb_destinations, fill = carrier)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = nb_destinations), vjust = -0.5, size = 3) +
  labs(title = "Nombre de destinations desservies par compagnie",
       x = "Compagnie (code)",
       y = "Nombre de destinations") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

print(g1)
cat("\n")

# Graphique 2 : Destinations par compagnie et aeroport d'origine
g2 <- ggplot(dest_par_compagnie_origin, aes(x = carrier, y = nb_destinations, fill = origin)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nombre de destinations par compagnie et aeroport d'origine",
       x = "Compagnie (code)",
       y = "Nombre de destinations",
       fill = "Aeroport d'origine") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(g2)
cat("\n")


# ==============================================================================
# QUESTION 4 : Vols vers Houston et Seattle
# ==============================================================================
cat("========== QUESTION 4 ==========\n\n")

# Vols ayant atterri a Houston (IAH ou HOU)
vols_houston <- flights %>%
  filter(dest %in% c("IAH", "HOU"))

cat("Nombre de vols ayant atterri a Houston (IAH ou HOU):", nrow(vols_houston), "\n\n")

# Vols de NYC vers Seattle
vols_seattle <- flights %>%
  filter(dest == "SEA")

nb_vols_seattle <- nrow(vols_seattle)
nb_compagnies_seattle <- vols_seattle %>%
  distinct(carrier) %>%
  nrow()
nb_avions_uniques_seattle <- vols_seattle %>%
  filter(!is.na(tailnum)) %>%
  distinct(tailnum) %>%
  nrow()

cat("Vols de NYC vers Seattle:\n")
cat("  - Nombre de vols:", nb_vols_seattle, "\n")
cat("  - Nombre de compagnies:", nb_compagnies_seattle, "\n")
cat("  - Nombre d'avions uniques:", nb_avions_uniques_seattle, "\n\n")

# Detail des compagnies
compagnies_seattle <- vols_seattle %>%
  group_by(carrier) %>%
  summarise(nb_vols = n()) %>%
  left_join(airlines, by = "carrier")

cat("Detail des compagnies desservant Seattle:\n")
print(compagnies_seattle)
cat("\n")


# ==============================================================================
# QUESTION 5 : Nombre de vols par destination et tri
# ==============================================================================
cat("========== QUESTION 5 ==========\n\n")

# Nombre de vols par destination
vols_par_destination <- flights %>%
  group_by(dest) %>%
  summarise(nb_vols = n()) %>%
  arrange(desc(nb_vols))

cat("Nombre de vols par destination (Top 20):\n")
print(vols_par_destination %>% head(20))
cat("\n")

# Tri des vols par destination, aeroport d'origine, compagnie (ordre alphabetique)
vols_tries <- flights %>%
  left_join(airports %>% select(faa, name), by = c("dest" = "faa")) %>%
  rename(dest_name = name) %>%
  left_join(airports %>% select(faa, name), by = c("origin" = "faa")) %>%
  rename(origin_name = name) %>%
  left_join(airlines, by = "carrier") %>%
  rename(carrier_name = name) %>%
  select(dest, dest_name, origin, origin_name, carrier, carrier_name,
         year, month, day, flight, tailnum) %>%
  arrange(dest_name, origin_name, carrier_name)

cat("Vols tries par destination, aeroport d'origine, compagnie (Top 20):\n")
print(vols_tries %>% head(20))
cat("\n")


# ==============================================================================
# QUESTION 6 : Couverture des compagnies
# ==============================================================================
cat("========== QUESTION 6 ==========\n\n")

# Nombre total d'aeroports d'origine
nb_total_origins <- flights %>%
  distinct(origin) %>%
  nrow()

# Nombre total de destinations
nb_total_destinations <- flights %>%
  distinct(dest) %>%
  nrow()

# Compagnies qui n'operent pas sur tous les aeroports d'origine
compagnies_origins <- flights %>%
  group_by(carrier) %>%
  summarise(nb_origins = n_distinct(origin)) %>%
  filter(nb_origins < nb_total_origins) %>%
  left_join(airlines, by = "carrier")

cat("Compagnies qui N'OPERENT PAS sur tous les aeroports d'origine:\n")
cat("(", nb_total_origins, "aeroports d'origine au total)\n\n")
print(compagnies_origins)
cat("\n")

# Compagnies qui desservent l'ensemble des destinations
compagnies_all_dest <- flights %>%
  group_by(carrier) %>%
  summarise(nb_destinations = n_distinct(dest)) %>%
  filter(nb_destinations == nb_total_destinations) %>%
  left_join(airlines, by = "carrier")

cat("Compagnies qui desservent L'ENSEMBLE des destinations:\n")
cat("(", nb_total_destinations, "destinations au total)\n\n")
if(nrow(compagnies_all_dest) > 0) {
  print(compagnies_all_dest)
} else {
  cat("Aucune compagnie ne dessert l'ensemble des destinations.\n")
}
cat("\n")

# Tableau recapitulatif : origines et destinations pour toutes les compagnies
tableau_recap <- flights %>%
  group_by(carrier) %>%
  summarise(
    nb_origins = n_distinct(origin),
    nb_destinations = n_distinct(dest),
    origins = paste(sort(unique(origin)), collapse = ", "),
    .groups = "drop"
  ) %>%
  left_join(airlines, by = "carrier") %>%
  select(carrier, name, nb_origins, origins, nb_destinations) %>%
  arrange(carrier)

cat("Tableau recapitulatif des origines et destinations par compagnie:\n")
print(tableau_recap)
cat("\n")


# ==============================================================================
# QUESTION 7 : Destinations exclusives
# ==============================================================================
cat("========== QUESTION 7 ==========\n\n")

# Destinations desservies par une seule compagnie
destinations_exclusives <- flights %>%
  group_by(dest) %>%
  summarise(
    nb_compagnies = n_distinct(carrier),
    compagnie = first(carrier)
  ) %>%
  filter(nb_compagnies == 1) %>%
  left_join(airports, by = c("dest" = "faa")) %>%
  left_join(airlines, by = c("compagnie" = "carrier")) %>%
  select(dest, name.x, compagnie, name.y) %>%
  rename(destination_name = name.x, carrier_name = name.y)

cat("Destinations exclusives a certaines compagnies:\n")
cat("(destinations desservies par une seule compagnie)\n\n")
print(destinations_exclusives)
cat("\nNombre total de destinations exclusives:", nrow(destinations_exclusives), "\n\n")


# ==============================================================================
# QUESTION 8 : Filtrer vols United, American, Delta
# ==============================================================================
cat("========== QUESTION 8 ==========\n\n")

# Recherche des codes carriers pour United, American, Delta
codes_recherches <- airlines %>%
  filter(grepl("United|American|Delta", name, ignore.case = TRUE))

cat("Codes des compagnies United, American, Delta:\n")
print(codes_recherches)
cat("\n")

# Filtrer les vols
vols_ua_aa_dl <- flights %>%
  filter(carrier %in% c("UA", "AA", "DL")) %>%
  left_join(airlines, by = "carrier")

cat("Nombre de vols exploites par United (UA), American (AA) ou Delta (DL):", nrow(vols_ua_aa_dl), "\n\n")

# Statistiques par compagnie
stats_ua_aa_dl <- vols_ua_aa_dl %>%
  group_by(carrier, name) %>%
  summarise(
    nb_vols = n(),
    nb_destinations = n_distinct(dest),
    nb_origins = n_distinct(origin),
    .groups = "drop"
  )

cat("Statistiques des vols United, American, Delta:\n")
print(stats_ua_aa_dl)
cat("\n")

# Apercu des donnees
cat("Apercu des vols United, American, Delta (10 premiers):\n")
print(vols_ua_aa_dl %>%
        select(carrier, name, flight, origin, dest, year, month, day) %>%
        head(10))
cat("\n")

cat("\n========== FIN DE LA MISSION 1 ==========\n")
