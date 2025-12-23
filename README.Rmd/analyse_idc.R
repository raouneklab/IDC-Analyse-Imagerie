# ==============================================================================
# PROJET : ANALYSE DES MÉTADONNÉES IDC (IMAGING DATA COMMONS)
# OBJECTIF : Extraction, Nettoyage et Visualisation pour Portfolio GitHub
# ==============================================================================

# 1. INSTALLATION ET CHARGEMENT DES LIBRAIRIES
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(bigrquery)) install.packages("bigrquery")
if(!require(DBI)) install.packages("DBI")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gridExtra)) install.packages("gridExtra")

library(tidyverse)
library(bigrquery)
library(DBI)
library(lubridate)
library(gridExtra)

# 2. CONNEXION À BIGQUERY
# Remplace par ton ID de projet si nécessaire
project_id <- "formal-envelope-476722-g2"

con <- dbConnect(
  bigrquery::bigquery(),
  project = "bigquery-public-data",
  dataset = "idc_current",
  billing = project_id
)

# 3. EXTRACTION DES DONNÉES (20 000 lignes aléatoires)
query_sql <- "
SELECT Modality, PatientSex, PatientAge, BodyPartExamined, StudyDate, Manufacturer
FROM `bigquery-public-data.idc_current.dicom_all`
WHERE StudyDate IS NOT NULL 
  AND PatientAge IS NOT NULL
  AND PatientSex IN ('M', 'F')
ORDER BY RAND() 
LIMIT 20000
"

print("Démarrage de l'extraction des données...")
df_raw <- dbGetQuery(con, query_sql)

# 4. NETTOYAGE ET TRANSFORMATION (ETL)
df_clean <- df_raw %>%
  mutate(
    # Conversion date et année
    Date = as.Date(StudyDate),
    Year = year(Date),
    
    # Nettoyage de l'âge DICOM (ex: 050Y -> 50)
    age_numeric = as.numeric(str_extract(PatientAge, "\\d+")),
    age_unit = str_extract(PatientAge, "[A-Z]"),
    Age = case_when(
      age_unit == "Y" ~ age_numeric,
      age_unit == "M" ~ age_numeric / 12,
      TRUE ~ age_numeric
    ),
    
    # Standardisation des textes
    Organ = str_to_upper(replace_na(BodyPartExamined, "NON SPÉCIFIÉ")),
    Modality = replace_na(Modality, "AUTRE")
  ) %>%
  # Filtres de qualité
  filter(Age <= 110, Year >= 2015, Organ != "PHANTOM") %>%
  # Groupes d'âge pour la visualisation
  mutate(Age_Group = cut(Age, 
                         breaks = c(0, 18, 45, 65, 110), 
                         labels = c("0-17", "18-44", "45-64", "65+")))

# 5. CRÉATION DES GRAPHIQUES
# --- G1: Modalités ---
p1 <- df_clean %>%
  count(Modality, sort = TRUE) %>%
  ggplot(aes(x = reorder(Modality, -n), y = n, fill = Modality)) +
  geom_col(show.legend = FALSE) +
  theme_minimal() +
  labs(title = "1. Modalités Utilisées", x = "Machine", y = "Volume")

# --- G2: Top Organes ---
p2 <- df_clean %>%
  filter(Organ != "NON SPÉCIFIÉ") %>%
  count(Organ, sort = TRUE) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(Organ, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "2. Top 10 Organes", x = "Organe", y = "Volume")

# --- G3: Démographie ---
p3 <- ggplot(df_clean, aes(x = Age_Group, fill = PatientSex)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  scale_fill_manual(values = c("F" = "#e75480", "M" = "#0072b2")) +
  labs(title = "3. Répartition Âge & Sexe", x = "Tranche d'âge", y = "Nombre", fill = "Sexe")

# 6. SAUVEGARDE ET DASHBOARD FINAL
if(!dir.exists("RENDU_FINAL_IDC")) dir.create("RENDU_FINAL_IDC")

# Sauvegarde individuelle
ggsave("RENDU_FINAL_IDC/1_modalites.png", p1, width = 6, height = 4)
ggsave("RENDU_FINAL_IDC/2_organes.png", p2, width = 6, height = 4)
ggsave("RENDU_FINAL_IDC/3_demographie.png", p3, width = 6, height = 4)

# Sauvegarde du Master Dashboard Haute Résolution
png("RENDU_FINAL_IDC/4_DASHBOARD_RESOLUTION_FINAL.png", width = 1200, height = 1000, res = 150)
grid.arrange(p1, p2, p3, ncol = 2, top = "RAPPORT FINAL - DATABASE IDC")
dev.off()

print("nalyse terminée ! Les fichiers sont dans le dossier 'RENDU_FINAL_IDC'.")