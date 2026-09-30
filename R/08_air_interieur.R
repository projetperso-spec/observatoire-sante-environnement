# Module 7 : qualité de l'air intérieur (action 15 du « Croissy d'Après » : « Mesurons, Agissons, Respirons »).
# Sources : annuaire de l'Éducation nationale (établissements scolaires géolocalisés),
#           INSEE, base permanente des équipements (crèches, accueils de loisirs),
#           Géorisques (potentiel radon), code de l'environnement (art. R221-30 et suivants).
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))

# --- Établissements scolaires -------------------------------------------------
u <- paste0("https://data.education.gouv.fr/api/explore/v2.1/catalog/datasets/fr-en-annuaire-education/records",
            "?where=", utils::URLencode(sprintf("code_commune=\"%s\" and etat=\"OUVERT\"", CODE_COMMUNE), reserved = TRUE),
            "&limit=100")
ecoles <- jsonlite::fromJSON(u)$results
ecoles <- data.frame(nom = ecoles$nom_etablissement, type = ecoles$type_etablissement,
                     statut = ecoles$statut_public_prive, latitude = ecoles$latitude, longitude = ecoles$longitude)
ecoles <- ecoles[order(ecoles$statut, ecoles$type, ecoles$nom), ]
utils::write.csv(ecoles, file.path(DOSSIER_TRAITE, "etablissements_scolaires.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Équipements petite enfance et loisirs (nombres seulement : la BPE n'est pas géolocalisée ici) ---
bpe <- jsonlite::fromJSON(sprintf("https://api.insee.fr/melodi/data/DS_BPE?GEO=2024-COM-%s&maxResult=1000", CODE_COMMUNE))$observations
codes <- c(D502 = "Établissement d'accueil du jeune enfant", D509 = "Micro-crèche",
           D505 = "Accueil de loisirs sans hébergement", C107 = "École maternelle",
           C108 = "École primaire", C109 = "École élémentaire")  # libellés de la nomenclature BPE (API Melodi)
bpe <- data.frame(code = bpe$dimensions$FACILITY_TYPE, annee = bpe$dimensions$TIME_PERIOD,
                  nombre = bpe$measures$OBS_VALUE_NIVEAU$value)
bpe <- bpe[bpe$code %in% names(codes), ]
bpe <- bpe[bpe$annee == max(bpe$annee), ]
bpe$equipement <- codes[bpe$code]
utils::write.csv(bpe[, c("equipement", "code", "nombre", "annee")], file.path(DOSSIER_TRAITE, "equipements_air_interieur.csv"),
                 row.names = FALSE, fileEncoding = "UTF-8")

# --- Radon ------------------------------------------------------------------------
radon <- jsonlite::fromJSON(sprintf("https://georisques.gouv.fr/api/v1/radon?code_insee=%s", CODE_COMMUNE))$data$classe_potentiel[1]
writeLines(as.character(radon), file.path(DOSSIER_TRAITE, "radon.txt"))

print(ecoles); print(bpe); cat("Radon : catégorie", radon, "\n")
