# Module 10 : présélection de terrains communaux pour un jardin partagé en permaculture.
# Sources : DGFiP, fichier des parcelles des personnes morales 2024 (propriétaire et nature des surfaces),
#           cadastre Etalab (contours des parcelles), modules 1, 2, 6 et 9 (quartiers, Regreen, PPRI, PLU).
#
# C'est une présélection sur données : l'usage actuel, la qualité du sol, l'ensoleillement
# et l'accès à l'eau restent à vérifier sur place avant toute décision.
source("R/00_config.R")
suppressPackageStartupMessages({ library(sf); library(arrow) })
library(dplyr, warn.conflicts = FALSE)
sf::sf_use_s2(FALSE)

SURFACE_MIN_M2 <- 300   # en dessous, trop petit pour un jardin partagé
NATURES_CULTIVABLES <- c("Terre", "Jardins", "Prés", "Landes", "Terrains d'agrément", "Vergers")

# --- Parcelles de la commune (propriétaire = la commune elle-même) --------------
f <- telecharger("https://static.data.gouv.fr/resources/fichiers-des-personnes-morales-unifies/20251029-085250/parcelles-personnes-morales-2024.parquet",
                 "parcelles_personnes_morales_2024.parquet")
lignes <- arrow::open_dataset(f) |>
  filter(code_insee == CODE_COMMUNE, groupe_personne_code == "4") |>
  collect()
lignes <- lignes[grepl("^COMMUNE", lignes$denomination), ]  # écarte les syndicats rangés dans le même groupe

# Une parcelle peut avoir plusieurs subdivisions fiscales (ex. une partie bâtie, une partie en jardin).
# Le fichier peut aussi répéter une subdivision (une ligne par droit de propriété) : on dédoublonne
# avant d'additionner. Contrôle fait sur Croissy : surfaces cohérentes avec les contours du cadastre.
parcelles <- lignes |>
  distinct(section, numero_parcelle, prefixe, subdivision_fiscale, nature_culture_libelle,
           contenance_subdivision_centiare, .keep_all = TRUE) |>
  group_by(section, numero_parcelle, prefixe) |>
  summarise(contenance_m2 = first(contenance_parcelle_centiare),
            surface_cultivable_m2 = sum(contenance_subdivision_centiare[nature_culture_libelle %in% NATURES_CULTIVABLES], na.rm = TRUE),
            natures = paste(sort(unique(na.omit(nature_culture_libelle))), collapse = ", "),
            adresse = paste(na.omit(c(first(na.omit(numero_voirie)), first(na.omit(nature_voie)), first(na.omit(nom_voie)))), collapse = " "),
            .groups = "drop") |>
  filter(surface_cultivable_m2 >= SURFACE_MIN_M2)

# Identifiant cadastral national : code INSEE + préfixe (3) + section (2) + numéro (4).
parcelles$id <- sprintf("%s%s%s%04d", CODE_COMMUNE, ifelse(is.na(parcelles$prefixe), "000", parcelles$prefixe),
                        formatC(parcelles$section, width = 2, flag = "0"), parcelles$numero_parcelle)
parcelles$id <- gsub(" ", "0", parcelles$id)

# --- Contours (cadastre Etalab) ------------------------------------------------
cadastre <- sf::st_read(sprintf("https://cadastre.data.gouv.fr/bundler/cadastre-etalab/communes/%s/geojson/parcelles", CODE_COMMUNE), quiet = TRUE)
cand <- merge(cadastre[, "id"], parcelles, by = "id")
message(nrow(parcelles), " parcelles communales avec au moins ", SURFACE_MIN_M2, " m² non bâtis ; ", nrow(cand), " retrouvées au cadastre")
cand <- sf::st_transform(sf::st_make_valid(cand), 2154)
centres <- sf::st_point_on_surface(sf::st_geometry(cand))

# --- Critères ------------------------------------------------------------------
dans <- function(couche) lengths(sf::st_intersects(centres, sf::st_transform(couche, 2154))) > 0
lire_geo <- function(f) sf::st_read(file.path(DOSSIER_TRAITE, f), quiet = TRUE)

iris <- sf::st_transform(lire_geo("iris.geojson"), 2154)
portrait <- utils::read.csv(file.path(DOSSIER_TRAITE, "portrait.csv"), colClasses = c(code_iris = "character"))
cand$quartier <- iris$nom_iris[vapply(sf::st_within(centres, iris), function(i) if (length(i)) i[1] else NA_integer_, 1L)]
pauvrete <- setNames(portrait$taux_pauvrete, portrait$nom_iris)
cand$taux_pauvrete_quartier <- unname(pauvrete[cand$quartier])

plu <- sf::st_transform(lire_geo("plu_secteurs.geojson"), 2154)
cand$secteur_plu <- plu$libelle[vapply(sf::st_within(centres, plu), function(i) if (length(i)) i[1] else NA_integer_, 1L)]
cand$dans_ppri <- dans(lire_geo("ppri.geojson"))

regreen <- lire_geo("regreen_grille.geojson")
cand$maille_prioritaire_sante <- dans(regreen[regreen$prioritaire_sante, ])

# Score simple et lisible (0 à 4) : un point par critère favorable.
cand$score <- (cand$surface_cultivable_m2 >= 1000) +                              # assez grand pour plusieurs parcelles de culture
              (cand$taux_pauvrete_quartier >= stats::median(pauvrete[portrait$echelle == "IRIS"])) +  # quartier plus modeste
              cand$maille_prioritaire_sante +                                      # zone qui manque de nature
              (!cand$dans_ppri)                                                    # abris et bacs possibles hors zone inondable
cand <- cand[order(-cand$score, -cand$surface_cultivable_m2), ]
cand$rang <- seq_len(nrow(cand))

sf::st_write(sf::st_transform(cand, 4326), file.path(DOSSIER_TRAITE, "permaculture_candidats.geojson"), delete_dsn = TRUE, quiet = TRUE)
tab <- sf::st_drop_geometry(cand)[, c("rang", "id", "adresse", "quartier", "secteur_plu", "surface_cultivable_m2",
                                      "natures", "dans_ppri", "maille_prioritaire_sante", "score")]
utils::write.csv(tab, file.path(DOSSIER_TRAITE, "permaculture_candidats.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(tab)
