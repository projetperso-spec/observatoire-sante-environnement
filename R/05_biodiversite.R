# Module 4 : biodiversité observée et espèces exotiques envahissantes (EEE).
# Sources : GBIF (observations d'espèces depuis 2010, dont une partie des données françaises du SINP)
#           GRIIS France (Global Register of Introduced and Invasive Species), publié sur GBIF.
#
# Limite à garder en tête : une observation dépend de l'effort des observateurs.
# Beaucoup d'oiseaux notés ne veut pas dire peu d'insectes présents, seulement moins d'observations.
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))
sf::sf_use_s2(FALSE)

ANNEE_DEBUT <- 2010
GRIIS_FRANCE <- "692a0aec-70e2-4038-ac9c-9a1d7acc025f"
gbif <- function(chemin) jsonlite::fromJSON(paste0("https://api.gbif.org/v1/", chemin), simplifyVector = TRUE)

# Contour simplifié (20 m) pour que la requête reste courte.
commune <- sf::st_read(file.path(DOSSIER_TRAITE, "commune.geojson"), quiet = TRUE)
contour <- sf::st_simplify(sf::st_transform(commune, 2154), dTolerance = 20) |> sf::st_transform(4326)
wkt <- utils::URLencode(sf::st_as_text(sf::st_geometry(contour)[[1]], digits = 6), reserved = TRUE)

# --- Observations (pagination de 300 en 300) ----------------------------------
champs <- c("key", "speciesKey", "species", "kingdom", "phylum", "class", "order", "year", "decimalLatitude", "decimalLongitude")
pages <- list(); decalage <- 0
repeat {
  r <- gbif(sprintf("occurrence/search?geometry=%s&year=%d,2100&hasCoordinate=true&limit=300&offset=%d",
                    wkt, ANNEE_DEBUT, decalage))
  if (length(r$results) == 0) break
  res <- r$results
  res[setdiff(champs, names(res))] <- NA
  pages[[length(pages) + 1]] <- res[champs]
  decalage <- decalage + 300
  if (isTRUE(r$endOfRecords)) break
}
obs <- do.call(rbind, pages)
obs <- obs[!is.na(obs$speciesKey), ]
message(nrow(obs), " observations identifiées à l'espèce depuis ", ANNEE_DEBUT)

# Groupes lisibles pour le tableau de bord. La classification GBIF actuelle ne range plus
# les poissons osseux dans une classe « Actinopterygii » : un vertébré qui n'est ni oiseau,
# ni mammifère, ni amphibien ou reptile est donc compté comme poisson.
groupe <- function(kingdom, phylum, class) {
  ifelse(class %in% "Aves", "Oiseaux",
  ifelse(class %in% "Mammalia", "Mammifères",
  ifelse(class %in% c("Amphibia", "Squamata", "Testudines", "Reptilia"), "Amphibiens et reptiles",
  ifelse(phylum %in% "Chordata", "Poissons",
  ifelse(class %in% "Insecta", "Insectes",
  ifelse(phylum %in% "Arthropoda", "Araignées et autres arthropodes",
  ifelse(phylum %in% "Mollusca", "Mollusques",
  ifelse(kingdom %in% "Plantae", "Plantes",
  ifelse(kingdom %in% "Fungi", "Champignons", "Autres")))))))))
}
obs$groupe <- groupe(obs$kingdom, obs$phylum, obs$class)

especes <- stats::aggregate(key ~ speciesKey + species + groupe, data = obs, FUN = length)
names(especes)[names(especes) == "key"] <- "nb_observations"
especes$derniere_annee <- stats::ave(obs$year, obs$speciesKey, FUN = max)[match(especes$speciesKey, obs$speciesKey)]

# --- Statut des espèces -------------------------------------------------------
# « Envahissante » = inscrite sur la liste des espèces exotiques envahissantes préoccupantes
# pour l'Union européenne (règlement UE 1143/2014) : c'est la liste réglementaire.
# « Introduite » = présente dans GRIIS France comme introduite. Ce second repère est plus large
# et moins sûr : il contient des espèces indigènes dans une partie du pays.
checklist <- function(dataset) {  # renvoie la clé dans la liste et la clé GBIF de référence (nubKey)
  morceaux <- list(); decalage <- 0
  repeat {
    r <- gbif(sprintf("species/search?datasetKey=%s&limit=1000&offset=%d", dataset, decalage))
    if (length(r$results) == 0) break
    morceaux[[length(morceaux) + 1]] <- r$results[, c("key", "nubKey")]
    decalage <- decalage + 1000
    if (isTRUE(r$endOfRecords)) break
  }
  d <- do.call(rbind, morceaux)
  d[!is.na(d$nubKey), ]
}
LISTE_UE <- "79d65658-526c-4c78-9d24-1870d67f8439"
liste_ue <- checklist(LISTE_UE)
especes$envahissante_ue <- especes$speciesKey %in% liste_ue$nubKey
especes$introduite_griis <- especes$speciesKey %in% checklist(GRIIS_FRANCE)$nubKey

# Date d'inscription sur la liste UE (la liste est complétée par règlements successifs).
date_ue <- function(cle_liste) {
  d <- gbif(sprintf("species/%s/descriptions", cle_liste))$results
  if (!is.data.frame(d)) return(NA_character_)
  x <- d$description[d$type %in% "entry into force"]
  if (length(x)) x[1] else NA_character_
}
especes$inscription_liste_ue <- NA_character_
i_ue <- which(especes$envahissante_ue)
especes$inscription_liste_ue[i_ue] <- vapply(liste_ue$key[match(especes$speciesKey[i_ue], liste_ue$nubKey)], date_ue, character(1))

# Nom français : GBIF agrège des noms de sources de qualité inégale (la première entrée
# peut être fausse). On retient le nom sur lequel le plus de sources s'accordent.
nom_francais <- function(cle) {
  v <- tryCatch(gbif(sprintf("species/%s/vernacularNames?limit=300", cle))$results, error = function(e) NULL)
  if (!is.data.frame(v) || !"language" %in% names(v)) return(NA_character_)
  fr <- trimws(sub(",.*$", "", v$vernacularName[v$language %in% "fra"]))  # "Perche-soleil, Boer" -> "Perche-soleil"
  if (!length(fr)) return(NA_character_)
  cle <- tolower(gsub("-", " ", fr))                  # le vote ignore casse et tirets...
  gagnant <- names(sort(table(cle), decreasing = TRUE))[1]
  variantes <- fr[cle == gagnant]                     # ...mais on affiche la graphie qui garde
  majuscules <- nchar(gsub("[^[:upper:]]", "", variantes))  # les noms propres (« du Japon »)
  majuscules[variantes == toupper(variantes)] <- -1          # sauf une graphie tout en capitales
  choix <- variantes[which.max(majuscules)]
  paste0(toupper(substr(choix, 1, 1)), substr(choix, 2, nchar(choix)))
}
a_nommer <- unique(c(which(especes$envahissante_ue | especes$introduite_griis),
                     order(-especes$nb_observations)[1:min(40, nrow(especes))]))
especes$nom_francais <- NA_character_
especes$nom_francais[a_nommer] <- vapply(especes$speciesKey[a_nommer], nom_francais, character(1))

especes <- especes[order(-especes$envahissante_ue, -especes$introduite_griis, -especes$nb_observations), ]
utils::write.csv(especes, file.path(DOSSIER_TRAITE, "especes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

synthese <- do.call(rbind, lapply(split(especes, especes$groupe), function(x) data.frame(
  groupe = x$groupe[1], nb_especes = nrow(x), nb_observations = sum(x$nb_observations),
  nb_introduites_griis = sum(x$introduite_griis), nb_envahissantes_ue = sum(x$envahissante_ue)
)))
synthese <- synthese[order(-synthese$nb_observations), ]
utils::write.csv(synthese, file.path(DOSSIER_TRAITE, "biodiversite_synthese.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Points d'observation des espèces envahissantes, pour la carte.
eee <- obs[obs$speciesKey %in% especes$speciesKey[especes$envahissante_ue], c("species", "year", "decimalLatitude", "decimalLongitude")]
eee$nom_francais <- especes$nom_francais[match(eee$species, especes$species)]
eee <- sf::st_as_sf(eee, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
sf::st_write(eee, file.path(DOSSIER_TRAITE, "eee_observations.geojson"), delete_dsn = TRUE, quiet = TRUE)

print(synthese)
print(especes[especes$envahissante_ue, c("species", "nom_francais", "groupe", "nb_observations", "derniere_annee", "inscription_liste_ue")])
print(head(especes[order(-especes$nb_observations), c("species", "nom_francais", "groupe", "nb_observations")], 15))
