# Module 9 : PLU vert (action 11 du « Croissy d'Après »).
# Croise le zonage du PLU en vigueur (Géoportail de l'Urbanisme) avec les mailles prioritaires
# de renaturation (Regreen, module 2) : dans quelles zones du PLU se trouvent les besoins ?
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))
sf::sf_use_s2(FALSE)

u <- paste0("https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=wfs_du:zone_urba",
            "&OUTPUTFORMAT=application/json&COUNT=5000&CQL_FILTER=",
            utils::URLencode(sprintf("partition='DU_%s'", CODE_COMMUNE), reserved = TRUE))
plu <- sf::st_read(u, quiet = TRUE)
if (!nrow(plu)) stop("Pas de document d'urbanisme publié sur le Géoportail de l'Urbanisme pour ", CODE_COMMUNE)
plu <- sf::st_make_valid(sf::st_transform(plu[, c("libelle", "libelong", "typezone", "datappro")], 2154))
plu$libelong <- trimws(gsub("\\s+", " ", plu$libelong))
# La date d'approbation n'est pas remplie dans les zones : on la lit dans le nom du document
# (norme CNIG : <insee>_<type>_<AAAAMMJJ>, ex. « 78190_PLU_20230220 »).
doc <- sf::st_read(sub("zone_urba", "document", u), quiet = TRUE)
date_plu <- as.Date(sub(".*_(\\d{8})$", "\\1", doc$name[1]), "%Y%m%d")

# Une ligne par secteur (libellé) : surface, et part de ses mailles Regreen prioritaires.
regreen <- sf::st_transform(sf::st_read(file.path(DOSSIER_TRAITE, "regreen_grille.geojson"), quiet = TRUE), 2154)
regreen$au_moins_un_enjeu <- regreen$nb_enjeux_prioritaires >= 1
centres <- sf::st_centroid(sf::st_geometry(regreen))
regreen$secteur <- plu$libelle[vapply(sf::st_within(centres, plu), function(i) if (length(i)) i[1] else NA_integer_, 1L)]

plu_sect <- stats::aggregate(plu["typezone"], by = list(libelle = plu$libelle), FUN = function(x) x[1])  # union des polygones, SCR conservé
plu_sect$typezone <- NULL
infos <- unique(sf::st_drop_geometry(plu)[, c("libelle", "libelong", "typezone")])
infos <- infos[!duplicated(infos$libelle), ]
plu_sect <- merge(plu_sect, infos, by = "libelle")
plu_sect$surface_ha <- round(as.numeric(sf::st_area(plu_sect)) / 1e4, 1)

g <- sf::st_drop_geometry(regreen)
stats_sect <- do.call(rbind, lapply(split(g, g$secteur), function(x) data.frame(
  libelle = x$secteur[1], mailles = nrow(x),
  part_mailles_prioritaires = round(100 * mean(x$au_moins_un_enjeu), 1),
  score_sante_moyen = round(mean(x$score_scv), 1), score_climat_moyen = round(mean(x$score_cc), 1)
)))
plu_sect <- merge(plu_sect, stats_sect, by = "libelle", all.x = TRUE)
plu_sect <- plu_sect[order(-plu_sect$surface_ha), ]

sf::st_write(sf::st_transform(plu_sect, 4326), file.path(DOSSIER_TRAITE, "plu_secteurs.geojson"), delete_dsn = TRUE, quiet = TRUE)
utils::write.csv(sf::st_drop_geometry(plu_sect), file.path(DOSSIER_TRAITE, "plu_secteurs.csv"), row.names = FALSE, fileEncoding = "UTF-8")
writeLines(format(date_plu), file.path(DOSSIER_TRAITE, "plu_date.txt"))
message("PLU approuvé le ", format(date_plu, "%d/%m/%Y"), ", ", nrow(plu_sect), " secteurs")
print(sf::st_drop_geometry(plu_sect)[, c("libelle", "typezone", "surface_ha", "mailles", "part_mailles_prioritaires")])
