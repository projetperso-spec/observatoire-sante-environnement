# Module 2 : vulnérabilité climatique et besoins de végétalisation.
# Sources : ARB Île-de-France / L'Institut Paris Région, méthode Regreen (enjeux de renaturation,
#           maille de 125 m) ; L'Institut Paris Région, espaces verts ouverts au public 2024.
#
# Lecture des scores Regreen (Note rapide n° 966, déc. 2022) : plus le score est BAS,
# plus l'enjeu de renaturation est FORT. Zones prioritaires retenues par la méthode :
# score 0-1 pour la biodiversité, 0-2 pour le changement climatique et pour la santé-cadre de vie.
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))
sf::sf_use_s2(FALSE)  # calculs de surface faits en Lambert 93, pas besoin de géométrie sphérique

iris <- sf::st_read(file.path(DOSSIER_TRAITE, "iris.geojson"), quiet = TRUE)
portrait <- utils::read.csv(file.path(DOSSIER_TRAITE, "portrait.csv"), colClasses = c(code_iris = "character"))

# --- Grille Regreen -----------------------------------------------------------
regreen <- couche_ipr(15, sprintf("insee=%s", CODE_COMMUNE),
                      champs = "num_125,type_zone,score_biodiv,score_cc,score_scv,couv_veg,alea_icu,vuln_icu,carence_ev,inondation")
regreen$prioritaire_biodiv <- regreen$score_biodiv <= 1
regreen$prioritaire_climat <- regreen$score_cc <= 2
regreen$prioritaire_sante  <- regreen$score_scv <= 2
regreen$nb_enjeux_prioritaires <- regreen$prioritaire_biodiv + regreen$prioritaire_climat + regreen$prioritaire_sante

# Chaque maille est rattachée à l'IRIS qui contient son centre (calcul en Lambert 93, en mètres).
iris_l93 <- sf::st_transform(iris, 2154)
centres <- sf::st_centroid(sf::st_geometry(sf::st_transform(regreen, 2154)))
dans <- sf::st_within(centres, iris_l93)
regreen$code_iris <- iris$code_iris[vapply(dans, function(i) if (length(i)) i[1] else NA_integer_, 1L)]
sf::st_write(regreen, file.path(DOSSIER_TRAITE, "regreen_grille.geojson"), delete_dsn = TRUE, quiet = TRUE)

g <- sf::st_drop_geometry(regreen)
g <- g[!is.na(g$code_iris), ]
par_iris <- do.call(rbind, lapply(split(g, g$code_iris), function(x) data.frame(
  code_iris = x$code_iris[1],
  nb_mailles = nrow(x),
  score_biodiv_moyen = mean(x$score_biodiv),
  score_climat_moyen = mean(x$score_cc),
  score_sante_moyen  = mean(x$score_scv),
  part_mailles_prio_biodiv = 100 * mean(x$prioritaire_biodiv),
  part_mailles_prio_climat = 100 * mean(x$prioritaire_climat),
  part_mailles_prio_sante  = 100 * mean(x$prioritaire_sante),
  part_mailles_2_enjeux_ou_plus = 100 * mean(x$nb_enjeux_prioritaires >= 2)
)))

# --- Espaces verts ouverts au public --------------------------------------------
ev <- couche_ipr(9, sprintf("insee=%s", CODE_COMMUNE), champs = "nom,categlib,statouvlib,surftotha")

# Surface d'espace vert public située dans chaque IRIS, rapportée à ses habitants.
# On mesure la géométrie cartographiée, pas l'attribut surftotha : pour l'Île de la Chaussée,
# surftotha vaut 13,16 ha (site entier) alors que le polygone de l'inventaire fait 0,33 ha.
ev_l93 <- sf::st_make_valid(sf::st_transform(ev, 2154))
ev$surface_cartographiee_ha <- round(as.numeric(sf::st_area(ev_l93)) / 1e4, 2)
sf::st_write(ev, file.path(DOSSIER_TRAITE, "espaces_verts.geojson"), delete_dsn = TRUE, quiet = TRUE)
inter <- suppressWarnings(sf::st_intersection(ev_l93[, "nom"], iris_l93[, "code_iris"]))
inter$surface_m2 <- as.numeric(sf::st_area(inter))
surf <- stats::aggregate(surface_m2 ~ code_iris, data = sf::st_drop_geometry(inter), FUN = sum)
par_iris <- merge(par_iris, surf, by = "code_iris", all.x = TRUE)
par_iris$surface_m2[is.na(par_iris$surface_m2)] <- 0
par_iris <- merge(par_iris, portrait[, c("code_iris", "population")], by = "code_iris")
par_iris$m2_espace_vert_public_par_hab <- par_iris$surface_m2 / par_iris$population
par_iris$surface_espace_vert_public_ha <- par_iris$surface_m2 / 1e4
par_iris$surface_m2 <- NULL; par_iris$population <- NULL

num <- vapply(par_iris, is.numeric, logical(1))
par_iris[num] <- lapply(par_iris[num], round, 1)
utils::write.csv(par_iris, file.path(DOSSIER_TRAITE, "climat_vegetalisation.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(par_iris)
print(sf::st_drop_geometry(ev)[order(-ev$surface_cartographiee_ha), ])
