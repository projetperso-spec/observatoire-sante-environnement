# Module 3 : santé environnementale et accès aux soins.
# Sources : DREES, accessibilité potentielle localisée (APL) aux médecins généralistes 2022-2024
#           ORS Île-de-France / L'Institut Paris Région / Ineris, cumuls d'expositions
#           environnementales (PRSE3), maille de 500 m et synthèse communale.
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))

# --- APL aux médecins généralistes -------------------------------------------
# Unité : consultations ou visites accessibles par an et par habitant standardisé
# (la population est pondérée par son âge, car les besoins de soins en dépendent).
f_apl <- telecharger(
  "https://data.drees.solidarites-sante.gouv.fr/api/explore/v2.1/catalog/datasets/530_l-accessibilite-potentielle-localisee-apl/attachments/indicateur_d_apl_aux_medecins_generalistes_xlsx",
  "apl_medecins_generalistes.xlsx"
)
feuilles <- grep("^APL [0-9]{4}$", readxl::excel_sheets(f_apl), value = TRUE)
code_dep <- substr(CODE_COMMUNE, 1, 2)

apl <- do.call(rbind, lapply(feuilles, function(feuille) {
  # Les 10 premières lignes sont des titres ; les données commencent à la 11e.
  d <- readxl::read_excel(f_apl, sheet = feuille, skip = 10, col_names = FALSE, col_types = "text",
                          .name_repair = "minimal")
  # Colonne 7 : population standardisée (celle à laquelle l'APL se rapporte).
  d <- data.frame(code = d[[1]], apl = en_nombre(d[[3]]), apl_moins_65 = en_nombre(d[[4]]),
                  pop = en_nombre(d[[7]]))
  d <- d[!is.na(d$apl) & !is.na(d$pop), ]
  moyenne <- function(x) sum(x$apl * x$pop) / sum(x$pop)  # moyenne pondérée par la population standardisée
  moyenne_65 <- function(x) sum(x$apl_moins_65 * x$pop, na.rm = TRUE) / sum(x$pop[!is.na(x$apl_moins_65)])
  dep <- d[substr(d$code, 1, 2) == code_dep, ]
  com <- d[d$code == CODE_COMMUNE, ]
  data.frame(
    annee = as.integer(sub("APL ", "", feuille)),
    echelle = c(NOM_COMMUNE, paste0("Département (", code_dep, ")"), "France"),
    apl = c(com$apl, moyenne(dep), moyenne(d)),
    apl_medecins_moins_65_ans = c(com$apl_moins_65, moyenne_65(dep), moyenne_65(d))
  )
}))
apl[3:4] <- lapply(apl[3:4], round, 2)
utils::write.csv(apl, file.path(DOSSIER_TRAITE, "apl_medecins.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(apl)

# --- Cumuls d'expositions environnementales (PRSE3) ---------------------------
# Synthèse communale : part de la population dans chaque classe du score
# « environnement » (cl*env) et du score « environnement et vulnérabilité » (cl*envuln).
prse_com <- sf::st_drop_geometry(couche_ipr(19, sprintf("insee=%s", CODE_COMMUNE)))
classes <- data.frame(
  classe = 1:6,
  part_pop_score_env = unlist(prse_com[paste0("cl", 1:6, "env_pop")]),
  part_pop_score_env_vuln = unlist(prse_com[paste0("cl", 1:6, "envuln_pop")])
)
classes[-1] <- lapply(classes[-1], round, 1)
utils::write.csv(classes, file.path(DOSSIER_TRAITE, "prse3_classes_commune.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(classes)

# Maille de 500 m : on garde les mailles dont la commune majoritaire est la nôtre.
prse_grille <- couche_ipr(18, sprintf("insee_majo=%s", CODE_COMMUNE),
                          champs = "num500,sco_env,cl_env,sco_envuln,cl_envuln,propair,propbruit,propeau,propssp,propied,pop_2016")
sf::st_write(prse_grille, file.path(DOSSIER_TRAITE, "prse3_grille.geojson"), delete_dsn = TRUE, quiet = TRUE)
print(sf::st_drop_geometry(prse_grille)[order(prse_grille$sco_env), ])
