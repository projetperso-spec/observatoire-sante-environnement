# Module 8 : déchets, tri et biodéchets (action 17 du « Croissy d'Après »).
# Source : ADEME, SINOE, flux de collecte - indicateurs de synthèse (kg par habitant et par an).
# Limite : la collecte relève de l'intercommunalité ; les chiffres portent sur tout son territoire,
# pas sur la seule commune.
source("R/00_config.R")

epci <- jsonlite::fromJSON(sprintf("https://geo.api.gouv.fr/communes/%s?fields=codeEpci,epci", CODE_COMMUNE))
NOM_EPCI <- epci$epci$nom
message("Intercommunalité : ", NOM_EPCI)

ademe <- function(parametre, champs) {
  u <- paste0("https://data.ademe.fr/data-fair/api/v1/datasets/rsqxbwsxhk-ngmmu5fcasf5t/lines?size=10000&",
              parametre, "&select=", paste(champs, collapse = ","))
  jsonlite::fromJSON(u)$results
}
champs <- c("annee", "n_acteur", "l_typologie", "c_dept", "ratio_omr", "ratio_ejm", "ratio_verre", "ratio_bio", "ratio_dma")

# SINOE écrit « Communauté d'agglomération X » là où l'API géo écrit « CA X » : on compare sans le préfixe.
coeur <- sub("^(CA|CC|CU|Métropole|Communauté (d'agglomération|de communes|urbaine))\\s+", "", NOM_EPCI)
territoire <- ademe(paste0("q=", utils::URLencode(coeur, reserved = TRUE)), champs)
territoire <- if (is.data.frame(territoire)) territoire[grepl(coeur, territoire$n_acteur, fixed = TRUE), ] else data.frame()
if (!nrow(territoire)) stop("Aucune donnée SINOE pour ", NOM_EPCI)
message("Acteur SINOE : ", unique(territoire$n_acteur))
typologie <- names(sort(table(territoire$l_typologie), decreasing = TRUE))[1]

# Référence : médiane des collectivités de même typologie (ex. « Urbain dense »), année par année.
ref <- ademe(paste0("l_typologie_in=", utils::URLencode(typologie, reserved = TRUE)), champs)
message(length(unique(ref$n_acteur)), " collectivités de typologie « ", typologie, " »")
mediane <- function(d, champ) tapply(d[[champ]], d$annee, stats::median, na.rm = TRUE)

indicateurs <- c(ratio_omr = "Ordures ménagères résiduelles", ratio_ejm = "Emballages et papiers",
                 ratio_verre = "Verre", ratio_bio = "Biodéchets et déchets verts", ratio_dma = "Total déchets ménagers et assimilés")
dechets <- do.call(rbind, lapply(names(indicateurs), function(ch) {
  t <- stats::aggregate(territoire[[ch]], list(annee = territoire$annee), mean, na.rm = TRUE)
  m <- mediane(ref, ch)
  data.frame(annee = t$annee, flux = indicateurs[[ch]], kg_hab_territoire = round(t$x, 1),
             kg_hab_mediane_typologie = round(as.numeric(m[as.character(t$annee)]), 1))
}))
dechets$territoire <- NOM_EPCI
dechets$typologie <- typologie
utils::write.csv(dechets, file.path(DOSSIER_TRAITE, "dechets.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(dechets[dechets$annee == max(dechets$annee), ])
