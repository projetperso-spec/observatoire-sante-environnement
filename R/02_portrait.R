# Module 1 : portrait de territoire par quartier IRIS.
# Sources : INSEE, recensement 2022 (base infracommunale Population)
#           INSEE, Filosofi 2021 (revenus disponibles par IRIS, dernier millésime publié).
source("R/00_config.R")

# --- Recensement 2022 : âges, catégories sociales -----------------------------
zip_rp <- telecharger(
  "https://www.insee.fr/fr/statistiques/fichier/8647014/base-ic-evol-struct-pop-2022_csv.zip",
  "base-ic-evol-struct-pop-2022_csv.zip"
)
rp_france <- lire_csv_zip(zip_rp, "^base-ic.*\\.csv$")  # France entière : sert aussi aux références

vars <- c(
  pop = "P22_POP", pop0002 = "P22_POP0002", pop0014 = "P22_POP0014",
  pop65p = "P22_POP65P", pop75p = "P22_POP75P", pop80p = "P22_POP80P",
  pop15p = "C22_POP15P", cadres = "C22_POP15P_STAT_GSEC13_23",
  employes = "C22_POP15P_STAT_GSEC15_25", ouvriers = "C22_POP15P_STAT_GSEC16_26",
  retraites = "C22_POP15P_STAT_GSEC32"
)
valeurs <- as.data.frame(lapply(rp_france[vars], en_nombre))
names(valeurs) <- names(vars)

# Somme les effectifs d'un groupe de lignes puis calcule les parts (en %).
indicateurs <- function(v) {
  s <- colSums(v, na.rm = TRUE)
  data.frame(
    population       = s[["pop"]],
    part_0_2_ans     = 100 * s[["pop0002"]] / s[["pop"]],
    part_0_14_ans    = 100 * s[["pop0014"]] / s[["pop"]],
    part_65_ans_plus = 100 * s[["pop65p"]] / s[["pop"]],
    part_75_ans_plus = 100 * s[["pop75p"]] / s[["pop"]],
    part_80_ans_plus = 100 * s[["pop80p"]] / s[["pop"]],
    part_cadres      = 100 * s[["cadres"]] / s[["pop15p"]],
    part_employes_ouvriers = 100 * (s[["employes"]] + s[["ouvriers"]]) / s[["pop15p"]],
    part_retraites   = 100 * s[["retraites"]] / s[["pop15p"]]
  )
}

dans_commune <- rp_france$COM == CODE_COMMUNE
code_dep <- substr(CODE_COMMUNE, 1, 2)
iris_rp <- do.call(rbind, lapply(which(dans_commune), function(i) {
  cbind(code_iris = rp_france$IRIS[i], echelle = "IRIS", indicateurs(valeurs[i, ]))
}))
references <- rbind(
  cbind(code_iris = CODE_COMMUNE, echelle = "Commune", indicateurs(valeurs[dans_commune, ])),
  cbind(code_iris = code_dep, echelle = "Département", indicateurs(valeurs[substr(rp_france$COM, 1, 2) == code_dep, ])),
  cbind(code_iris = "FR", echelle = "France", indicateurs(valeurs))
)
rm(rp_france)

# --- Filosofi 2021 : revenus et pauvreté par IRIS -----------------------------
zip_filo <- telecharger(
  "https://www.insee.fr/fr/statistiques/fichier/8229323/BASE_TD_FILO_IRIS_2021_DISP_CSV.zip",
  "filo_iris_2021_disp.zip"
)
filo <- filtrer_commune(lire_csv_zip(zip_filo, "^BASE_TD_FILO_IRIS.*\\.csv$"), "IRIS")
revenus <- data.frame(
  code_iris             = filo$IRIS,
  revenu_median         = en_nombre(filo$DISP_MED21),   # € par unité de consommation et par an
  taux_pauvrete         = en_nombre(filo$DISP_TP6021),  # % sous le seuil de 60 % du niveau de vie médian
  rapport_interdecile   = en_nombre(filo$DISP_RD21),    # D9 / D1 : écart entre les 10 % les plus aisés et les plus modestes
  part_prestations_soc  = en_nombre(filo$DISP_PPSOC21)  # % du revenu venant des prestations sociales
)

portrait <- merge(iris_rp, revenus, by = "code_iris", all.x = TRUE)
iris_noms <- sf::st_drop_geometry(sf::st_read(file.path(DOSSIER_TRAITE, "iris.geojson"), quiet = TRUE))
portrait <- merge(iris_noms[, c("code_iris", "nom_iris")], portrait, by = "code_iris", all.y = TRUE)

references$nom_iris <- ifelse(references$echelle == "Commune", NOM_COMMUNE,
                       ifelse(references$echelle == "Département", paste0("Département (", code_dep, ")"), "France"))
references[setdiff(names(portrait), names(references))] <- NA
portrait <- rbind(portrait, references[names(portrait)])

num <- vapply(portrait, is.numeric, logical(1))
portrait[num] <- lapply(portrait[num], round, 1)
utils::write.csv(portrait, file.path(DOSSIER_TRAITE, "portrait.csv"), row.names = FALSE, fileEncoding = "UTF-8")
print(portrait)
