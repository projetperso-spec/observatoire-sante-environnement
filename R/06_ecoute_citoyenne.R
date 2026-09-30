# Module 5 : écoute citoyenne. Analyse des réponses au questionnaire (enquete/questionnaire.md).
#
# Fichier lu : enquete/reponses.csv (vraies réponses) s'il existe, sinon enquete/reponses_exemple.csv,
# un jeu FICTIF qui sert uniquement à tester la chaîne. Le tableau de bord affiche alors un avertissement.
#
# Les réponses libres sont classées par thème à l'aide d'un dictionnaire de mots-clés.
# Méthode volontairement simple et vérifiable : chacun peut lire la liste ci-dessous,
# la corriger, et comprendre pourquoi une réponse a été rangée dans un thème.
source("R/00_config.R")

fichier_reel <- "enquete/reponses.csv"
source_donnees <- if (file.exists(fichier_reel)) "reelles" else "exemple fictif"
rep <- utils::read.csv(if (source_donnees == "reelles") fichier_reel else "enquete/reponses_exemple.csv",
                       colClasses = "character", encoding = "UTF-8")
message(nrow(rep), " réponses (", source_donnees, ")")

# Expressions régulières, écrites sans accents (le texte est aussi débarrassé de ses accents).
# \\b = début ou fin de mot : sans lui, « air » reconnaîtrait « salaire » et « tri » « triste ».
THEMES <- list(
  "Chaleur et ombre"            = c("chaleur", "\\bchaud", "canicule", "fournaise", "\\bombr", "\\bfrais\\b", "fraiche"),
  "Arbres et espaces verts"     = c("arbre", "vegetal", "espaces? verts?", "square", "\\bparcs?\\b", "jardin", "beton", "bitume", "desimpermeab"),
  "Accès aux soins"             = c("medecin", "generaliste", "pediatre", "\\bkine", "infirmier", "maison de sante", "centre de sante", "urgences", "\\bsoins\\b", "rendez vous"),
  # (?<!plein ) : « en plein air » ne parle pas de pollution.
  "Qualité de l'air"            = c("(?<!plein )\\bair\\b", "respirat", "\\btouss", "\\bgaz\\b", "pollution", "humidite"),
  "Bruit"                       = c("bruit", "bruyant", "\\bcalme", "dormir"),
  "Déchets, tri et biodéchets"  = c("dechet", "poubelle", "\\btri\\b", "\\btrier\\b", "compost", "collecte"),
  "Biodiversité et espèces envahissantes" = c("biodiversite", "invasi", "envahi", "renouee", "especes?\\b", "\\bberges?\\b"),
  "Mobilité"                    = c("\\bvelo", "cyclable", "circulation", "voiture", "trottoir", "\\broutes?\\b"),
  "Isolement et personnes âgées" = c("\\bseule?s?\\b", "\\bisole", "personnes? agees?", "a mon age", "a domicile", "\\bvisites?\\b")
)

sans_accents <- function(x) tolower(iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Les apostrophes et tirets deviennent des espaces : « l'air » -> « l air », « rendez-vous » -> « rendez vous ».
texte <- gsub("['’-]", " ", sans_accents(paste(rep$sante_quartier, "|", rep$priorite)))
for (theme in names(THEMES)) {
  rep[[theme]] <- grepl(paste(THEMES[[theme]], collapse = "|"), texte, perl = TRUE)
}

# Contrôle des faux positifs sur des phrases pièges (le script s'arrête si l'un d'eux réapparaît).
pieges <- c("mon salaire est necessaire", "je suis triste", "seulement le dimanche", "une aire de jeux",
            "du sport en plein air")
stopifnot(!any(grepl(paste(c(THEMES[["Qualité de l'air"]], THEMES[["Déchets, tri et biodéchets"]],
                             THEMES[["Isolement et personnes âgées"]]), collapse = "|"), pieges, perl = TRUE)))

# --- Thèmes : part des répondants qui l'évoquent, en tout et par quartier -------
themes_total <- data.frame(
  theme = names(THEMES),
  nb_repondants = vapply(names(THEMES), function(t) sum(rep[[t]]), integer(1)),
  part = vapply(names(THEMES), function(t) round(100 * mean(rep[[t]]), 1), numeric(1))
)
themes_total <- themes_total[order(-themes_total$nb_repondants), ]

themes_quartier <- do.call(rbind, lapply(split(rep, rep$quartier), function(x) data.frame(
  quartier = x$quartier[1], theme = names(THEMES), nb_repondants_quartier = nrow(x),
  part = vapply(names(THEMES), function(t) round(100 * mean(x[[t]]), 1), numeric(1))
)))

# --- Verbatims : jusqu'à 3 citations par thème ---------------------------------
# On cite la réponse (question 6 ou 7) qui contient réellement le thème, pas l'autre.
prepare <- function(x) gsub("['’-]", " ", sans_accents(x))
verbatims <- do.call(rbind, lapply(names(THEMES), function(t) {
  x <- rep[rep[[t]], ]
  if (!nrow(x)) return(NULL)
  x <- x[seq_len(min(3, nrow(x))), ]
  motif <- paste(THEMES[[t]], collapse = "|")
  dans_q6 <- grepl(motif, prepare(x$sante_quartier), perl = TRUE)
  data.frame(theme = t, quartier = x$quartier, age = x$age,
             citation = ifelse(dans_q6, x$sante_quartier, x$priorite))
}))

# --- Questions fermées ---------------------------------------------------------
repartition <- function(colonne, niveaux) {
  d <- as.data.frame(table(factor(rep[[colonne]], levels = niveaux), rep$quartier))
  names(d) <- c("reponse", "quartier", "nb")
  d
}
lieu_frais <- repartition("lieu_frais", c("Oui", "Plutôt oui", "Plutôt non", "Non"))
delai_rdv  <- repartition("delai_rdv", c("Moins d'une semaine", "1 à 4 semaines", "Plus d'un mois",
                                         "Je n'ai pas réussi à en obtenir un", "Non concerné"))
participer <- as.data.frame(table(unlist(strsplit(rep$participer, ";", fixed = TRUE))))
names(participer) <- c("action", "nb")
participer <- participer[participer$action != "Aucun", ]
participer <- participer[order(-participer$nb), ]

ecrire <- function(d, nom) utils::write.csv(d, file.path(DOSSIER_TRAITE, nom), row.names = FALSE, fileEncoding = "UTF-8")
ecrire(themes_total, "ecoute_themes.csv")
ecrire(themes_quartier, "ecoute_themes_quartier.csv")
ecrire(verbatims, "ecoute_verbatims.csv")
ecrire(lieu_frais, "ecoute_lieu_frais.csv")
ecrire(delai_rdv, "ecoute_delai_rdv.csv")
ecrire(participer, "ecoute_participation.csv")
ecrire(data.frame(source = source_donnees, nb_reponses = nrow(rep),
                  sans_medecin_traitant = sum(rep$medecin_traitant == "Non")), "ecoute_meta.csv")

print(themes_total, row.names = FALSE)
print(participer, row.names = FALSE)
