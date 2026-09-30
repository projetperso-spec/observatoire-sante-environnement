# Configuration commune à tous les scripts.
# Pour changer de commune : modifier CODE_COMMUNE et NOM_COMMUNE, puis relancer run_all.R.

CODE_COMMUNE <- "78190"              # code INSEE (pas le code postal)
NOM_COMMUNE  <- "Croissy-sur-Seine"

DOSSIER_BRUT    <- "data/raw"        # téléchargements (non versionnés)
DOSSIER_TRAITE  <- "data/processed"  # fichiers prêts pour le tableau de bord (versionnés)

dir.create(DOSSIER_BRUT, recursive = TRUE, showWarnings = FALSE)
dir.create(DOSSIER_TRAITE, recursive = TRUE, showWarnings = FALSE)
options(timeout = 900)

# Télécharge une seule fois : si le fichier est déjà là, on le réutilise.
telecharger <- function(url, nom) {
  chemin <- file.path(DOSSIER_BRUT, nom)
  if (!file.exists(chemin)) {
    message("Téléchargement : ", url)
    download.file(url, chemin, mode = "wb", quiet = TRUE)
  }
  chemin
}

# Lit un CSV INSEE national (séparateur ;) contenu dans un zip.
# Lecture en texte pour ne pas perdre les zéros des codes (ex. "0101").
lire_csv_zip <- function(zip, motif_fichier) {
  fichiers <- unzip(zip, list = TRUE)$Name
  csv <- fichiers[grepl(motif_fichier, fichiers, ignore.case = TRUE)][1]
  if (is.na(csv)) stop("Aucun fichier '", motif_fichier, "' dans ", zip)
  utils::read.csv(unz(zip, csv), sep = ";", colClasses = "character", check.names = FALSE)
}

# Garde les lignes de la commune : les 5 premiers caractères d'un code IRIS
# sont le code commune, ce filtre marche donc pour une colonne COM comme IRIS.
filtrer_commune <- function(d, colonne) d[substr(d[[colonne]], 1, 5) == CODE_COMMUNE, , drop = FALSE]

# Interroge une couche du serveur open data de L'Institut Paris Région (ArcGIS REST)
# et renvoie un objet sf en WGS84. Le serveur limite à 1000 objets par requête : on pagine.
IPR_SERVICE <- "https://geoweb.iau-idf.fr/agsmap1/rest/services/OPENDATA/OpendataIAU4/MapServer"
couche_ipr <- function(id_couche, where, champs = "*") {
  morceaux <- list(); decalage <- 0
  repeat {
    url <- paste0(IPR_SERVICE, "/", id_couche, "/query?where=", utils::URLencode(where, reserved = TRUE),
                  "&outFields=", champs, "&outSR=4326&f=json",  # Esri JSON : f=geojson est mal formé sur ce serveur
                  "&resultOffset=", decalage, "&resultRecordCount=1000")
    m <- sf::st_read(url, quiet = TRUE)
    if (nrow(m) == 0) break
    morceaux[[length(morceaux) + 1]] <- m
    if (nrow(m) < 1000) break
    decalage <- decalage + 1000
  }
  if (length(morceaux) == 0) stop("Couche ", id_couche, " : aucun objet pour ", where)
  do.call(rbind, morceaux)
}

# Convertit en nombre les colonnes INSEE (virgule ou point décimal, "s"/"nd" = secret).
en_nombre <- function(x) suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
