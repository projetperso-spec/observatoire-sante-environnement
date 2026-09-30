# Module 6 : risques naturels et vulnérabilité climatique (complète le module chaleur).
# Sources : Géorisques (API : radon, arrêtés CatNat, TRI, atlas des zones inondables ;
#           WFS BRGM : retrait-gonflement des argiles, remontées de nappe),
#           Géoportail de l'Urbanisme (servitude PM1 : enveloppe du plan de prévention des risques d'inondation).
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))
sf::sf_use_s2(FALSE)

iris <- sf::st_read(file.path(DOSSIER_TRAITE, "iris.geojson"), quiet = TRUE)
iris_l93 <- sf::st_transform(iris, 2154)
commune_l93 <- sf::st_union(iris_l93)
api <- function(chemin) jsonlite::fromJSON(paste0("https://georisques.gouv.fr/api/v1/", chemin))

# --- Données communales (API Géorisques) --------------------------------------
radon <- api(sprintf("radon?code_insee=%s", CODE_COMMUNE))$data
catnat <- api(sprintf("gaspar/catnat?code_insee=%s&page_size=100", CODE_COMMUNE))$data
tri <- api(sprintf("gaspar/tri?code_insee=%s", CODE_COMMUNE))$data
catnat <- data.frame(debut = catnat$date_debut_evt, fin = catnat$date_fin_evt,
                     risque = catnat$libelle_risque_jo, arrete_jo = catnat$date_publication_jo)
catnat <- catnat[order(as.Date(catnat$debut, "%d/%m/%Y"), decreasing = TRUE), ]
utils::write.csv(catnat, file.path(DOSSIER_TRAITE, "catnat.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Couches cartographiques ---------------------------------------------------
# Le WFS du BRGM attend une emprise en Lambert 93 (en WGS84 l'ordre des axes le fait renvoyer zéro objet).
bb <- sf::st_bbox(commune_l93)
wfs_brgm <- function(couche) {
  u <- sprintf(paste0("https://mapsref.brgm.fr/wxs/georisques/risques?service=WFS&version=1.1.0&request=GetFeature",
                      "&typeName=%s&srsName=EPSG:2154&bbox=%.0f,%.0f,%.0f,%.0f,EPSG:2154"),
               couche, bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
  x <- sf::st_read(u, quiet = TRUE)
  sf::st_set_crs(sf::st_make_valid(x), 2154)
}
decouper <- function(x) suppressWarnings(sf::st_intersection(x, sf::st_sf(geometry = commune_l93)))

# Argiles : le champ id vaut 1, 2 ou 3 (exposition faible, moyenne, forte). Sens vérifié en
# interrogeant l'API Géorisques /rga en un point de chaque classe (id 2 -> « Exposition moyenne »).
argiles <- decouper(wfs_brgm("ms:ALEARG_REALISE")[, "id"])
argiles$exposition <- c("Faible", "Moyenne", "Forte")[argiles$id]

nappes <- decouper(wfs_brgm("ms:REMNAPPE")[, "classe"])

# Enveloppe du PPRI (servitude PM1 des Yvelines). Sur ce WFS, BBOX en EPSG:4326 attend latitude puis longitude.
bbl <- sf::st_bbox(iris)
u <- paste0("https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=wfs_sup:assiette_sup_s",
            "&OUTPUTFORMAT=application/json&COUNT=500&CQL_FILTER=",
            utils::URLencode(sprintf("BBOX(the_geom,%f,%f,%f,%f)", bbl["ymin"], bbl["xmin"], bbl["ymax"], bbl["xmax"]), reserved = TRUE))
sup <- sf::st_read(u, quiet = TRUE)
ppri <- sup[sup$suptype == "pm1" & grepl(paste0("_SUP_", substr(CODE_COMMUNE, 1, 2), "_PM1"), sup$partition), ]
ppri <- decouper(sf::st_make_valid(sf::st_transform(sf::st_union(ppri), 2154)) |> sf::st_sf(geometry = _))

# --- Part de chaque quartier concernée ------------------------------------------
part_dans <- function(couche) {
  vapply(seq_len(nrow(iris_l93)), function(i) {
    x <- suppressWarnings(sf::st_intersection(sf::st_geometry(couche), sf::st_geometry(iris_l93)[i]))
    100 * as.numeric(sum(sf::st_area(x))) / as.numeric(sf::st_area(iris_l93[i, ]))
  }, numeric(1))
}
par_iris <- data.frame(
  code_iris = iris_l93$code_iris, nom_iris = iris_l93$nom_iris,
  part_ppri = part_dans(ppri),
  part_argiles_moyenne_forte = part_dans(argiles[argiles$id >= 2, ]),
  part_nappe_debordement = part_dans(nappes[grepl("débordements", nappes$classe), ]),
  part_nappe_caves = part_dans(nappes[grepl("inondations de cave", nappes$classe), ])
)
par_iris[-(1:2)] <- lapply(par_iris[-(1:2)], round, 1)
utils::write.csv(par_iris, file.path(DOSSIER_TRAITE, "risques_iris.csv"), row.names = FALSE, fileEncoding = "UTF-8")

synthese <- data.frame(
  indicateur = c("Potentiel radon (1 = faible, 3 = significatif)", "Arrêtés de catastrophe naturelle",
                 "Territoire à risque important d'inondation", "Surface en exposition moyenne ou forte aux argiles (ha)",
                 "Surface dans l'enveloppe du PPRI (ha)", "Surface de la commune (ha)"),
  valeur = c(radon$classe_potentiel[1], nrow(catnat),
             if (length(tri) && nrow(tri)) tri$libelle_tri[1] else "Non",
             round(sum(as.numeric(sf::st_area(argiles[argiles$id >= 2, ]))) / 1e4, 1),
             round(sum(as.numeric(sf::st_area(ppri))) / 1e4, 1),
             round(as.numeric(sf::st_area(commune_l93)) / 1e4, 1))
)
utils::write.csv(synthese, file.path(DOSSIER_TRAITE, "risques_synthese.csv"), row.names = FALSE, fileEncoding = "UTF-8")

ecrire_geo <- function(x, nom) sf::st_write(sf::st_transform(x, 4326), file.path(DOSSIER_TRAITE, nom), delete_dsn = TRUE, quiet = TRUE)
ecrire_geo(argiles, "argiles.geojson")
ecrire_geo(nappes, "nappes.geojson")
ecrire_geo(ppri, "ppri.geojson")

print(synthese); print(par_iris); print(catnat)
