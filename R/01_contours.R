# Module 0 : contours des quartiers IRIS et de la commune (IGN, Géoplateforme WFS).
source("R/00_config.R")
suppressPackageStartupMessages(library(sf))

wfs <- function(couche, filtre) {
  url <- paste0(
    "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature",
    "&TYPENAMES=", couche, "&OUTPUTFORMAT=application/json",
    "&CQL_FILTER=", utils::URLencode(filtre, reserved = TRUE)
  )
  sf::st_read(url, quiet = TRUE)
}

iris <- wfs("STATISTICALUNITS.IRIS:contours_iris", sprintf("code_insee='%s'", CODE_COMMUNE))
if (nrow(iris) == 0) stop("Aucun IRIS trouvé pour la commune ", CODE_COMMUNE)
iris <- iris[, c("code_iris", "nom_iris", "type_iris")]
iris <- sf::st_transform(iris, 4326)

commune <- sf::st_union(iris) |> sf::st_sf(code_insee = CODE_COMMUNE, nom = NOM_COMMUNE, geometry = _)

sf::st_write(iris, file.path(DOSSIER_TRAITE, "iris.geojson"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(commune, file.path(DOSSIER_TRAITE, "commune.geojson"), delete_dsn = TRUE, quiet = TRUE)
message(nrow(iris), " IRIS enregistrés : ", paste(iris$nom_iris, collapse = ", "))
