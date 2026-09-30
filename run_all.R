# Relance toute la chaîne : téléchargement des données, calculs, puis tableau de bord.
# Usage : Rscript run_all.R   (depuis le dossier du projet)
# Pour une autre commune d'Île-de-France : modifier CODE_COMMUNE et NOM_COMMUNE dans R/00_config.R.

scripts <- c(
  "R/01_contours.R",               # quartiers IRIS
  "R/02_portrait.R",               # population et revenus
  "R/03_sante.R",                  # médecins, expositions environnementales
  "R/04_climat_vegetalisation.R",  # Regreen, espaces verts
  "R/05_biodiversite.R",           # espèces, espèces envahissantes
  "R/06_ecoute_citoyenne.R",       # questionnaire
  "R/07_risques.R",                # inondation, argiles, nappes, CatNat
  "R/08_air_interieur.R",          # établissements, radon
  "R/09_dechets.R",                # tonnages ADEME
  "R/10_plu.R",                    # zonage du PLU
  "R/11_permaculture.R"            # terrains communaux
)
for (s in scripts) {
  message("\n=== ", s, " ===")
  source(s, local = new.env(), encoding = "UTF-8")
}

quarto <- Sys.which("quarto")
if (!nzchar(quarto)) quarto <- "C:/Program Files/RStudio/resources/app/bin/quarto/bin/quarto.exe"  # Quarto fourni avec RStudio
system2(quarto, c("render", "tableau-de-bord.qmd"))
system2(quarto, c("render", "note/note-dgs.qmd"))

# Version publiée en ligne (GitHub Pages sert le dossier docs/ ; index.html est la page d'accueil).
dir.create("docs", showWarnings = FALSE)
file.copy("tableau-de-bord.html", "docs/index.html", overwrite = TRUE)
file.copy(c("note/note-dgs.html", "note/note-dgs.docx"), "docs", overwrite = TRUE)
