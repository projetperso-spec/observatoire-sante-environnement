# Observatoire santé-environnement communal

Diagnostic santé-environnement d'une commune, à l'échelle de ses quartiers, construit uniquement à partir de données publiques et entièrement reproductible. Appliqué à **Croissy-sur-Seine (Yvelines)** et à son plan de transition « Le Croissy d'Après ».

Le projet produit :

- un **tableau de bord interactif** (`tableau-de-bord.html`) de 12 pages : portrait social, santé et air intérieur, climat et risques, végétalisation, biodiversité, PLU et permaculture, déchets, « Une seule santé », écoute des habitants, suivi des actions ;
- une **note de synthèse de 3 pages** à l'attention de la direction générale des services (`note/note-dgs.docx`) ;
- un **questionnaire** prêt à diffuser aux habitants (`enquete/questionnaire.md`) ;
- un **tableau de suivi** des 27 actions du « Croissy d'Après » (`suivi/actions_croissy_dapres.csv`).

## Principaux constats pour Croissy-sur-Seine

| Constat | Chiffre clé | Source |
|---|---|---|
| Accès aux médecins généralistes deux fois plus faible qu'en France | 1,60 consultation par habitant contre 3,26 (2024) | DREES |
| Vieillissement concentré dans le Vieux Croissy | 11,6 % de 80 ans ou plus, contre 5,8 % pour la commune | INSEE 2022 |
| Plaine des Courlis : pauvreté et risque d'inondation | 9 % de pauvreté ; 57 % de la surface dans l'enveloppe du PPRI | INSEE, Géoportail de l'Urbanisme |
| Centre Ville à végétaliser en premier | 0,3 m² d'espace vert public par habitant | L'Institut Paris Région, ARB Île-de-France |
| Espèces exotiques envahissantes réglementées | 6 espèces de la liste UE observées | GBIF |

## Méthode

Chaque module est un script R qui télécharge ses données, les croise avec les quartiers IRIS et écrit un fichier prêt pour le tableau de bord.

| Script | Contenu | Sources |
|---|---|---|
| `01_contours.R` | Quartiers IRIS | IGN (Géoplateforme) |
| `02_portrait.R` | Âges, catégories sociales, revenus, pauvreté | INSEE (recensement 2022, Filosofi 2021) |
| `03_sante.R` | Accès aux médecins, cumul d'expositions | DREES (APL), ORS Île-de-France (PRSE3) |
| `04_climat_vegetalisation.R` | Enjeux de renaturation, espaces verts publics | ARB Île-de-France et L'Institut Paris Région (Regreen) |
| `05_biodiversite.R` | Espèces observées, espèces envahissantes | GBIF, liste UE (règlement 1143/2014) |
| `06_ecoute_citoyenne.R` | Thèmes et citations des réponses au questionnaire | Enquête propre |
| `07_risques.R` | Inondation, argiles, nappes, arrêtés CatNat, radon | Géorisques, BRGM, Géoportail de l'Urbanisme |
| `08_air_interieur.R` | Établissements soumis à la surveillance de l'air intérieur | Éducation nationale, INSEE (BPE) |
| `09_dechets.R` | Tonnages par habitant, comparaison à des territoires similaires | ADEME (SINOE) |
| `10_plu.R` | Zonage du PLU croisé avec les besoins de renaturation | Géoportail de l'Urbanisme |
| `11_permaculture.R` | Présélection de terrains communaux | DGFiP (parcelles des personnes morales), cadastre Etalab |

## Relancer l'analyse

Prérequis : R (4.4 ou plus) avec les paquets `sf`, `terra`, `arrow`, `dplyr`, `leaflet`, `plotly`, `DT`, `readxl`, `jsonlite`, et Quarto (fourni avec RStudio).

```
Rscript run_all.R
```

Les données brutes sont téléchargées dans `data/raw/` (non versionné) ; les fichiers traités sont dans `data/processed/`.

**Pour une autre commune d'Île-de-France**, modifier `CODE_COMMUNE` et `NOM_COMMUNE` dans `R/00_config.R`. Les modules Regreen, espaces verts et PRSE3 ne couvrent que l'Île-de-France. Les textes de synthèse du tableau de bord et de la note citent des quartiers de Croissy et doivent être réécrits.

## Limites connues

- **Qualité de l'air extérieur** : le service de téléchargement des cartes annuelles d'Airparif renvoyait une erreur lors de la réalisation ; l'air n'est couvert qu'indirectement (PRSE3, Regreen).
- **Écoute citoyenne** : tant que `enquete/reponses.csv` n'existe pas, le tableau de bord affiche un jeu de **réponses fictives**, signalé comme tel sur la page.
- **Projections climatiques** : Climadiag Commune (Météo-France) n'a pas d'accès automatisé.
- **« Croissy d'Après »** : 9 intitulés d'actions sur 27 n'ont pas pu être relevés sur la plateforme de la ville ; aucun statut d'avancement n'est inventé.
- **Biodiversité** : les observations reflètent l'effort des observateurs et ne constituent pas un inventaire.
- **Risques** : cartes au 1/100 000, indicatives à l'échelle d'un quartier.

## Avertissement

Projet personnel réalisé à partir de données publiques. Il n'a pas été commandé par la commune de Croissy-sur-Seine et n'engage que son auteur.
