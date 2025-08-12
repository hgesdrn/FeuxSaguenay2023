# FEUX 2023 — Saguenay–Lac-Saint-Jean

Application **Shiny** pour l’affichage et l’exploration des polygones de feux de forêt 2023 au Saguenay–Lac-Saint-Jean (fond imagerie/positron, sélection d’un feu, zoom automatique, info-bulle Numéro de feu, panneau latéral avec info + barplot pseudo-log).

[![Deploy to shinyapps.io](https://github.com/hgesdrn/FeuxSaguenay2023/actions/workflows/deploy.yml/badge.svg?branch=main)](https://github.com/hgesdrn/FeuxSaguenay2023/actions/workflows/deploy.yml)

**Application en ligne :**  
`https://hgesdrn.shinyapps.io/FeuxSaguenay2023/`  

---

## Fonctionnalités
- Fond de carte **Esri World Imagery** ou **CartoDB Positron** au choix.
- Polygones **tous les feux** (remplissage #990000, opacité réglable) + **feu sélectionné** (contour #ff9900).
- **Zoom automatique** sur le feu choisi (menu déroulant ou clic sur la carte).
- **Info-bulle** (NOFEU) au survol.
- **Panneau gauche** : infos du feu (cause, superficie, dates) + **barplot** (axe Y pseudo-log, bornes fixes).
- Données lues **depuis GitHub Raw** (fallback local `data/` pour le dev).

---

## Données
data/
├── REGION_SAG_WGS84.qs # Polygone de la région du Saguenay (sf)
├── feux_2023_simpl.qs # Polygones des feux 2023 (sf) [NOFEU, SUP_HA, geometry, lid…]
└── feux_2023_table.csv # Table attributaire [NOFEU, CAUSE, SUP_HA, DATE_DEBUT, DATE_ETEIN]



