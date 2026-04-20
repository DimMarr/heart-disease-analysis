# Heart Disease — Projet Data Visualisation R

Analyse statistique du jeu de données **BRFSS 2020** (CDC) — facteurs de risque des maladies cardiaques.

## Structure du projet

```
projet/
├── data/
│   └── heart.csv           # Jeu de données (319 795 patients, 19 variables)
├── src/
│   ├── rapport.Rmd          # Rapport statistique complet
│   └── app.R                # Application R Shiny interactive
├── build/
│   ├── Dockerfile.rmd       # Image Docker pour le rendu du rapport
│   └── Dockerfile.shiny     # Image Docker pour l'application Shiny
├── docker-compose.yml       # Orchestration des deux services
└── README.md
```

## Lancement via Docker (recommandé)

### Prérequis
- [Docker](https://docs.docker.com/get-docker/) et [Docker Compose](https://docs.docker.com/compose/) installés

### Démarrage

```bash
docker compose up --build
```

Le premier build installe les packages R et rend le rapport (~10 min).

### Accès

| Service | URL |
|---------|-----|
| Rapport HTML | http://localhost:8080/rapport.html |
| Application Shiny | http://localhost:3838 |

### Arrêt

```bash
docker compose down
```

## Lancement local (sans Docker)

### Prérequis R

```r
install.packages(c(
  "rmarkdown", "knitr", "dplyr", "ggplot2", "tidyr", "scales",
  "kableExtra", "corrplot", "forcats", "broom", "gridExtra",
  "shiny", "shinydashboard", "plotly"
))
```

### Rendre le rapport

```r
rmarkdown::render("src/rapport.Rmd", output_file = "../rapport.html")
```

Ouvrir `rapport.html` dans un navigateur.

### Lancer l'application Shiny

```r
shiny::runApp("src/app.R")
```
