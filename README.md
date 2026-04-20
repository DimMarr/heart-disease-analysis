# heart-disease-analysis

> Analyse statistique des facteurs de risque cardiovasculaires — Rapport R Markdown & application Shiny interactive

Jeu de données **BRFSS 2020** (CDC) · 319 795 patients · 18 variables

## Structure du projet

```
projet/
├── data/
│   └── heart.csv                  # Jeu de données
├── src/
│   ├── rapport.Rmd                # Rapport statistique
│   └── app.R                      # Application R Shiny
├── build/
│   ├── Dockerfile.frontend        # Multi-stage : rendu Rmd + nginx
│   ├── Dockerfile.shiny           # Application Shiny
│   ├── nginx.conf                 # Reverse proxy (/ → rapport, /app → Shiny)
│   ├── shiny-server.conf          # Shiny-server au chemin /app
│   └── index.html                 # Landing page
├── .github/workflows/deploy.yml   # CI/CD → GitHub Pages
├── docker-compose.yml
└── README.md
```

## Lancement via Docker

### Prérequis
- [Docker](https://docs.docker.com/get-docker/) et Docker Compose installés

### Démarrage

```bash
docker compose up --build
```

Le premier build installe les packages R et rend le rapport (~10 min).

### Accès

| URL | Service |
|-----|---------|
| `http://localhost` | Landing page |
| `http://localhost/rapport.html` | Rapport statistique |
| `http://localhost/app` | Application R Shiny |

### Arrêt

```bash
docker compose down
```

## CI/CD — GitHub Pages

Le rapport est automatiquement rendu et déployé sur **GitHub Pages** à chaque push sur `main`
(si `src/rapport.Rmd` ou `data/` ont changé).

### Activation (une seule fois)

1. Aller dans **Settings → Pages** du dépôt GitHub
2. Source : **GitHub Actions**

Le rapport sera disponible à `https://<user>.github.io/heart-disease-analysis/`.

## Lancement local (sans Docker)

```r
install.packages(c(
  "rmarkdown", "knitr", "dplyr", "ggplot2", "tidyr", "scales",
  "kableExtra", "corrplot", "forcats", "broom", "gridExtra",
  "shiny", "shinydashboard", "plotly"
))

# Rapport
rmarkdown::render("src/rapport.Rmd", output_dir = ".", output_file = "rapport.html")

# App Shiny
shiny::runApp("src/app.R")
```
