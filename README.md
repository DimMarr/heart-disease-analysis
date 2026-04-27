# heart-disease-analysis

> Analyse statistique des facteurs de risque cardiovasculaires — Rapport R Markdown & application Shiny interactive

Jeu de données **BRFSS 2020** (CDC) · 319 795 patients · 18 variables

## Structure du projet

```
projet/
├── data/
│   └── heart.csv                         # Jeu de données
├── src/
│   ├── rapport.Rmd                       # Rapport statistique
│   └── app.R                             # Application R Shiny
├── build/
│   ├── Dockerfile.frontend               # Multi-stage : rendu Rmd + nginx
│   ├── Dockerfile.shiny                  # Application Shiny
│   ├── nginx.conf                        # Reverse proxy (/ → rapport, /app → Shiny)
│   ├── shiny-server.conf                 # Shiny-server au chemin /app
│   └── index.html                        # Landing page
├── .github/workflows/
│   ├── deploy.yml                        # CI/CD → GitHub Pages
│   └── docker-publish.yml               # CI/CD → GHCR (images Docker)
├── docker-compose.yml                    # Build local depuis les sources
├── docker-compose.ghcr.yml              # Lancement rapide via images GHCR
└── README.md
```

## Lancement via Docker

### Prérequis
- [Docker](https://docs.docker.com/get-docker/) et Docker Compose installés

---

### Option 1 — Images pré-buildées depuis GHCR (recommandé)

Télécharge et lance directement les images publiées sur le GitHub Container Registry,
sans avoir à installer R ni reconstruire quoi que ce soit (~30 secondes).

```bash
docker compose -f docker-compose.ghcr.yml pull
docker compose -f docker-compose.ghcr.yml up
```

> Les images sont publiées automatiquement à chaque push sur `main`.
> Elles sont publiques : aucune authentification requise pour les télécharger.

---

### Option 2 — Build local depuis les sources

Reconstruit toutes les images localement (installe les packages R, rend le rapport).
Utile pour tester des modifications avant de pousser.

```bash
docker compose up --build
```

> Le premier build est long (~10 min) car il installe les dépendances R.

---

### Accès

| URL | Service |
|-----|---------|
| `http://localhost` | Landing page |
| `http://localhost/rapport.html` | Rapport statistique |
| `http://localhost/app` | Application R Shiny |

### Arrêt

```bash
# Selon l'option utilisée
docker compose -f docker-compose.ghcr.yml down
# ou
docker compose down
```

## CI/CD — GitHub Pages

Le rapport est automatiquement rendu et déployé sur **GitHub Pages** à chaque push sur `main`
(si `src/rapport.Rmd` ou `data/` ont changé).

### Activation (une seule fois)

1. Aller dans **Settings → Pages** du dépôt GitHub
2. Source : **GitHub Actions**

Le rapport sera disponible à `https://<user>.github.io/heart-disease-analysis/`.

## CI/CD — GHCR (images Docker)

Les images Docker sont buildées et poussées automatiquement vers le **GitHub Container Registry**
à chaque push sur `main` (si les sources ou les Dockerfiles ont changé).

Les images publiées sont :

| Image | Description |
|-------|-------------|
| `ghcr.io/dimmarr/heart-disease-analysis-shiny:latest` | Application R Shiny |
| `ghcr.io/dimmarr/heart-disease-analysis-frontend:latest` | Nginx + rapport HTML |

Un tag `sha-<commit>` est également créé pour chaque build, permettant d'épingler une version précise.

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
