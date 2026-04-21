library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(forcats)

# ── Données ──────────────────────────────────────────────────────────────────

DATA_PATH <- if (file.exists("data/heart.csv")) "data/heart.csv" else "../data/heart.csv"
df_raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE) %>%
  select(-1) %>%
  mutate(
    HeartDisease = factor(HeartDisease, levels = c("No", "Yes")),
    Smoking = factor(Smoking, levels = c("No", "Yes")),
    AlcoholDrinking = factor(AlcoholDrinking, levels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c("No", "Yes")),
    DiffWalking = factor(DiffWalking, levels = c("No", "Yes")),
    Sex = factor(Sex),
    Diabetic = factor(Diabetic),
    PhysicalActivity = factor(PhysicalActivity, levels = c("No", "Yes")),
    GenHealth = factor(GenHealth, levels = c("Poor", "Fair", "Good", "Very good", "Excellent")),
    Asthma = factor(Asthma, levels = c("No", "Yes")),
    KidneyDisease = factor(KidneyDisease, levels = c("No", "Yes")),
    SkinCancer = factor(SkinCancer, levels = c("No", "Yes")),
    AgeCategory = factor(AgeCategory, levels = c(
      "18-24", "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80 or older"
    )),
    Race = factor(Race)
  )

# Score de comorbidités (0–8)
bin_risk_vars <- c("Smoking","AlcoholDrinking","Stroke","DiffWalking",
                   "Asthma","KidneyDisease","SkinCancer")
df_raw <- df_raw %>%
  mutate(
    Diabetic_bin     = as.integer(Diabetic %in% c("Yes", "Yes (during pregnancy)")),
    ScoreComorbidite = rowSums(across(all_of(bin_risk_vars), ~ as.integer(. == "Yes"))) + Diabetic_bin
  )

PALETTE <- c("No" = "#43A047", "Yes" = "#E53935")
BLUE <- "#1565C0"

vars_quali <- c(
  "Tabagisme"           = "Smoking",
  "Alcool"              = "AlcoholDrinking",
  "Antécédent AVC"      = "Stroke",
  "Difficultés marche"  = "DiffWalking",
  "Sexe"                = "Sex",
  "Diabète"             = "Diabetic",
  "Activité physique"   = "PhysicalActivity",
  "Santé générale"      = "GenHealth",
  "Asthme"              = "Asthma",
  "Maladie rénale"      = "KidneyDisease",
  "Cancer peau"         = "SkinCancer",
  "Tranche d'âge"       = "AgeCategory",
  "Origine ethnique"    = "Race"
)

vars_quanti <- c(
  "IMC (BMI)" = "BMI",
  "Jours mauvaise santé physique" = "PhysicalHealth",
  "Jours mauvaise santé mentale" = "MentalHealth",
  "Heures de sommeil / nuit" = "SleepTime"
)

# Variables binaires Yes/No disponibles pour le Sankey
vars_sankey <- c(
  "Tabagisme"           = "Smoking",
  "Alcool"              = "AlcoholDrinking",
  "Antécédent AVC"      = "Stroke",
  "Difficultés marche"  = "DiffWalking",
  "Sexe"                = "Sex",
  "Activité physique"   = "PhysicalActivity",
  "Asthme"              = "Asthma",
  "Maladie rénale"      = "KidneyDisease",
  "Cancer peau"         = "SkinCancer"
)

theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey50", size = 11),
      legend.position = "top"
    )
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Heart Disease"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil",           tabName = "accueil",      icon = icon("heart")),
      menuItem("Exploration",       tabName = "exploration",  icon = icon("chart-bar")),
      menuItem("Analyse bivariée",  tabName = "bivariate",    icon = icon("not-equal")),
      menuItem("Facteurs de risque",tabName = "risque",       icon = icon("triangle-exclamation")),
      menuItem("Tendances",         tabName = "tendances",    icon = icon("chart-line")),
      menuItem("Comorbidités",      tabName = "comorbidites", icon = icon("layer-group")),
      menuItem("Flux Sankey",       tabName = "sankey",       icon = icon("diagram-project")),
      menuItem("Profil patient",    tabName = "profil",       icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #F5F7FA; }
      .box { border-top: 3px solid #1565C0; }
      .value-box .icon { font-size: 40px !important; }
      .small-box h3 { font-size: 32px; }
    "))),
    tabItems(
      # ── Accueil ────────────────────────────────────────────────────────────
      tabItem(
        "accueil",
        fluidRow(
          valueBoxOutput("vb_total", width = 3),
          valueBoxOutput("vb_malade", width = 3),
          valueBoxOutput("vb_pct", width = 3),
          valueBoxOutput("vb_vars", width = 3)
        ),
        fluidRow(
          box(
            width = 12, title = "À propos du jeu de données", status = "primary",
            solidHeader = TRUE,
            p(
              "Ce tableau de bord explore le jeu de données ",
              strong("Heart Disease (BRFSS 2020)"), " publié par le CDC américain."
            ),
            p(
              "Chaque ligne représente un patient. La variable cible ",
              code("HeartDisease"), " indique si le patient déclare une maladie cardiaque."
            ),
            tags$ul(
              tags$li(strong("319 795 patients"), " — enquête nationale représentative"),
              tags$li("18 variables : habitudes de vie, antécédents médicaux, données démographiques"),
              tags$li("Problématique globale : ", em("Quels facteurs sont associés au risque de maladie cardiaque ?"))
            ),
            hr(),
            plotlyOutput("accueil_target_plot", height = "280px")
          )
        )
      ),

      # ── Exploration univariée ──────────────────────────────────────────────
      tabItem(
        "exploration",
        fluidRow(
          box(width = 12, status = "primary",
            p(icon("circle-info"), " Explorez la distribution de chaque variable du jeu de données.
              Sélectionnez une variable ", strong("qualitative"), " pour obtenir un diagramme en barres
              des proportions, ou une variable ", strong("quantitative"), " pour un histogramme dont
              vous contrôlez le nombre de classes.")
          )
        ),
        fluidRow(
          box(
            width = 3, title = "Paramètres", status = "primary", solidHeader = TRUE,
            radioButtons("expl_type", "Type de variable",
              choices = c("Qualitative", "Quantitative"), selected = "Qualitative"
            ),
            conditionalPanel(
              "input.expl_type == 'Qualitative'",
              selectInput("expl_quali_var", "Variable", choices = vars_quali)
            ),
            conditionalPanel(
              "input.expl_type == 'Quantitative'",
              selectInput("expl_quanti_var", "Variable", choices = vars_quanti),
              sliderInput("expl_bins", "Nombre de classes", min = 10, max = 80, value = 40, step = 5)
            )
          ),
          box(
            width = 9, title = "Graphique", status = "primary", solidHeader = TRUE,
            plotlyOutput("expl_plot", height = "420px")
          )
        )
      ),

      # ── Analyse bivariée ───────────────────────────────────────────────────
      tabItem(
        "bivariate",
        fluidRow(
          box(width = 12, status = "warning",
            p(icon("circle-info"), " Comparez une variable au regard de ", strong("HeartDisease"),
              ". Pour une variable qualitative, les barres montrent la part de malades et de non-malades
              dans chaque modalité. Pour une variable quantitative, choisissez entre un ",
              strong("boxplot"), " (médiane + quartiles) et un ", strong("violin plot"),
              " (forme complète de la distribution). Le filtre HeartDisease permet d'isoler
              un sous-groupe.")
          )
        ),
        fluidRow(
          box(
            width = 3, title = "Paramètres", status = "warning", solidHeader = TRUE,
            radioButtons("biv_type", "Type de variable X",
              choices = c("Qualitative", "Quantitative"), selected = "Qualitative"
            ),
            conditionalPanel(
              "input.biv_type == 'Qualitative'",
              selectInput("biv_quali_var", "Variable X", choices = vars_quali)
            ),
            conditionalPanel("input.biv_type == 'Quantitative'",
              selectInput("biv_quanti_var", "Variable X", choices = vars_quanti),
              radioButtons("biv_plot_type", "Type de graphique",
                choices = c("Boxplot", "Violin"), selected = "Boxplot")
            ),
            hr(),
            checkboxGroupInput("biv_hd_filter", "Filtrer HeartDisease",
              choices = c("No", "Yes"), selected = c("No", "Yes")
            )
          ),
          box(
            width = 9, title = "Graphique", status = "warning", solidHeader = TRUE,
            plotlyOutput("biv_plot", height = "420px")
          )
        )
      ),

      # ── Facteurs de risque ─────────────────────────────────────────────────
      tabItem(
        "risque",
        fluidRow(
          box(width = 12, status = "danger",
            p(icon("circle-info"), " Pour chaque facteur, le graphique principal affiche le ",
              strong("taux de maladie cardiaque (%)"), " dans chaque modalité, trié par ordre
              décroissant. Dépliez la section ", em("Vue d'ensemble"), " pour comparer tous les
              facteurs binaires en un coup d'œil. La ", strong("heatmap"), " en bas croise l'âge
              et l'état de santé général pour révéler les zones à très haut risque.")
          )
        ),
        fluidRow(
          box(
            width = 3, title = "Paramètres", status = "danger", solidHeader = TRUE,
            selectInput("risque_var", "Facteur à analyser", choices = vars_quali),
            hr(),
            p(em("Taux de maladie cardiaque (%) pour chaque modalité du facteur sélectionné."))
          ),
          box(
            width = 9, title = "Taux de maladie cardiaque par facteur", status = "danger",
            solidHeader = TRUE,
            plotlyOutput("risque_plot", height = "420px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Vue d'ensemble — tous les facteurs binaires",
            status = "danger", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            plotlyOutput("risque_overview", height = "380px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Heatmap — Taux de HeartDisease par Âge et Santé générale",
            status = "danger", solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("risque_heatmap", height = "440px")
          )
        )
      ),

      # ── Tendances ──────────────────────────────────────────────────────────
      tabItem("tendances",
        fluidRow(
          box(width = 12, status = "primary",
            p(icon("circle-info"), " La ", strong("courbe de prévalence"), " montre comment le taux de
              maladie cardiaque évolue avec l'âge, séparément pour les hommes et les femmes —
              survolez les points pour afficher les valeurs exactes. La ",
              strong("pyramide des âges"), " en dessous représente la structure démographique de
              l'échantillon en distinguant les cas positifs (couleurs foncées) et négatifs
              (couleurs claires), côté Femmes à gauche et côté Hommes à droite.")
          )
        ),
        fluidRow(
          box(width = 12, title = "Prévalence de la maladie cardiaque par âge et sexe",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("tendances_prevalence", height = "420px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Pyramide des âges — distribution par sexe et HeartDisease",
            status = "primary", solidHeader = TRUE,
            plotlyOutput("tendances_pyramide", height = "520px")
          )
        )
      ),

      # ── Comorbidités ───────────────────────────────────────────────────────
      tabItem("comorbidites",
        fluidRow(
          box(width = 12, status = "success",
            p(icon("circle-info"), " Le ", strong("score de comorbidités"), " est calculé pour chaque
              patient en comptant le nombre de facteurs de risque simultanément présents parmi :
              Tabagisme, Alcool, AVC, Difficultés de marche, Diabète, Asthme, Maladie rénale,
              Cancer de la peau ", em("(score de 0 à 8)"), ". Le graphique de gauche montre
              combien de patients ont chaque score. Celui de droite révèle comment le risque de
              maladie cardiaque ", strong("augmente progressivement"), " à mesure que le score
              s'élève.")
          )
        ),
        fluidRow(
          box(width = 6, title = "Distribution du score de comorbidités",
            status = "success", solidHeader = TRUE,
            plotlyOutput("comorb_dist", height = "380px")
          ),
          box(width = 6, title = "Taux de HeartDisease par score",
            status = "success", solidHeader = TRUE,
            plotlyOutput("comorb_taux", height = "380px")
          )
        )
      ),

      # ── Flux Sankey ────────────────────────────────────────────────────────
      tabItem("sankey",
        fluidRow(
          box(width = 12, status = "info",
            p(icon("circle-info"), " Un ", strong("diagramme de Sankey"), " représente des flux :
              chaque bande est proportionnelle au nombre de patients qui passent d'une catégorie
              à l'autre. Sélectionnez deux variables intermédiaires dans le panneau gauche pour
              construire la chaîne ", em("Variable 1 → Variable 2 → HeartDisease"), " et
              visualiser comment les patients se répartissent à chaque étape.
              Survolez les nœuds ou les liens pour afficher les effectifs.")
          )
        ),
        fluidRow(
          box(width = 3, title = "Paramètres", status = "info", solidHeader = TRUE,
            p(em("Choisissez les deux variables intermédiaires.")),
            selectInput("sankey_v1", "1ère variable", choices = vars_sankey, selected = "Sex"),
            selectInput("sankey_v2", "2ème variable", choices = vars_sankey, selected = "Smoking"),
            hr(),
            p(em("Seules les variables binaires sont proposées pour garantir la lisibilité du diagramme."))
          ),
          box(width = 9, title = "Flux patients — Sankey", status = "info", solidHeader = TRUE,
            plotlyOutput("sankey_plot", height = "520px")
          )
        )
      ),

      # ── Profil patient ─────────────────────────────────────────────────────
      tabItem(
        "profil",
        fluidRow(
          box(width = 12, status = "info",
            p(icon("circle-info"), " Définissez un profil patient à gauche en renseignant ses
              caractéristiques (âge, sexe, habitudes de vie, antécédents). L'application filtre
              les patients du jeu de données qui correspondent à ce profil et compare leur ",
              strong("taux de maladie cardiaque"), " au taux global de la population.
              Le résumé coloré indique si le profil est à risque élevé ",
              span("(rouge)", style="color:#C62828;font-weight:bold;"), ", modéré ",
              span("(orange)", style="color:#F57F17;font-weight:bold;"), " ou faible ",
              span("(vert)", style="color:#2E7D32;font-weight:bold;"), ".")
          )
        ),
        fluidRow(
          box(width = 4, title = "Paramètres du profil", status = "info", solidHeader = TRUE,
            sliderInput("p_age",  "Tranche d'âge (indice 1–13)", min = 1, max = 13, value = 7),
            selectInput("p_sex",   "Sexe",    choices = c("Female","Male")),
            selectInput("p_smoke", "Tabagisme", choices = c("No","Yes")),
            selectInput("p_diab",  "Diabète",   choices = c("No","Yes","Yes (during pregnancy)")),
            selectInput("p_stroke","AVC",        choices = c("No","Yes")),
            selectInput("p_kidney","Maladie rénale", choices = c("No","Yes")),
            selectInput("p_health","Santé générale",
              choices = c("Poor","Fair","Good","Very good","Excellent"), selected = "Good"),
            sliderInput("p_bmi",   "IMC", min = 12, max = 70, value = 25, step = 0.5),
            sliderInput("p_sleep", "Heures de sommeil / nuit", min = 1, max = 24, value = 7)
          ),
          box(
            width = 8, title = "Profil par rapport à la population", status = "info",
            solidHeader = TRUE,
            plotlyOutput("profil_radar", height = "380px"),
            hr(),
            uiOutput("profil_resume")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {
  n_total <- nrow(df_raw)
  n_malade <- sum(df_raw$HeartDisease == "Yes")

  # ── Value boxes ──────────────────────────────────────────────────────────
  output$vb_total  <- renderValueBox(
    valueBox(format(n_total,  big.mark=" "), "Patients au total",
             icon = icon("users"), color = "blue")
  )
  output$vb_malade <- renderValueBox(
    valueBox(format(n_malade, big.mark=" "), "Cas positifs",
             icon = icon("heart-crack"), color = "red")
  )
  output$vb_pct    <- renderValueBox(
    valueBox(paste0(round(100*n_malade/n_total, 1), "%"), "Prévalence",
             icon = icon("percent"), color = "orange")
  )
  output$vb_vars <- renderValueBox(
    valueBox(18, "Variables disponibles", icon = icon("table-columns"), color = "green")
  )

  # ── Accueil — distribution cible ─────────────────────────────────────────
  output$accueil_target_plot <- renderPlotly({
    p <- df_raw %>%
      count(HeartDisease) %>%
      mutate(pct = n / sum(n),
             label = paste0(format(n, big.mark=" "), "\n(", percent(pct, .1), ")")) %>%
      ggplot(aes(x = HeartDisease, y = n, fill = HeartDisease, text = label)) +
      geom_col() +
      scale_fill_manual(values = PALETTE) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(
        title = "Distribution de la variable cible HeartDisease",
        x = "Maladie cardiaque", y = "Nombre de patients"
      ) +
      theme_app() +
      theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  # ── Exploration ──────────────────────────────────────────────────────────
  output$expl_plot <- renderPlotly({
    if (input$expl_type == "Qualitative") {
      var <- input$expl_quali_var
      lbl <- names(vars_quali)[vars_quali == var]
      p <- df_raw %>%
        count(.data[[var]]) %>%
        mutate(pct = n / sum(n)) %>%
        ggplot(aes(
          x = .data[[var]], y = pct, fill = .data[[var]],
          text = paste0(.data[[var]], ": ", percent(pct, .1), " (n=", n, ")")
        )) +
        geom_col(show.legend = FALSE) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste("Distribution —", lbl), x = NULL, y = "Proportion") +
        theme_app() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      ggplotly(p, tooltip = "text")
    } else {
      var <- input$expl_quanti_var
      lbl <- names(vars_quanti)[vars_quanti == var]
      p <- ggplot(df_raw, aes(x = .data[[var]])) +
        geom_histogram(bins = input$expl_bins, fill = BLUE) +
        scale_y_continuous(labels = label_number(big.mark = " ")) +
        labs(title = paste("Distribution —", lbl), x = NULL, y = "Effectif") +
        theme_app()
      ggplotly(p)
    }
  })

  # ── Analyse bivariée ─────────────────────────────────────────────────────
  output$biv_plot <- renderPlotly({
    req(length(input$biv_hd_filter) > 0)
    df_f <- df_raw %>% filter(HeartDisease %in% input$biv_hd_filter)

    if (input$biv_type == "Qualitative") {
      var <- input$biv_quali_var
      lbl <- names(vars_quali)[vars_quali == var]
      p <- df_f %>%
        count(.data[[var]], HeartDisease) %>%
        group_by(.data[[var]]) %>%
        mutate(pct = n / sum(n)) %>%
        ggplot(aes(x = .data[[var]], y = pct, fill = HeartDisease,
                   text = paste0(HeartDisease, ": ", percent(pct, .1)))) +
        geom_col(position = "fill") +
        scale_fill_manual(values = PALETTE) +
        scale_y_continuous(labels = percent_format()) +
        labs(title = paste(lbl, "vs HeartDisease"), x = NULL, y = "Proportion") +
        theme_app() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      ggplotly(p, tooltip = "text")
    } else {
      var <- input$biv_quanti_var
      lbl <- names(vars_quanti)[vars_quanti == var]
      if (input$biv_plot_type == "Violin") {
        p <- df_f %>%
          ggplot(aes(x = HeartDisease, y = .data[[var]], fill = HeartDisease)) +
          geom_violin(alpha = .7, trim = FALSE) +
          geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA, alpha = 0.9) +
          scale_fill_manual(values = PALETTE) +
          labs(title = paste(lbl, "selon HeartDisease — Violin plot"), x = NULL, y = lbl) +
          theme_app() + theme(legend.position = "none")
      } else {
        p <- df_f %>%
          ggplot(aes(x = HeartDisease, y = .data[[var]], fill = HeartDisease)) +
          geom_boxplot(alpha = .75, outlier.size = .4, outlier.alpha = .2) +
          scale_fill_manual(values = PALETTE) +
          labs(title = paste(lbl, "selon HeartDisease"), x = NULL, y = lbl) +
          theme_app() + theme(legend.position = "none")
      }
      ggplotly(p)
    }
  })

  # ── Facteurs de risque ────────────────────────────────────────────────────
  output$risque_plot <- renderPlotly({
    var <- input$risque_var
    lbl <- names(vars_quali)[vars_quali == var]
    p <- df_raw %>%
      group_by(.data[[var]], HeartDisease) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(.data[[var]]) %>%
      mutate(pct = n / sum(n)) %>%
      filter(HeartDisease == "Yes") %>%
      ggplot(aes(x = fct_reorder(.data[[var]], pct), y = pct,
                 text = paste0(.data[[var]], " : ", percent(pct, .1)))) +
      geom_col(fill = "#E53935", alpha = .85) +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, .15))) +
      coord_flip() +
      labs(
        title = paste("Taux de HeartDisease selon", lbl),
        x = NULL, y = "Taux de maladie cardiaque"
      ) +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  output$risque_overview <- renderPlotly({
    overview_vars <- c("Smoking","AlcoholDrinking","Stroke","DiffWalking",
                       "Diabetic","PhysicalActivity","Asthma","KidneyDisease","SkinCancer")
    df_taux <- lapply(overview_vars, function(v) {
      df_raw %>%
        filter(.data[[v]] == "Yes") %>%
        summarise(Variable = v, taux = mean(HeartDisease == "Yes"))
    }) %>% bind_rows()

    labels_map <- c(
      Smoking = "Tabagisme", AlcoholDrinking = "Alcool", Stroke = "AVC",
      DiffWalking = "Diff. marche", Diabetic = "Diabète",
      PhysicalActivity = "Activité physique", Asthma = "Asthme",
      KidneyDisease = "Maladie rénale", SkinCancer = "Cancer peau"
    )
    df_taux$Variable <- labels_map[df_taux$Variable]

    p <- df_taux %>%
      mutate(Variable = fct_reorder(Variable, taux)) %>%
      ggplot(aes(x = Variable, y = taux,
                 text = paste0(Variable, " : ", percent(taux, .1)))) +
      geom_col(fill = "#E53935", alpha = .8) +
      geom_hline(yintercept = mean(df_raw$HeartDisease == "Yes"),
                 linetype = "dashed", color = "grey40") +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, .1))) +
      coord_flip() +
      labs(
        title = "Taux de maladie cardiaque chez les individus positifs à chaque facteur",
        subtitle = "Ligne pointillée = taux global", x = NULL, y = "Taux"
      ) +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  # Heatmap Âge × Santé générale
  output$risque_heatmap <- renderPlotly({
    df_heat <- df_raw %>%
      group_by(AgeCategory, GenHealth) %>%
      summarise(
        taux = mean(HeartDisease == "Yes"),
        n    = n(),
        .groups = "drop"
      )

    p <- df_heat %>%
      ggplot(aes(x = GenHealth, y = AgeCategory, fill = taux,
                 text = paste0("Âge : ", AgeCategory,
                               "\nSanté : ", GenHealth,
                               "\nTaux HD : ", percent(taux, .1),
                               "\nN : ", format(n, big.mark = " ")))) +
      geom_tile(color = "white", linewidth = 0.5) +
      scale_fill_gradient2(
        low      = "#43A047",
        mid      = "#FFF176",
        high     = "#E53935",
        midpoint = 0.15,
        labels   = percent_format(),
        name     = "Taux HD"
      ) +
      labs(title = "Taux de HeartDisease par tranche d'âge et état de santé général",
           x = "Santé générale", y = "Tranche d'âge") +
      theme_app() + theme(legend.position = "right")
    ggplotly(p, tooltip = "text")
  })

  # ── Tendances ─────────────────────────────────────────────────────────────

  # Courbe de prévalence par âge et sexe
  output$tendances_prevalence <- renderPlotly({
    df_prev <- df_raw %>%
      group_by(AgeCategory, Sex) %>%
      summarise(
        taux = mean(HeartDisease == "Yes"),
        n    = n(),
        .groups = "drop"
      )

    p <- df_prev %>%
      ggplot(aes(x = AgeCategory, y = taux, color = Sex, group = Sex,
                 text = paste0(ifelse(Sex == "Female", "Femme", "Homme"),
                               " — ", AgeCategory,
                               "\nTaux : ", percent(taux, .1),
                               " (n=", format(n, big.mark = " "), ")"))) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 3.5) +
      scale_color_manual(
        values = c("Female" = "#E91E63", "Male" = "#1565C0"),
        labels = c("Female" = "Femme", "Male" = "Homme")
      ) +
      scale_y_continuous(labels = percent_format()) +
      labs(title = "Prévalence de la maladie cardiaque par âge et sexe",
           x = "Tranche d'âge", y = "Taux de HeartDisease", color = NULL) +
      theme_app() +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))
    ggplotly(p, tooltip = "text")
  })

  # Pyramide des âges
  output$tendances_pyramide <- renderPlotly({
    df_pyr <- df_raw %>%
      count(AgeCategory, Sex, HeartDisease) %>%
      mutate(
        n_oriented = ifelse(Sex == "Female", -n, n),
        grp        = interaction(Sex, HeartDisease),
        label      = paste0(
          ifelse(Sex == "Female", "Femme", "Homme"), " | ", AgeCategory,
          "\nHD : ", HeartDisease,
          "\nn = ", format(n, big.mark = " ")
        )
      )

    p <- df_pyr %>%
      ggplot(aes(x = AgeCategory, y = n_oriented, fill = grp, text = label)) +
      geom_col(position = "stack") +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) format(abs(x), big.mark = " "),
        name   = "Nombre de patients    ← Femmes | Hommes →"
      ) +
      scale_fill_manual(
        values = c(
          "Female.No"  = "#F48FB1",
          "Female.Yes" = "#C62828",
          "Male.No"    = "#90CAF9",
          "Male.Yes"   = "#0D47A1"
        ),
        labels = c(
          "Female.No"  = "Femme — No HD",
          "Female.Yes" = "Femme — Yes HD",
          "Male.No"    = "Homme — No HD",
          "Male.Yes"   = "Homme — Yes HD"
        ),
        name = NULL
      ) +
      labs(title = "Pyramide des âges par sexe et HeartDisease",
           x = "Tranche d'âge") +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  # ── Comorbidités ──────────────────────────────────────────────────────────

  output$comorb_dist <- renderPlotly({
    p <- df_raw %>%
      count(ScoreComorbidite) %>%
      mutate(pct = n / sum(n)) %>%
      ggplot(aes(
        x    = factor(ScoreComorbidite),
        y    = n,
        text = paste0("Score ", ScoreComorbidite, " : ",
                      format(n, big.mark = " "), " patients (",
                      percent(pct, .1), ")")
      )) +
      geom_col(fill = BLUE, alpha = .85) +
      scale_y_continuous(labels = label_number(big.mark = " ")) +
      labs(title = "Distribution du score de comorbidités",
           x = "Nombre de facteurs de risque", y = "Nombre de patients") +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  output$comorb_taux <- renderPlotly({
    df_ct <- df_raw %>%
      group_by(ScoreComorbidite) %>%
      summarise(
        taux = mean(HeartDisease == "Yes"),
        n    = n(),
        .groups = "drop"
      )

    p <- df_ct %>%
      ggplot(aes(
        x    = ScoreComorbidite,
        y    = taux,
        text = paste0("Score ", ScoreComorbidite,
                      "\nTaux HD : ", percent(taux, .1),
                      "\n(n=", format(n, big.mark = " "), ")")
      )) +
      geom_col(aes(fill = taux), show.legend = FALSE, alpha = .9) +
      geom_line(color = "grey40", linewidth = .8) +
      geom_point(color = "grey20", size = 2.5) +
      scale_fill_gradient(low = "#43A047", high = "#E53935") +
      scale_x_continuous(breaks = 0:8) +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, .1))) +
      labs(title = "Taux de HeartDisease selon le score de comorbidités",
           x = "Score de comorbidités", y = "Taux de HeartDisease") +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  # ── Sankey ────────────────────────────────────────────────────────────────
  output$sankey_plot <- renderPlotly({
    v1 <- input$sankey_v1
    v2 <- input$sankey_v2
    req(v1 != v2)

    lbl1 <- names(vars_sankey)[vars_sankey == v1]
    lbl2 <- names(vars_sankey)[vars_sankey == v2]

    df_flow <- df_raw %>%
      count(.data[[v1]], .data[[v2]], HeartDisease)

    v1_lvls <- as.character(unique(df_raw[[v1]]))
    v2_lvls <- as.character(unique(df_raw[[v2]]))
    hd_lvls <- c("No", "Yes")

    node_labels <- c(
      paste0(lbl1, ": ", v1_lvls),
      paste0(lbl2, ": ", v2_lvls),
      paste0("HD: ", hd_lvls)
    )

    n1 <- length(v1_lvls)
    n2 <- length(v2_lvls)

    v1_idx <- setNames(seq_along(v1_lvls) - 1L,          v1_lvls)
    v2_idx <- setNames(n1 + seq_along(v2_lvls) - 1L,     v2_lvls)
    hd_idx <- setNames(n1 + n2 + seq_along(hd_lvls) - 1L, hd_lvls)

    df_l1 <- df_flow %>%
      group_by(.data[[v1]], .data[[v2]]) %>%
      summarise(value = sum(n), .groups = "drop") %>%
      mutate(
        source = v1_idx[as.character(.data[[v1]])],
        target = v2_idx[as.character(.data[[v2]])]
      )

    df_l2 <- df_flow %>%
      group_by(.data[[v2]], HeartDisease) %>%
      summarise(value = sum(n), .groups = "drop") %>%
      mutate(
        source = v2_idx[as.character(.data[[v2]])],
        target = hd_idx[as.character(HeartDisease)]
      )

    node_colors <- c(
      rep("#1565C0", n1),
      rep("#7B1FA2", n2),
      c("#43A047", "#E53935")
    )

    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label     = node_labels,
        color     = node_colors,
        pad       = 20,
        thickness = 25,
        line      = list(color = "black", width = 0.5)
      ),
      link = list(
        source = c(df_l1$source, df_l2$source),
        target = c(df_l1$target, df_l2$target),
        value  = c(df_l1$value,  df_l2$value)
      )
    ) %>%
      layout(
        title = list(
          text = paste("Flux patients :", lbl1, "→", lbl2, "→ HeartDisease"),
          font = list(size = 15)
        ),
        font = list(size = 12)
      )
  })

  # ── Profil patient ────────────────────────────────────────────────────────
  profil_stats <- reactive({
    age_lvl <- levels(df_raw$AgeCategory)[input$p_age]
    df_raw %>%
      filter(
        AgeCategory   == age_lvl,
        Sex           == input$p_sex,
        Smoking       == input$p_smoke,
        Diabetic      == input$p_diab,
        Stroke        == input$p_stroke,
        KidneyDisease == input$p_kidney,
        GenHealth     == input$p_health
      )
  })

  output$profil_radar <- renderPlotly({
    df_p <- profil_stats()
    n_p <- nrow(df_p)

    if (n_p < 10) {
      return(plotly::plotly_empty() %>%
        layout(title = "Pas assez de patients correspondants à ce profil"))
    }

    taux_profil <- mean(df_p$HeartDisease == "Yes")
    taux_global <- mean(df_raw$HeartDisease == "Yes")

    df_bar <- tibble(
      Groupe = c("Taux global", "Votre profil"),
      Taux   = c(taux_global, taux_profil),
      N      = c(nrow(df_raw), n_p)
    )

    p <- df_bar %>%
      ggplot(aes(x = Groupe, y = Taux, fill = Groupe,
                 text = paste0(Groupe, "\nTaux : ", percent(Taux, .1),
                               "\n(n=", format(N, big.mark = " "), ")"))) +
      geom_col(width = 0.4, show.legend = FALSE) +
      geom_text(aes(label = percent(Taux, .1)), vjust = -0.4, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Taux global" = "#90CAF9", "Votre profil" = "#E53935")) +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0, .2))) +
      labs(
        title    = "Taux de maladie cardiaque — votre profil vs population",
        subtitle = paste0("Patients correspondants : ", format(n_p, big.mark = " ")),
        x = NULL, y = "Taux"
      ) +
      theme_app()

    ggplotly(p, tooltip = "text")
  })

  output$profil_resume <- renderUI({
    df_p <- profil_stats()
    n_p <- nrow(df_p)
    if (n_p < 10) {
      return(p("Profil trop rare dans le jeu de données."))
    }

    taux_profil <- mean(df_p$HeartDisease == "Yes")
    taux_global <- mean(df_raw$HeartDisease == "Yes")
    ratio <- round(taux_profil / taux_global, 1)
    age_lbl <- levels(df_raw$AgeCategory)[input$p_age]

    couleur <- if (taux_profil > taux_global * 1.5) {
      "#C62828"
    } else if (taux_profil < taux_global * 0.7) {
      "#2E7D32"
    } else {
      "#F57F17"
    }

    tagList(
      tags$div(
        style = paste0("border-left: 4px solid ", couleur, "; padding-left: 12px;"),
        tags$h4("Résumé du profil"),
        tags$p(strong("Tranche d'âge :"), age_lbl),
        tags$p(strong("Taux de maladie cardiaque dans ce profil :"),
               span(percent(taux_profil, .1),
                    style = paste0("color:", couleur, "; font-weight:bold;"))),
        tags$p(strong("Taux global :"), percent(taux_global, .1)),
        tags$p(strong("Ratio profil/global :"), paste0(ratio, "×")),
        tags$p(strong("Nombre de patients correspondants :"), format(n_p, big.mark = " "))
      )
    )
  })
}

shinyApp(ui, server)
