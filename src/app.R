library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(forcats)
library(tidyr)

# ── Données ──────────────────────────────────────────────────────────────────

DATA_PATH <- if (file.exists("data/heart.csv")) "data/heart.csv" else "../data/heart.csv"
df_raw <- read.csv(DATA_PATH, stringsAsFactors = FALSE) %>%
  select(-1) %>%
  mutate(
    HeartDisease     = factor(HeartDisease,     levels = c("No", "Yes")),
    Smoking          = factor(Smoking,          levels = c("No", "Yes")),
    AlcoholDrinking  = factor(AlcoholDrinking,  levels = c("No", "Yes")),
    Stroke           = factor(Stroke,           levels = c("No", "Yes")),
    DiffWalking      = factor(DiffWalking,      levels = c("No", "Yes")),
    Sex              = factor(Sex),
    Diabetic         = factor(Diabetic),
    PhysicalActivity = factor(PhysicalActivity, levels = c("No", "Yes")),
    GenHealth        = factor(GenHealth, levels = c("Poor","Fair","Good","Very good","Excellent")),
    Asthma           = factor(Asthma,           levels = c("No", "Yes")),
    KidneyDisease    = factor(KidneyDisease,    levels = c("No", "Yes")),
    SkinCancer       = factor(SkinCancer,       levels = c("No", "Yes")),
    AgeCategory      = factor(AgeCategory, levels = c(
      "18-24","25-29","30-34","35-39","40-44","45-49",
      "50-54","55-59","60-64","65-69","70-74","75-79","80 or older"
    )),
    Race = factor(Race)
  )

PALETTE <- c("No" = "#43A047", "Yes" = "#E53935")
BLUE    <- "#1565C0"

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
  "IMC (BMI)"                          = "BMI",
  "Jours mauvaise santé physique"       = "PhysicalHealth",
  "Jours mauvaise santé mentale"        = "MentalHealth",
  "Heures de sommeil / nuit"            = "SleepTime"
)

theme_app <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey50", size = 11),
      legend.position = "bottom"
    )
}

# ── UI ───────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = "Heart Disease — Analyse"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil",           tabName = "accueil",   icon = icon("heart")),
      menuItem("Exploration",       tabName = "exploration", icon = icon("chart-bar")),
      menuItem("Analyse bivariée",  tabName = "bivariate",  icon = icon("not-equal")),
      menuItem("Facteurs de risque",tabName = "risque",     icon = icon("triangle-exclamation")),
      menuItem("Profil patient",    tabName = "profil",     icon = icon("user"))
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
      tabItem("accueil",
        fluidRow(
          valueBoxOutput("vb_total",   width = 3),
          valueBoxOutput("vb_malade",  width = 3),
          valueBoxOutput("vb_pct",     width = 3),
          valueBoxOutput("vb_vars",    width = 3)
        ),
        fluidRow(
          box(width = 12, title = "À propos du jeu de données", status = "primary",
            solidHeader = TRUE,
            p("Ce tableau de bord explore le jeu de données ",
              strong("Heart Disease (BRFSS 2020)"), " publié par le CDC américain."),
            p("Chaque ligne représente un patient. La variable cible ",
              code("HeartDisease"), " indique si le patient déclare une maladie cardiaque."),
            tags$ul(
              tags$li(strong("319 795 patients"), " — enquête nationale représentative"),
              tags$li("18 variables : habitudes de vie, antécédents médicaux, données démographiques"),
              tags$li("Problématique : ", em("Quels facteurs sont associés au risque de maladie cardiaque ?"))
            ),
            hr(),
            plotlyOutput("accueil_target_plot", height = "280px")
          )
        )
      ),

      # ── Exploration univariée ──────────────────────────────────────────────
      tabItem("exploration",
        fluidRow(
          box(width = 3, title = "Paramètres", status = "primary", solidHeader = TRUE,
            radioButtons("expl_type", "Type de variable",
              choices = c("Qualitative", "Quantitative"), selected = "Qualitative"),
            conditionalPanel("input.expl_type == 'Qualitative'",
              selectInput("expl_quali_var", "Variable", choices = vars_quali)
            ),
            conditionalPanel("input.expl_type == 'Quantitative'",
              selectInput("expl_quanti_var", "Variable", choices = vars_quanti),
              sliderInput("expl_bins", "Nombre de classes", min = 10, max = 80, value = 40, step = 5)
            )
          ),
          box(width = 9, title = "Graphique", status = "primary", solidHeader = TRUE,
            plotlyOutput("expl_plot", height = "420px")
          )
        )
      ),

      # ── Analyse bivariée ───────────────────────────────────────────────────
      tabItem("bivariate",
        fluidRow(
          box(width = 3, title = "Paramètres", status = "warning", solidHeader = TRUE,
            radioButtons("biv_type", "Type de variable X",
              choices = c("Qualitative", "Quantitative"), selected = "Qualitative"),
            conditionalPanel("input.biv_type == 'Qualitative'",
              selectInput("biv_quali_var", "Variable X", choices = vars_quali)
            ),
            conditionalPanel("input.biv_type == 'Quantitative'",
              selectInput("biv_quanti_var", "Variable X", choices = vars_quanti)
            ),
            hr(),
            checkboxGroupInput("biv_hd_filter", "Filtrer HeartDisease",
              choices = c("No", "Yes"), selected = c("No", "Yes"))
          ),
          box(width = 9, title = "Graphique", status = "warning", solidHeader = TRUE,
            plotlyOutput("biv_plot", height = "420px")
          )
        )
      ),

      # ── Facteurs de risque ─────────────────────────────────────────────────
      tabItem("risque",
        fluidRow(
          box(width = 3, title = "Paramètres", status = "danger", solidHeader = TRUE,
            selectInput("risque_var", "Facteur à analyser", choices = vars_quali),
            hr(),
            p(em("Taux de maladie cardiaque (%) pour chaque modalité du facteur sélectionné."))
          ),
          box(width = 9, title = "Taux de maladie cardiaque par facteur", status = "danger",
            solidHeader = TRUE,
            plotlyOutput("risque_plot", height = "420px")
          )
        ),
        fluidRow(
          box(width = 12, title = "Vue d'ensemble — tous les facteurs binaires", status = "danger",
            solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
            plotlyOutput("risque_overview", height = "380px")
          )
        )
      ),

      # ── Profil patient ─────────────────────────────────────────────────────
      tabItem("profil",
        fluidRow(
          box(width = 4, title = "Paramètres du profil", status = "info", solidHeader = TRUE,
            sliderInput("p_age",  "Tranche d'âge (indice 1–13)", min = 1, max = 13, value = 7),
            selectInput("p_sex",   "Sexe",    choices = c("Female","Male")),
            selectInput("p_smoke", "Tabagisme", choices = c("No","Yes")),
            selectInput("p_diab",  "Diabète",   choices = c("No","Yes","Yes (during pregnancy)")),
            selectInput("p_stroke","AVC",        choices = c("No","Yes")),
            selectInput("p_kidney","Maladie rénale", choices = c("No","Yes")),
            selectInput("p_health","Santé générale", choices = c("Poor","Fair","Good","Very good","Excellent"), selected = "Good"),
            sliderInput("p_bmi",   "IMC", min = 12, max = 70, value = 25, step = 0.5),
            sliderInput("p_sleep", "Heures de sommeil / nuit", min = 1, max = 24, value = 7)
          ),
          box(width = 8, title = "Profil par rapport à la population", status = "info",
            solidHeader = TRUE,
            plotlyOutput("profil_radar",  height = "380px"),
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

  n_total  <- nrow(df_raw)
  n_malade <- sum(df_raw$HeartDisease == "Yes")

  # Value boxes
  output$vb_total  <- renderValueBox(
    valueBox(format(n_total,  big.mark=" "), "Patients au total",   icon = icon("users"),        color = "blue")
  )
  output$vb_malade <- renderValueBox(
    valueBox(format(n_malade, big.mark=" "), "Cas positifs",         icon = icon("heart-crack"),  color = "red")
  )
  output$vb_pct    <- renderValueBox(
    valueBox(paste0(round(100*n_malade/n_total,1),"%"), "Prévalence", icon = icon("percent"),   color = "orange")
  )
  output$vb_vars   <- renderValueBox(
    valueBox(18, "Variables disponibles", icon = icon("table-columns"), color = "green")
  )

  # Accueil — distribution cible
  output$accueil_target_plot <- renderPlotly({
    p <- df_raw %>%
      count(HeartDisease) %>%
      mutate(pct = n / sum(n), label = paste0(format(n, big.mark=" "), "\n(", percent(pct,.1), ")")) %>%
      ggplot(aes(x = HeartDisease, y = n, fill = HeartDisease, text = label)) +
      geom_col(width = 0.45) +
      scale_fill_manual(values = PALETTE) +
      scale_y_continuous(labels = label_number(big.mark=" ")) +
      labs(title = "Distribution de la variable cible HeartDisease",
           x = "Maladie cardiaque", y = "Nombre de patients") +
      theme_app() + theme(legend.position = "none")
    ggplotly(p, tooltip = "text")
  })

  # ── Exploration ─────────────────────────────────────────────────────────
  output$expl_plot <- renderPlotly({
    if (input$expl_type == "Qualitative") {
      var <- input$expl_quali_var
      lbl <- names(vars_quali)[vars_quali == var]
      p <- df_raw %>%
        count(.data[[var]]) %>%
        mutate(pct = n / sum(n)) %>%
        ggplot(aes(x = .data[[var]], y = pct, fill = .data[[var]],
                   text = paste0(.data[[var]], ": ", percent(pct, .1), " (n=", n, ")"))) +
        geom_col(show.legend = FALSE) +
        scale_y_continuous(labels = percent_format()) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = paste("Distribution —", lbl), x = lbl, y = "Proportion") +
        theme_app() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      ggplotly(p, tooltip = "text")
    } else {
      var <- input$expl_quanti_var
      lbl <- names(vars_quanti)[vars_quanti == var]
      vals <- df_raw[[var]]
      p <- ggplot(df_raw, aes(x = .data[[var]])) +
        geom_histogram(bins = input$expl_bins, fill = BLUE, color = "white", alpha = .85) +
        scale_y_continuous(labels = label_number(big.mark=" ")) +
        labs(title = paste("Distribution —", lbl), x = lbl, y = "Effectif") +
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
                   text = paste0(HeartDisease, ": ", percent(pct,.1)))) +
        geom_col(position = "fill") +
        scale_fill_manual(values = PALETTE) +
        scale_y_continuous(labels = percent_format()) +
        labs(title = paste(lbl, "vs HeartDisease"), x = lbl, y = "Proportion") +
        theme_app() +
        theme(axis.text.x = element_text(angle = 30, hjust = 1))
      ggplotly(p, tooltip = "text")
    } else {
      var <- input$biv_quanti_var
      lbl <- names(vars_quanti)[vars_quanti == var]
      p <- df_f %>%
        ggplot(aes(x = HeartDisease, y = .data[[var]], fill = HeartDisease)) +
        geom_boxplot(alpha = .75, outlier.size = .4, outlier.alpha = .2) +
        scale_fill_manual(values = PALETTE) +
        labs(title = paste(lbl, "selon HeartDisease"), x = "Maladie cardiaque", y = lbl) +
        theme_app() + theme(legend.position = "none")
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
      arrange(desc(pct)) %>%
      ggplot(aes(x = fct_reorder(.data[[var]], pct), y = pct,
                 text = paste0(.data[[var]], " : ", percent(pct, .1)))) +
      geom_col(fill = "#E53935", alpha = .85) +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,.15))) +
      coord_flip() +
      labs(title = paste("Taux de HeartDisease selon", lbl),
           x = NULL, y = "Taux de maladie cardiaque") +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  output$risque_overview <- renderPlotly({
    bin_vars <- c("Smoking","AlcoholDrinking","Stroke","DiffWalking",
                  "Diabetic","PhysicalActivity","Asthma","KidneyDisease","SkinCancer")
    df_taux <- lapply(bin_vars, function(v) {
      df_raw %>%
        filter(.data[[v]] == "Yes") %>%
        summarise(Variable = v, taux = mean(HeartDisease == "Yes"))
    }) %>% bind_rows()

    labels_map <- c(
      Smoking="Tabagisme", AlcoholDrinking="Alcool", Stroke="AVC",
      DiffWalking="Diff. marche", Diabetic="Diabète",
      PhysicalActivity="Activité physique", Asthma="Asthme",
      KidneyDisease="Maladie rénale", SkinCancer="Cancer peau"
    )
    df_taux$Variable <- labels_map[df_taux$Variable]

    p <- df_taux %>%
      mutate(Variable = fct_reorder(Variable, taux)) %>%
      ggplot(aes(x = Variable, y = taux,
                 text = paste0(Variable, " : ", percent(taux, .1)))) +
      geom_col(fill = "#E53935", alpha = .8) +
      geom_hline(yintercept = mean(df_raw$HeartDisease == "Yes"),
                 linetype = "dashed", color = "grey40") +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,.1))) +
      coord_flip() +
      labs(title = "Taux de maladie cardiaque chez les individus positifs à chaque facteur",
           subtitle = "Ligne pointillée = taux global", x = NULL, y = "Taux") +
      theme_app()
    ggplotly(p, tooltip = "text")
  })

  # ── Profil patient ────────────────────────────────────────────────────────
  profil_stats <- reactive({
    age_lvl <- levels(df_raw$AgeCategory)[input$p_age]
    df_raw %>%
      filter(
        AgeCategory      == age_lvl,
        Sex              == input$p_sex,
        Smoking          == input$p_smoke,
        Diabetic         == input$p_diab,
        Stroke           == input$p_stroke,
        KidneyDisease    == input$p_kidney,
        GenHealth        == input$p_health
      )
  })

  output$profil_radar <- renderPlotly({
    df_p <- profil_stats()
    n_p  <- nrow(df_p)

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
                 text = paste0(Groupe, "\nTaux : ", percent(Taux, .1), "\n(n=", format(N, big.mark=" "), ")"))) +
      geom_col(width = 0.4, show.legend = FALSE) +
      geom_text(aes(label = percent(Taux, .1)), vjust = -0.4, size = 5, fontface = "bold") +
      scale_fill_manual(values = c("Taux global" = "#90CAF9", "Votre profil" = "#E53935")) +
      scale_y_continuous(labels = percent_format(), expand = expansion(mult = c(0,.2))) +
      labs(
        title    = "Taux de maladie cardiaque — votre profil vs population",
        subtitle = paste0("Patients correspondants : ", format(n_p, big.mark=" ")),
        x = NULL, y = "Taux"
      ) +
      theme_app()

    ggplotly(p, tooltip = "text")
  })

  output$profil_resume <- renderUI({
    df_p <- profil_stats()
    n_p  <- nrow(df_p)
    if (n_p < 10) return(p("Profil trop rare dans le jeu de données."))

    taux_profil <- mean(df_p$HeartDisease == "Yes")
    taux_global <- mean(df_raw$HeartDisease == "Yes")
    ratio       <- round(taux_profil / taux_global, 1)
    age_lbl     <- levels(df_raw$AgeCategory)[input$p_age]

    couleur <- if (taux_profil > taux_global * 1.5) "#C62828" else
               if (taux_profil < taux_global * 0.7) "#2E7D32" else "#F57F17"

    tagList(
      tags$div(style = paste0("border-left: 4px solid ", couleur, "; padding-left: 12px;"),
        tags$h4("Résumé du profil"),
        tags$p(strong("Tranche d'âge :"), age_lbl),
        tags$p(strong("Taux de maladie cardiaque dans ce profil :"),
               span(percent(taux_profil, .1), style = paste0("color:", couleur, "; font-weight:bold;"))),
        tags$p(strong("Taux global :"), percent(taux_global, .1)),
        tags$p(strong("Ratio profil/global :"), paste0(ratio, "×")),
        tags$p(strong("Nombre de patients correspondants :"), format(n_p, big.mark=" "))
      )
    )
  })
}

shinyApp(ui, server)
