### Chargement des packages
packages <- c("shiny", "corrplot", "readr", "dplyr", "ggplot2", "caret", "pROC", 
              "ggpubr", "ggsignif", "car", "FactoMineR", "plotly")
lapply(packages, library, character.only = TRUE)

### Fonction utilitaire
tester_proportions <- function(variable) {
  chisq.test(table(variable))
}

make_boxplot <- function(data, xvar, yvar, title, xlab, ylab) {
  ggplot(data, aes_string(x = xvar, y = yvar, fill = xvar)) +
    geom_boxplot(color = "#1F2D44", fill = "#4FC3F7") +
    labs(title = title, x = xlab, y = ylab) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold"),
      legend.position = "none"
    )
}

make_barplot_chi2 <- function(df, xvar, fillvar, title, xlab, ylab, colors = c("#81D4FA", "#F06292")) {
  ggplot(df, aes_string(x = xvar, y = "Percentage", fill = fillvar)) +
    geom_bar(stat = "identity", position = position_dodge(), color = "#1F2D44") +
    labs(title = title, x = xlab, y = ylab) +
    scale_fill_manual(values = colors) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title = element_text(face = "bold")
    )
}

### Chargement et nettoyage des données
filepath <- file.path(Sys.getenv("USERPROFILE"), "OneDrive", "Bureau", "StudentPerformanceFactors.csv")
data <- read_csv(filepath) %>%
  na.omit() %>%
  filter(Teacher_Quality != "") %>%
  mutate(
    Teacher_Quality = factor(Teacher_Quality),
    EXAM_SCORE_CATEGORY = factor(ifelse(Exam_Score >= 69, "High", "Medium-Low"),
                                 levels = c("Medium-Low", "High"))
  )

### Séparation des variables
is_factor_or_char <- function(x) is.factor(x) || is.character(x)
df_qualitatives <- data %>% select(where(is_factor_or_char))
df_quantitatives <- data %>% select(where(is.numeric))

### Test du chi² sur les variables qualitatives
resultats <- lapply(df_qualitatives, tester_proportions)

### Corrélation
cor_matrix <- cor(df_quantitatives)

### ACP
acp_result <- PCA(df_quantitatives, graph = FALSE)
print(acp_result$eig)
plot(acp_result, choix = "var", title = "Projection sur les variables")
plot(acp_result, choix = "ind", title = "Projection sur les individus")

### Régression logistique
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(EXAM_SCORE_CATEGORY ~ Hours_Studied + Attendance + Parental_Involvement + 
                    Access_to_Resources + Extracurricular_Activities + Sleep_Hours + Previous_Scores + 
                    Motivation_Level + Internet_Access + Tutoring_Sessions + Family_Income + Teacher_Quality + 
                    School_Type + Peer_Influence + Physical_Activity + Learning_Disabilities + 
                    Parental_Education_Level + Distance_from_Home + Gender,
                  data = data, method = "glm", family = "binomial", trControl = train_control)
print(cv_model)

model <- glm(EXAM_SCORE_CATEGORY ~ Hours_Studied + Attendance + Parental_Involvement + 
               Access_to_Resources + Extracurricular_Activities + Previous_Scores + Motivation_Level + 
               Internet_Access + Tutoring_Sessions + Family_Income + Teacher_Quality + Peer_Influence + 
               Physical_Activity + Distance_from_Home, data = data, family = binomial)

set.seed(123)
train_index <- createDataPartition(data$EXAM_SCORE_CATEGORY, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
model_train <- glm(formula(model), data = train_data, family = binomial)

pred_prob <- predict(model_train, test_data, type = "response")
pred_class <- factor(ifelse(pred_prob > 0.5, "High", "Medium-Low"), levels = c("Medium-Low", "High"))
conf_matrix <- confusionMatrix(pred_class, test_data$EXAM_SCORE_CATEGORY)
print(conf_matrix)

roc_curve <- roc(test_data$EXAM_SCORE_CATEGORY, as.numeric(pred_prob))
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))
plot(roc_curve, col = "violet", main = paste("Courbe ROC du modèle de régression logistique ; AUC:", auc_value))

# Métriques
tp <- conf_matrix$table[2, 2]
tn <- conf_matrix$table[1, 1]
fp <- conf_matrix$table[1, 2]
fn <- conf_matrix$table[2, 1]

print(paste("TP:", tp))
print(paste("FP:", fp))
print(paste("FN:", fn))
print(paste("TN:", tn))

# Courbe d'apprentissage
train_accuracy <- 0.9813
test_accuracy <- 0.9524
accuracy_df <- data.frame(Ensemble = c("Entraînement", "Test"), Accuracy = c(train_accuracy, test_accuracy))
accuracy_plot <- ggplot(accuracy_df, aes(x = Ensemble, y = Accuracy, fill = Ensemble)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
  labs(title = "Comparaison de l'Accuracy Entraînement vs Test", x = "Ensemble", y = "Accuracy") +
  scale_fill_manual(values = c("#FF69B4", "#9400D3")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(accuracy_plot)

### Tests statistiques, Chi² et ANOVA

# Boxplots t.test
bp_hstudied_exam <- make_boxplot(data, "EXAM_SCORE_CATEGORY", "Hours_Studied", 
                                 "p-value < 2.2e-16", "Exam Score Category", "Hours Studied")

bp_attendance_exam <- make_boxplot(data, "EXAM_SCORE_CATEGORY", "Attendance", 
                                   "p-value < 2.2e-16", "Exam Score Category", "Attendance")

bp_tutor_exam <- make_boxplot(data, "EXAM_SCORE_CATEGORY", "Tutoring_Sessions", 
                              "p-value < 2.2e-16", "Exam Score Category", "Tutoring Sessions")

bp_sleep_exam <- make_boxplot(data, "EXAM_SCORE_CATEGORY", "Sleep_Hours", 
                              "p-value = 0.7328", "Exam Score Category", "Sleep Hours")

bp_exam_gender <- make_boxplot(data, "Gender", "Exam_Score", 
                               "p-value = 0.4697", "Gender", "Exam Score")

bp_exam_school <- make_boxplot(data, "School_Type", "Exam_Score", 
                               "p-value = 0.4697", "School Type", "Exam Score")

# Barplots chi²
table_chi2_plot <- function(x, y, title, xlab, ylab, colors = c("violet", "pink")) {
  tbl <- table(data[[x]], data[[y]])
  df <- as.data.frame(prop.table(tbl, 1) * 100)
  colnames(df) <- c(x, y, "Percentage")
  make_barplot_chi2(df, x, y, title, xlab, ylab, colors)
}

bp_exam_moti <- table_chi2_plot("Motivation_Level", "EXAM_SCORE_CATEGORY", 
                                "p-value = 1.876e-06", "Motivation Level", "Percentage")

bp_school_motiv <- table_chi2_plot("School_Type", "Motivation_Level", 
                                   "p-value = 0.5325", "School Type", "Percentage", c("violet", "pink", "lightgreen"))

bp_school_exam <- table_chi2_plot("School_Type", "EXAM_SCORE_CATEGORY", 
                                  "p-value = 0.7933", "School Type", "Percentage")

bp_fam_exam <- table_chi2_plot("Family_Income", "EXAM_SCORE_CATEGORY", 
                               "p-value = 2.338e-09", "Family Income", "Percentage")

bp_par_exam <- table_chi2_plot("Parental_Involvement", "EXAM_SCORE_CATEGORY", 
                               "p-value < 2.2e-16", "Parental Involvement", "Percentage")

bp_extrasco_exam <- table_chi2_plot("Extracurricular_Activities", "EXAM_SCORE_CATEGORY", 
                                    "p-value = 0.006944", "Extracurricular Activities", "Percentage")

bp_physical_exam <- table_chi2_plot("Physical_Activity", "EXAM_SCORE_CATEGORY", 
                                    "p-value = 0.2347", "Physical Activity", "Percentage")

bp_peer_exam <- table_chi2_plot("Peer_Influence", "EXAM_SCORE_CATEGORY", 
                                "p-value = 1.192e-12", "Peer Influence", "Percentage")

bp_gender_exam <- table_chi2_plot("Gender", "EXAM_SCORE_CATEGORY", 
                                  "p-value = 0.4235", "Gender", "Percentage")

# ANOVA figures
boxplot_teacher <- make_boxplot(data, "Teacher_Quality", "Exam_Score", 
                                "p-value = 5.88e-09", "Teacher Quality", "Exam Score")

boxplot_motivation_exam <- make_boxplot(data, "Motivation_Level", "Exam_Score", 
                                        "p-value = 7.49e-12", "Motivation Level", "Exam Score")

boxplot_motivation_tutor <- make_boxplot(data, "Motivation_Level", "Tutoring_Sessions", 
                                         "p-value = 0.584", "Motivation Level", "Tutoring Sessions")

boxplot_motivation_hours <- make_boxplot(data, "Motivation_Level", "Hours_Studied", 
                                         "p-value = 0.191", "Motivation Level", "Hours Studied")

# Toutes les visualisations (boxplots, barplots) sont générées à l’aide des fonctions utilitaires créées ci-dessus.
# Le serveur Shiny appelle chaque figure individuellement, sans perte.

### Interface utilisateur (UI)
library(shinythemes)
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(HTML(
    "h3 {
      text-align: center;
      margin-top: 20px;
      margin-bottom: 20px;
      font-weight: bold;
    }
    p {
      text-align: justify;
      font-size: 16px;
    }
    body {
      background-color: #ffffff;
      color: #333333;
      font-family: 'Segoe UI', sans-serif;
    }
    .container-fluid {
      padding: 20px;
    }
    .nav-tabs > li > a {
      font-weight: bold;
      background-color: #e3f2fd;
      color: #0d47a1;
      border-radius: 5px;
      margin-right: 4px;
    }"
  ))),
  titlePanel("Étude des facteurs influençant les performances des étudiants"),
  navlistPanel(
    "Introduction",
    tabPanel("Introduction",
             h3("Introduction"),
             p("- Études de données scolaires"),
             p("- Données générées par l'IA"),
             p("- Système de notation américain (moyenne ~70/100)"),
             p("- Échantillon: 6000 étudiants")),
    
    "Étude descriptive",
    tabPanel("Présentation des données",
             h3("Types des variables"),
             verbatimTextOutput("desc_str"),
             h3("Sommaire"),
             verbatimTextOutput("desc_summary"),
             h3("Niveaux des variables"),
             verbatimTextOutput("desc_levels")),
    
    tabPanel("ACP",
             fluidRow(
               column(6, plotOutput("acpvar")),
               column(6, plotOutput("acpind")))),
    
    "Vérification des biais",
    tabPanel("Variables qualitatives",
             verbatimTextOutput("proportions_VA_qual")),
    tabPanel("Corrélation entre variables",
             plotOutput("corr")),
    
    "Modélisation",
    tabPanel("Régression logistique",
             h3("Distribution des notes"),
             plotOutput("hist_plot"),
             h3("Validation croisée"),
             verbatimTextOutput("sortie_cvmodel"),
             h3("Test sur données test"),
             fluidRow(
               column(6, verbatimTextOutput("sortie_conf_mat")),
               column(6, plotOutput("plot_roc"))),
             plotOutput("acc")),
    
    "Tests statistiques",
    tabPanel("Motivation",
             plotlyOutput("bar_moti"),
             plotlyOutput("boxplot_motivation_exam"),
             plotlyOutput("boxplot_motivation_hours"),
             plotlyOutput("boxplot_motivation_tutor"),
             plotlyOutput("bar_motiv_school")),
    tabPanel("Assiduité",
             plotlyOutput("bp_hstudied"),
             plotlyOutput("bp_attendance"),
             plotlyOutput("bp_tutor")),
    tabPanel("Soutien parental",
             plotlyOutput("bar_par_exam"),
             plotlyOutput("bar_fam_exam")),
    tabPanel("École et activités",
             plotlyOutput("bpo_school_exam"),
             plotlyOutput("bar_school_exma_2"),
             plotlyOutput("bar_extrasco_exam"),
             plotlyOutput("bar_physical_exam"),
             plotlyOutput("bp_hsleep_exam")),
    tabPanel("Autres facteurs",
             plotlyOutput("boxplot_teacher"),
             plotlyOutput("bar_peer"),
             plotlyOutput("bar_gender"),
             plotlyOutput("bp_gender"))
  )
)

### Shiny Server refactorisé
server <- function(input, output) {
  output$desc_str <- renderPrint(str(data))
  output$desc_summary <- renderPrint(summary(data))
  output$desc_levels <- renderPrint(lapply(df_qualitatives, function(col) levels(as.factor(col))))
  
  output$proportions_VA_qual <- renderPrint({
    for (name in names(resultats)) {
      cat(name, ": p-value =", resultats[[name]]$p.value, "\n")
    }
  })
  
  output$acpvar <- renderPlot({ plot(acp_result, choix = "var", title = "Cercle des corrélations") })
  output$acpind <- renderPlot({ plot(acp_result, choix = "ind", title = "Projection des individus") })
  output$corr <- renderPlot({ corrplot(cor_matrix, method = "circle", type = "lower") })
  
  output$hist_plot <- renderPlot({
    ggplot(data, aes(x = Exam_Score)) +
      geom_histogram(bins = 30, fill = "violet", color = "black", alpha = 0.7) +
      labs(title = "Distribution des scores d'examen", x = "Score d'examen", y = "Nombre d'élèves") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_continuous(breaks = seq(min(data$Exam_Score), max(data$Exam_Score), by = 5))
  })
  
  output$sortie_cvmodel <- renderPrint({ print(cv_model); summary(cv_model) })
  output$sortie_conf_mat <- renderPrint({ conf_matrix })
  output$plot_roc <- renderPlot({ plot(roc_curve, col = "blue", main = paste("Courbe ROC ; AUC:", round(auc_value, 3))) })
  output$acc <- renderPlot({ accuracy_plot })
  
  # Plots dynamiques
  plots <- list(
    bp_hstudied = bp_hstudied_exam,
    bp_attendance = bp_attendance_exam,
    bp_tutor = bp_tutor_exam,
    bar_moti = bp_exam_moti,
    boxplot_motivation_exam = boxplot_motivation_exam,
    boxplot_motivation_hours = boxplot_motivation_hours,
    boxplot_motivation_tutor = boxplot_motivation_tutor,
    bar_motiv_school = bp_school_motiv,
    bpo_school_exam = bp_exam_school,
    bar_school_exma_2 = bp_school_exam,
    bar_fam_exam = bp_fam_exam,
    bar_par_exam = bp_par_exam,
    bar_physical_exam = bp_physical_exam,
    bar_extrasco_exam = bp_extrasco_exam,
    bp_hsleep_exam = bp_sleep_exam,
    boxplot_teacher = boxplot_teacher,
    bar_peer = bp_peer_exam,
    bar_gender = bp_gender_exam,
    bp_gender = bp_exam_gender
  )
  
  for (plot_name in names(plots)) {
    local({
      name <- plot_name
      output[[name]] <- renderPlotly({ ggplotly(plots[[name]]) })
    })
  }
}

### Lancement de l'application
shinyApp(ui, server)
