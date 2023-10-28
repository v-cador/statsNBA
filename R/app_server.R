#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  global <- reactiveValues()

  global$load_local_data = TRUE

  mod_choix_journees_server("select_journee", global = global)
  mod_scoreurs_server("scoreurs1", global = global, nb_joueurs = 1, seuil_min_scoreurs = 20)
  mod_scoreurs_server("scoreurs2", global = global, nb_joueurs = 2, seuil_min_scoreurs = 40)

}
