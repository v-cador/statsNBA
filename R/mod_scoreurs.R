#' scoreurs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#'
#' @importFrom shiny NS tagList selectInput numericInput dataTableOutput
#' @importFrom shinyWidgets airDatepickerInput
#' @importFrom DT DTOutput
mod_scoreurs_ui <- function(id, seuil_min_scoreurs = 20){
  ns <- NS(id)
  tagList(
    numericInput(inputId = ns("nb_last_games"),
                 label = "Nombre de matchs pr\u00e9c\u00e9dents",
                 value = 5, min = 0, max = 20, step = 1),
    numericInput(inputId = ns("seuil_pts"),
                 label = "Seuil de points",
                 value = seuil_min_scoreurs,
                 min = seuil_min_scoreurs,
                 max = seuil_min_scoreurs+20,
                 step = 5),
    DTOutput(outputId = ns("scores_joueurs"))
  )
}

#' scoreurs Server Functions
#'
#' @importFrom utils head
#' @importFrom shiny reactiveValues
#' @importFrom nbastatR game_logs
#' @importFrom dplyr select group_by arrange mutate ungroup left_join right_join desc
#' @importFrom tidyr pivot_wider
#' @importFrom DT datatable formatStyle styleInterval renderDT
#' @noRd
mod_scoreurs_server <- function(id, global, nb_joueurs, seuil_min_scoreurs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    cat("Entrée dans le module scoreurs avec",nb_joueurs,"joueurs \n")

    # Valeurs réactives utilisées uniquement dans ce module
    local_rv <- reactiveValues(nb_joueurs = nb_joueurs)

    observeEvent(c(input$nb_last_games, input$seuil_pts),{
      local_rv$nb_last_games = input$nb_last_games

      # Pts marqués sur les derniers matchs
      local_rv$pts_last_x_games_long <- global$pts_last_20_games_long %>%
        filter(nb_joueurs == local_rv$nb_joueurs,
               id_last_game <= local_rv$nb_last_games) %>%
        select(-nb_joueurs)

      local_rv$seuil_pts = input$seuil_pts
      # Table du nb de pts par match pour chaque joueur
      local_rv$pts_last_x_games_long <- get_nb_pts_seuil(local_rv$pts_last_x_games_long,
                                                         local_rv$seuil_pts)
    })

    output$scores_joueurs <- renderDT({
      datatable(global$matchs_du_jour %>%
                  select(nameTeam) %>%
                  left_join(local_rv$pts_last_x_games_long %>%
                              arrange(id_last_game) %>%
                              pivot_wider(names_from = id_last_game, values_from = pts),
                            by = "nameTeam") %>%
                  arrange(desc(pts_moy))) %>%
        formatStyle(as.character(1:local_rv$nb_last_games),
                    backgroundColor = styleInterval(cuts = seuil_min_scoreurs + seq(0,20,5) - 1,
                                                    values = c("#FFFFFF",c("#F5DCCF","#FFAA95","#cc8087","#8e646f","#696571"))))
    })

  })
}

## To be copied in the UI
# mod_scoreurs_ui("scoreurs_1")

## To be copied in the server
# mod_scoreurs_server("scoreurs_1")
