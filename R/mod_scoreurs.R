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
mod_scoreurs_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             selectInput(inputId = ns("saison"),
                         label = "Saisons",
                         choices = 2018:2024,
                         selected = 2023:2024,
                         multiple = TRUE),
             airDatepickerInput(inputId = ns("journee"),
                                label = "Journ\u00e9e de matchs",
                                value = Sys.Date(),
                                multiple = TRUE)),
      column(10,
             numericInput(inputId = ns("nb_last_games"),
                          label = "Nombre de matchs pr\u00e9c\u00e9dents",
                          value = 5, min = 0, max = 20, step = 1),
             numericInput(inputId = ns("seuil_pts"),
                          label = "Seuil de points",
                          value = 20, min = 20, max = 40, step = 5),
             DTOutput(outputId = ns("scores_joueurs")))
    )

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
mod_scoreurs_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Valeurs réactives utilisées uniquement dans ce module
    local_rv <- reactiveValues()

    observeEvent(input$saison,{
      cat("Get game logs for season",as.numeric(input$saison),"\n")
      global$game_data = suppressWarnings(game_logs(seasons = as.numeric(input$saison)))
      # Creation de la variable "season_game" pour trier sur les x derniers matchs
      global$game_data = global$game_data %>%
        mutate(season_game = factor(paste0(yearSeason, sprintf("%02d",numberGameTeamSeason)), ordered = TRUE))
    })

    observeEvent(input$journee, {
      cat("Get match days for day",input$journee,"\n")
      local_rv$matchs_du_jour = suppressWarnings(get_match_day(input$journee))
    })

    observeEvent(input$nb_last_games,{
      local_rv$nb_last_games = input$nb_last_games

      # id "saison/id_game" des derniers xx matchs
      # local_rv$last_x_season_game = global$game_data %>%
      #   arrange(desc(season_game)) %>%
      #   pull(season_game) %>% unique() %>%
      #   head(local_rv$nb_last_games)

      # Pts marqués sur les derniers matchs
      # local_rv$pts_last_games_long = global$game_data %>%
      #   # filter(season_game %in% local_rv$last_x_season_game) %>%
      #   right_join(get_last_x_games_per_team(global$game_data, local_rv$nb_last_games),
      #              by = c("dateGame","nameTeam")) %>%
      #   select(nameTeam, namePlayer, pts, id_last_game) %>%
      #   group_by(nameTeam, namePlayer) %>% mutate(pts_moy = round(mean(pts, na.rm = TRUE),1)) %>% ungroup() %>%
      #   arrange(desc(id_last_game))
      local_rv$pts_last_games_long <- get_pts_last_games_long(global$game_data,
                                                              local_rv$nb_last_games)
    })

    observeEvent(input$seuil_pts, {
      local_rv$seuil_pts = input$seuil_pts
      # Table du nb de pts par match pour chaque joueur
      local_rv$pts_last_games_long <- get_nb_pts_seuil(local_rv$pts_last_games_long,
                                                       local_rv$seuil_pts)
    })

    output$scores_joueurs <- renderDT({
      datatable(local_rv$matchs_du_jour %>%
                  select(nameTeam) %>%
                  left_join(local_rv$pts_last_games_long %>%
                              arrange(id_last_game) %>%
                              pivot_wider(names_from = id_last_game, values_from = pts),
                            by = "nameTeam") %>%
                  arrange(desc(pts_moy))) %>%
        formatStyle(as.character(1:local_rv$nb_last_games),
                    backgroundColor = styleInterval(cuts = c(20,25,30,35,40)-1,
                                                    values = c("#FFFFFF",c("#F5DCCF","#FFAA95","#cc8087","#8e646f","#696571"))))
    })

  })
}

## To be copied in the UI
# mod_scoreurs_ui("scoreurs_1")

## To be copied in the server
# mod_scoreurs_server("scoreurs_1")
