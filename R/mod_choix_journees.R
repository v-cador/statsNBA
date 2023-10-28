#' choix_journees UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shinyWidgets airDatepickerInput
mod_choix_journees_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("saison"),
                label = "Saisons",
                choices = 2018:2024,
                selected = 2023:2024,
                multiple = TRUE),
    airDatepickerInput(inputId = ns("journee"),
                       label = "Journ\u00e9e de matchs",
                       value = Sys.Date(),
                       multiple = TRUE)
  )
}

#' choix_journees Server Functions
#'
#' @importFrom dplyr mutate bind_rows select left_join filter
#'
#' @noRd
mod_choix_journees_server <- function(id, global){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$saison,{
      if(global$load_local_data){
        cat("OFFLINE : Get game logs until 27/10/2023 \n")
        global$game_data = readRDS("dev/game_data_20231027.RDS")
      } else {
        cat("Get game logs for season",as.numeric(input$saison),"\n")
        global$game_data = suppressWarnings(game_logs(seasons = as.numeric(input$saison)))
        # # Creation de la variable "season_game" pour trier sur les x derniers matchs
        # global$game_data = global$game_data %>%
        #   mutate(season_game = factor(paste0(yearSeason, sprintf("%02d",numberGameTeamSeason)), ordered = TRUE))
        cat("game logs OK \n")
      }

      # On calcule au format long pour les 20 derniers matchs pour tous les joueurs
      global$pts_last_20_games_long <- get_pts_last_games_long(global$game_data, nb_last_games = 20)

      # Et pour les binômes de joueurs
      global$pts_last_20_games_long = global$pts_last_20_games_long %>%
        mutate(nb_joueurs = 1) %>%
        # Ajout de la table pour 2 joueurs
        bind_rows(global$pts_last_20_games_long %>%
                    select(nameTeam, namePlayer, pts, id_last_game) %>%
                    left_join(global$pts_last_20_games_long %>%
                                select(nameTeam, namePlayer2=namePlayer, pts2=pts, id_last_game),
                              by = c("nameTeam","id_last_game")) %>%
                    filter(namePlayer < namePlayer2) %>% # Pour que (b,a) ne soit pas présent si (a,b) l'est déjà
                    mutate(pts = pts + pts2,
                           namePlayer = paste(namePlayer, namePlayer2, sep=" / ")) %>%
                    select(-c(pts2, namePlayer2)) %>%
                    mutate(nb_joueurs = 2))
    })

    observeEvent(input$journee, {
      if(global$load_local_data){
        cat("OFFLINE : Get match days of 27/10/2023 \n")
        global$matchs_du_jour = readRDS("dev/matchs_du_jour_20231027.RDS")
      } else {
        cat("Get match days for day",input$journee,"\n")
        global$matchs_du_jour = suppressWarnings(get_match_day(input$journee))
      }
    })
  })
}

## To be copied in the UI
# mod_choix_journees_ui("choix_journees_1")

## To be copied in the server
# mod_choix_journees_server("choix_journees_1")
