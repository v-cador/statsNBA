#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinybusy add_busy_spinner
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    add_busy_spinner(spin = "cube-grid"),
    # Your application UI logic
    fluidPage(
      h1("statsNBA"),

      fluidRow(
        column(2,
               mod_choix_journees_ui("select_journee")),
        column(10,
               mod_scoreurs_ui("scoreurs1", seuil_min_scoreurs = 20),
               HTML("<hr>"),
               mod_scoreurs_ui("scoreurs2", seuil_min_scoreurs = 40))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "statsNBA"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
