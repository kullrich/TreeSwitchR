#' @title TreeSwitchR
#' @name TreeSwitchR
#' @description
#' @param tree_df
#' @param treefile
#' @param posfile
#' @param gtffile
#' @param return.app Optional boolean indicating whether a Shiny app should be
#' returned [TRUE]
#' If \code{FALSE}, a named list of app elements (ui and server) will be
#' returned instead. Useful for deploying as a standalone shiny app.
#' @import shiny
#' @import shinydashboard
#' @import shinyWidgets
#' @examples
#' library(TreeSwitchR)
#' # Create app with no data loaded.
#' app <- TreeSwitchR()
#' if(interactive()) {
#'     shiny::runApp(app)
#' }
#' @export TreeSwitchR
#' @author Kristian K Ullrich
#'
TreeSwitchR <- function(
    tree_df = NULL,
    treefile = NULL,
    posfile = NULL,
    gtffile = NULL,
    return.app = TRUE
) {
    # Increase file upload size limit to 500MB
    options(shiny.maxRequestSize = 500 * 1024**2)
    if(!is.null(treefile)) {

    }
    if(!is.null(posfile)) {

    }
    if (!is.null(gtffile)) {

    }
    ui <- shinydashboard::dashboardPage(
        header = shinydashboard::dashboardHeader(title = "TreeSwitchR"),
        sidebar = shinydashboard::dashboardSidebar(
          shinydashboard::sidebarMenu(
            shinydashboard::menuItem("Data Input",
                shiny::fileInput(
                    "treefile",
                    "Choose Tree File",
                    accept = c(".txt", ".nwk")),
                shiny::fileInput(
                    "posfile",
                    "Choose tree Position File",
                    accept = c(".txt", ".bed", ".tsv")),
                shiny::fileInput(
                    "gtffile",
                    "Choose General Feature Format File (GFF3/GTF)",
                    accept = c(".gtf", ".gff", ".gff3"))
            ),
            shinydashboard::menuItem("Display Options",
                shinyWidgets::radioGroupButtons(
                    inputId = "Id062",
                    label = "Choose tree type",
                    choices = c("original",
                                "rooted",
                                "topology"),
                    direction = "vertical"
                ),
                shinyWidgets::prettyCheckbox(
                    inputId = "ShowFeatures",
                    label = "Show Features",
                    value = TRUE
                )
            ),
            shinydashboard::menuItem("About")
          )
        ),
        body = shinydashboard::dashboardBody(
        # Boxes need to be put in a row (or column)
            shiny::fluidRow(
                shinydashboard::box(
                    plotly::plotlyOutput(outputId = "plot1"),
                    width = NULL
                )
            )
        )
    )
    server <- function(input, output) {
      set.seed(122)
      #chrom <- NULL
      #chromPad <- 1
      #pts <- TreeSwitchR::plotTreeSummary(tree_df = tree_df)
      #output$plot1 <- plotly::renderPlotly({
      #  pts$fig
      #})
    }
    if(return.app) {
        shinyApp(
            ui = ui,
            server = server)
    } else {
        return(list(
            ui = ui,
            server = server))
    }
}
