source("global.R")
import::here(Sidebar, .from="views/layouts/sidebar.R")
import::here(Body, .from="views/layouts/body.R")


App = R6Class(
    "App",
    inherit = ShinyModule,
    public = list(
        # attributes
        sidebar = NULL,
        body = NULL,
        
        # initializer
        initialize = function(){
            self$sidebar = Sidebar$new()
            self$body = Body$new()
        },
        
        # UI
        ui = function(){
            dashboardPage(
                header = dashboardHeader(title = "SEC Fractions"),
                sidebar = self$sidebar$ui(),
                body = self$body$ui(),
                title = "Proteomics Data"
            )
        },
        
        # server
        server = function(input, output, session){
            sidebarData = self$sidebar$call()
            observeEvent(sidebarData$update, {
                body = self$body$call(props = sidebarData)
            })
        }
    )
)

app = App$new()

shiny::shinyApp(ui = app$ui(), server = app$server)