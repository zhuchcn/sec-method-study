import::here(XICTablePage, .from="../pages/XICTablePage.R")
import::here(ProteinScoresPage, .from="../pages/ProteinScoresPage.R")
import::here(FiguresPage, .from="../pages/FiguresPage.R")


Body = R6Class(
    "Body",
    inherit = ShinyModule,
    public = list(
        # attributes
        xicPage = NULL,
        proteinScoresPage = NULL,
        figuresPage = NULL,
        
        # initializer
        initialize = function(){
            self$xicPage = XICTablePage$new()
            self$proteinScoresPage = ProteinScoresPage$new()
            self$figuresPage = FiguresPage$new()
        },
        
        # UI
        ui = function(){
            dashboardBody(
                tags$link(href="styles.css", rel = "stylesheet"),
                tabItems(
                    tabItem("xic", self$xicPage$ui()),
                    tabItem("protein-scores", self$proteinScoresPage$ui()),
                    tabItem("figures", self$figuresPage$ui())
                )
            )
        },
        
        # server
        server = function(input, output, session, props){
            observeEvent(props$update, {
                observeEvent(props$density, {
                    DATA$update_density(props$density)
                })
                DATA$update(
                    n_peptides = props$n_peptides, 
                    n_spectra = props$n_spectra, 
                    log_prob = props$log_prob,
                    best_log_prob = props$best_log_prob,
                    best_score = props$best_score,
                    coverage = props$coverage,
                    n_aa = props$n_aa,
                    exclude = props$exclude
                )
                self$xicPage$call()
                self$proteinScoresPage$call()
                self$figuresPage$call()
            })
        }
    )
)