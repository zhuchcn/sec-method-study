Sidebar = R6Class(
    "Sidebar",
    inherit = ShinyModule,
    public = list(
        # attributes
        data = reactiveValues(
            update = 0,
            n_peptdies = 2,
            n_spectra = 0,
            log_score = 0,
            best_log_score = 0,
            best_score = 0,
            coverage = 0,
            n_aa = 0,
            exclude = character(),
            density = "210"
        ),
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            dashboardSidebar(
                sidebarMenu(
                    radioButtons(
                        "density",
                        "density",
                        choices = c("210", "250"),
                        selected = "210",
                        inline = TRUE
                    ),
                    numericInput(
                        "n_peptides",
                        "Minimal # of unique peptides",
                        min = 0, max = 20, step = 1, value = 2
                    ),
                    numericInput(
                        "n_spectra",
                        "Minimal # of spectra",
                        min = 0, max = 20, step = 1, value = 0
                    ),
                    numericInput(
                        "log_prob",
                        "Minimal |Log Prob|",
                        min = 0, max = 100, step = 0.01, value = 4.6
                    ),
                    numericInput(
                        "best_log_prob",
                        "Minimal Best |Log Prob|",
                        min = 0, max = 100, step = 0.01, value = 6.9
                    ),
                    numericInput(
                        "best_score",
                        "Minimal Best Score",
                        min = 0, max = 1000, step = 0.1, value = 400
                    ),
                    numericInput(
                        "coverage",
                        "Minimal Coverage",
                        min = 0, max = 100, step = 0.01, value = 0
                    ),
                    numericInput(
                        "n_aa",
                        "Minimal # of AA's in protein",
                        min = 0, max = 5000, step = 1, value = 0
                    ),
                    selectInput(
                        "exclude",
                        "Manully exclude:",
                        choices = DATA$get_all_protein_short_names(),
                        selected = c(
                            "SHRM3", "TRY1", "TRY3", "TRY6", "K2C1", "K1C9",
                            "K1C10", "K1C14", "K22E", "K2C5", "K2C6B", "K2C1B"
                        ),
                        multiple = TRUE
                    ),
                    menuItem("XIC Table", tabName = "xic"),
                    menuItem("Protein Scores", tabName = "protein-scores"),
                    menuItem("Figures", tabName = "figures")
                )
            )
        },
        
        # server
        server = function(input, output, session){
            observeEvent(input$density, {
                self$data$density = input$density
                self$update()
            })
            observeEvent(input$n_peptides, {
                self$data$n_peptides = input$n_peptides
                self$update()
            })
            observeEvent(input$n_spectra, {
                self$data$n_spectra = input$n_spectra
                self$update()
            })
            observeEvent(input$exclude, {
                self$data$exclude = input$exclude
                self$update()
            })
            observeEvent(input$log_prob, {
                self$data$log_prob = input$log_prob
                self$update()
            })
            observeEvent(input$best_log_prob, {
                self$data$best_log_prob = input$best_log_prob
                self$update()
            })
            observeEvent(input$best_score, {
                self$data$best_score = input$best_score
                self$update()
            })
            observeEvent(input$coverage, {
                self$data$coverage = input$coverage
                self$update()
            })
            observeEvent(input$n_aa, {
                self$data$n_aa = input$n_aa
                self$update()
            })
            
            return(self$data)
        },
        
        update = function(){
            self$data$update = self$data$update + 1
        }
    )
)