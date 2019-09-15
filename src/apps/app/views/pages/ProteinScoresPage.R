ProteinScoresPage = R6Class(
    "ProteinScoresPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "protein-scores",
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                column(
                    width = 12,
                    box(
                        width = NULL,
                        plotlyOutput(ns("plot"), height = "250px")
                    ),
                    box(
                        width = NULL,
                        selectInput(
                            inputId = ns("id"),
                            "Fraction:",
                            choices = paste0("F", 0:8),
                            width = "25%"
                        ),
                        DTOutput(ns("table"))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$table = renderDT({
                datatable(
                    DATA$get_protein_scores(input$id) %>%
                        mutate(
                            uniprot_id = glue::glue('<a href="https://www.uniprot.org/uniprot/{uniprot_id}" target="_blank">{uniprot_id}</a>')
                        ),
                    escape = FALSE,
                    options = list(
                        pageLength = 50,
                        lengthMenu = c(25, 50, 100, 500)
                    ),
                    selection = list(
                        mode = "single",
                        selected = 13,
                        target = "column"
                    )
                )
            })
            
            output$plot = renderPlotly({
                DATA$protein_score_histogram(
                    input$id, input$table_columns_selected
                )
            })
        }
    )
)