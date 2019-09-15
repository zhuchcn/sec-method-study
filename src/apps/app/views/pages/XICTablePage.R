XICTablePage = R6Class(
    "XICTablePage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id= 'xic',
        data = reactiveValues(),
        
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
                        tags$div(
                            class = "col-sm-12 col-md-8 col-lg-4",
                            selectInput(
                                ns("param"),
                                "Parameter:",
                                choices = c(
                                    "xic", "# of unique peptides", "# of spectra", 
                                    "|Log Prob|", "Best |Log Prob|", "Best score",
                                    "Coverage %", "# AA's in protein"
                                ),
                                selected = "xic"
                            )
                        ),
                        DTOutput(ns("table"))
                    )
                )
            )
        },
        
        # server
        server = function(input, output, session){
            
            observeEvent(input$param, {
                if(input$param == "xic") {
                    self$data$table = DATA$get_xic_table()
                } else {
                    self$data$table = DATA$get_score_table(input$param)
                }
            })
            
            output$table = renderDT({
                datatable(
                    self$data$table %>%
                        mutate(
                            long_name = glue::glue('<a href="https://www.uniprot.org/uniprot/{uniprot_id}" target="_blank">{long_name}</a>')
                        ),
                    escape = FALSE,
                    options = list(
                        pageLength = 25,
                        lengthMenu = c(10, 25, 50, 100, 500)
                    ),
                    selection = list(
                        mode = "single",
                        selected = "1"
                    )
                ) %>%
                    formatSignif(paste0("F", 0:7), digits = 3)
            })
            
            output$plot = renderPlotly({
                self$plot(index = input$table_rows_selected)
            })
        },
        
        plot = function(index){
            data.frame(
                value = as.numeric(self$data$table[index, paste0("F", 0:7)]),
                fraction = paste0("F", 0:7)
            ) %>%
                ggplot() +
                geom_col(aes(x = fraction, y = value), fill = "steelblue") +
                labs(title = as.character(self$table$long_name[index])) +
                theme_bw() +
                theme(
                    axis.title = element_blank()
                )
        }
    )
)