FiguresPage = R6Class(
    "FiguresPage",
    inherit = ShinyModule,
    public = list(
        # attributes
        id = "figures",
        
        # initializer
        initialize = function(){
        
        },
        
        # UI
        ui = function(){
            ns = NS(self$id)
            tagList(
                fluidRow(
                    column(
                        width = 6,
                        box(
                            width = NULL,
                            plotlyOutput(ns("barplot1"))
                        )
                    ),
                    column(
                        width = 6,
                        box(
                            width = NULL,
                            plotlyOutput(ns("barplot2"))
                        )
                    )
                ),
                fluidRow(
                    lapply(0:2, function(i){
                        column(
                            width = 12,
                            box(
                                width = NULL,
                                lapply(0:2, function(j){
                                    column(
                                        width = 4,
                                        plotlyOutput(ns(paste0("pie", i * 3 + j)))
                                    )
                                })
                            )
                        )
                    })
                )
            )
        },
        
        # server
        server = function(input, output, session){
            output$barplot1 = renderPlotly({
                DATA$proteins_bar_plot(percentage = TRUE)
            })
            output$barplot2 = renderPlotly({
                DATA$proteins_bar_plot(percentage = FALSE)
            })

            output$pie0 = renderPlotly({DATA$pie_plot("F0")})
            output$pie1 = renderPlotly({DATA$pie_plot("F1")})
            output$pie2 = renderPlotly({DATA$pie_plot("F2")})
            output$pie3 = renderPlotly({DATA$pie_plot("F3")})
            output$pie4 = renderPlotly({DATA$pie_plot("F4")})
            output$pie5 = renderPlotly({DATA$pie_plot("F5")})
            output$pie6 = renderPlotly({DATA$pie_plot("F6")})
            output$pie7 = renderPlotly({DATA$pie_plot("F7")})
            output$pie8 = renderPlotly({DATA$pie_plot("F8")})

            # for(i in 0:8){
            #     output[[paste0("pie", i)]] = renderPlotly({
            #         DATA$pie_plot(paste0("F", i))
            #     })
            # }
        }
    )
)