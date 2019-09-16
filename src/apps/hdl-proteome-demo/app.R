source("global.R")

ui = fluidPage(
    tags$head(
        tags$script(
            src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
            type="text/javascript"
        )
    ),
    fluidRow(
        plotlyOutput("barplot")
    ),
    fluidRow(
        column(
            width = 6,
            selectInput(
                "fraction",
                "Fraction",
                choices = fraction_names,
                selected = "small HDL"
            ),
            DTOutput("table")
        ),
        column(
            width = 6,
            plotlyOutput("pieplot")
        )
    ),
    HTML('<div data-iframe-height></div>')
)

server = function(input, output, session){
    output$barplot = renderPlotly({
        protein_to_plot = input$table_rows_selected
        data.frame(
            fraction = factor(fraction_names, levels = fraction_names),
            value = as.numeric(data$xic[protein_to_plot, fraction_names])
        ) %>%
            ggplot(aes(x = fraction, y = value)) +
            geom_col(aes(fill = fraction == input$fraction), width = 0.75) +
            scale_fill_manual(values = c("gray30", "firebrick1")) +
            theme_bw() +
            labs(y = "XIC", title = as.character(data$xic$long_name[protein_to_plot])) +
            theme(
                axis.title.x = element_blank(),
                axis.text = element_text(color = "black"),
                axis.ticks = element_line(color = "black"),
                panel.grid = element_blank(),
                panel.border = element_rect(color = "black"),
                legend.position = "none"
            )
    })
    
    output$table = renderDT({
        datatable(
            data$xic %>% 
                select(c(uniprot_id, short_name, long_name, input$fraction)) %>%
                mutate(long_name = glue::glue('<a href="https://www.uniprot.org/uniprot/{uniprot_id}" target="_blank">{long_name}</a>')),
            escape = FALSE,
            options = list(
                pageLength = 10,
                lengthMenu = c(10, 25, 50),
                order = list(list(4, 'desc'))
            ),
            selection = list(
                mode = "single",
                selected = "2"
            )
        ) %>%
            formatSignif(input$fraction, digits = 3)
    })
    
    output$pieplot = renderPlotly({
        data.frame(
            protein = data$xic$short_name,
            value = data$xic[, input$fraction],
            stringsAsFactors = FALSE
        ) %>%
            filter(!is.na(value)) %>%
            arrange(desc(value)) %>%
            mutate(protein = factor(protein, levels = protein)) %>%
            plot_ly(labels = ~protein, values = ~value, type = "pie") %>%
            layout(
                title = input$fraction,
                xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                showlegend = F
            )
    })
}

shiny::shinyApp(ui = ui, server = server)