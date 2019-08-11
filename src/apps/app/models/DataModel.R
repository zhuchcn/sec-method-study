DataModel = R6Class(
    "DataModel",
    public = list(
        # attributes
        raw_data = NULL,
        param = list(
            n_peptdies = 2,
            n_spectra = 0,
            log_prob = 0,
            best_log_prob = 0,
            best_score = 0,
            coverage = 0,
            n_aa = 0,
            exclude = character()
        ),
        
        xic = data.frame(),
        uniprot_ids = NULL,
        
        # initializer
        initialize = function(rda_file = "data/data.rda"){
            load(rda_file)
            self$raw_data = data
            self$fix_mislabel()
        },
        
        fix_mislabel = function(){
            temp = self$raw_data$xic[,"F3"]
            self$raw_data$xic[,"F3"] = self$raw_data$xic[,"F4"]
            self$raw_data$xic[,"F4"] = temp
            
            temp = self$raw_data$RT[,"F3"]
            self$raw_data$RT[,"F3"] = self$raw_data$RT[,"F4"]
            self$raw_data$RT[,"F4"] = temp
            
            temp = self$raw_data$bionic[["F3"]]
            self$raw_data$bionic[["F3"]] = self$raw_data$bionic[["F4"]]
            self$raw_data$bionic[["F4"]] = temp
        },
        
        update = function(n_peptides = 2, n_spectra = 0, exclude = character(), 
                          log_prob = 0, best_log_prob = 0, best_score = 0,
                          coverage = 0, n_aa = 0){
            self$param = list(
                n_peptides = n_peptides,
                n_spectra = n_spectra,
                exclude = exclude,
                log_prob = log_prob,
                best_log_prob = best_log_prob,
                best_score = best_score,
                coverage = coverage,
                n_aa = n_aa
            )
            self$update_uniq_protein_id()
            self$update_xic()
        },
        
        update_uniq_protein_id = function(){
            byonic = self$raw_data$byonic
            uniprot_ids = lapply(names(byonic), function(fraction){
                byonic[[fraction]]$proteins %>%
                    filter(
                        !is.na(`# of spectra`) 
                        & `# of spectra` >= self$param$n_spectra 
                        & `# of unique peptides` >= self$param$n_peptides
                        & `|Log Prob|` >= self$param$log_prob
                        & `Best |Log Prob|` >= self$param$best_log_prob
                        & `Best score` >= self$param$best_score
                        & `Coverage %` >= self$param$coverage
                        & `# AA's in protein` >= self$param$n_aa
                        & !(short_name %in% self$param$exclude)
                    ) %>%
                    `[`(,"uniprot_id", drop = TRUE) %>%
                    as.character()
            })
            uniprot_ids = do.call(c, uniprot_ids)
            self$uniprot_ids = unique(uniprot_ids)
        },
        
        update_xic = function(){
            xic = self$raw_data$xic
            res = lapply(self$uniprot_ids, function(id){
                xic2 = xic %>%
                    filter(uniprot_id == id)
                xic2 =  xic2 %>%
                    mutate(
                        sample_coverage = apply(
                            xic2[, paste0("F", 0:8)], 1,
                            function(row) sum(!is.na(row))
                        )
                    ) %>%
                    filter(sample_coverage == max(sample_coverage, na.rm = TRUE)) 
                xic2 = xic2 %>%
                    mutate(mean = rowMeans(xic2[,paste0("F", 0:8)], na.rm = TRUE)) %>%
                    filter(mean == max(mean, na.rm = TRUE)) %>%
                    select(-mean)
                return(xic2)
            })
            res = do.call("rbind", res)
            self$xic = res
        },
        
        get_all_protein_short_names = function(){
            return(unique(self$raw_data$xic$short_name))
        },
        
        get_xic_table = function(){
            return(self$xic[,c("uniprot_id", "short_name", "long_name", "sequence", "charge", paste0("F", 0:8))])
        },
        
        get_protein_scores = function(id){
            return(
                self$raw_data$byonic[[id]]$proteins %>%
                    filter(uniprot_id %in% self$uniprot_ids)
            )
        },
        
        get_score_table = function(param){
            if(!(param %in% names(self$raw_data$byonic$F0$proteins)))
                stop("param not valid")
            
            table = self$xic
            for(f in paste0("F", 0:8)){
                proteins = self$raw_data$byonic[[f]]$proteins %>%
                    column_to_rownames("uniprot_id")
                table[,f] = proteins[as.character(table$uniprot_id), param]
            }
            return(table)
        },
        
        protein_bar_plot = function(index){
            data.frame(
                value = as.numeric(self$xic[index, paste0("F", 0:8)]),
                fraction = paste0("F", 0:8)
            ) %>%
                ggplot() +
                geom_col(aes(x = fraction, y = value), fill = "steelblue") +
                labs(title = as.character(self$xic$long_name[index])) +
                theme_bw() +
                theme(
                    axis.title = element_blank()
                )
        },
        
        protein_score_histogram = function(fraction, index){
            df = self$raw_data$byonic[[fraction]]$proteins %>%
                filter(uniprot_id %in% self$uniprot_ids)
            if(!is.numeric(param <- df[,index])){
                return()
            }
            ggplot(df) +
                geom_histogram(aes_string(param)) +
                labs(title = param) +
                theme_bw() +
                theme(
                    axis.title = element_blank()
                )
        },
        
        proteins_bar_plot = function(percentage = TRUE){
            df = self$xic[,paste0("F", 0:8)]
            if(percentage){
                df = sapply(df, function(col){
                    col/sum(col, na.rm = TRUE)
                }) %>%
                    as.data.frame()
            }
            df %>%
                mutate(protein = self$xic$short_name) %>%
                melt(id.vars = "protein", variable.name = "fraction") %>%
                ggplot() +
                geom_col(aes(x = fraction, y = value, fill = protein), stat = "identity") +
                theme_bw()
        },
        
        pie_plot = function(fraction){
            if(!(fraction %in% paste0("F", 0:8))) {
                stop("not valid fraction")
            }
            data.frame(
                protein = self$xic$short_name,
                value = self$xic[,fraction],
                stringsAsFactors = FALSE
            ) %>%
                filter(!is.na(value)) %>%
                arrange(desc(value)) %>%
                mutate(protein = factor(protein, levels = protein)) %>%
                plot_ly(labels = ~protein, values = ~value, type = "pie") %>%
                layout(
                    title = fraction,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    showlegend = F
                )
        }
    )
)
