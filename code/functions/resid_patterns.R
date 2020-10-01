
resid_plot <- 
    
    function(
        var, 
        fit_info_ls = fit_info[[length(fit_info)]], 
        accuracy = 1, 
        log_color = TRUE, 
        log_size = TRUE,
        facet_var = "start_station_name"
    ) {
        
        resid_plot <-
            fit_info_ls$fit_aug %>% 
            
            ggplot(aes(!!enquo(var))) + 
            dark_theme_minimal() +
            theme(plot.background = element_rect(color = NA)) +
            geom_hline(yintercept = 0, color = "cyan", size = 0.5) +
            coord_cartesian(ylim = c(-2, 2), expand = FALSE)
        
        
        
        if (is.factor(fit_info_ls$fit_aug %>% pull(!!enquo(var)))) {
            
            if (!is.null(facet_var)) {
                
                aug_data_summ <- 
                    fit_info_ls$fit_aug %>% 
                    group_by(!!sym(facet_var), !!enquo(var)) %>%
                    summarise(
                        resid_mean = mean(resid),
                        resid_se   = se(resid)
                    )
                
            } else {
                
                aug_data_summ <- 
                    fit_info_ls$fit_aug %>% 
                    group_by(!!enquo(var)) %>%
                    summarise(
                        resid_mean = mean(resid),
                        resid_se   = se(resid)
                    )
                
            }
            
            
            resid_plot <-
                resid_plot +
                geom_crossbar(
                    data = aug_data_summ,
                    aes(
                        x = !!enquo(var),
                        y = resid_mean,
                        ymin = resid_mean - resid_se,
                        ymax = resid_mean + resid_se
                    ),
                    fill = "gray30",
                    color = NA,
                    alpha = .75, 
                    inherit.aes = FALSE
                ) +
                geom_crossbar(
                    data = aug_data_summ,
                    aes(
                        x = !!enquo(var),
                        y = resid_mean,
                        ymin = resid_mean,
                        ymax = resid_mean
                    ),
                    size = .25,
                    fill = NA,
                    color = "white",
                    inherit.aes = FALSE
                )
            
            
        } else {
            
            resid_plot <-
                resid_plot +
                geom_smooth(
                    aes(y = resid),
                    method = "loess",
                    formula = y ~ x,
                    color = "white",
                    span = 0.75,
                    size = 0.5,
                    alpha = .625,
                    na.rm = TRUE,
                    fullrange = FALSE
                )
            
        }
        
        if (!is.null(facet_var)) {
            
            resid_plot <-
                resid_plot + 
                facet_wrap(vars(!!sym(facet_var)), ncol = 10)
            
        }
        
        ggsave(
            filename = path(
                plots_dir, 
                fit_info_ls$fit_name,
                str_c(
                    fit_info_ls$fit_name, 
                    "resid_by",
                    as_label(enquo(var)), 
                    "plot", 
                    sep = "_"
                )
            ) %>% path_ext_set("png"),
            plot = resid_plot,
            device = "png",
            width = 10,
            height = 7,
            dpi = 300,
            scale = 1.5
        )
        
    }
