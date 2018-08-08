##################################################-
##################################################-
##
## Time series fitted value plot function
##
##################################################-
##################################################-


ts_fitted_plot_stations <-
    
    function(fit,
             data,
             save = TRUE,
			 return.aug = TRUE,
             path = "ts_plot.png") {
        
        require(tidyverse)
        require(glue)
        require(broom)
        require(lubridate)
        require(magrittr)
        
        df_augment <-
            augment(fit) %>%
            select(.fitted:.mu) %>%
            bind_cols(data %>% select(date_time_hourly, start_station_id, trips, month), .)
        
        month_breaks <-
            df_augment %>%
            as_tibble() %>%
            ungroup() %>%
            group_by(month) %>%
            summarise(
                first_day = min(as_date(date_time_hourly)) %>% as.character(),
                ten_days = (min(as_date(
                    date_time_hourly
                )) + days(9)) %>% as.character(),
                twenty_days = (min(as_date(
                    date_time_hourly
                )) + days(19)) %>% as.character(),
                thirty_days = (min(as_date(
                    date_time_hourly
                )) + days(29)) %>% as.character()
            ) %>%
            mutate(breaks = str_c(
                first_day,
                ten_days,
                twenty_days,
                thirty_days,
                sep = ",",
                collapse = ","
            )) %>%
            pull(breaks) %>%
            extract2(1) %>%
            str_split(",") %>%
            unlist() %>%
            as_datetime(tz = "US/Eastern")
        
        month_nums <-
            df_augment %>%
            as_tibble() %>%
            ungroup() %>%
            pull(date_time_hourly) %>%
            month(label = FALSE) %>%
            unique() %>%
            as.numeric()
        
        month_names <-
            df_augment %>%
            as_tibble() %>%
            ungroup() %>%
            pull(date_time_hourly) %>%
            month(label = TRUE, abbr = FALSE) %>%
            unique() %>%
            droplevels() %>%
            as.character() %>%
            set_names(month_nums)
        
        
        df_augment_sum <-
            df_augment %>%
            group_by(date_time_hourly) %>%
            summarise(trips   = sum(trips, na.rm = TRUE),
                      .mu = sum(.mu, na.rm = TRUE)) %>%
            left_join(
                .,
                df_augment %>%
                    select(date_time_hourly, month) %>%
                    distinct(date_time_hourly, .keep_all = TRUE),
                by = "date_time_hourly"
            )
        
        
        ts_plot <-
            df_augment_sum %>%
            ggplot(aes(x = date_time_hourly)) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = trips, ymin = 0),
                fill = "gray15",
                # fill = "deepskyblue2",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = .5,
                color = NA
            ) +
            
            scale_x_datetime(breaks = month_breaks,
                             labels = month_breaks,
                             date_labels = "%a %x") +
            labs(
                x = "Date",
                y = "Total hourly trips",
                title = glue(
                    "Fitted values vs. data for Citi Bike hourly trips, ",
                    "{as_date(min(df_augment_sum$date_time_hourly)) %>% format('%x')}",
                    " to ",
                    "{as_date(max(df_augment_sum$date_time_hourly)) %>% format('%x')}"
                ),
                subtitle = str_wrap(
                    glue(
                        "Fitted model: {deparse(as.formula(getCall(fit)), width.cutoff = 500L)}"
                    ),
                    width = 300L
                ),
                caption = "Note: gold = model over-predictions, black = model under-predictions"
            ) +
            facet_wrap(
                ~ month,
                ncol = 1,
                scales = "free_x",
                labeller = as_labeller(month_names)
            ) +
            theme_dark() +
            # theme_gray() +
            # theme_bw() +
            theme(plot.subtitle = element_text(size = 7)) +
            coord_cartesian(ylim = c(0, 9000), expand = FALSE)
        
        if (save == TRUE) {
            ggsave(
                glue("{path}"),
                plot = ts_plot,
                width = 12,
                height = 4,
                dpi = 300,
                scale = 1.2
            )
            
        } else {
            print(ts_plot)
            
        }
		
		if (return.aug == TRUE) {
		    
		    return(list(df_augment = df_augment, df_augment_sum = df_augment_sum))
		    
		}
		
    }
