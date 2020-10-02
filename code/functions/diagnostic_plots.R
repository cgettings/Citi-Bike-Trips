###########################################################################################-
###########################################################################################-
##
## Diagnostic plots for model fits ---
##
###########################################################################################-
###########################################################################################-

# this script constructs plots for model diagnostics, mostly by comparing observed and
#   predicted values
 
#-----------------------------------------------------------------------------------------#
# Defining function arguments
#-----------------------------------------------------------------------------------------#

diagnostic_plots <- 
    
    function(
        fit_info, 
        row_var,
        col_var,
        station_ribbons = TRUE,
        save.plots = TRUE, 
        plots.path = "",
        output.path = ""
    ) {
        
        #=========================================================================================#
        # Setting up ----
        #=========================================================================================#
        
        require(tidyverse)
        require(glue)
        require(lubridate)
        require(fs)
        
        # Setting default mono font
        
        windows_mono <- windowsFonts()$mono
        
        windowsFonts(mono = "DejaVu Sans Mono for Powerline")
        
        
        #-----------------------------------------------------------------------------------------#
        # stopping if `fit_info` doesn't have the correct elements
        #-----------------------------------------------------------------------------------------#
        
        needed_elems <- c("fit_obj", "fit_name", "fit_months", "fit_aug")
        
        fit_info_elems <- names(fit_info)
        
        missing_elems <- setdiff(needed_elems, fit_info_elems)
        
        if (length(missing_elems) > 0) {
            
            stop(str_c("`fit_info` needs to contain ", str_c("`", missing_elems, "`", collapse = ", ")))
            
        }
        
        
        #-----------------------------------------------------------------------------------------#
        # Assigning re-used properties as objects
        #-----------------------------------------------------------------------------------------#
        
        fit_obfun <- round(fit_info$fit_obj$fit$objective)
        
        # deparsing model formula
        
        fit_formula <- 
            deparse(formula(fit_info$fit_obj), width.cutoff = 500L) %>% 
            str_c(collapse = "") %>% 
            str_replace("\\+ ar1\\(", "+\n        ar1(")
        
        
        #-----------------------------------------------------------------------------------------#
        # Creating folder for plots
        #-----------------------------------------------------------------------------------------#
        
        fit_folder <- path(plots.path, fit_info$fit_name)
        
        dir_create(fit_folder)
        
        
        #=========================================================================================#
        # Observed vs. fitted - scatterplot
        #=========================================================================================#
        
        #-----------------------------------------------------------------------------------------#
        # Constructing plot
        #-----------------------------------------------------------------------------------------#
        
        observed_fitted_scatterplot <- 
            fit_info$fit_aug %>% 
            ggplot(aes(trips, fitted)) + 
            geom_point(alpha = .25, size = .75, shape = 16) + 
            geom_abline(slope = 1, intercept = 0, color = "red4") + 
            labs(
                x = "Observed trips",
                y = "Predicted trips",
                title = fit_formula,
                subtitle = fit_obfun
            ) +
            scale_x_continuous(breaks = seq(0, 100, 25)) +
            scale_y_continuous(breaks = seq(0, 100, 25)) +
            coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                plot.background = element_rect(color = NA, fill = NA)
            )
        
        
        #-----------------------------------------------------------------------------------------#
        # Saving plot
        #-----------------------------------------------------------------------------------------#
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_fitted_scatterplot.png"), 
                plot = observed_fitted_scatterplot,
                device = "png",
                width = 10,
                height = 10,
                units = "in", 
                scale = 1,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(observed_fitted_scatterplot)
            
        }
        
        
        #=========================================================================================#
        # Observed vs. fitted - scatterplot heatmap
        #=========================================================================================#
        
        #-----------------------------------------------------------------------------------------#
        # Constructing plot
        #-----------------------------------------------------------------------------------------#
        
        observed_fitted_heatmap <- 
            fit_info$fit_aug %>% 
            ggplot(aes(trips, fitted)) +
            geom_abline(slope = 1, intercept = 0, color = "gray30", size = .25) +
            geom_hex(aes(fill = stat(count)), binwidth = 1) +
            scale_fill_viridis_c("Count", trans = "log10", option = "B") +
            
            labs(
                x = "Observed trips",
                y = "Predicted trips",
                title = fit_formula,
                subtitle = fit_obfun
            ) +
            scale_x_continuous(breaks = seq(0, 100, 25)) +
            scale_y_continuous(breaks = seq(0, 100, 25)) +
            coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                panel.background = element_rect(color = NA, fill = "gray15"),
                plot.background = element_rect(color = NA, fill = NA)
            )
        
        
        #-----------------------------------------------------------------------------------------#
        # Saving plot
        #-----------------------------------------------------------------------------------------#
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_fitted_heatmap.png"), 
                plot = observed_fitted_heatmap,
                device = "png",
                width = 10,
                height = 9,
                units = "in", 
                scale = 1,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(observed_fitted_scatterplot)
            
        }
        
        
        #=========================================================================================#
        # Observed vs. fitted - 2 time variables - scatterplot
        #=========================================================================================#
        
        #-----------------------------------------------------------------------------------------#
        # Constructing plot
        #-----------------------------------------------------------------------------------------#
        
        observed_fitted_period_wend_scatterplot <- 
            fit_info$fit_aug %>% 
            ggplot(aes(trips, fitted)) + 
            geom_abline(slope = 1, intercept = 0, color = "red4", size = .25) + 
            geom_point(alpha = .25, size = .75, shape = 16) + 
            labs(
                x = "Observed trips",
                y = "Predicted trips",
                title = fit_formula,
                subtitle = fit_obfun
            ) +
            scale_x_continuous(breaks = seq(0, 100, 25)) +
            scale_y_continuous(breaks = seq(0, 100, 25)) +
            coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
            facet_grid(
                rows = vars(!!sym(row_var)),
                cols = vars(!!sym(col_var)),
                labeller = label_both
            ) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                plot.background = element_rect(color = NA, fill = NA)
            )
        
        
        #-----------------------------------------------------------------------------------------#
        # Saving plot
        #-----------------------------------------------------------------------------------------#
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_fitted_period_wend_scatterplot.png"),
                plot = observed_fitted_period_wend_scatterplot,
                device = "png",
                width = 15,
                height = 9,
                units = "in", 
                scale = 1,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(observed_fitted_scatterplot)
            
        }
        
        
        #=========================================================================================#
        # Observed vs. fitted - 2 time variables - scatterplot heatmap
        #=========================================================================================#
        
        #-----------------------------------------------------------------------------------------#
        # Constructing plot
        #-----------------------------------------------------------------------------------------#
        
        observed_fitted_period_wend_heatmap <- 
            fit_info$fit_aug %>% 
            ggplot(aes(trips, fitted)) +
            geom_abline(slope = 1, intercept = 0, color = "gray30", size = .25) +
            geom_hex(aes(fill = stat(count)), binwidth = 1) +
            scale_fill_viridis_c("Count", trans = "log10", option = "B") +
            labs(
                x = "Observed trips",
                y = "Predicted trips",
                title = fit_formula,
                subtitle = fit_obfun
            ) +
            scale_x_continuous(breaks = seq(0, 100, 25)) +
            scale_y_continuous(breaks = seq(0, 100, 25)) +
            coord_fixed(xlim = c(0, 100), ylim = c(0, 100)) +
            facet_grid(
                rows = vars(!!sym(row_var)),
                cols = vars(!!sym(col_var)),
                labeller = label_both
            ) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                panel.background = element_rect(color = NA, fill = "gray14"),
                plot.background = element_rect(color = NA, fill = NA)
            )
        
        
        #-----------------------------------------------------------------------------------------#
        # Saving plot
        #-----------------------------------------------------------------------------------------#
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_fitted_period_wend_heatmap.png"), 
                plot = observed_fitted_period_wend_heatmap,
                device = "png",
                width = 15,
                height = 8.5,
                units = "in", 
                scale = 1,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(observed_fitted_scatterplot)
            
        }
        
        
        #=========================================================================================#
        # Time series polygon plots
        #=========================================================================================#
        
        weekdays <- 
            fit_info$fit_aug %>% 
            filter(hour(date_time_hourly) == 12L) %>%
            distinct(date_time_hourly, .keep_all = TRUE) %>% 
            select(date_time_hourly, wday_wend) %>% 
            mutate(
                wday = wday(date_time_hourly, label = TRUE),
                month = month(date_time_hourly, label = TRUE)
            )
        
        week_breaks <- 
            unique(floor_date(weekdays$date_time_hourly, "month")) %>% 
            imap( 
                ~ seq(
                    .x, 
                    rollback(unique(ceiling_date(weekdays$date_time_hourly, "month")))[.y],
                    "1 week"
                )
            ) %>% 
            flatten_dbl() %>% 
            as_datetime(tz = "US/Eastern")
        
        #-----------------------------------------------------------------------------------------#
        # Mean
        #-----------------------------------------------------------------------------------------#
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Setting up
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        fit_aug_mean_ts <-
            fit_info$fit_aug %>%
            group_by(date_time_hourly) %>%
            summarise(
                trips = mean(trips, na.rm = TRUE),
                .mu   = mean(fitted, na.rm = TRUE)
            ) %>% 
            left_join(
                .,
                fit_info$fit_aug %>%
                    select(date_time_hourly) %>%
                    distinct(date_time_hourly, .keep_all = TRUE),
                by = "date_time_hourly"
            ) %>% 
            mutate(month = month(date_time_hourly, label = TRUE))
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Constructing plot
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        fit_ts_mean_plot <-
            fit_aug_mean_ts %>%
            ggplot(aes(x = date_time_hourly)) +
            geom_rect(
                data = weekdays %>% filter(wday %in% c("Sun", "Sat")),
                aes(
                    xmin = floor_date(date_time_hourly, "1 day"),
                    xmax = ceiling_date(date_time_hourly, "1 day"),
                    group = month
                ),
                ymin = 0,
                ymax = max(c(fit_aug_mean_ts$trips, fit_aug_mean_ts$.mu)) * 1.025,
                color = NA,
                fill = "gray12",
                alpha = .5
            ) +
            geom_vline(
                data = weekdays,
                aes(
                    xintercept = floor_date(date_time_hourly, "1 day"),
                    group = month
                ),
                color = "gray14",
                size = .25
            ) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = trips, ymin = 0),
                fill = "blue4",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = .325,
                color = NA
            ) +
            labs(
                x = "Date/Time",
                y = "Average hourly trips",
                title = fit_formula,
                subtitle = fit_obfun,
                caption = "Note: blue = model under-predictions; gold = model over-predictions"
            ) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                panel.background = element_rect(color = NA, fill = "gray20"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.background = element_rect(color = NA, fill = NA)
            ) +
            scale_x_datetime(
                breaks = week_breaks,
                labels = strftime(week_breaks, "%b %e")
            ) +
            coord_cartesian(ylim = c(0, max(fit_aug_mean_ts$trips) * 1.025), expand = FALSE) +
            facet_wrap(vars(month), nrow = length(fit_info$fit_months), scales = "free_x")
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Saving plot
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_ts_ribbon_mean.png"), 
                plot = fit_ts_mean_plot,
                device = "png",
                width = 10,
                height = 1.25 + length(fit_info$fit_months),
                units = "in", 
                scale = 1.75,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(fit_ts_mean_plot)
            
        }
        
        
        #-----------------------------------------------------------------------------------------#
        # Median
        #-----------------------------------------------------------------------------------------#
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Setting up
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        fit_aug_median_ts <-
            fit_info$fit_aug %>%
            group_by(date_time_hourly) %>%
            summarise(
                trips = median(trips, na.rm = TRUE),
                .mu   = median(fitted, na.rm = TRUE)
            ) %>% 
            left_join(
                .,
                fit_info$fit_aug %>%
                    select(date_time_hourly) %>%
                    distinct(date_time_hourly, .keep_all = TRUE),
                by = "date_time_hourly"
            ) %>% 
            mutate(month = month(date_time_hourly, label = TRUE))
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Constructing plot
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        fit_ts_median_plot <-
            fit_aug_median_ts %>%
            ggplot(aes(x = date_time_hourly)) +
            geom_rect(
                data = weekdays %>% filter(wday %in% c("Sun", "Sat")),
                aes(
                    xmin = floor_date(date_time_hourly, "1 day"),
                    xmax = ceiling_date(date_time_hourly, "1 day"),
                    group = month
                ),
                ymin = 0,
                ymax = max(c(fit_aug_median_ts$trips, fit_aug_median_ts$.mu)) * 1.025,
                color = NA,
                fill = "gray12",
                alpha = .5
            ) +
            geom_vline(
                data = weekdays,
                aes(
                    xintercept = floor_date(date_time_hourly, "1 day"),
                    group = month
                ),
                color = "gray14",
                size = .25
            ) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = trips, ymin = 0),
                fill = "blue4",
                alpha = 1,
                color = NA
            ) +
            geom_ribbon(
                aes(ymax = .mu, ymin = 0),
                fill = "gold",
                alpha = .325,
                color = NA
            ) +
            labs(
                x = "Date/Time",
                y = "Median hourly trips",
                title = fit_formula,
                subtitle = fit_obfun,
                caption = "Note: blue = model under-predictions; gold = model over-predictions"
            ) +
            dark_theme_minimal() +
            theme(
                plot.title = element_text(size = rel(1), family = "mono"),
                plot.subtitle = element_text(family = "mono"),
                panel.background = element_rect(color = NA, fill = "gray20"),
                panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                plot.background = element_rect(color = NA, fill = NA)
            ) +
            scale_x_datetime(
                breaks = week_breaks,
                labels = strftime(week_breaks, "%b %e")
            ) +
            coord_cartesian(ylim = c(0, max(fit_aug_median_ts$trips) * 1.025), expand = FALSE) +
            facet_wrap(vars(month), nrow = length(fit_info$fit_months), scales = "free_x")
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # Saving plot
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        if (save.plots == TRUE) {
            
            ggsave(
                filename = glue("{fit_folder}/{fit_info$fit_name}_ts_ribbon_median.png"), 
                plot = fit_ts_median_plot,
                device = "png",
                width = 10,
                height = 1.25 + length(fit_info$fit_months),
                units = "in", 
                scale = 1.75,
                dpi = 300,
                bg = "black"
            )
            
        } else {
            
            print(fit_ts_median_plot)
            
        }
        
        
        if (station_ribbons == TRUE) {
            
            #-----------------------------------------------------------------------------------------#
            # Stations
            #-----------------------------------------------------------------------------------------#
            
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            # Setting up
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            
            stations_folder <- dir_create(path(fit_folder, "stations"))
            
            for (i in 1:fit_info$fit_n_stations) {
                
                fit_aug_station_ts <-
                    fit_info$fit_aug %>%
                    filter(start_station_name == unique(start_station_name)[i]) %>% 
                    mutate(month = month(date_time_hourly, label = TRUE)) %>% 
                    select(date_time_hourly, start_station_name, start_station_id, trips, .mu = fitted, month)
                
                # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
                # Constructing plot
                # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
                
                fit_ts_station_plot <-
                    fit_aug_station_ts %>%
                    ggplot(aes(x = date_time_hourly)) +
                    geom_rect(
                        data = weekdays %>% filter(wday %in% c("Sun", "Sat")),
                        aes(
                            xmin = floor_date(date_time_hourly, "1 day"),
                            xmax = ceiling_date(date_time_hourly, "1 day"),
                            group = month
                        ),
                        ymin = 0,
                        ymax = max(c(fit_aug_station_ts$trips, fit_aug_station_ts$.mu)) * 1.025,
                        color = NA,
                        fill = "gray12",
                        alpha = .5
                    ) +
                    geom_vline(
                        data = weekdays,
                        aes(
                            xintercept = floor_date(date_time_hourly, "1 day"),
                            group = month
                        ),
                        color = "gray14",
                        size = .25
                    ) +
                    geom_ribbon(
                        aes(ymax = .mu, ymin = 0),
                        fill = "gold",
                        alpha = 1,
                        color = NA
                    ) +
                    geom_ribbon(
                        aes(ymax = trips, ymin = 0),
                        fill = "blue4",
                        alpha = 1,
                        color = NA
                    ) +
                    geom_ribbon(
                        aes(ymax = .mu, ymin = 0),
                        fill = "gold",
                        alpha = .325,
                        color = NA
                    ) +
                    labs(
                        x = "Date/Time",
                        y = "Hourly trips",
                        caption = "Note: blue = model under-predictions; gold = model over-predictions",
                        title = fit_aug_station_ts$start_station_name
                    ) +
                    dark_theme_minimal() +
                    theme(
                        plot.title = element_text(size = rel(1), family = "mono"),
                        plot.subtitle = element_text(family = "mono"),
                        panel.background = element_rect(color = NA, fill = "gray20"),
                        panel.grid.major.x = element_blank(),
                        panel.grid.minor.x = element_blank(),
                        plot.background = element_rect(color = NA, fill = NA)
                    ) +
                    scale_x_datetime(
                        breaks = week_breaks,
                        labels = strftime(week_breaks, "%b %e")
                    ) +
                    coord_cartesian(ylim = c(0, max(fit_aug_station_ts$trips) * 1.025), expand = FALSE) +
                    facet_wrap(vars(month), nrow = length(fit_info$fit_months), scales = "free_x")
                
                
                # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
                # Saving plot
                # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
                
                ggsave(
                    filename = 
                        glue(
                            "{stations_folder}/{fit_info$fit_name}_",
                            "ts_ribbon_station_{unique(fit_aug_station_ts$start_station_id)}.png"
                        ), 
                    plot = fit_ts_station_plot,
                    device = "png",
                    width = 10,
                    height = 1 + length(fit_info$fit_months),
                    units = "in", 
                    scale = 1.75,
                    dpi = 300,
                    bg = "black"
                )
                
            }
            
        }
        
        # #=========================================================================================#
        # # Time series residual plot
        # #=========================================================================================#
        # 
        # #-----------------------------------------------------------------------------------------#
        # # Setting up
        # #-----------------------------------------------------------------------------------------#
        # 
        # fit_aug_sum_resid <- 
        #     fit_info$fit_aug %>%
        #     # fit_info[[length(fit_info)]]$fit_aug %>%
        #     group_by(start_station_name, wday_hour) %>%
        #     summarise(
        #         n = n(),
        #         resid_mean = mean(resid)
        #     ) %>% 
        #     mutate(
        #         resid_direction = if_else(resid_mean < 0, "un", "ov")
        #     ) %>% 
        #     ungroup()
        # 
        # 
        # hours_7 <- rep(0:23, 7)
        # hours_7[hours_7 %% 2 == 1] <- ""
        # 
        # 
        # #-----------------------------------------------------------------------------------------#
        # # Constructing plot
        # #-----------------------------------------------------------------------------------------#
        # 
        # fit_resid_plot <- 
        #     fit_aug_sum_resid %>% 
        #     ggplot(aes(x = wday_hour, y = resid_mean, group = 1)) +
        #     annotate(
        #         geom = "rect",
        #         xmin = unique(str_subset(fit_aug_sum_resid$wday_hour, "S.+ 0")),
        #         xmax = unique(str_subset(fit_aug_sum_resid$wday_hour, "S.+ 23")),
        #         ymin = -10,
        #         ymax = 10,
        #         color = NA,
        #         fill = "gray12",
        #         alpha = .5
        #     ) +
        #     geom_vline(
        #         xintercept = c("Tue 0", "Wed 0", "Thu 0", "Fri 0"),
        #         color = "gray15",
        #         size = .25
        #     ) +
        #     geom_hline(yintercept = 0, color = "gray20", size = .25) +
        #     geom_point(
        #         aes(color = resid_direction),
        #         size = 0.75,
        #         alpha = 1,
        #         shape = 16
        #     ) +
        #     geom_point(
        #         data = fit_info$fit_aug,
        #         # data = fit_info[[length(fit_info)]]$fit_aug,
        #         aes(x = wday_hour, y = resid, color = resid_direction),
        #         size = 0.5,
        #         alpha = 0.5,
        #         shape = 16, 
        #         inherit.aes = FALSE
        #     ) +
        #     scale_x_discrete(limits = levels(fit_aug_sum_resid$wday_hour), labels = hours_7) +
        #     scale_color_manual(values = c("dodgerblue1", "gold")) +
        #     labs(
        #         x = "Date/Time",
        #         y = "Residuals",
        #         title = fit_formula,
        #         subtitle = fit_obfun,
        #         caption = "Note: blue = model over-predictions, gold = model under-predictions"
        #     ) +
        #     guides(
        #         alpha = "none",
        #         size = "none",
        #         color = guide_legend("sign", override.aes = list(alpha = 1, size = 5))
        #     ) +
        #     facet_wrap(vars(start_station_name), ncol = 10) +
        #     dark_theme_minimal() +
        #     coord_cartesian(ylim = c(-10, 10), expand = FALSE) +
        #     theme(
        #         panel.grid.major.x = element_blank(), 
        #         axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        #         plot.title = element_text(size = rel(1), family = "mono"),
        #         plot.subtitle = element_text(family = "mono"),
        #         panel.border = element_rect(fill = NA, color = "gray25"),
        #         plot.background = element_rect(color = NA, fill = NA)
        #     )
        # 
        # 
        # #-----------------------------------------------------------------------------------------#
        # # Saving plot
        # #-----------------------------------------------------------------------------------------#
        # 
        # if (save.plots == TRUE) {
        #     
        #     ggsave(
        #         filename = glue("{fit_folder}/{fit_info$fit_name}_ts_resid.png"), 
        #         plot = fit_resid_plot,
        #         device = "png",
        #         width = 16,
        #         height = 10,
        #         units = "in", 
        #         scale = 1.5,
        #         dpi = 300,
        #         bg = "black"
        #     )
        #     
        # } else {
        #     
        #     print(fit_resid_plot)
        #     
        # }
        
        
        #=========================================================================================#
        # Finishing
        #=========================================================================================#
        
        #-----------------------------------------------------------------------------------------#
        # Restoring default mono font
        #-----------------------------------------------------------------------------------------#
        
        windows_mono <- windowsFonts()$mono
        
        windowsFonts(mono = windows_mono)
        
    }


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
