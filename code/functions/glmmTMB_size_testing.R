###########################################################################################-
###########################################################################################-
##
## glmmTMB size testing helper function ---
##
###########################################################################################-
###########################################################################################-

# samples the data, fits the model with `{glmmTMB}`, gives details on progress, appends 
#   model summary output to CSV file, and returns a list with the fitted model object and 
#   some metadata

#-----------------------------------------------------------------------------------------#
# Defining function arguments
#-----------------------------------------------------------------------------------------#

glmmTMB_size_testing <- 
    
    function(formula,
             data,
             months,
             n_stations,
             family,
             ziformula = ~ 0,
             dispformula = ~ 1,
             parallel = NULL,
             seed = 1,
             output_dir,
             csv_name,
             verbose = FALSE,
             profile = TRUE,
             optCtrl = NULL,
             optimizer = "nlminb") {
        
        if (is.null(optCtrl)) {
            
            optCtrl <-
                list(
                    iter.max = 10000,
                    eval.max = 10000,
                    rel.tol = 1e-9,
                    x.tol = 0,
                    trace = 0
                )
        }
        
        #-----------------------------------------------------------------------------------------#
        # Data operations
        #-----------------------------------------------------------------------------------------#
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # subsetting data
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        set.seed(seed)
        
        if (is.finite(n_stations)) {
            
            data <- 
                data %>% 
                filter(month(date_time_hourly) %in% months) %>%
                filter(start_station_name %in% sample(unique(start_station_name), n_stations))
            
        } else {
            
            data <- 
                data %>% 
                filter(month(date_time_hourly) %in% months)
            
            n_stations <- n_distinct(data$start_station_name)
            
        }
        
        # arranging so that residual matrix viz works right
        
        data <- 
            data %>% 
            arrange(start_station_name, date, hour)
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # printing data info
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        data_params <- 
            str_c(
                "| months:", 
                str_c(months, collapse = " "),
                "|",
                n_stations,
                "stations | observations",
                nrow(data),
                "| family =", 
                family,
                "|",
                sep = " "
            )
        
        cat(
            data_params,
            "\n",
            rep("-", str_length(data_params)) %>% str_c(collapse = ""),
            "\n",
            sep = ""
        )
        
        
        #-----------------------------------------------------------------------------------------#
        # Metadata for output
        #-----------------------------------------------------------------------------------------#

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # timing run
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        fit_time <- now("US/Eastern")
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # fit name
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        zi   <- if_else(ziformula != formula(~0), "_zi", "")
        disp <- if_else(dispformula != formula(~1), "_disp", "")
        
        fit_name <- 
            glue(
                "glmmTMB_fit_m{str_c(months, collapse = '')}_{n_stations}s_",
                "{family}{zi}{disp}_{format(fit_time, '%H%M%S')}"
            )
        
        cat(fit_name, "\n")
        
        cat(fit_time %>% format("%r"), "\n")
        
        tic()
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # verbose output
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        if (verbose == TRUE) {
        
            verbose_output_name <- path(output_dir, str_c(fit_name, "_verbose.txt"))
                
            write_file(
                str_c(
                    fit_name, "\n", 
                    str_c(rep("-", str_length(fit_name)), collapse = ""),
                    "\n\n"
                ), 
                verbose_output_name
            )
            
        } else {
            
            verbose_output_name <- NULL
            
        }
        
        
        #-----------------------------------------------------------------------------------------#
        # Model fitting
        #-----------------------------------------------------------------------------------------#

        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # fitting model, and optionally capturing verbose output
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        capture.output(

            {
                glmmTMB_fit <-
                    
                    glmmTMB(
                        
                        formula = formula,
                        data = data,
                        family = family,
                        ziformula = ziformula,
                        dispformula = dispformula,
                        verbose = verbose,
                        control = 
                            glmmTMBControl(
                                optCtrl = optCtrl,
                                optimizer = optimizer,
                                profile = profile, 
                                collect = FALSE,
                                parallel = parallel
                            )
                    )
            },

            file = verbose_output_name,
            append = TRUE
        )
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # timing run
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        cat(now() %>% format("%r"), "\n")
        glmmTMB_fit_toc <- toc(quiet = TRUE)
        glmmTMB_fit_elapsed <- glmmTMB_fit_toc$toc - glmmTMB_fit_toc$tic
        
        
        #-----------------------------------------------------------------------------------------#
        # Fit stats
        #-----------------------------------------------------------------------------------------#
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # getting the objective function value
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        # so i can assess model fit, even if convergence problems
        
        glmmTMB_fit_ob <- glmmTMB_fit$fit$objective
        
        model_df <- glmmTMB_fit$modelInfo$nobs - glance(glmmTMB_fit)$df.residual
        
        cat(
            "\n",
            "time:     ", glmmTMB_fit_elapsed, "sec", "\n",
            "objective:", glmmTMB_fit_ob, "\n",
            "df:       ", model_df, "\n"
        )

        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # poisson soecific
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        if (family == "poisson") {
            
            #==== calculating variance inflation ====#
            
            glmmTMB_fit_disp_param <-  sigma(glmmTMB_fit) - 1
            
            glmmTMB_fit_varinfl <- round(glmmTMB_fit_disp_param, 3)
            
            if (is.null(terms(glmmTMB_fit, "zi"))) {
                
                #==== calculating over-dispersion ratio ====#
                
                glmmTMB_fit_disp <- check_overdispersion(glmmTMB_fit)$dispersion_ratio
                cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
                
            } else {
                
                glmmTMB_fit_disp <- NA_real_
                cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
                
            }
            
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # genpois specific
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            
        } else if (family == "genpois") {
                
            #==== calculating variance inflation ====#
            
            glmmTMB_fit_disp_param <-  (1 * sigma(glmmTMB_fit)) - 1
            
            glmmTMB_fit_varinfl <- round(glmmTMB_fit_disp_param, 3)
            
            if (is.null(terms(glmmTMB_fit, "zi"))) {
                
                #==== calculating over-dispersion ratio ====#
                
                glmmTMB_fit_disp <- check_overdispersion(glmmTMB_fit)$dispersion_ratio
                cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
                
            } else {
                
                glmmTMB_fit_disp <- NA_real_
                cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
                
            }
            
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            # nbinom1 specific
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

        } else if (family == "nbinom1") {
            
            #==== calculating variance inflation ====#
            
            glmmTMB_fit_disp_param <-  (1 * (1 + sigma(glmmTMB_fit))) - 1
            
            glmmTMB_fit_varinfl <- round(glmmTMB_fit_disp_param, 3)
            
            #==== no over-dispersion ratio ====#
            
            glmmTMB_fit_disp <- NA_real_
            cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
            
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            # nbinom2 specific
            # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
            
        } else if (family == "nbinom2") {
            
            #==== calculating variance inflation ====#
            
            glmmTMB_fit_disp_param <-  (1 + (1/sigma(glmmTMB_fit))) - 1
            
            glmmTMB_fit_varinfl <- round(glmmTMB_fit_disp_param, 3)
            
            #==== no over-dispersion ratio ====#
            
            glmmTMB_fit_disp <- NA_real_
            cat(" dispersion ratio:", glmmTMB_fit_disp, "\n")
            
        }
        
        
        #-----------------------------------------------------------------------------------------#
        # appending fit & run results to a spreadsheet
        #-----------------------------------------------------------------------------------------#
        
        results_path <- 
            path(
                output_dir, 
                csv_name
            ) %>%
            path_ext_set("csv")
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # creating spreadsheet if it doesn't exist
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        if (!file_exists(results_path)) {
            
            optimizer_results_template <- 
                tibble(
                    time = NA_character_,
                    date = NA_character_,
                    fit_name_rds = NA_character_,
                    
                    months = NA_real_,
                    n_stations = NA_real_,
                    n_obs = NA_real_,
                    df = NA_real_,
                    
                    model_overdisp = NA_real_,
                    model_varinfl = NA_real_,
                    
                    run_time = NA_real_,
                    objective = NA_real_,
                    family = NA_character_,
                    fixed = NA_character_,
                    random = NA_character_,
                    zi_formula = NA_character_,
                    .rows = 0
                )
            
            write_csv(optimizer_results_template, results_path)
            
        }
        
        
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        # writing
        # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
        
        #   (assigning it to a variable because `write_csv` returns its x argument, and i don't want that
        
        written_csv <- 
            write_csv(
                tibble(
                    time = fit_time %>% as_hms() %>% round_hms(1) %>% as.character(),
                    date = as_date(fit_time) %>% as.character(),
                    fit_name_rds = fit_name,
                    months = str_c(months, collapse = ","),
                    n_stations = n_stations,
                    n_obs = nrow(model.frame(glmmTMB_fit)),
                    df = model_df,
                    
                    model_overdisp = round(glmmTMB_fit_disp, 2),
                    model_varinfl = glmmTMB_fit_varinfl,
                    
                    run_time = round(glmmTMB_fit_elapsed, 0),
                    objective = round(glmmTMB_fit_ob, 0),
                    family = family,
                    fixed = glmmTMB_fit %>% 
                        formula() %>% 
                        deparse() %>% 
                        str_c(collapse = "") %>% 
                        str_squish() %>% 
                        str_remove("\\+ us\\(.+"),
                    
                    random = glmmTMB_fit %>% 
                        formula() %>% 
                        deparse() %>% 
                        str_c(collapse = "") %>% 
                        str_squish() %>% 
                        str_extract("us\\(.+"),
                    
                    zi_formula = deparse(terms(glmmTMB_fit, "zi")) %>% if_else(. == "NULL", " ", .)
                ), 
                append = TRUE,
                results_path
            )
        
        
        #-----------------------------------------------------------------------------------------#
        # returning list of output
        #-----------------------------------------------------------------------------------------#
        
        return(
            list(
                "fit_obj"  = glmmTMB_fit,
                "fit_name" = fit_name, 
                "fit_months" = months,
                "fit_n_stations" = n_stations,
                "fit_elapsed" = glmmTMB_fit_elapsed
            )
        )
        
    }


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
