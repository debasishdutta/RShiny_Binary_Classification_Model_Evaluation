################################################################################################
#################### Binary Classification Model Evaluation Using RShiny #######################
#########################    Developer:   Debasish Dutta            ############################
#########################    Date:   	  December 2017             ############################
################## Master Functions: shinyConfMatrix & shinyPerfMeasures  ######################
################################################################################################


########################################################################################
############################ Package Loading Functions #################################
########################################################################################

pkgTest <- function() {
  if (!require("dplyr", character.only = TRUE))
  {
    install.packages("dplyr", dep = TRUE)
    if (!require("dplyr", character.only = TRUE))
      stop("Package not found")
  }
  
  if (!require("ggplot2", character.only = TRUE))
  {
    install.packages("ggplot2", dep = TRUE)
    if (!require("ggplot2", character.only = TRUE))
      stop("Package not found")
  }
  
  if (!require("shiny", character.only = TRUE))
  {
    install.packages("shiny", dep = TRUE)
    if (!require("shiny", character.only = TRUE))
      stop("Package not found")
  }
  
  if (!require("tidyr", character.only = TRUE))
  {
    install.packages("tidyr", dep = TRUE)
    if (!require("tidyr", character.only = TRUE))
      stop("Package not found")
  }
  
  if (!require("stats", character.only = TRUE))
  {
    install.packages("stats", dep = TRUE)
    if (!require("stats", character.only = TRUE))
      stop("Package not found")
  }
}

########################################################################################
################################# Helper Functions #####################################
########################################################################################

error_handler <- function(list_models, arg, method) {
  if (!is.numeric(arg)) {
    if (method == "Performance_Measure") {
      stop("Error: The argument g should be numeric")
    } else {
      stop("Error: The argument t should be numeric")
    }
  }
  
  count_cols <- sapply(list_models, function(x)
    ncol(x))
  if (sum(count_cols != 2) > 0) {
    stop(
      "Error: Each dataframe in the list should consist of only 2 columns.
      The first column with class labels (0/1) and the second column indicating the predicted probability"
    )
    
  }
  
  check_col_type <- sum(unlist(lapply(list_models, function(x)
    lapply(x, function(y)
      ! is.numeric(y)))))
  
  if (check_col_type > 0) {
    stop("Error: All columns in each dataframe should be numeric")
    
  }
  
  check_class_lables <-
    unlist(lapply(list_models, function(x)
      unique(x[, 1])))
  
  if (sum (!(check_class_lables == 0 |
             check_class_lables == 1)) > 0) {
    stop("Error: Class labels should be either 0 or 1")
    
  }
  
  check_pred_prob <-
    unlist(lapply(list_models, function(x)
      range(x[, 2])))
  
  if (sum (!(check_pred_prob >= 0 & check_pred_prob <= 1)) > 0) {
    stop("Error: Predicted probability should be between zero and one")
    
  }
  
}

obs_exp_summary <- function(x) {
  cut_points <- obs <- pred <- NULL
  colnames(x) <- c('obs', 'pred', 'cut_points')
  obs_exp <-
    x %>% group_by(cut_points) %>% summarise(
      obs_zero = length(obs[obs == 0]),
      obs_one = length(obs[obs ==
                             1]),
      exp_zero = (1 - mean(pred)) * n(),
      exp_one = mean(pred) * n()
    )
  obs_exp
  
}


hl_function <- function(x, g, sample_size_concord = NULL) {
  cut_points <- obs <- pred <- NULL
  x <-
    x %>% mutate(cut_points = cut(
      x[, 2],
      breaks = unique(quantile(x[, 2], seq(0, 1, 1 / g))),
      include.lowest = T
    ))
  
  obs_exp <- obs_exp_summary(x)
  return(list(obs_exp))
}

hl_test <- function(x, g, sample_size_concord = NULL) {
  hl_df <- hl_function(x, g)
  hl_df <- as.data.frame(hl_df)
  colnames(hl_df) <- gsub("hl_df.", "", colnames(hl_df))
  
  chi_square <-
    sum(with(
      hl_df,
      (obs_zero - exp_zero) ^ 2 / exp_zero + (obs_one - exp_one) ^ 2 / exp_one
    ))
  p_value <- 1 - pchisq(chi_square, g - 2)
  hl_out <- data.frame(chi_square, p_value)
  return(list(hl_out))
  
}

calib_function <- function(x, g, sample_size_concord = NULL) {
  cut_points <- obs <-  NULL
  
  colnames(x) <- c('obs', 'pred')
  cut_points_mid <-
    data.frame(bin_mid = round(((
      seq(0, 1, 1 / g) + lag(seq(0, 1, 1 / g))
    ) / 2)[-1], 2))
  obs_prob <-
    x %>% mutate(cut_points = cut(x[, 2], breaks = seq(0, 1, 1 / g), include.lowest = T)) %>%
    group_by(cut_points) %>% summarise(obs_rate = sum(obs) / n())
  mid_point <- function(x) {
    round(mean(as.numeric(unlist(
      regmatches(x, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", x))
    ))), 2)
  }
  
  
  calib_df <-
    data.frame(bin_mid = sapply(1:nrow(obs_prob), function(i)
      mid_point(obs_prob$cut_points[i])),
      obs_prob) %>% select(-cut_points)
  calib_df <-
    merge(
      cut_points_mid,
      calib_df[c("bin_mid", "obs_rate")],
      by.x = "bin_mid",
      by.y = "bin_mid",
      all.x = T
    )
  calib_df[is.na(calib_df)] <- 0
  rownames(calib_df) <- NULL
  
  return(list(calib_df))
  
}

lift_function <- function(x, g, sample_size_concord = NULL) {
  obs_zero <- obs_one <- NULL
  
  if (g < length(unique(x[, 2]))) {
    x <-
      x %>% mutate(cut_points = cut(
        x[, 2],
        breaks = unique(quantile(x[, 2], seq(0, 1, 1 / g))),
        include.lowest = T
      ))
    
  } else {
    x <- x %>% mutate(cut_points = as.factor(round(x[, 2], 2)))
    
  }
  
  obs_exp <- as.data.frame(obs_exp_summary(x))
  obs_exp <-
    obs_exp[nrow(obs_exp):1, ] %>% mutate(Total = obs_zero + obs_one)
  cum_capture_1 <-
    with(obs_exp, round((cumsum(obs_one) / (sum(
      obs_one
    ))) * 100, 2))
  fpr <-
    with(obs_exp, round((cumsum(obs_zero) / (sum(
      obs_zero
    ))) * 100, 2))
  lift_index <-
    with(obs_exp, cum_capture_1 / (cumsum(Total) / sum(Total)))
  lift_df <-
    data.frame(obs_exp["cut_points"], cum_capture_1, fpr, lift_index, KS = cum_capture_1 - fpr)
  rownames(lift_df) = NULL
  return(list(lift_df))
  
}

conc_disc <- function(x, g, sample_size_concord) {
  obs <- pred <- p_one <- p_zero <- compare <- Count <- NULL
  
  colnames(x) <- c('obs', 'pred')
  
  if (nrow(x) > 5000) {
    x <- x[sample(nrow(x), sample_size_concord), ]
  }
  
  df_one <- unlist(x %>% filter(obs == 1) %>% select(pred))
  df_zero <- unlist(x %>% filter(obs == 0) %>% select(pred))
  
  con_dis <- expand.grid(df_one, df_zero)
  colnames(con_dis)[1:2] <- c("p_one", "p_zero")
  con_dis <-
    con_dis %>% mutate(compare = c("Lesser than", "tied", "Greater than")[sign(p_one - p_zero) +
                                                                            2])
  
  con_dis <-
    con_dis %>% group_by(compare) %>% summarize(Count = n())
  con_dis <-
    con_dis %>% mutate(Perct = paste(round((Count / sum(
      Count
    )) * 100, 2), "%", sep = ""))
  count <- as.numeric(con_dis$Count)
  c_stat <-
    paste(round(((
      count[1] + 0.5 * count[3]
    ) / sum(count)) * 100, 2), "%", "")
  c_stat_df <- data.frame(label = "C-statistic", val = c_stat)
  con_dis <- con_dis[-2]
  
  colnames(c_stat_df) <- colnames(con_dis)
  
  con_dis <- rbind(con_dis, c_stat_df)
  return(list(con_dis))
  
}


combine_df <- function(list_df, index) {
  comb_df <- do.call(rbind, sapply(list_df, "[[", index))
  rep_nos <-
    sapply(sapply(list_df, "[[", index), function(x)
      nrow(x))
  comb_df <-
    data.frame(Model = unlist(lapply(seq_along(rep_nos), function(i)
      rep(paste("Model", i), rep_nos[i]))),
      comb_df)
  
  comb_df
}


plot_HL <- function(df) {
  Model <- Expected <- Value <- obs_one <- exp_one <- bins <- NULL
  
  df$bins <-
    unlist(lapply(1:length(unique(df$Model)), function(i)
      paste("Bin", seq(1, nrow(
        filter(df,
               Model == paste("Model", i))
      )))))
  df$bins <- factor(df$bins, levels = unique(df$bins))
  df <- df %>% gather(Expected, Value, obs_one, exp_one)
  g <- ggplot(df, aes(x = bins, y = Value, fill = Expected))
  g <-
    g + geom_bar(stat = "identity", position = "dodge") + facet_wrap(~ Model)
  g
}

plot_calib <- function(df) {
  bin_mid <- obs_rate <- Model <- NULL
  calib_plot <-
    ggplot(df, aes(bin_mid, obs_rate, colour = Model)) + geom_line(size = 1) + geom_point(size = 3)
  calib_plot <- calib_plot + geom_abline(intercept = 0, slope = 1)
  calib_plot
}

plot_lift <- function(df) {
  Model <- bins <- lift_index <- NULL
  
  df$bins <-
    unlist(lapply(1:length(unique(df$Model)), function(i)
      paste("Bin", seq(1, nrow(
        filter(df,
               Model == paste("Model", i))
      )))))
  df$bins <- factor(df$bins, levels = unique(df$bins))
  g <-
    ggplot(df, aes(
      x = bins,
      y = lift_index,
      group = Model,
      colour = Model
    ))
  g <- g + geom_line(size = 1) + geom_point(size = 3)
  g
}

plot_condis <- function(df) {
  obs <- pred <- NULL
  
  for (i in 1:length(df)) {
    colnames(df[[i]]) <- c('obs', 'pred')
  }
  
  comb_df <- do.call(rbind, df)
  colnames(comb_df) <- c('obs', 'pred')
  reps_no <- sapply(df, function(x)
    nrow(x))
  comb_df <-
    data.frame(Model = unlist(lapply(seq_along(reps_no), function(i)
      rep(paste("Model", i), reps_no[i]))),
      comb_df)
  
  g <-
    ggplot(comb_df, aes(x = pred, fill = as.factor(obs))) + geom_density(alpha = 0.5) + facet_wrap(~
                                                                                                     Model)
  g
  
}


conf_mat <- function(x, t) {
  x <-
    x %>% mutate(pred_prob = as.factor(ifelse(x[, 2] >= t, "Pred-1", "Pred-0")))
  x$pred_prob <-
    factor(x = x$pred_prob,
           levels = c("Pred-0", "Pred-1"))
  conf_mat <- table(x[, 3], x[, 1])
  conf_mat
  
}

conf_mat_metrics <- function(x, t) {
  matrix <- conf_mat(x, t)
  Acc <- (matrix[1, 1] + matrix[2, 2]) / sum(matrix)
  Acc <- round(Acc * 100, 2)
  TPR <- matrix[2, 2] / sum(matrix[, 2])
  TPR <- round(TPR * 100, 2)
  FPR <- matrix[2, 1] / sum(matrix[, 1])
  FPR <- round(FPR * 100, 2)
  Prec <- matrix[2, 2] / sum(matrix[2, ])
  Prec <- round(Prec * 100, 2)
  output <-
    data.frame(
      Threshold = t,
      Acc = Acc,
      TPR = TPR,
      FPR = FPR,
      Prec = Prec
    )
  output
  
}

conf_range <- function(x, reps, all.unique = F) {
  if (all.unique == T) {
    prob_values <- sort(unique(x[, 2]))
  } else {
    prob_values <- seq(0, 1, 1 / reps)
  }
  
  out <-
    lapply(seq_along(prob_values), function(i)
      conf_mat_metrics(x, prob_values[i]))
  out <- do.call(rbind, out)
  out
  
}

########################################################################################
################################# Master Functions #####################################
########################################################################################

staticPerfMeasures <- function(list_models,
                               g,
                               perf_measures = c('hosmer', 'calibration', 'lift', 'concord'),
                               sample_size_concord = 5000) {
  pkgTest()
  error_handler(list_models, g, method = "Performance_Measure")
  
  perf_functions <-
    list(
      hosmer_df = hl_function,
      hosmer_results = hl_test,
      calibration = calib_function,
      lift = lift_function,
      concord = conc_disc
    )
  
  perf_functions <-
    perf_functions[grep(paste(perf_measures, collapse = "|"), names(perf_functions))]
  
  df_out <-
    lapply(list_models, function(x)
      lapply(perf_functions, function(f)
        f(x, g, sample_size_concord)))
  df_out <-
    lapply(1:length(perf_functions), function(i)
      combine_df(df_out, i))
  names(df_out) <- names(perf_functions)
  
  if (length(grep("hosmer_results|concord", names(df_out))) != 0) {
    plot_dfs <- df_out[-grep("hosmer_results|concord", names(df_out))]
    
  } else {
    plot_dfs <- df_out
  }
  
  plot_functions <- list(hosmer = plot_HL,
                         calibration = plot_calib,
                         lift = plot_lift)
  plot_functions <-
    plot_functions[grep(paste(perf_measures, collapse = "|"), names(plot_functions))]
  
  plots_out <- Map(function(f, x)
    f(x), plot_functions, plot_dfs)
  
  if ("concord" %in% perf_measures) {
    plots_out$concord <- plot_condis(list_models)
  }
  
  return(list(data = df_out, plots = plots_out))
  
}

staticConfMatrix <-
  function(list_models,
           t,
           reps = NULL,
           reps.all.unique = F) {
    pkgTest()
    error_handler(list_models, t, method = "Confusion")
    
    if (!is.null(reps)) {
      out_metrics_range <-
        lapply(list_models, function(x)
          conf_range(x, reps))
      out_metrics_range <- do.call(rbind, out_metrics_range)
      Model = unlist(lapply(seq_along(list_models), function(i)
        rep(paste("Model", i), reps + 1)))
      out_metrics_range <- data.frame(Model, out_metrics_range)
    }
    
    if (reps.all.unique) {
      out_metrics_range <-
        lapply(list_models, function(x)
          conf_range(x, reps, all.unique = T))
      out_metrics_range <- do.call(rbind, out_metrics_range)
      Model = unlist(lapply(seq_along(list_models), function(i)
        rep(paste("Model", i), nrow(
          unique(list_models[[i]][2])
        ))))
      out_metrics_range <- data.frame(Model, out_metrics_range)
    }
    
    out_conf <- lapply(list_models, function(x)
      conf_mat(x, t))
    out_conf <- as.data.frame(do.call(cbind, out_conf))
    out_conf <- data.frame(Pred = c("Pred-0", "Pred-1"), out_conf)
    
    colnames(out_conf)[-1] <-
      rep(c('Actuals-0', 'Actuals-1'), times = (ncol(out_conf) - 1) / 2)
    rownames(out_conf) <- NULL
    
    out_metrics <-
      lapply(list_models, function(x)
        conf_mat_metrics(x, t))
    out_metrics <- do.call(rbind, out_metrics)
    Model = sapply(seq_along(list_models), function(i)
      paste("Model", i))
    out_metrics <- data.frame(Model, out_metrics)
    
    if ((!is.null(reps)) || (reps.all.unique)) {
      return(list(
        metrics_range = out_metrics_range,
        conf = out_conf,
        metrics = out_metrics
      ))
    } else {
      return(list(matrix = out_conf, measures = out_metrics))
    }
    
  }

shinyPerfMeasures <-
  function(list_models,
           sample_size_concord = 5000,
           model_function = NULL,
           data = NULL,
           y = NULL) {
    pkgTest()
    if (is.null(model_function)) {
      error_handler(list_models, arg = 0, method = "Performance_Measure")
    }
    
    return_fun_value <- function(model_function, form) {
      model_function(form)
    }
    
    
    
    shinyApp(
      ui = fluidPage(sidebarLayout(
        sidebarPanel(
          uiOutput("build_model_interactively"),
          sliderInput("bins", "Select No of Bins To Be Created", 1, 30, 10),
          selectInput(
            "perf_measures",
            label = strong("Select Performance Metrice"),
            choices = list(
              "Hosmer Lemeshow" = "hosmer",
              "Calibration Plot" = "calibration",
              "Lift Index" = "lift",
              "Conordance-Discordance" = "concord"
            )
          ),
          
          conditionalPanel(
            condition = " input.build_model == 'Y' ",
            uiOutput("variables"),
            sliderInput(
              "numModels",
              "No. of Additional Models",
              min = 0,
              max = 10,
              value = 0
            ),
            uiOutput("addModels")
          ),
          
          actionButton("perf_measure_button", "Run Analysis")
        ),
        
        mainPanel(
          conditionalPanel(condition = " input.perf_measures == 'hosmer' ",
                           dataTableOutput("hosmer_results")),
          
          h4(strong("Goodness of Fit Test Statistics"), style = "color:green"),
          plotOutput("plot_perf_measure"),
          dataTableOutput("df_perf_measure")
        )
      )),
      
      server = function(input, output, session) {
        perf_func_reactive <- reactive({
          if (input$build_model == 'Y')   {
            var_list_combined <- list()
            var_list_combined[[1]] <-
              paste(input$Var1, collapse = "+")
            
            ModelsCount <- as.numeric(input$numModels)
            
            if (ModelsCount > 0) {
              for (i in 1:ModelsCount) {
                if ((input[[paste("Mod", i + 1)]]) != "") {
                  var_list_i <-
                    paste(var_list_combined[[1]], input[[paste("Mod", i + 1)]], sep = "")
                  var_list_combined[[i + 1]] <- var_list_i
                }
              }
            }
            
            run_formula <- function(x) {
              formula_val <- as.formula(paste(y, " ~ ", x, sep = ""))
              df = return_fun_value(model_function, formula_val)
              df
            }
            
            
            out_df <-
              lapply(seq_along(var_list_combined), function(i)
                run_formula(var_list_combined[[i]]))
            out <-
              staticPerfMeasures(
                out_df,
                g = input$bins,
                perf_measures = input$perf_measures,
                sample_size_concord
              )
            
          } else {
            g <- as.numeric(input$bins)
            out <-
              staticPerfMeasures(list_models,
                                 g,
                                 perf_measures = input$perf_measures,
                                 sample_size_concord)
          }
          
          
          if (input$perf_measures == 'hosmer') {
            perf_df <- out$data$hosmer_df
            hosmer_test <- out$data$hosmer_results
            perf_plot <- out$plots$hosmer
            return(list(
              df = perf_df,
              plot = perf_plot,
              hosmer_test = hosmer_test
            ))
          }
          
          else{
            perf_df <- out[[1]]
            perf_plot <- out[[2]]
            return(list(df = perf_df, plot = perf_plot))
          }
          
          
        })
        
        button_click <- eventReactive(input$perf_measure_button, {
          perf_func_reactive()
          
        })
        
        output$plot_perf_measure <- renderPlot({
          button_click()$plot
          
        })
        
        output$df_perf_measure <- renderDataTable({
          df <- as.data.frame(button_click()$df)
          df
          
        })
        
        output$hosmer_results <- renderDataTable({
          df <- as.data.frame(button_click()$hosmer_test)
          df
          
        })
        
        output$variables <- renderUI({
          selectizeInput(
            "Var1",
            "Select Independent Vars Excluding Dependent Var",
            colnames(data),
            multiple = T
          )
          
        })
        
        output$addModels <- renderUI({
          ModelsCount <- as.integer(input$numModels)
          if (ModelsCount > 0) {
            lapply(1:ModelsCount, function(i) {
              textInput(
                inputId = paste("Mod", i + 1),
                label = "Additional Model",
                value = ""
              )
            })
          }
          
        })
        
        output$build_model_interactively <- renderUI({
          if (!is.null(model_function)) {
            radioButtons(
              "build_model",
              "Build Model Interactively?",
              choices = c("Yes" = "Y", "No" = "N"),
              selected = "Y"
            )
          } else {
            radioButtons(
              "build_model",
              "Build Model Interactively?",
              choices = c("Yes" = "Y", "No" = "N"),
              selected = "N"
            )
          }
          
        })
        
        session$onSessionEnded(function() {
          stopApp()
          
        })
      }
    )
  }

shinyConfMatrix <-
  function(list_models,
           model_function = NULL,
           data = NULL,
           y = NULL) {
    pkgTest()
    if (is.null(model_function)) {
      error_handler(list_models, arg = 0, method = "Confusion")
    }
    
    return_fun_value <- function(model_function, form) {
      model_function(form)
    }
    
    shinyApp(
      ui = fluidPage(sidebarLayout(
        sidebarPanel(
          uiOutput("build_model_interactively"),
          
          conditionalPanel(
            condition = " input.build_model == 'Y' ",
            uiOutput("variables"),
            sliderInput(
              "numModels",
              "No. of Additional Models",
              min = 0,
              max = 10,
              value = 0
            ),
            uiOutput("addModels")
          ),
          
          radioButtons(
            "Option",
            'Select Analysis',
            choices = list("Confusion Matrix" = "matrix", "Plots & Metrics" =
                             "plot"),
            selected = "matrix"
          ),
          
          conditionalPanel(
            condition = " input.Option == 'matrix' ",
            sliderInput("Threshold", "Set Threshold", 0, 1, 0.1)
          ),
          
          conditionalPanel(
            condition = " input.Option == 'plot' ",
            selectInput(
              "Metric",
              "Select Metric",
              choices = list(
                "Accuracy" = "Acc",
                "True Positive Rate" = "TPR",
                "False Positive Rate" = "FPR",
                "Precision" = "Prec"
              )
            ),
            sliderInput("Reps", "No of Samples", 10, 100, 10)
          ),
          
          actionButton("Confusion_Button", "Run Analysis")
        ),
        mainPanel(
          conditionalPanel(
            condition = " input.Option == 'matrix' ",
            h4(strong("Confusion Matrix"), style = "color:green"),
            dataTableOutput('confusion_df'),
            dataTableOutput('metrics_df')
          ),
          
          conditionalPanel(
            condition = "input.Option=='plot'",
            h4(strong("Selected Metric vs Prob Threshold"), style = "color:green"),
            plotOutput('metrics_range_plot'),
            dataTableOutput('metrics_range_df')
          )
          
        )
      )),
      server = function(input, output, session) {
        confusion_func_shiny <- reactive({
          if (input$build_model == 'Y')   {
            var_list_combined <- list()
            var_list_combined[[1]] <-
              paste(input$Var1, collapse = "+")
            ModelsCount <- as.integer(input$numModels)
            
            if (ModelsCount > 0) {
              for (i in 1:ModelsCount) {
                if ((input[[paste("Mod", i + 1)]]) != "") {
                  var_list_i <-
                    paste(var_list_combined[[1]], input[[paste("Mod", i + 1)]], sep = "")
                  var_list_combined[[i + 1]] <- var_list_i
                }
              }
            }
            
            
            build_model <- function(x) {
              formula_val <- as.formula(paste(y, " ~ ", x, sep = ""))
              df = return_fun_value(model_function, formula_val)
              df
            }
            
            
            out_df <-
              lapply(seq_along(var_list_combined), function(i)
                build_model(var_list_combined[[i]]))
            out <-
              staticConfMatrix(out_df,
                               t = as.numeric(input$Threshold),
                               reps = input$Reps)
          } else {
            t <- as.numeric(input$Threshold)
            out <-
              staticConfMatrix(list_models, t = t, reps = input$Reps)
          }
          
          out_list <-
            list(
              metrics_range = out[[1]],
              confusion = out[[2]],
              metrics = out[[3]]
            )
          
        })
        
        confusion_event <- eventReactive(input$Confusion_Button, {
          confusion_func_shiny()
          
        })
        
        output$confusion_df <- renderDataTable({
          as.data.frame(confusion_event()$confusion)
          
        })
        output$metrics_df <- renderDataTable({
          as.data.frame(confusion_event()$metrics)
          
        })
        
        output$metrics_range_plot <- renderPlot({
          out_metrics_range <- as.data.frame(confusion_event()$metrics_range)
          
          Metric <- as.character(input$Metric)
          
          plot_val <-
            ggplot(out_metrics_range,
                   aes_string(x = "Threshold", y = Metric, color = "Model")) + geom_line()
          plot_val
          
        })
        
        output$metrics_range_df <- renderDataTable({
          as.data.frame(confusion_event()$metrics_range)
          
        })
        output$variables <- renderUI({
          selectizeInput(
            "Var1",
            "Select Independent Vars Excluding Dependent Var",
            colnames(data),
            multiple = T
          )
        })
        
        output$addModels <- renderUI({
          ModelsCount <- as.integer(input$numModels)
          if (ModelsCount > 0) {
            lapply(1:ModelsCount, function(i) {
              textInput(
                inputId = paste("Mod", i + 1),
                label = "Aditional Model",
                value = ""
              )
            })
          }
          
        })
        
        output$build_model_interactively <- renderUI({
          if (!is.null(model_function)) {
            radioButtons(
              "build_model",
              "Build Model Interactively?",
              choices = c("Yes" = "Y", "No" = "N"),
              selected = "Y"
            )
          } else {
            radioButtons(
              "build_model",
              "Build Model Interactively?",
              choices = c("Yes" = "Y", "No" = "N"),
              selected = "N"
            )
          }
          
        })
        
        
        session$onSessionEnded(function() {
          stopApp()
          
        })
        
      }
    )
  }