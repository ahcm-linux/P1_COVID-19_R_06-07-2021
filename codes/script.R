###############################################################################################################
# Data analysis of COVID-19 published at: (article submitted waiting for publication)
# date of creation: 06/07/2021 (date in US format)
# R version: 4.0.3
# script name: script.R
# aim: data analysis
# input: files from the folder 'data'
# output: files saved in the subdirectories of folder 'outputs'
# external sources: files 'functions.R' and 'packages.R' from folder 'codes' 
###############################################################################################################


# PARAMETERS --------------------------------------------------------------------------------------------------

# choose colors (change these parameters to get figures with the desired colors)
color_male = "deepskyblue2" # color for representing Gender = Male
color_female = "chocolate2" # color for representing Gender = Female
color_alive = "chartreuse2" # color for representing Outcome = Alive
color_dead = "red2"         # color for representing Outcome = Dead


# EXTERNAL SOURCES --------------------------------------------------------------------------------------------

# install and load packages
base::source("codes/packages.R")

# load subroutines
base::source("codes/functions.R")


# DATA --------------------------------------------------------------------------------------------------------

# load datasets
data_long <- utils::read.csv('data/data_long.csv', header = TRUE, sep = '\t')
data_wide <- utils::read.csv('data/data_wide.csv', header = TRUE, sep = '\t')

# levels
analytes_levels <- base::c("Eritrocitos", "Plaquetas", "Monocitos", "Neutrofilos", "Leucocitos")

# analytes
Analytes <- base::paste0("Hemograma_X_", analytes_levels)

# labels
analytes_labels_long <- base::c("Hemograma_X_Plaquetas" = "Platelets", "Hemograma_X_Monocitos" = "Monocytes", "Hemograma_X_Eritrocitos" = "Erythrocytes", "Hemograma_X_Neutrofilos" = "Neutrophils", "Hemograma_X_Leucocitos" = "Leukocytes")
analytes_labels_wide <- base::c("Plaquetas" = "Platelets", "Monocitos" = "Monocytes", "Eritrocitos" = "Erythrocytes", "Neutrofilos" = "Neutrophils", "Leucocitos" = "Leukocytes")


# DESCRIPTIVE ANALYSIS ----------------------------------------------------------------------------------------

## Data -------------------------------------------------------------------------------------------------------

# hsl data, long
sub_hsl_long <- base::subset(data_long, hospital == 'HSL' & !base::is.na(de_desfecho) & exame_analito %in% Analytes)
obito <- base::grep('óbito', sub_hsl_long$de_desfecho)
sub_hsl_long$de_desfecho[obito] <- 'Dead'
sub_hsl_long$de_desfecho[base::setdiff(1:base::nrow(sub_hsl_long), obito)] <- 'Alive'
sub_hsl_long$exame_analito <- base::factor(sub_hsl_long$exame_analito, levels = base::paste0("Hemograma_X_", analytes_levels), ordered = TRUE)
sub_hsl_long <- plyr::ddply(sub_hsl_long, ~ id_paciente, dplyr::mutate, time_of_death = base::ifelse(midpoint_interval == base::max(midpoint_interval) & de_desfecho == "Dead", 1, 0))

## Plots ------------------------------------------------------------------------------------------------------

custom_theme <- ggplot2::theme_light(base_size = 12) +
  ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_blank(),
        legend.position = "top",
        legend.box.background = ggplot2::element_rect(colour = "black", fill = NA),
        legend.key = ggplot2::element_rect(colour = NA, fill = NA),
        strip.text = ggplot2::element_text(colour = "black", hjust = 0, size = 14),
        strip.background = ggplot2::element_rect(fill = NA, colour = NA))

# (Alive) Male and Female average trajectories
plot_gender_trajectories <- base::subset(sub_hsl_long, de_desfecho == "Alive") %>%
  dplyr::group_by(gender, midpoint_interval, exame_analito) %>% 
  dplyr::mutate(mean_traj = stats::median(median_obs)) %>%
  dplyr::mutate(lwr = stats::quantile(median_obs, probs = 0.25)) %>%
  dplyr::mutate(upr = stats::quantile(median_obs, probs = 0.75)) %>%
  ggplot2::ggplot(ggplot2::aes(x = midpoint_interval, y = median_obs, group = id_paciente)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr, fill = gender), alpha = 0.05, show.legend = FALSE) +
  ggplot2::geom_point(alpha = 0.5, colour = grDevices::grey(0.2)) +
  ggplot2::geom_line(alpha = 0.3, colour = grDevices::grey(0.2)) +
  ggplot2::geom_line(ggplot2::aes(y = mean_traj, group = gender, colour = gender), size = 2, alpha = 0.8) +
  ggplot2::facet_wrap(~ exame_analito, scales = "free", nrow = 2, labeller = ggplot2::as_labeller(analytes_labels_long)) +
  ggplot2::scale_colour_manual("Gender: ", values = base::c("M" = color_male, "F" = color_female), labels = base::c("M" = "Male", "F" = "Female")) +
  ggplot2::scale_fill_manual(values = base::c("M" = color_male, "F" = color_female)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::labs(x = "Hospitalization time", y = "Count/ul") +
  custom_theme

# Dead and Alive average trajectories
plot_outcome_trajectories <- sub_hsl_long %>%
  dplyr::group_by(de_desfecho, midpoint_interval, exame_analito) %>% 
  dplyr::mutate(mean_traj = stats::median(median_obs)) %>%
  dplyr::mutate(lwr = stats::quantile(median_obs, probs = 0.25)) %>%
  dplyr::mutate(upr = stats::quantile(median_obs, probs = 0.75)) %>%
  ggplot2::ggplot(ggplot2::aes(x = midpoint_interval, y = median_obs, group = id_paciente)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr, fill = de_desfecho), alpha = 0.05, show.legend = FALSE) +
  ggplot2::geom_point(alpha = 0.5, colour = grDevices::grey(0.2)) +
  ggplot2::geom_line(alpha = 0.3, colour = grDevices::grey(0.2)) +
  ggplot2::geom_line(ggplot2::aes(y = mean_traj, group = de_desfecho, colour = de_desfecho), size = 2, alpha = 0.8) +
  ggplot2::facet_wrap(~ exame_analito, scales = "free_y", nrow = 2, labeller = ggplot2::as_labeller(analytes_labels_long)) +
  ggplot2::scale_colour_manual("Outcome: ", values = base::c("Alive" = color_alive, "Dead" = color_dead)) +
  ggplot2::scale_fill_manual(values = base::c("Alive" = color_alive, "Dead" = color_dead)) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::labs(x = "Hospitalization time", y = "Count/ul") +
  custom_theme

# Percentages and Sum trajectories
df_perc_gender <- sub_hsl_long %>% base::subset(exame_analito == "Hemograma_X_Plaquetas")  %>% dplyr::select(dplyr::starts_with(base::c("gender", "midpoint_interval", "time_of_death")))
df_perc_outcome <- sub_hsl_long %>% base::subset(exame_analito == "Hemograma_X_Plaquetas")  %>% dplyr::select(dplyr::starts_with(base::c("de_desfecho", "midpoint_interval", "time_of_death")))
base::colnames(df_perc_gender) <- base::colnames(df_perc_outcome) <- base::c("variable", "time", "death")

df_perc_male <- plyr::ddply(df_perc_gender, ~ time, plyr::summarise, perc_variable = base::sum(variable == "M") / base::length(time), sum_variable = base::sum(variable == "M"))
df_perc_female <- plyr::ddply(df_perc_gender, ~ time, plyr::summarise, perc_variable = base::sum(variable == "F") / base::length(time), sum_variable = base::sum(variable == "F"))
df_perc_gender <- base::rbind(base::data.frame(df_perc_male, variable = "M"), base::data.frame(df_perc_female, variable = "F"))

df_perc_alive <- plyr::ddply(df_perc_outcome, ~ time, plyr::summarise, perc_variable = base::sum(death == 0) / base::length(time), sum_variable = base::sum(death == 0))
df_perc_dead <- plyr::ddply(df_perc_outcome, ~ time, plyr::summarise, perc_variable = base::sum(death == 1) / base::length(time), sum_variable = base::sum(death == 1))
df_perc_outcome <- base::rbind(base::data.frame(df_perc_alive, variable = "A"), base::data.frame(df_perc_dead, variable = "D"))

df_perc <- base::rbind(base::data.frame(df_perc_gender, group = "1"), base::data.frame(df_perc_outcome, group = "2"))

plot_perc_trajectories <- ggplot2::ggplot(base::subset(df_perc, variable %in% base::c("M", "D")), ggplot2::aes(x = time, y = perc_variable)) +
  ggplot2::geom_line(ggplot2::aes(group = variable, colour = variable), alpha = 0.8, show.legend = FALSE) +
  ggplot2::geom_point(ggplot2::aes(group = variable, colour = variable), size = 2, alpha = 1, show.legend = FALSE) +
  ggplot2::facet_wrap(~ group, scales = "free_y", ncol = 1, labeller = ggplot2::as_labeller(base::c("1" = "Gender (percentage)", "2" = "Outcome (percentage)"))) +
  ggplot2::scale_colour_manual("Legend: ", values = base::c("M" = color_male, "F" = color_female, "A" = color_alive, "D" = color_dead), labels = base::c("M" = "Male", "F" = "Female", "A" = "Alive", "D" = "Dead")) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::labs(x = "", y = "") +
  custom_theme + ggplot2::theme(strip.text = ggplot2::element_text(size = 12))

plot_sum_trajectories <- ggplot2::ggplot(base::subset(df_perc, variable %in% base::c("M", "F", "D")), ggplot2::aes(x = time, y = sum_variable)) +
  ggplot2::geom_line(ggplot2::aes(group = variable, colour = variable), alpha = 0.8) +
  ggplot2::geom_point(ggplot2::aes(group = variable, colour = variable), size = 2, alpha = 1) +
  ggplot2::facet_wrap(~ group, scales = "free_y", ncol = 1, labeller = ggplot2::as_labeller(base::c("1" = "Gender (count)", "2" = "Outcome (count)"))) +
  ggplot2::scale_colour_manual("Legend: ", values = base::c("M" = color_male, "F" = color_female, "A" = color_alive, "D" = color_dead), labels = base::c("M" = "Male", "F" = "Female", "A" = "Alive", "D" = "Dead")) +
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
  ggplot2::labs(x = "", y = "") +
  custom_theme + ggplot2::theme(strip.text = ggplot2::element_text(size = 12))

shared_legend <- g_legend(plot_sum_trajectories)
shared_y_title <- grid::textGrob("Hospitalization time", gp = grid::gpar(fontsize = 12), vjust = -1.5)

plot_perc_sum_trajectories <- gridExtra::arrangeGrob(shared_legend,
                                          gridExtra::arrangeGrob(plot_perc_trajectories + ggplot2::theme(legend.position = "none"),
                                                                 plot_sum_trajectories + ggplot2::theme(legend.position = "none"),
                                                                 nrow = 1, bottom = shared_y_title),
                                          nrow = 2, heights = base::c(2, 10))

## Tables -----------------------------------------------------------------------------------------------------

# count Dead and Alive by gender
base::suppressMessages(table_desc_data <- sub_hsl_long %>% dplyr::group_by(gender, de_desfecho) %>% dplyr::select(dplyr::starts_with("id_paciente")) %>% base::unique() %>% dplyr::count())
base::colnames(table_desc_data) <- base::c("Gender", "Outcome", "Count")
table_desc_data$Gender <- base::gsub("F", "Female", table_desc_data$Gender)
table_desc_data$Gender <- base::gsub("M", "Male", table_desc_data$Gender)


# INFERENTIAL ANALYSIS ----------------------------------------------------------------------------------------

## Data -------------------------------------------------------------------------------------------------------

# hsl data, wide
sub_hsl <- base::subset(data_wide, hospital == 'HSL' & !base::is.na(de_desfecho))
obito <- base::grep('óbito', sub_hsl$de_desfecho)
sub_hsl$de_desfecho[obito] <- 'Dead'
sub_hsl$de_desfecho[base::setdiff(1:base::nrow(sub_hsl), obito)] <- 'Alive'
sub_hsl$break_point <- stats::relevel(base::factor(base::ifelse(sub_hsl$n_days > 20, "A", "B")), ref = "B")
sub_hsl$GenderOutcome <- base::paste(sub_hsl$gender, sub_hsl$de_desfecho, sep = "_")


## Models -----------------------------------------------------------------------------------------------------

# fit
set.seed(2021)

analytes_models <- base::lapply(Analytes, function(.x) {
  base::cat("wait while R is fitting a model for", analytes_labels_long[.x], "...\n")
  
  y <- sub_hsl[, .x]
  dataset <- base::data.frame(sub_hsl, y = y)
  
  # model
  fit <- nlme::lme(y ~ gender + de_desfecho + splines::bs(midpoint_interval) + gender : splines::bs(midpoint_interval) + de_desfecho : splines::bs(midpoint_interval),
             random =~ 1|id_paciente, data = dataset)
  
  # bootstrap
  var_comp <- base::as.double(nlme::VarCorr(fit))
  fit_boot <- generic.group.boot.REB.0.2(y, X = stats::model.matrix(fit, data = dataset)[, -1], alpha = fit$coef$fixed[1],
                                         beta = fit$coef$fixed[-1], sigma.u = var_comp[3], sigma.e = var_comp[4],
                                         group = dataset$id_paciente, k = 1000, verbose = FALSE, stop = FALSE)
  
  base::list(fit = fit, boot = fit_boot)
})
base::names(analytes_models) <- Analytes

# results
analytes_fit <- base::lapply(analytes_models, "[[", 1)
analytes_boot <- base::lapply(analytes_models, "[[", 2)

# summary
models_summary <- base::lapply(analytes_fit, base::summary)

# regression fixed effects
models_coef <- base::lapply(analytes_fit, function(.x) {
  fixed_eff <- .x$coef$fixed
  sigma <- base::as.double(nlme::VarCorr(.x))
  base::c(fixed_eff, sigma.u = sigma[1], sigma.e = sigma[2], lambda = sigma[1] / sigma[2])
})

# var and sd of random effects and residuals
models_sd <- base::lapply(analytes_fit, function(.x) {
  var_sd <- nlme::VarCorr(.x)
  matrix(base::as.double(var_sd), ncol = 2, dimnames = base::list(base::rownames(var_sd), base::colnames(var_sd)))
})

# AIC
models_aic <- base::lapply(analytes_fit, stats::AIC)

# bootstrap CIs
models_boot_ci <- base::lapply(1:base::length(analytes_boot), function(.x) {
  boot_ci <- base::lapply(analytes_boot[[.x]], function(.y) {
    ci <- base::t(base::apply(.y, 2, stats::quantile, probs = base::c(0.025, 0.975)))
    base::data.frame(est = models_coef[[.x]], ci, check.names = FALSE)
  })
})
base::names(models_boot_ci) <- base::names(analytes_boot)

models_boot_ci_adj <- base::lapply(models_boot_ci, "[[", 2)

## Plots ------------------------------------------------------------------------------------------------------

standard_theme <- ggplot2::theme(
  panel.grid.minor.x = ggplot2::element_blank(),
  panel.grid.minor.y = ggplot2::element_blank(),
  legend.position = "top",
  legend.box.background = ggplot2::element_rect(colour = "black", fill = NA),
  legend.key = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
  strip.text = ggplot2::element_text(face = "bold", colour = "black"),
  strip.background = ggplot2::element_rect(fill = NA, colour = "black")
)


# time x gender
p_time_gender <- base::list()
for (.x in 1:base::length(analytes_fit)) {
  analyte <- base::names(analytes_fit)[[.x]]
  y <- sub_hsl[, analyte]
  dataset <- base::data.frame(sub_hsl, y = y)
  
  base::suppressMessages(
    p <- sjPlot::plot_model(analytes_fit[[.x]], type = "eff", terms = base::c("midpoint_interval", "gender"),
                    robust = TRUE, vcov.fun = "vcovHC", vcov.type = "HC3", show.data = TRUE,
                    dot.alpha = 0.3, dot.size = 1, line.alpha = 0.8, line.size = 1,
                    title = analytes_labels_long[analyte]) +
      ggplot2::scale_colour_manual("Gender: ", values = base::c("M" = color_male, "F" = color_female), labels = base::c("M" = "Male", "F" = "Female")) +
      ggplot2::scale_fill_manual("Gender: ", values = base::c("M" = color_male, "F" = color_female), labels = base::c("M" = "Male", "F" = "Female")) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::labs(x = "Hospitalization time", y = "Count/ul") +
      ggplot2::theme_light(base_size = 12) + standard_theme
    )
  
  if (.x != 1) p <- p + ggplot2::theme(legend.position = "none")
  
  p_time_gender[[.x]] <- p
}

shared_legend <- g_legend(p_time_gender[[1]])
p_time_gender[[1]] <- p_time_gender[[1]] + ggplot2::theme(legend.position = "none")
plot_time_gender <- gridExtra::arrangeGrob(shared_legend, gridExtra::arrangeGrob(grobs = p_time_gender, nrow = 2), nrow = 2, heights = base::c(5, 40))

# time x outcome
p_time_outcome <- base::list()
for (.x in 1:base::length(analytes_fit)) {
  analyte <- base::names(analytes_fit)[[.x]]
  y <- sub_hsl[, analyte]
  dataset <- base::data.frame(sub_hsl, y = y)
  
  base::suppressMessages(
    p <- sjPlot::plot_model(analytes_fit[[.x]], type = "eff", terms = base::c("midpoint_interval", "de_desfecho"),
                    robust = TRUE, vcov.fun = "vcovHC", vcov.type = "HC3", show.data = TRUE,
                    dot.alpha = 0.3, dot.size = 1, line.alpha = 0.8, line.size = 1,
                    title = analytes_labels_long[analyte]) +
      ggplot2::scale_colour_manual("Outcome: ", values = base::c("Alive" = color_alive, "Dead" = color_dead)) +
      ggplot2::scale_fill_manual("Outcome: ", values = base::c("Alive" = color_alive, "Dead" = color_dead)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
      ggplot2::labs(x = "Hospitalization time", y = "Count/ul") +
      ggplot2::theme_light(base_size = 12) + standard_theme
    )
  
  if (.x != 1) p <- p + ggplot2::theme(legend.position = "none")
  
  p_time_outcome[[.x]] <- p
}

shared_legend <- g_legend(p_time_outcome[[1]])
p_time_outcome[[1]] <- p_time_outcome[[1]] + ggplot2::theme(legend.position = "none")
plot_time_outcome <- gridExtra::arrangeGrob(shared_legend, gridExtra::arrangeGrob(grobs = p_time_outcome, nrow = 2), nrow = 2, heights = base::c(5, 40))

# uncomment the code below to generate plots of residuals

#plot_residuals <- base::lapply(analytes_fit, function(.x) {
#  df_res <- base::data.frame(res = stats::residuals(.x), fit = stats::fitted(.x), index = 1:base::nrow(sub_hsl))
#  p_res_fit <- ggplot2::ggplot(df_res, ggplot2::aes(x = fit, y = res)) +
#    ggplot2::geom_hline(yintercept = base::c(-2, 2), linetype = 'dashed', size = 0.5) +
#    ggplot2::geom_point(shape = 20, size = 2, alpha = 0.5) +
#    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::theme_light(base_size = 12) +
#    ggplot2::labs(y = "Quantile residual", x = "Fitted value")
#  p_res_index <- ggplot2::ggplot(df_res, ggplot2::aes(x = index, y = res)) +
#    ggplot2::geom_hline(yintercept = base::c(-2, 2), linetype = 'dashed', size = 0.5) +
#    ggplot2::geom_point(shape = 20, size = 2, alpha = 0.5) +
#    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::theme_light(base_size = 12) +
#    ggplot2::labs(y = "Quantile residual", x = "Index of observation")
#  p_res_density <- ggplot2::ggplot(df_res, ggplot2::aes(x = res, fill = "1")) +
#    ggplot2::geom_density(position = "identity", alpha = 0.1, size = 0.5, show.legend = FALSE) +
#    ggplot2::scale_fill_manual(values = grDevices::grey(0.4)) +
#    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::theme_light(base_size = 12) +
#    ggplot2::labs(y = "Density", x = "Quantile residual")
#  p_res_qq <- ggplot2::ggplot(df_res, ggplot2::aes(sample = res)) +
#    ggplot2::stat_qq(size = 1, alpha = 0.5) + ggplot2::stat_qq_line() +
#    ggplot2::scale_fill_manual(values = grDevices::grey(0.4)) +
#    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
#    ggplot2::theme_light(base_size = 12) +
#    ggplot2::labs(y = "Sample quantile", x = "Theoretical quantile")
#  p_diagnostics <- gridExtra::arrangeGrob(p_res_fit, p_res_index, p_res_density, p_res_qq, nrow = 2)
#})

## Tables -----------------------------------------------------------------------------------------------------

# create tables of model estimates and corresponding bootstrap CIs
models_tables <- base::lapply(1:base::length(analytes_fit), function(.x) {
  html_table <- sjPlot::tab_model(analytes_fit[[.x]], dv.labels = analytes_labels_long[base::names(analytes_fit)[.x]],
                          show.re.var = TRUE, show.p = FALSE, show.se = FALSE, show.stat = FALSE,
                          show.aic = FALSE, show.r2 = FALSE, show.reflvl = TRUE, title = "Linear Mixed Model")
  
  td <- base::unlist(strsplit(html_table$page.complete, split = "</td>"))
  col1 <- base::grep("col1", td)[-1]
  boot_ci <- base::sapply(col1, function(.y) {
    results <- models_boot_ci_adj[[.x]]
    iv_name <- base::gsub("(.+>)", "", td[.y])
    iv_boot <- base::which(base::rownames(results) == iv_name)
    lwr <- base::round(results[iv_boot, 2], 2)
    upr <- base::round(results[iv_boot, 3], 2)
    if (base::sign(lwr) == base::sign(upr)) {
      if (lwr < 0) lwr <- base::paste0("45;", base::format(base::abs(lwr), nsmall = 2))
      if (lwr < 0) lwr <- base::paste0("45;", base::format(base::abs(upr), nsmall = 2))
      ci <- base::paste0(lwr, "&nbsp;&ndash;&nbsp;", upr, " *")
    } else {
      if (lwr < 0) lwr <- base::paste0("45;", base::format(base::abs(lwr), nsmall = 2))
      if (lwr < 0) lwr <- base::paste0("45;", base::format(base::abs(upr), nsmall = 2))
      ci <- base::paste0(lwr, "&nbsp;&ndash;&nbsp;", upr)
    }
    html_ci <- base::gsub("(?!.*\">)(\\d.+\\d)", ci, td[.y + 2], perl = TRUE)
  })
  td[col1[1:base::length(boot_ci)] + 2] <- boot_ci
  
  html_table$page.complete <- base::paste(td, collapse = "</td>")
  html_table$page.complete <- base::gsub("CI", "Bootstrap CI", html_table$page.complete)
  html_table$page.complete <- base::gsub("splines::bs\\(midpoint_interval\\)1", "Time [1st term]", html_table$page.complete)
  html_table$page.complete <- base::gsub("splines::bs\\(midpoint_interval\\)2", "Time [2nd term]", html_table$page.complete)
  html_table$page.complete <- base::gsub("splines::bs\\(midpoint_interval\\)3", "Time [3rd term]", html_table$page.complete)
  html_table$page.complete <- base::gsub("genderM", "Gender [Male]", html_table$page.complete)
  html_table$page.complete <- base::gsub("de_desfechoDead", "Outcome [Dead]", html_table$page.complete)
  html_table$page.complete <- base::gsub("paciente", "patient", html_table$page.complete)
  
  base::return(html_table)
})
base::names(models_tables) <- base::names(analytes_fit)


# OUTPUTS -----------------------------------------------------------------------------------------------------

## Plots ------------------------------------------------------------------------------------------------------

# gender trajectories
ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigA1", ".pdf"), plot = plot_gender_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigA1", ".png"), plot = plot_gender_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigA1", ".jpg"), plot = plot_gender_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm", dpi = 100)

# outcome trajectories
ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigA2", ".pdf"), plot = plot_outcome_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigA2", ".png"), plot = plot_outcome_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigA2", ".jpg"), plot = plot_outcome_trajectories, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm", dpi = 100)

# percentages and sum trajectories
ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigS1", ".pdf"), plot = plot_perc_sum_trajectories, width = 2 * 105, height = 1.7 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigS1", ".png"), plot = plot_perc_sum_trajectories, width = 2 * 105, height = 1.7 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigS1", ".jpg"), plot = plot_perc_sum_trajectories, width = 2 * 105, height = 1.7 * 74.25, units = "mm", dpi = 100)

# time x gender
ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigA3", ".pdf"), plot = plot_time_gender, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigA3", ".png"), plot = plot_time_gender, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigA3", ".jpg"), plot = plot_time_gender, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm", dpi = 100)

# tme x outcome
ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigA4", ".pdf"), plot = plot_time_outcome, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigA4", ".png"), plot = plot_time_outcome, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm")
ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigA4", ".jpg"), plot = plot_time_outcome, width = 2.5 * 105, height = 2.5 * 74.25, units = "mm", dpi = 100)

# uncomment the code below to save plots of residuals

#save_plot_residuals <- base::lapply(1:base::length(plot_residuals), function(.x) {
#  p <- plot_residuals[[.x]]
#  title <- analytes_labels_long[base::names(plot_residuals)[.x]]
#  ggplot2::ggsave(filename = base::paste0("./outputs/figures/pdf/", "FigS3_Model_Residuals_", title, ".pdf"), plot = p, width = 2 * 105, height = 1.5 * 74.25, units = "mm")
#  ggplot2::ggsave(filename = base::paste0("./outputs/figures/png/", "FigS3_Model_Residuals_", title, ".png"), plot = p, width = 2 * 105, height = 1.5 * 74.25, units = "mm")
#  ggplot2::ggsave(filename = base::paste0("./outputs/figures/jpg_low-quality/", "FigS3_Model_Residuals_", title, ".jpg"), plot = p, width = 2 * 105, height = 1.5 * 74.25, units = "mm", dpi = 100)
#})

## Tables -----------------------------------------------------------------------------------------------------

# data description (csv)
utils::write.table(table_desc_data, file = "./outputs/tables/csv/TabS1.csv", sep = ";", quote = FALSE, row.names = FALSE)

# LMM estimates (csv)
save_tables_csv <- base::lapply(1:base::length(models_boot_ci_adj), function(.x) {
  title <- analytes_labels_long[base::names(models_boot_ci_adj)[.x]]
  
  results <- models_boot_ci_adj[[.x]]
  results[base::c("sigma.u", "sigma.e", "lambda"), 2:3] <- NA
  results[base::c("lambda"), 1] <- results["sigma.u", 1] / (results["sigma.u", 1] + results["sigma.e", 1])
  
  base::row.names(results) <- base::gsub("splines::bs\\(midpoint_interval\\)1", "Time [1st term]", base::row.names(results))
  base::row.names(results) <- base::gsub("splines::bs\\(midpoint_interval\\)2", "Time [2nd term]", base::row.names(results))
  base::row.names(results) <- base::gsub("splines::bs\\(midpoint_interval\\)3", "Time [3rd term]", base::row.names(results))
  base::row.names(results) <- base::gsub("genderM", "Gender [Male]", base::row.names(results))
  base::row.names(results) <- base::gsub("de_desfechoDead", "Outcome [Dead]", base::row.names(results))
  base::row.names(results) <- base::gsub("sigma.e", "sigma2", base::row.names(results))
  base::row.names(results) <- base::gsub("sigma.u", "tau00id_patient", base::row.names(results))
  base::row.names(results) <- base::gsub("lambda", "ICC", base::row.names(results))
  
  N <- base::matrix(base::c(base::length(base::unique(sub_hsl$id_paciente)), base::nrow(sub_hsl), base::rep(NA, 4)), ncol = 3)
  base::rownames(N) <- base::c("Nid_patient", "Observations")
  base::colnames(N) <- base::colnames(results)
  results <- base::rbind(results, N)
  
  base::colnames(results) <- base::c("Estimates", "Lower Limit Bootstrap CI", "Upper Limit Bootstrap CI")
  
  utils::write.table(base::round(results, 2), file = base::paste0("./outputs/tables/csv/", "TabS2_", title, ".csv"), sep = ";", quote = FALSE, row.names = TRUE, col.names = TRUE)
})

# data description (html)
utils::write.table(htmlTable::htmlTable(table_desc_data, rnames = FALSE), file = "./outputs/tables/html/TabS1.html", sep = ";", quote = FALSE, row.names = FALSE, col.names = FALSE)

# LMM estimates (html)
save_tables_html <- base::lapply(1:base::length(analytes_fit), function(.x) {
  title <- analytes_labels_long[base::names(analytes_fit)[.x]]
  utils::write.table(models_tables[[.x]]$page.complete, file = base::paste0("./outputs/tables/html/", "TabS2_", title, ".html"), quote = FALSE, row.names = FALSE, col.names = FALSE)
})


## RData ------------------------------------------------------------------------------------------------------

# uncomment the code below to save the image of all R objects create through the script

#base::save.image('script_objects.RData')