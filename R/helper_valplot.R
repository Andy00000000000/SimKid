#' Helper function for `validate_kid()` to create validation plots for the 
#' virtual population
#'
#' @param data Input data frame matching `sim_kid()` output.
#' @param ped0 A manipulated version of the `internal_kid0` data frame.
#' @param age0to2yr_chart `"CDC"` or `"WHO"` or `"FENTON"` matching that used 
#' during the `sim_kid()` call.
#' @param x String of length one specifying the x-axis variable.
#' @param y String of length one specifying the y-axis variable.
#' @param overlay_percentile `NULL` (default) for no ribbon overlay of 
#' simulated percentiles. Or a numeric greater than `0` and less than `1` 
#' specifying the simulated percentile interval to overlay. For example, 
#' input of `0.90` would overlay the 5th and 95th percentiles of simulated data.
#' @param alpha Numeric between `0` and `1` specifying the simulated data 
#' transparency in validation plots. Default of `0.4`.
#'
#' @return A list of 5 'ggplot2' plot objects.
#'
#' @noRd
helper_valplot <- function(
    data = NULL, ped0 = NULL, age0to2yr_chart = NULL, 
    x = NULL, y = NULL, overlay_percentile = NULL, alpha = 0.4
){
  
  pt_alpha <- alpha
  rb_alpha <- alpha
  ln_alpha <- 0.7
  
  pt_color <- "lightblue"
  ln_color <- "purple"
  
  tmp_ped <- ped0 %>%
    dplyr::mutate(SEXF = ifelse(.data$SEXF==0, "Male", "Female"))%>%
    dplyr::filter(.data$VAR == y)
  
  tmp_ped$X <- tmp_ped[,x]
  
  tmp_demo <- data %>%
    dplyr::mutate(SEXF = ifelse(.data$SEXF==0, "Male", "Female"))
  
  tmp_demo$X <- tmp_demo[,x]
  tmp_demo$Y <- tmp_demo[,y]
  
  xlab <- ifelse(
    x == "AGEMO", 
    "Age (months)", 
    ifelse(
      x == "HTCM", 
      "Height (cm)",
      ifelse(
        x == "GAWK", 
        "Gestational Age (weeks)", "ERROR"
      )))
  
  ylab <- ifelse(
    y == "HTCM", 
    "Height (cm)", 
    ifelse(
      y == "WTKG", 
      "Weight (kg)", 
      ifelse(
        y == "BMI", 
        "BMI (kg/m\u00b2)", 
        "ERROR"
      )))
  
  title <- ifelse(
    age0to2yr_chart != "FENTON", 
    paste0(
      age0to2yr_chart,
      " Growth for Ages 0 to 2 years and CDC Growth for Ages 2 to 20 years"
    ), 
    "Fenton Growth for Preterm Infants at Birth"
  )
  
  caption <- ifelse(
    age0to2yr_chart != "FENTON", 
    paste0(
      "Note: 3rd, 5th, 10th, 25th, 50th, 75th, 90th, 95th, 97th percentiles of",
      " growth charts (black lines)\n     are overlaid with the individual ",
      "virtual subjects (",pt_color," points)"),
    paste0(
      "Note: 3rd, 10th, 50th, 90th, 97th percentiles of growth charts ",
      "(black lines)\n     are overlaid with the individual virtual subjects (",
      pt_color," points)")
  )
  
  if(age0to2yr_chart != "FENTON"){
    xbreaks <- c(seq(0,24,6),seq(24,240,12))
  }else{
    xbreaks <- seq(22,40,2)
  }
  
  if(length(unique(tmp_demo$ID)) == nrow(tmp_demo)){
    
    # Raw simulated virtual subject data for sim_kid()
    tmpp <- ggplot2::ggplot()+
      ggplot2::geom_point(
        ggplot2::aes(x = .data$X, y = .data$Y),
        tmp_demo, alpha = pt_alpha, color = pt_color
      )+
      ggplot2::facet_wrap(~SEXF)
    
  }else{
    
    # Raw simulated virtual subject data for grow_kid() following sim_kid()
    tmpp <- ggplot2::ggplot()+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$Y, group = as.factor(.data$ID)), 
        tmp_demo, alpha = pt_alpha, color = pt_color
      )+
      ggplot2::facet_wrap(~SEXF)
  }
  
  if(!is.na(overlay_percentile) == TRUE){
    
    perc_lo <- (1-overlay_percentile)/2
    perc_hi <- 1 - perc_lo
    
    caption <- paste0(
      caption, 
      " and the median and ",
      paste0(100*c(perc_lo,perc_hi), collapse = " and "),
      " percentiles (",ln_color," line and ribbon)"
    )
    
    tmp_summ <- tmp_demo %>%
      dplyr::filter(!is.na(.data$Y))%>%
      dplyr::group_by(.data$SEXF, .data$X)%>%
      dplyr::summarise(
        PLO = stats::quantile(.data$Y, probs = perc_lo),
        PHI = stats::quantile(.data$Y, probs = perc_hi),
        P50 = stats::quantile(.data$Y, probs = 0.50)
      )
    
    tmpp <- tmpp+ # Summary stats of simulated virtual subject data
      ggplot2::geom_ribbon(
        ggplot2::aes(x = .data$X, ymin = .data$PLO, ymax = .data$PHI), 
        tmp_summ, alpha = rb_alpha, color = NA, fill = ln_color
      )+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$P50), 
        tmp_summ, alpha = ln_alpha, color = ln_color, linewidth = 1
      )
  }
  
  tmpp <- tmpp+ # Anthropometric growth chart percentile data
    ggplot2::geom_line(
      ggplot2::aes(x = .data$X, y = .data$P3, group = as.factor(.data$SEXF)),
      tmp_ped, alpha = 0.7, color = "black", linetype = 3
    )+
    ggplot2::geom_line(
      ggplot2::aes(x = .data$X, y = .data$P10, group = as.factor(.data$SEXF)),
      tmp_ped, alpha = 0.7, color = "black", linetype = 3
    )+
    ggplot2::geom_line(
      ggplot2::aes(x = .data$X, y = .data$P50, group = as.factor(.data$SEXF)),
      tmp_ped, alpha = 0.7, color = "black", linetype = 1
    )+
    ggplot2::geom_line(
      ggplot2::aes(x = .data$X, y = .data$P90, group = as.factor(.data$SEXF)),
      tmp_ped, alpha = 0.7, color = "black", linetype = 3
    )+
    ggplot2::geom_line(
      ggplot2::aes(x = .data$X, y = .data$P97, group = as.factor(.data$SEXF)),
      tmp_ped, alpha = 0.7, color = "black", linetype = 3
    )
  
  if(age0to2yr_chart != "FENTON"){
    # Anthropometric growth chart percentile data missing for FENTON
    tmpp <- tmpp+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$P5, group = as.factor(.data$SEXF)),
        tmp_ped, alpha = 0.7, color = "black", linetype = 3
      )+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$P25, group = as.factor(.data$SEXF)),
        tmp_ped, alpha = 0.7, color = "black", linetype = 3
      )+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$P75, group = as.factor(.data$SEXF)),
        tmp_ped, alpha = 0.7, color = "black", linetype = 3
      )+
      ggplot2::geom_line(
        ggplot2::aes(x = .data$X, y = .data$P95, group = as.factor(.data$SEXF)),
        tmp_ped, alpha = 0.7, color = "black", linetype = 3
      )
  }
  
  tmpp <- tmpp+ # aesthetics and labels
    ggplot2::labs(
      x = xlab,
      y = ylab,
      title = title,
      caption = paste0(caption,".")
    )+
    ggplot2::scale_x_continuous(breaks = xbreaks)+
    ggplot2::theme_bw()+
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust=0)
    )
  
  tmpp
}