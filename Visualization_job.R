

##################################################################################################################################
#                                   ******* USEFUL *******                                                                       #
#                               **FONCTION FOR VISUALIZATION**                                                                   #
##################################################################################################################################



library(ggplot2)     #|
library(highcharter) #| to make visualization
library(plotly)      #|



#' Create a dynamic visualization of indicator 
#'
#'
#' @param country Character. Name of the country.
#' @param segment Character. Name of the segment.
#' @param Indicators Character vector. Columns to visualize.
#' @param data Data frame. The dataset in long format.
#' @param range Numeric vector. Time period to plot, e.g., 1970:2024.
#' @param scale_opt Character. Scale option for plot, default is 'log'.
#'
#' @return A ggplot object or a highcharter plot (depending on implementation)

VizInd <- function(country,
                segment,
                Indicators,   # column's name vector
                data = table,
                range = 1970:2024,
                scale_opt = 'log') {
  
  
            # data filtering
            df_filtered <- data %>% filter(COUNTRY==country, SEGMENT==segment, YEAR %in% range) %>% collect()
            
            # logarithmic scale
            if (scale_opt == "log") {
              df_filtered <- df_filtered %>%
                mutate(across(where(is.numeric) & all_of(Indicators), ~ ifelse(. > 0, log(.), NA)))
            }

            # Graphic creation
            hc <- highchart() %>% hc_boost(enabled = TRUE)
            
            # Dynamic add
            colors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))(length(Indicators))
            
            for(i in seq_along(Indicators)) {
              hc <- hc %>%
                hc_add_series(
                  data = df_filtered,
                  type = 'line',
                  hcaes(x = YEAR, y = !!sym(Indicators[i])),
                  name = Indicators[i],
                  color = colors[i]
                )
            } 
  
            
 
            return(hc)
            
        }








VizCtry <- function(countries,
                 segment,
                 Indicator,   # column's name vector
                 data = table,
                 range = 1970:2024,
                 scale_opt = 'log') {
  
  
  # data filtering
  df_filtered <- data %>% filter(COUNTRY %in% countries, SEGMENT==segment, YEAR %in% range) %>% collect()
  
  # logarithmic scale
  if (scale_opt == "log") {
    df_filtered <- df_filtered %>%
      mutate(across(Indicator, ~ ifelse(. > 0, log(.), NA)))
  }
  
  # Graphic creation
  hc <- highchart() %>% hc_boost(enabled = TRUE)
  
  

  hc <- hc %>%
        hc_add_series(
        data = df_filtered,
        type = 'line',
        hcaes(x = YEAR, y = !!sym(Indicator), group = COUNTRY),

        )
  
  return(hc)
  
}   
