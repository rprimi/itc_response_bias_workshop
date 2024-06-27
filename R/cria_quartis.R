cria_quartis <- function(
  x, 
  probs = c(0, .25, .50, .75, 1),
  labels = c("<P25", "P25-P50", "P50-P75", ">P75")
  ) {
  q <- quantile(x, probs = probs, na.rm = TRUE )
  x <-  cut(x, q, ordered_result =FALSE, 
            include.lowest = TRUE,  
            na.rm = TRUE,
            labels = labels )
    }