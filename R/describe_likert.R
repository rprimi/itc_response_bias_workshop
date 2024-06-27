describe_likert <- function(
  data,                        # raw data on items
  coditem,                     # item unique code
  item_text,                   # text label of items
  pole,                        # item polarity +:1  -:0
  item_location,                  # thurstone treshold
  item_text_max = 28 ,         # size of item label
  center = 3,                   # neutral category
  categ_levels = c("1", "2",  "3", "4", "5"),
  categ_labels = c("nada", "pouco", "moderad.", "muit.", "totalm."),
  low.color = "#DF4949",
  neutral.color = "#EEE657",
  high.color = "#2CCA90"

)  {

 library(likert)
 library(RColorBrewer)
 library(purrr)



 dic <- tibble(
  coditem,
  item_text,
  pole,
  b = item_location
 )



 # Create summary
 data <- map_df(data, factor, levels = 1:5)
 names(data) <- dic$item_text

 table_summary = likert(as.data.frame(data), nlevels = length(categ_levels))

 table_summary$results$Item <-
  factor(table_summary$results$Item, levels =  dic$item_text[order(dic$b)])

 plot(table_summary, centered=TRUE, center=center,
      include.center=TRUE,
      wrap=item_text_max,
      low.color = low.color,
      neutral.color = neutral.color,
      high.color = high.color,
      ordered = FALSE)  +  theme(legend.position="top") +
  theme_bw()

}
