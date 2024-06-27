score_tests_v3 <- function(
  df,
  item_dic,
  scr_tot = FALSE,
  filename = "item.stats.xlsx",
  save_item_stat = FALSE,
  reversed = FALSE,
  score_fast = FALSE
) {
 # Check for required packages and attempt to load them
 required_packages <- c("plyr", "psych", "writexl", "dplyr", "purrr", "tidyr")
 
 lapply(required_packages, function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
   stop(paste("Package", package, "is required but not installed."))
  }
 })
 
 # Attempt to make keys
 tryCatch({
  keys <- dic2keys(item_dic, reversed)
 }, error = function(e) {
  stop("Failed to create keys from item dictionary: ", e$message)
 })
 
 # Validate input data frame columns against keys
 if (!all(rownames(keys) %in% colnames(df))) {
  stop("Not all items from keys are present in the data frame columns.")
 }
 
 # Scoring process
 tryCatch({
  min_max <- function(x) c(min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
  min <- apply(df[, rownames(keys)], 2, min_max)
  max <- apply(df[, rownames(keys)], 2, min_max)
  
  scoring_function <- if (score_fast) scoreFast else scoreItems
  
  obj <- scoring_function(
   keys = keys,
   items = df[, rownames(keys)],
   totals = scr_tot,
   missing = TRUE,
   impute = "none",
   delete = TRUE,
   min = min,
   max = max,
   digits = ifelse(score_fast, 2, 3)
  )
 }, error = function(e) {
  stop("Failed during the scoring process: ", e$message)
 })
 
 # Calculate psychometrics via alpha function to get r.drop
 tryCatch({
  alpha_orig <- item_dic %>%
   mutate(pole2 = ifelse(pole == 0, -1, 1)) %>%
   select(coditem, scale, pole2) %>%
   group_by(scale) %>%
   nest() %>%
   mutate(
    vars = map(data, "coditem"),
    keys = map(data, "pole2"),
    alfa = map(vars, ~psych::alpha(x = df[, .x]))
   ) %>%
   mutate(
    scale_stat = map(alfa, "total"),
    item_stats = map(alfa, "item.stats")
   )
  
  alpha_orig_scale_stat <- ungroup(select(alpha_orig, scale, scale_stat) %>% unnest_wider(scale_stat))
  
  alpha_orig_item_stat <- ungroup(select(alpha_orig, scale, vars, item_stats) %>% unnest(cols = c(vars, item_stats)))
  
 }, error = function(e) {
  stop("Failed during psychometric calculations: ", e$message)
 })
 
 # Save item stats
 if (save_item_stat) {
  tryCatch({
   list_of_dfs <- list(
    "item_stats" = item_stats, 
    "scale_stats" = scale_stats, 
    "scale_cor" = scale_cor, 
    "alpha_scale_stat" = alpha_orig_scale_stat
   )
   
   writexl::write_xlsx(x = list_of_dfs, path = filename)
   
  }, error = function(e) {
   warning("Failed to save item statistics: ", e$message)
  })
 }
 
 # Return results
 return(list(
  psicom = obj,
  keys = keys,
  item_stats = if(save_item_stat) item_stats else NULL,
  scale_stats = if(save_item_stat) scale_stats else NULL,
  alpha = alpha_orig,
  alpha_scale_stat = alpha_orig_scale_stat
 ))
}


dic2keys <- function(item_dic, reversed = TRUE) {
 # Check for required packages
 if (!require(plyr, character.only = TRUE, quietly = TRUE)) {
  stop("Package 'plyr' is required but not installed.")
 }
 
 if (!require(psych, character.only = TRUE, quietly = TRUE)) {
  stop("Package 'psych' is required but not installed.")
 }
 
 # Input validation
 required_columns <- c("scale", "order", "pole", "coditem")
 if (!all(required_columns %in% names(item_dic))) {
  stop("The item dictionary does not contain all required columns: 'scale', 'order', 'pole', 'coditem'.")
 }
 
 # Adding order column if it doesn't exist
 if (!"order" %in% names(item_dic)) {
  item_dic$order <- 1:nrow(item_dic)  # Creates order
 }
 
 # Ensure 'pole' column is present for processing reversed items
 if (!"pole" %in% names(item_dic)) {
  stop("'pole' column is missing from item_dic, which is necessary for processing reversed items.")
 }
 
 item_dic$um <- 1  # Auxiliary column to assist with order calculation
 
 # Adjusting order based on 'reversed' flag and 'pole' values
 item_dic$order2 <- if (reversed) {
  item_dic$order
 } else {
  ifelse(item_dic$pole == 1, item_dic$order, item_dic$order * -1)
 }
 
 # Creating a list of keys split by 'scale'
 tryCatch({
  keys.list <- plyr::dlply(item_dic[, c("scale", "order2")], .(scale))
  keys.list <- lapply(keys.list, `[`, 2)  # Extracting order2 column
  
  # Using psych::make.keys to create the keys matrix
  keys <- psych::make.keys(
   nvars = nrow(item_dic),
   keys.list = keys.list,
   item.labels = item_dic$coditem
  )
 }, error = function(e) {
  stop("Error in creating keys matrix: ", e$message)
 })
 
 # Returning as matrix and ensuring columns match unique 'scale' values
 return(as.matrix(keys[, unique(as.character(item_dic$scale))]))
}
