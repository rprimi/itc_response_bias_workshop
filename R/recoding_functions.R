
# Main recoding fucntion ------------------------------------------------------


# Run psychometric analysis ------------------------------------------------------

find_psychometrics <- function(obj, likert = 5, center = 0) {

  # browser()
  # Reverse negative items and recenter
  # Code above changes order of items !!!

  unique_items <- obj$item_dic %>%
    dplyr::select(coditem, pole) %>%
    dplyr::group_by(coditem, pole) %>%
    dplyr::count()

  unique_items <- unique_items[match(unique(obj$item_dic$coditem), unique_items$coditem), ]


  obj$data_acq_recoded <- obj$data_acq_recoded %>%
    dplyr::mutate_if(unique_items$pole == 0, funs((-1 * .) + center))

  obj$data_acq_recoded <- obj$data_acq_recoded %>%
    dplyr::mutate_if(unique_items$pole == 1, funs((1 * .) + center))

  obj$data <- obj$data %>%
    dplyr::mutate_if(unique_items$pole == 0, funs(likert + 1 - .))



  # make keys
  keys <- dic2keys(obj$item_dic)

  # Calculate psychometrics and scores for centered (c) and original
  # negative items are inverted

  # original
  min <- apply(obj$data[, rownames(keys)], 2, function(x) {
    min(x, na.rm = TRUE)
  })
  max <- apply(obj$data[, rownames(keys)], 2, function(x) {
    max(x, na.rm = TRUE)
  })

  psicom_orig <- scoreItems(
    keys = keys,
    # unique is used in case we have more scales with same items
    items = obj$data[, rownames(keys)],
    missing = TRUE, impute = "none",
    min = min,
    max = max,
    digits = 3
  )

  # Recoded
  min <- apply(obj$data_acq_recoded[, rownames(keys)], 2, function(x) {
    min(x, na.rm = TRUE)
  })
  max <- apply(obj$data_acq_recoded[, rownames(keys)], 2, function(x) {
    max(x, na.rm = TRUE)
  })

  psicom_recoded <- scoreItems(
    keys = keys,
    # unique is used in case we have more scales with same items
    items = obj$data_acq_recoded[, rownames(keys)],
    missing = TRUE,
    impute = "none"
  )


  dimnames(psicom_orig$scores)[[2]] <- paste(dimnames(psicom_orig$scores)[[2]], "ori", sep = "_")
  dimnames(psicom_recoded$scores)[[2]] <- paste(dimnames(psicom_recoded$scores)[[2]], "rec", sep = "_")

  # Calculate psychometrics via alpha function to get r.drop



  # Original
  alpha_orig <- obj$item_dic %>%
    dplyr::mutate(pole2 = ifelse(pole == 0, -1, 1)) %>%
    dplyr::select(coditem, scale, pole2) %>%
    group_by(scale) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      vars = purrr::map(data, "coditem"),
      keys = purrr::map(data, "pole2")
    ) %>%
    dplyr::mutate(
      alfa = purrr::map(vars, ~ psych::alpha(x = obj$data[, .x]))
    ) %>%
    dplyr::mutate(
      scale_stat = map(alfa, "total"),
      item_stats = map(alfa, "item.stats")
    )

  alpha_orig_scale_stat <- alpha_orig %>%
    dplyr::select(scale, scale_stat) %>%
    tidyr::unnest_wider(scale_stat) %>%
    dplyr::ungroup()

  alpha_orig_item_stat <- alpha_orig %>%
    dplyr::select(scale, vars, item_stats) %>%
    tidyr::unnest(cols = c(vars, item_stats)) %>%
    dplyr::ungroup()

  # Recoded
  alpha_rec <- obj$item_dic %>%
    dplyr::mutate(pole2 = ifelse(pole == 0, -1, 1)) %>%
    dplyr::select(coditem, scale, pole2) %>%
    group_by(scale) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      vars = purrr::map(data, "coditem"),
      keys = purrr::map(data, "pole2")
    ) %>%
    dplyr::mutate(
      alfa = purrr::map(vars, ~ psych::alpha(x = obj$data_acq_recoded[, .x]))
    ) %>%
    dplyr::mutate(
      scale_stat = map(alfa, "total"),
      item_stats = map(alfa, "item.stats")
    )

  alpha_rec_scale_stat <- alpha_rec %>%
    dplyr::select(scale, scale_stat) %>%
    tidyr::unnest_wider(scale_stat) %>%
    dplyr::ungroup()

  alpha_rec_item_stat <- alpha_rec %>%
    dplyr::select(scale, vars, item_stats) %>%
    tidyr::unnest(cols = c(vars, item_stats)) %>%
    dplyr::ungroup()


  # Dataframe
  scores <- NULL
  scores <- as.data.frame(
    cbind(
      psicom_orig$scores,
      psicom_recoded$scores,
      obj$acq_index
    )
  )

  # Returns original and recoded scores
  results <- list(
    scores = scores,
    psicom_orig = psicom_orig,
    psicom_recoded = psicom_recoded,
    alpha_orig_scale_stat = alpha_orig_scale_stat,
    alpha_orig_item_stat = alpha_orig_item_stat,
    alpha_rec_scale_stat = alpha_rec_scale_stat,
    alpha_rec_item_stat = alpha_rec_item_stat,
    item_dic = obj$item_dic,
    keys = keys
  )

  # return object

  return(results)
}

# Save item psychometrics ---------------------------------------------------

save_item_psicom <- function(obj, filename) {

  # browser()

  library(writexl)
  library(dplyr)

  item_cor_o <- as.data.frame(round(obj$psicom_ori$item.corrected, digits = 3))
  item_cor_r <- as.data.frame(round(obj$psicom_recoded$item.corrected, digits = 3))

  # por causa de um pau no psych
  item_stats_o <- as.data.frame(round(obj$psicom_ori$item.corrected, digits = 3))
  item_stats_r <- as.data.frame(round(obj$psicom_recoded$item.corrected, digits = 3))


  names(item_stats_o) <- paste(names(item_stats_o), "ori", sep = "_")
  names(item_stats_r) <- paste(names(item_stats_r), "rec", sep = "_")

  item_stats_o$coditem <- rownames(item_stats_o)
  item_stats_r$coditem <- rownames(item_stats_r)

  obj$alpha_orig_item_stat <- obj$alpha_orig_item_stat %>%
    purrr::set_names(paste0, "_ori")

  obj$alpha_rec_item_stat <- obj$alpha_rec_item_stat %>%
    purrr::set_names(paste0, "_rec")

  item_stats <- obj$item_dic %>%
    dplyr::left_join(item_stats_o, by = "coditem") %>%
    dplyr::left_join(obj$alpha_orig_item_stat, by = c("coditem" = "vars_ori", "scale" = "scale_ori"))



  item_stats <- item_stats %>%
    dplyr::left_join(item_stats_r, by = "coditem") %>%
    dplyr::left_join(obj$alpha_rec_item_stat, by = c("coditem" = "vars_rec", "scale" = "scale_rec"))


  if (!is.null(obj$psicom_ori$response.freq)) {
    resp_frq <- as.data.frame(
      round(obj$psicom_ori$response.freq, digits = 3)
    ) %>%
      mutate(coditem = rownames(.))
    item_stats <- item_stats %>%
      left_join(resp_frq, by = "coditem")
  }



  scale_stats_ori <- rbind(
    round(obj$psicom_ori$alpha, digits = 3),
    obj$psicom_ori$n.items,
    round(obj$psicom_ori$G6, digits = 3),
    t(psych::describe(obj$psicom_ori$scores))
  )

  scale_stats_rec <- rbind(
    round(obj$psicom_recoded$alpha, digits = 3),
    obj$psicom_recoded$n.items,
    round(obj$psicom_recoded$G6, digits = 3),
    t(psych::describe(obj$psicom_recoded$scores))
  )


  scale_cor_o <- round(obj$psicom_orig$cor, digits = 3)
  scale_cor_r <- round(obj$psicom_recoded$cor, digits = 3)

  write_xlsx(
    x = list(
      item_stats = as.data.frame(item_stats),
      scale_stats_ori = as.data.frame(scale_stats_ori),
      scale_stats_rec = as.data.frame(scale_stats_rec),
      scale_cor_o = as.data.frame(scale_cor_o),
      scale_cor_r = as.data.frame(scale_cor_r),
      alpha_ori = obj$alpha_orig_scale_stat,
      alpha_rec = obj$alpha_rec_scale_stat
    ),
    path = filename
  )


  # write.xlsx( item_stats, filename, sheetName="item_stats",
  #    col.names=TRUE, row.names=TRUE, append=TRUE, showNA=FALSE)

  # write.xlsx( scale_stats_ori, filename, sheetName="scale_stats_ori",
  #    col.names=TRUE, row.names=TRUE, append=TRUE, showNA=FALSE)
  # write.xlsx( scale_stats_rec, filename, sheetName="scale_stats_rec",
  #    col.names=TRUE, row.names=TRUE, append=TRUE, showNA=FALSE)

  #  write.xlsx( scale_cor_o, filename, sheetName="scale_cor_ori",
  #    col.names=TRUE, row.names=TRUE, append=TRUE, showNA=FALSE)
  #  write.xlsx( scale_cor_r, filename, sheetName="scale_cor_rec",
  #    col.names=TRUE, row.names=TRUE, append=TRUE, showNA=FALSE)
}



# Make keys -------------------------------------------------------

dic2keys <- function(item_dic, reversed = TRUE) {

  # Assume que os itens ja foram invertidos
  item_dic$order <- 1:dim(item_dic)[1] # cria order
  item_dic$um <- 1

  if (reversed) {
    item_dic$order2 <- item_dic$order
  } else {
    item_dic$order2 <- ifelse(item_dic$pole == 1, item_dic$order, item_dic$order * -1)
  }


  keys.list <- plyr::dlply(item_dic[, c("scale", "order2")], .(scale))
  keys.list <- lapply(keys.list, `[`, 2)

  keys <- psych::make.keys(
    nvars = dim(item_dic)[1],
    keys.list = keys.list,
    item.labels = item_dic$coditem
  )
  return(as.matrix(keys[, unique(as.character(item_dic$scale))]))
}
