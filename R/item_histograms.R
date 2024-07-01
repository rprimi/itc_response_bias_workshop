item_histograms <- function(df, coditems, scales, poles, r_levels){
 
 dic <- dplyr::tibble(
  coditem = coditems,
  scale = scales,
  pole = poles
 )
 
 df %>% 
  dplyr::select(all_of(coditems)) %>%
  tidyr::pivot_longer(cols = all_of(coditems), names_to = "coditem", values_to ="r") %>% 
  dplyr::left_join(dic, by="coditem") %>%
  dplyr::filter(!is.na(r)) %>%
  dplyr::group_by(scale, pole, r) %>% 
  tally(name = "freq") %>%
  dplyr::mutate(
   pole = factor(pole),
   tot = sum(freq),
   prop = freq/tot,
   r = factor(r, levels = r_levels)
  ) %>%  
  ggplot(aes(y=prop, x=r, fill = scale)) +
  geom_histogram(stat= "identity", alpha=.5, color ="darkgray") +
  scale_fill_brewer(palette = "Spectral") +
  facet_grid(pole~scale) +
  theme_minimal()
}

