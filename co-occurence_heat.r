data_dong_sankey$left = factor(data_dong_sankey$left)
data_dong_sankey$right = factor(data_dong_sankey$right)

data_dong_sankey %>% select(-Year)

data_dong_pivot_table1 <- data_dong_sankey %>% dplyr::count(left, right) %>% pivot_wider(names_from = left, values_from = n,values_fill = 0) %>% ungroup()


data_long_sankey_summary <- data_dong_pivot_table1 %>% 
  pivot_longer(
    cols = `Four Wheeler`:`Two Wheeler`,
    names_to = "left",
    values_to = "value"
    
  )
data_long_sankey_summary %>% ggplot(aes(right, left, fill = value)) + geom_tile() + geom_label(aes(label = value), fill = "white")+ scale_colour_viridis_d() +  theme_minimal() + labs(title = "Gist of Accident data")
