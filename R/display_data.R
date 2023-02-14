std_table_create = function(tab_data){
  tab_df = tab_data %>%
    select(element, std_data)%>%
    unnest(std_data) %>%
    mutate(
      hour  = hour(date_time),
      min = minute(date_time),
      SampleID = paste0(SampleID, " (", hour, ":", min, ")"),
      Value = as.character(Value)
    ) %>%
    select(-date_time, -std_type, -hour, -min) %>%
    pivot_longer(
      -one_of(c("element", "SampleID")),
      names_to = "measure",
      values_to = "temp"
    ) %>%
    unite(c_name, SampleID, measure, sep = ";") %>%
    pivot_wider(names_from = c_name, values_from = temp) %>%
    ungroup()
  
  c_names = colnames(tab_df)[str_detect(colnames(tab_df), "QC Check")]
  
  cell_coloring = function(col, compare){
    cells_body(
      columns = !!sym(col),
      rows = str_detect(!!sym(col), compare)
    )
  }
  
  gt(tab_df, rowname_col = "element") %>%
    tab_style(
      style = list(cell_fill(color = '#FDE725FF'), cell_text(weight = 'bold')),
      locations = lapply(c_names, cell_coloring, compare = "Warning")
    ) %>%
    tab_style(
      style = list(cell_fill(color = '#F2637F'), cell_text(weight = 'bold')),
      locations = lapply(c_names, cell_coloring, compare = "Action")
    ) %>%
    tab_spanner_delim(delim = ";") %>%
    return()
}

std_graph_format = function(std_data, h_lines, tar_ele,  date_range, icp_method, method_std){
  ## The function graphs takes the filtered data and a y variable to plot
  ## Paramters: data (tibble) and y variable (string)
  ## Returns: ggplot
  low_line <- filter(h_lines, element == tar_ele) %>% pull(warn_low)
  high_line <- filter(h_lines, element == tar_ele) %>% pull(warn_high)
  std_data %>%
    select(-std_type) %>%
    pivot_longer(-date_time, names_to = "element", values_to = "value") %>%
    filter(element == tar_ele) %>%
    ggplot(aes(x = date_time, y = value)) +
    geom_point(size = 3) +
    geom_hline(aes(yintercept = low_line)) +
    geom_hline(aes(yintercept = high_line)) +
    theme_cowplot(16) +
    xlim(c(date_range[1], date_range[2])) +
    labs(
      x = "Time", y = tar_ele, title = icp_method, subtitle = method_std
    ) +
    panel_border() +
    background_grid() %>%
    return()
}

sam_graph_format = function(g_data, tar_ele){
  ## The function graphs takes the filtered data and a y variable to plot
  ## Paramters: data (tibble) and y variable (string)
  ## Returns: ggplot
  g_data %>%
    select(element, data) %>%
    filter(element == tar_ele) %>%
    unnest(data) %>%
    ggplot(aes(x = date_time, y = value, color = flags)) +
    geom_point(size = 2.5) +
    scale_color_manual(
      breaks = c("Normal", "Over Cal", "Under Cal"),
      values = c("black", "#D23105FF", "#466BE3FF")
    ) +
    theme_cowplot(12) +
    labs(
      x = "Time",
      y = tar_ele,
      title = paste0("Samples for ", tar_ele, " with blank value line"),
    ) %>%
    return()
}

