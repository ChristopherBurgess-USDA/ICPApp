icp_load = function(data_path){
  
  temp_data = tryCatch(
    read_csv(data_path, skip = 2, skip_empty_rows = T) %>%
      filter(
        !str_detect(`Solution Label`, "Solution Label|Worksheet.*|Continuing.*")
      ) %>%
      select(
        "SampleID" = "Solution Label", Type, "date_time" = "Date Time",
        contains("ppm")
      ) %>%
      mutate(across(contains("ppm"), as.character)),
    error = function(cnd){
      read_tsv(data_path, skip = 2, skip_empty_rows = T) %>%
        filter(
          !str_detect(`Solution Label`, "Solution Label|Worksheet.*|Continuing.*")
        ) %>%
        select(
          "SampleID" = "Solution Label", Type, "date_time" = "Date Time",
          contains("ppm")
        )%>%
        mutate(across(contains("ppm"), as.character))
    }
  )
  
  temp_data = tryCatch(
    mutate(temp_data, date_time = parse_date_time(date_time, "mdYHM", tz = "US/Pacific")),
    warning = function(cnd){
      mutate(temp_data, date_time = parse_date_time(date_time, "mdYHMSOp", tz = "US/Pacific"))
    }
  )
  
  std_ts = filter(temp_data, Type == "Blank") %>%
    pull(date_time) %>%
    max()  
  
  temp_data %>%
    filter(date_time >= std_ts, Type == "Sample") %>%
    pivot_longer(
      -c(SampleID, Type, date_time),
      names_to = "element", values_to = "value"
    ) %>%
    mutate(
      flags = str_extract(value, "[ou]"),
      flags = case_when(
        flags == "o" ~ "Over Cal",
        flags == "u" ~ "Under Cal",
        T ~ "Normal"
      ),
      value = str_extract(value, "[:digit:]+\\.?[:digit:]*"),
      value = as.numeric(value)
    ) %>%
    nest_by(element, .key = "raw_data") %>%
    mutate(
      blank = filter(raw_data, SampleID %in% c("Blank", "Method Blank")) %>% pull(value) %>% mean()
    ) %>%
    return()
}



