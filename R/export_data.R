upload_google = function(up_data, method){
  up_data %>%
    select(element, std_data) %>%
    unnest(std_data) %>%
    mutate(Date = lubridate::date(date_time)) %>%
    select(element, SampleID, Date, `STD Type`, Value) %>%
    pivot_wider(names_from = element, values_from = Value) %>%
    select(-SampleID) %>%
    sheet_append(
      "1ON6vDrDc8fmQKMlF--SCl_sA1OCeU5V81YDtvyp8Xc8",
      data = .,
      sheet = method
    )
}

download_data = function(input_data){
  std_temp = select(input_data, element, std_data) %>%
    unnest(std_data) %>%
    mutate(Value = as.character(Value)) %>%
    pivot_longer(
      cols = c("Value", "QC Check"),
      names_to = "type", values_to = "temp"
    ) %>%
    pivot_wider(names_from = element, values_from = temp)
  
  parsed_temp = select(input_data, element, data) %>%
    unnest(data) %>%
    select(-flags) %>%
    pivot_wider(names_from = element, values_from = value)
  
  raw_temp = select(input_data, element, raw_data) %>%
    unnest(raw_data) %>%
    unite(value, value, flags, sep = " ") %>%
    pivot_wider(names_from = element, values_from = value)
  
  list(
    "Standard Evaluation" = std_temp,
    "Parsed Data" = parsed_temp,
    "Raw Data"  = raw_temp
  ) %>%
    return()
}
