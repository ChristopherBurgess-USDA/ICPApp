std_lookup = function(std_data, target_ele, in_type, in_data){
  std_data = filter(std_data, element == target_ele, std_type == in_type)
  mutate(in_data, qc_check = case_when(
    value >= pull(std_data, act_high) ~ "Action High",
    value <= pull(std_data, act_low) ~ "Action Low",
    value >= pull(std_data, warn_high) ~ "Warning High",
    value <= pull(std_data, warn_low) ~ "Warning Low",
    T ~ "Normal"
  )
  ) %>%
    return()
}

check_std = function(target_ele, input_data, std_data){
  filter(input_data, str_detect(SampleID, "CAL|CTFS")) %>%
    mutate(std_type = if_else(str_detect(SampleID, "CTFS"), "CTFS", "CAL")) %>%
    nest_by(std_type) %>%
    mutate(data = list(std_lookup(std_data, target_ele, std_type, data))) %>%
    unnest(data) %>%
    select(
      SampleID, date_time,
      `STD Type` = std_type,
      Value = value,
      `QC Check` = qc_check
    )%>%
    return()
}

sub_blank = function(data, blank){
  mutate(data, value = value - !!blank) %>%
    return()
}

parsing_data = function(input_data, blank, std_data){
  if(blank == 1){
    input_data %>%
      mutate(
        data = list(sub_blank(raw_data, blank)),
        std_data = list(check_std(element, data, std_data)),
        data = list(filter(data, !str_detect(SampleID, "CAL|Blank|CTFS")))
      ) %>%
      return()
  } else{
    input_data %>%
      mutate(
        data = list(raw_data),
        std_data = list(check_std(element, data, std_data)),
        data = list(filter(data, !str_detect(SampleID, "CAL|Blank|CTFS")))
      ) %>%
      return()
  }
}