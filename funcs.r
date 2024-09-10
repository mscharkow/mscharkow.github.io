library(tidyverse)
library(xaringanExtra)

make_schedule = function(start_date, end_date, no_classes, topics){
  classes = seq.Date(from = start_date, to = end_date, by = "1 week") %>%
    discard(~.x %in% no_classes)

  tibble(Datum = classes) |>
    head(length(topics)) |>
    mutate(Thema = topics) |>
    rownames_to_column("Sitzung") |>
    mutate(Thema = paste0("<a href='#", Sitzung, "'>", topics, "<a/>"))
}