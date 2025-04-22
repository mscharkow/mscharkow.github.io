library(tidyverse)
library(xaringanExtra)

make_schedule = function(start_date, end_date, no_classes, topics, onlyslides = F){
  classes = seq.Date(from = start_date, to = end_date, by = "1 week") %>%
    discard(~.x %in% no_classes)

  plan = tibble(Datum = classes) |>
    head(length(topics)) |>
    mutate(Thema = topics) |>
    rownames_to_column("Sitzung") |>
    mutate(Thema = paste0("<a href='#", Sitzung, "'>", topics, "<a/>"))

  if(onlyslides==T) {
    plan = plan |>
      mutate(Thema = paste0("<a href='slides.html#", Sitzung, "'>", topics, "<a/>"))
  }

  write_tsv(plan, "plan.tsv")
  plan
}