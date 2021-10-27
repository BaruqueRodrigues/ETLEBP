map(2013:2020, ~ anp_2015%>%mutate(!!sym(paste0("gasto_", .x)) := case_when(
  year(data_inicio) == .x & year(prazo_utilizacao) == .x ~ time_length(prazo_utilizacao - data_inicio, "days"),
  year(data_inicio) == .x & year(prazo_utilizacao)  > .x ~ time_length(ymd(paste(.x, "-12-31")) - data_inicio,  "days"),
  year(data_inicio)  < .x & year(prazo_utilizacao) == .x  ~ time_length(prazo_utilizacao - ymd(paste(.x, "-01-01")),  "days"),
  year(data_inicio)  < .x & year(prazo_utilizacao) > .x  ~ time_length(ymd(paste(.x, "-12-31")) - ymd(paste(.x, "-01-01")),  "days"),
  year(data_inicio)  > .x ~ 0
) ))%>%bind_cols()

