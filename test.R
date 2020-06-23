


test <- data_jobless_claims3 %>%
  filter(rptdate > Sys.Date() - (365*3))%>%
  group_by(state_group, rptdate)%>%
  summarise(
    jobless = sum(jobless_claims),
    continuing_claims = sum(continuing_claims))



test <- data_jobless_claims3 %>%
  filter(country == 1)%>%
  filter(rptdate > Sys.Date() - 365)


test2 <- test%>%
  select(rptdate, jobless_claims, continuing_claims, initial_claims_relative_pop, continuing_claims_relative_pop)%>%
  gather(variable, value, initial_claims_relative_pop:continuing_claims_relative_pop)

p1 <- ggplot() + geom_line(aes(y = value, x = rptdate, colour = variable),
                           data = test2, stat="identity")

p1
