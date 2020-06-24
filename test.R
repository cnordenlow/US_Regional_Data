


test <- data_jobless_claims3 %>%
  filter(country == 1)%>%
  filter(rptdate > Sys.Date() - 365)%>%
  mutate(
    'Initial Jobess  Claims' = jobless_claims,
    'Continuing Jobess  Claims' = continuing_claims,
             )


test2 <- test%>%
  select(rptdate, 'Initial Jobess  Claims' , 'Continuing Jobess  Claims', initial_claims_relative_pop, continuing_claims_relative_pop)%>%
  gather(variable, value, 'Initial Jobess  Claims':'Continuing Jobess  Claims')



fig <- plot_ly(data = test2, x = ~rptdate, y = ~value, color = ~variable, symbol = ~variable,
               colors = 'Purple', type = 'scatter', mode = 'lines',
               
               text = ~paste('<br>',"Initial Claims: ", '<br>Continuing Claims:'))%>%
  layout(
  title = "Initial Jobless Claims and Continuing Jobless Claims",
  xaxis = list(title = "Date", gridcolor="grey"),
  yaxis = list(title = "Claims",gridcolor="grey")
  
  )%>%
  
   # xaxis = list(title = "Continuing Claims", range=c(0, max(data_jobless_claims3$continuing_claims_relative_pop))),
    #yaxis = list(title = "Initial Jobless Claims", range=c(0, max(data_jobless_claims3$initial_claims_relative_pop))))%>%
  layout(legend = list(orientation = "h",xanchor = "center", x = 0.4, y = -0.2))%>%
  hide_colorbar() %>%
  plotly::config(displayModeBar = F)%>%
  layout(annotations = 
           list(x = 1, y = -0.1, text = "Source: US Department of Labor", 
                showarrow = F, xref='paper', yref='paper', 
                xanchor='right', yanchor='auto', xshift=0, yshift=0,
                font=list(size=10, color="black")))
  fig
