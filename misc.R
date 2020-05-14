
add to choropleth

Claims:


      min_legend <- min(data_jobless_claims3$initial_claims_relative_pop)
      max_legend <- max(data_jobless_claims3$initial_claims_relative_pop)


        min_legend <- min(data_jobless_claims3$continuing_claims_relative_pop)
        max_legend <- max(data_jobless_claims3$continuing_claims_relative_pop)


labor: 


      min_legend <- min(data_employment2$unemployment)
      max_legend <- max(data_employment2$unemployment)

      min_legend <- min(data_employment2$participation_rate)
      max_legend <- max(data_employment2$participation_rate)


      min_legend <- min(data_employment2$employed_to_pop_rate)
      max_legend <- max(data_employment2$employed_to_pop_rate)


gdp:

      min_legend <- min(table_gdp_1_change$gdp_qoq)
      max_legend <- max(table_gdp_1_change$gdp_qoq)
      
      min_legend <- min(table_gdp_1_change$gdp_yoy)
      max_legend <- max(table_gdp_1_change$gdp_yoy)
      
       min_legend <- min(table_gdp_1_change$gdp_share)
      max_legend <- max(table_gdp_1_change$gdp_share)



      plot_ly(
        dataset,
        z=claims_diff,
        locations=dataset$state_short,
        #        text=paste0(dataset$state_long, '<br>Initial Claims: ', dataset$claims_diff),
        text = text_hover,
        type="choropleth",
        locationmode="USA-states",
        colors = 'Purples',
        zmin=min_legend,
        zmax=max_legend,
        
        
        
        
