library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(ggplot2)
library(readr)
library(dplyr)
library(here)
library(purrr)
library(ggthemes)
library(plotly)


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data', 'us_counties_processed.csv'))
states <- unique(df$state)
dd_options <- states %>%
    purrr::map(function(state) list(label = state, value = state))

starter <- list('Alabama', 'California')

app$layout(
    dbcContainer(
        list(htmlH1('Your Happy Place: Temperatures'),
             dccGraph(id='plot-area'),
             htmlBr(),
             dccDropdown(
                 id='state-select',
                 options = dd_options, 
                 multi = TRUE,
                 value=starter)
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('state-select', 'value')),
    function(input_value) {
        plot_data <- df %>%
            filter(state %in% input_value)%>%
            group_by(state, year, month) %>% 
            summarize(mean_temp = mean(mean_temp))
            
        p <- ggplot(plot_data, aes(x = month,
                                  y = mean_temp,
                                  color = state)) +
            geom_line() + 
            xlab("Month") + 
            ylab("Mean Temperature (F)") + 
            ggtitle("Mean temperature by state") + 
            labs(color="State")
        ggplotly(p)
    }
)
#app$run_server(debug = T)
app$run_server(host = '0.0.0.0')