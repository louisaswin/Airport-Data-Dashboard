# Websites I used to learn how to do this:
# https://rstudio.github.io/bslib/articles/sidebars/index.html
# https://www.youtube.com/watch?v=O6WLERr5bKU


library(shiny)
library(tidyverse)
library(bslib)
library(formattable)
library(thematic)
library(htmltools)
library(tigris)

airport_data <- read.csv("data10_10.csv")
airport_data = airport_data %>%
  rename(AirlineCode =Operating_Airline.)
#head(airport_data)



# grab reference table for airline names to match IATA code to name 

library(rvest) 
# Read the HTML content of the website 
webpage <- read_html("https://aspm.faa.gov/aspmhelp/index/ASQP__Carrier_Codes_and_Names.html") 
# Select the table using CSS selector 
table_node <- html_nodes(webpage, "table") 
# Extract the table content 
table_content <- html_table(table_node)[[1]] 


data_airports <- data.frame(
  Origin = c("MCO", "DFW", "MIA", "DEN", "LAS", "ORD", "CLT", "PHX", "LAX", "ATL"),
  City = c("Orlando", "Dallas", "Miami", "Denver", "Las Vegas", "Chicago", "Charlotte", "Phoenix", "Los Angeles", "Atlanta")
)


# filter table by the top 10 airlines 
# top 10 US airlines (American Airlines, Delta Airlines, United Airlines, Southwest Airlines, 
# Alaska Airlines, JetBlue Airlines, Spirit Airlines, Frontier Airlines, Allegiant Air, Hawaiian Airlines)
airport_data = airport_data %>%
  left_join(table_content, by = join_by(AirlineCode == `IATA Code` )) %>%
  rename(Airline = `Air Carrier Name`) %>%
  left_join(data_airports, by = join_by(Origin == Origin))
unique(airport_data$Airline)
head(airport_data)




# filter out cancellations for the delay data
airport_data2 = airport_data %>%
  filter(Cancelled != 1)


## CHANGES FOR PUBLICATION
# cancellation code to city 
# cancel <-read.csv("data_filter.csv")
airport_names <- c("MCO" = "Orlando",
                   "DFW" = "Dallas",
                   "MIA" = "Miami",
                   "DEN" = "Denver",
                   "LAS" = "Las Vegas",
                   "ORD" = "Chicago",
                   "CLT" = "Charlotte",
                   "PHX" = "Phoenix",
                   "LAX" = "Los Angeles",
                   "ATL" = "Atlanta")
topAirports = c("ATL", "DFW", "DEN", "ORD", "LAX", "CLT", "MCO", "LAS", "PHX", "MIA")
# cancel<-cancel%>%
#   filter(origin %in% topAirports)%>%
#   filter(cancelled==1) %>%
#   mutate(AirportName = airport_names[origin])

# write new file of city_agg because data_filter.csv is too large to publish shiny app
# write.csv(cancel, 'cancel.csv', row.names = FALSE)
cancel = read.csv('cancel.csv')


# train data for cancelled
library(caret)
set.seed(248)
index = createDataPartition(airport_data$Cancelled, p = 0.7, list = FALSE)
train = airport_data[index,]
test = airport_data[-index, ]



# Ricki, logs data
df.delaydep <- read.csv(file="airline_delay_odds.csv", header = TRUE, row.names = 1, check.names = FALSE)
df.delaydep <- as.matrix(df.delaydep)

df.delaycanc <- read.csv(file="airline_cancel_odds.csv", header = TRUE, row.names = 1, check.names = FALSE)
df.delaycanc <- as.matrix(df.delaycanc)

## CHANGES FOR PUBLISHING
# city_agg
# merged_data <- read.csv("merged_data.csv")
city_agg1 <- read.csv("city_agg.csv")


# city_agg <- merged_data %>%
#   group_by(year, month, city, state, timezone, origin, name, lat, lng) %>%
#   mutate(delays_not_cancelled = ifelse(cancelled == 0, depdelayminutes, NA)) %>%
#   summarise(`Average Delay (Min)` = mean(delays_not_cancelled, na.rm = TRUE),
#             `Proportion of Cancelled Flights` = sum(cancelled, na.rm = TRUE)/n()) 

# write new file of city_agg because merged_data.csv is too large to publish shiny app
# write.csv(city_agg, 'city_agg_df.csv', row.names = FALSE)
city_agg = as_tibble(read.csv('city_agg_df.csv')) %>%
  rename(`Average Delay (Min)` = Average.Delay..Min.,
         `Proportion of Cancelled Flights` = Proportion.of.Cancelled.Flights)

states_sf <- states(cb = TRUE)


#thematic_shiny()

# Define UI for application
library(shiny)
library(shinydashboardPlus)
library(shinythemes)
library(leaflet)
library(dplyr)
library(ggplot2)
library(shinyBS)

# Define UI for application
months_choices <- format(seq(as.Date("2022-04-01"), as.Date("2023-03-01"), by = "month"), "%B %Y")


#thematic_shiny()


# Define UI for application
ui<- page_navbar(
  
  # Application title
  title = "Airport Data Dashboard",
  
  #bootstrapLib(bs_theme()),
  # add theme
  theme =  bs_theme(bootswatch = "cerulean"),
  #theme = bs_theme_update(theme, preset = "cerulean"),
  #theme = bs_theme(version = version_default(), bootswatch = "cerulean"),
  
  # Sidebar layout with input and output definitions
  sidebar = sidebar(
    # Input: Select a single airline
    selectInput("airlineInput", "Select Airline:",
                choices = unique(airport_data$Airline)),
    selectInput("airportInput", "Select Airport:",
                choices = c("All",unique(cancel$AirportName)),
                selected = "Chicago")
  ),
  
  nav_panel("Overview of Top Airports and Airlines", 
            layout_columns(
              
              # Display line plot of average DepDelayMinutes over months
              card(full_screen = TRUE, card_header("Average Delay Time by Month"), plotOutput("allairlinesdelay")),
              
              
              
              # Adding tabs for Odds Cancelled and Odds Delayed
              card(full_screen = TRUE, card_header("Average Delay Time by Airport"), card_body(plotOutput("depAirport"))),
              
              card(full_screen = TRUE, card_header("Cancellations by Type for Airport"), 
                   plotOutput("cancelPie")),
              
              # New tab for additional content 
              card(full_screen = TRUE, card_header("Proportion of Cancelled Flights by Airport"), 
                   plotOutput("cancelledAirport")),
              
              # information at bottom
              card(full_screen = TRUE,
                   card_body(HTML("This page offers an overview of airline and airport performance, comparing them in terms of delays and cancellations.
                                  While most of this page is static, the pie chart can be filtered by airport."))
              ),
              
              col_widths = c(6, 6, 6, 6, 12)  # Adjust column widths as needed
            )
            
  ),
  
  nav_panel("Specific Airline Information",
            # Main panel for displaying outputs
            layout_columns(
              # Display descriptive statistic - Average Delay
              value_box(title = "Average Delay in Minutes", textOutput("flightCount")),
              # Display proportion of flights delayed
              value_box(title = "Percent Delayed", textOutput("propDelayed")),
              # Display proportion of flights cancelled
              value_box(title = "Percent Cancelled", textOutput("propCancelled")),
              
              # Display line plot of average DepDelayMinutes over months
              card(full_screen = TRUE, card_header("Average Departure Delay Time vs. Month"), plotOutput("flightPlot")),
              
              # Adding tabs for Odds Cancelled and Odds Delayed
              navset_card_tab(full_screen = TRUE,
                              nav_panel(title = "Odds Cancelled", plotOutput("oddsCancelled")),
                              nav_panel(title = "Odds Delayed", plotOutput("oddsDelayed"))
              ),
              
              # information at bottom
              card(full_screen = TRUE,
                   card_body(HTML("This page evaluates the performance of specific airlines in terms of delays and cancellations. 
                                  All features can be filtered by airline. All features except the Log-Odds Chart
                                  can also be filtered by airport."))
              ),
              
              
              col_widths = c(4, 4, 4, 6, 6, 12)  # Adjust column widths as needed
            )
            
            
  ), 

  
  
  nav_panel("A Geographic Overview",
            fluidPage(
              # Leaflet map
              fluidRow(
                column(12,
                       card(full_screen = TRUE,
                         card_header("US Map of Delays and Cancellations"),
                         leafletOutput("usMap", height = "600px")  # Increase height for a larger map
                       )
                )
              ),
              
              # Filter for month
              fluidRow(
                column(12,
                       card(full_screen = TRUE,
                         card_header("Filters"),
                         HTML("<p>Use filter options below for this page rather than the left sidebar.</p>"),
                         selectInput("filter1", "Select Month:", choices = c("All", months_choices)),
                         selectInput("state", "State", choices = c("All", unique(city_agg$state)), selected = "All"),
                         uiOutput("city_ui"),
                         selectInput("timezone", "Timezone", choices = c("All", unique(city_agg$timezone)), selected = "All"),
                         selectInput("metric", "Metric", 
                                     choices = c("Average Delay" = "Average Delay (Min)", 
                                                 "Proportion of Cancelled Flights" = "Proportion of Cancelled Flights"), 
                                     selected = "total_delays")
                       )
                )
              ),
              
              # First bar chart
              fluidRow(
                column(6,
                       card(full_screen = TRUE,
                         card_header("Average Delay by City"),
                         plotOutput("heatmap1")
                       )
                ),
                
                # Second bar chart
                column(6,
                       card(
                         card_header("Proportion of Cancelled Flights by City"),
                         plotOutput("heatmap2")
                       )
                )
              )
            )
  )
  
  
)




# Define server logic
server <- function(input, output, session) {
  #theme dropdown
  #bs_themer()
  
  # Reactive expression to filter data based on selected airline
  filtered_data <- reactive({
    
    if (input$airportInput != "All") {
      airport_data <- airport_data %>%
        filter(City == input$airportInput,
               Airline == input$airlineInput)
    } else{
      airport_data <- airport_data %>%
        filter(Airline == input$airlineInput)
    }
  })
  
  # Reactive expression to filter data based on selected airline and remove cancellations
  filtered_data2 <- reactive({
    
    if (input$airportInput != "All") {
      airport_data2 <- airport_data2 %>%
        filter(City == input$airportInput,
               Airline == input$airlineInput)
    } else {
      airport_data2 %>%
        filter(Airline == input$airlineInput)
    }
  })
  
  # Reactive expression to prepare training data for logistic regression
  train2 <- reactive({
    train %>%
      mutate(Airline = ifelse(Airline == input$airlineInput, paste0("A. ", input$airlineInput), Airline))
  })
  
  # Logistic regression model for flight cancellation
  cancel_logit2 <- reactive({
    glm(Cancelled ~ as.factor(Airline), data = train2(),
        family = binomial(link = "logit"))
  })
  
  # Calculate average DepDelayMinutes by month
  avg_delay <- reactive({
    filtered <- filtered_data()
    avg_delay <- filtered %>%
      group_by(Month) %>%
      summarise(avg_delay = mean(DepDelayMinutes, na.rm = TRUE))
    avg_delay
  })
  
  # Calculate proportion of flights delayed
  output$propDelayed <- renderText({
    filtered <- filtered_data2()
    total_flights <- nrow(filtered)
    delayed_flights <- sum(filtered$DepDel15 == 1)
    proportion_delayed <- delayed_flights / total_flights
    paste0(round(proportion_delayed * 100, 2), "%")
  })
  
  # Calculate proportion of flights cancelled
  output$propCancelled <- renderText({
    filtered <- filtered_data()
    total_flights <- nrow(filtered)
    cancelled_flights <- sum(filtered$Cancelled == 1)
    proportion_cancelled <- cancelled_flights / total_flights 
    paste0(round(proportion_cancelled * 100, 2), "%")
  })
  
  # Output descriptive statistic - count of flights
  output$flightCount <- renderText({
    filtered <- filtered_data()
    round(mean(filtered$DepDelayMinutes, na.rm = TRUE),2)
    
  })
  
  # Output line plot of average DepDelayMinutes over months
  output$flightPlot <- renderPlot({
    avg_delay_data <- avg_delay()
    
    ggplot(avg_delay_data, aes(x = Month, y = avg_delay)) +
      geom_line(color = "#2FA4E7", stat="identity",show.legend=FALSE) + 
      geom_point(color= "darkblue") +
      labs(x = "Month", y = "Average Departure Delay Time (Minutes)") +
      scale_x_continuous(breaks = 1:12, labels = month.name[1:12], expand = c(0, 0)) +  # Set expand argument to remove padding
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),  # Adjust y-axis text size
            axis.title = element_text(size = 13),   # Adjust axis title text size
      ) 
  })
  
  
  
  # Output odds plot of Cancelled
  output$oddsCancelled <- renderPlot({
    
    plot.list <- df.delaycanc[input$airlineInput,!colnames(df.delaycanc) %in% c(input$airlineInput)]
    plot.adjusted <- plot.list-1
    
    # par(mar = c(7, 4, 4, 2))
    odd.plot <- barplot(plot.adjusted, 
                        ylim = c(-1, 3),  # Set the y-axis limits to accommodate negative values
                        names.arg = names(plot.adjusted),  # Labels for each bar
                        col = ifelse(plot.adjusted < 0, "#73a839", "#c71c22"),  # Color bars below 1 differently
                        main = input$airlineInput,
                        ylab = "Odds Ratio",
                        #xlab = "Airline",
                        border = NA, xaxt = "n",
                        axes = FALSE)  # No border around bars
    #axis(side = 2, at=c(-1:1),labels = c(0:2),col = "black")
    axis(side = 2, 
         at = -1:3,  # Match the y-axis range from -1 to 3
         labels = -1:3,  # Use corresponding labels
         col = "black")
    text(odd.plot, par("usr")[3], labels = names(plot.adjusted), srt = 40, adj=1, xpd = TRUE, cex=0.8) 
  })
  
  
  # Output odds plot of Delayed
  output$oddsDelayed <- renderPlot({
    plot.list <- df.delaydep[input$airlineInput,!colnames(df.delaydep) %in% c(input$airlineInput)]
    plot.adjusted <- plot.list-1
    # par(mar = c(7, 4, 4, 2))
    odd.plot <- barplot(plot.adjusted,
                        ylim = c(-1, 3),  # Set the y-axis limits to accommodate negative values
                        names.arg = names(plot.adjusted),  # Labels for each bar
                        col = ifelse(plot.adjusted < 0, "#73a839", "#c71c22"),  # Color bars below 1 differently
                        main = input$airlineInput,
                        ylab = "Odds Ratio",
                        #xlab = "Airline",
                        border = NA, xaxt = "n",
                        axes = FALSE)  # No border around bars
    #axis(side = 2, at=c(-1:1),labels = c(0:2),col = "black")
    axis(side = 2, 
         at = -1:3,  # Match the y-axis range from -1 to 3
         labels = -1:3,  # Use corresponding labels
         col = "black")
    text(odd.plot, par("usr")[3], labels = names(plot.adjusted), srt = 40, adj=1, xpd = TRUE, cex=0.8)
  })
  
  
  # all airline delays
  output$allairlinesdelay <- renderPlot({
    Avg.Dep.Data <- airport_data2 %>%
      group_by(Airline, Month) %>%
      summarise(Avg.DelTime = mean(DepDelayMinutes))
    
    ggplot(Avg.Dep.Data, aes(x = Month, y = Avg.DelTime, group = Airline, color = Airline)) + 
      geom_line(linewidth = 0.8) +
      scale_x_continuous(breaks = 1:12, labels = month.abb[1:12], expand = c(0, 0)) +
      scale_color_discrete(name = "Operating Airline") +
      labs(x = "Month", y = "Average Delay Time (Minutes)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 12),  # Adjust x-axis text size
        axis.text.y = element_text(size = 12),  # Adjust y-axis text size
        axis.title = element_text(size = 13),   # Adjust axis title text size
        legend.title = element_text(size = 13), # Adjust legend title text size
        legend.text = element_text(size = 12)   # Adjust legend text size
      )
  })
  
  output$cancelledAirport <- renderPlot({
    
    airport_names <- c("MCO" = "Orlando",
                       "DFW" = "Dallas",
                       "MIA" = "Miami",
                       "DEN" = "Denver",
                       "LAS" = "Las Vegas",
                       "ORD" = "Chicago",
                       "CLT" = "Charlotte",
                       "PHX" = "Phoenix",
                       "LAX" = "Los Angeles",
                       "ATL" = "Atlanta")
    
    ca.data<-airport_data%>%
      group_by(Origin) %>%
      summarise(ProportionCancelled = mean(Cancelled, na.rm = TRUE)) %>%
      arrange(desc(ProportionCancelled)) %>%
      mutate(AirportName = airport_names[Origin])
    
    ca.data$Origin <- factor(ca.data$Origin, 
                             levels = ca.data$Origin[order(-ca.data$ProportionCancelled)])
    
    
    ggplot(ca.data, aes(x = Origin, y = ProportionCancelled)) + 
      geom_bar(stat = "identity", fill = "#2FA4E7") +
      labs(x = "Airport", y = "Proportion Cancelled") +
      scale_x_discrete(labels = ca.data$AirportName) + 
      theme_minimal() +
      theme(axis.text.x = element_text(size=12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),  # Adjust y-axis text size
            axis.title = element_text(size = 13))   # Adjust axis title text size)  
    
  })
  
  output$depAirport <- renderPlot({
    
    airport_names <- c("MCO" = "Orlando",
                       "DFW" = "Dallas",
                       "MIA" = "Miami",
                       "DEN" = "Denver",
                       "LAS" = "Las Vegas",
                       "ORD" = "Chicago",
                       "CLT" = "Charlotte",
                       "PHX" = "Phoenix",
                       "LAX" = "Los Angeles",
                       "ATL" = "Atlanta")
    
    # Assuming dd.data is your original dataset
    Avg.Dep.Data <- airport_data %>%
      group_by(Origin) %>%
      summarise(Avg.DelTime = mean(DepDelayMinutes, na.rm=TRUE)) %>%
      arrange(desc(Avg.DelTime)) %>%
      mutate(AirportName = airport_names[Origin])
    
    Avg.Dep.Data$Origin <- factor(Avg.Dep.Data$Origin, 
                                  levels = Avg.Dep.Data$Origin[order(-Avg.Dep.Data$Avg.DelTime)])
    ggplot(Avg.Dep.Data, aes(x = Origin, y = Avg.DelTime)) + 
      geom_bar(stat = "identity", fill = "#2FA4E7") +
      labs(x = "Airport", y = "Average Departure Delay Time (Minutes)") +
      scale_x_discrete(labels = Avg.Dep.Data$AirportName) + 
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 12),  # Adjust y-axis text size
            axis.title = element_text(size = 13),   # Adjust axis title text size
      )   
    
  })
  
  output$cancelPie <- renderPlot({
    
    #creating df with the proportion of each cancellation code for each airport
    cancellation_summary <- cancel %>%
      group_by(AirportName, cancellationcode) %>%
      summarise(Count = sum(!is.na(cancellationcode))) %>%  # Count occurrences of each cancellation code
      ungroup() %>%
      complete(AirportName, cancellationcode, fill = list(Count = 0)) %>%  # Ensure all combinations are represented
      group_by(AirportName) %>%
      mutate(Proportion = Count / sum(Count))
    
    # Filter data for selected airport
    sel.air <- cancellation_summary[cancellation_summary$AirportName == input$airportInput, ]#you would just have to change the origin to the desired airport
    # Define colors and corresponding cancellation codes
    cancellation_colors <- c("#aa80ff", "#90ed7d", "#f7a35c", "#7cb5ec")
    cancellation_labels <- c("Carrier Cancellation", "Weather", "NAS", "Security")
    
    ggplot(sel.air, aes(x = "", y = Proportion, fill = cancellationcode)) +
      geom_bar(width = 1, stat = "identity") +  
      facet_wrap(~ AirportName, ncol = 3, scales = "free") +  
      coord_polar(theta = "y") +  
      scale_y_continuous(labels = NULL) + 
      scale_fill_manual(values = cancellation_colors, name = "Cancellation Code", labels = cancellation_labels) +  # Custom colors and labels
      labs(x = NULL, y = NULL) +  # Remove axis labels
      theme_minimal()+
      theme(strip.text = element_text(size = 14),
            axis.text.x = element_text(size = 12),  # Adjust x-axis text size
            axis.title = element_text(size = 13),   # Adjust axis title text size
            legend.title = element_text(size = 13), # Adjust legend title text size
            legend.text = element_text(size = 12)   # Adjust legend text size
      )
    
  })
  
  # Convert month-year string to year and month
  parse_month <- reactive({
    req(input$filter1)  # Ensure input$filter1 is not NULL
    if (input$filter1 == "All") {
      NULL  # Return NULL if "All" is selected
    } else {
      as.Date(paste("01", input$filter1), format = "%d %B %Y")
    }
  })
  
  # Reactive filtered data
  filtered_data3 <- reactive({
    data <- city_agg
    
    if (input$state != "All") {
      data <- data %>% filter(state == input$state)
    }
    
    if (!is.null(input$name) && length(input$name) > 0) {
      data <- data %>% filter(name %in% input$name)
    }
    
    if (input$timezone != "All") {
      data <- data %>% filter(timezone == input$timezone)
    }
    
    if (!is.null(parse_month())) {
      data <- data %>% filter(format(as.Date(paste(year, month, "01", sep = "-")), "%Y-%m") == format(parse_month(), "%Y-%m"))
    }
    
    data
  })
  
  # Render city checkbox UI
  output$city_ui <- renderUI({
    checkboxGroupInput("name", "Airline", choices = unique(city_agg$name), selected = unique(city_agg$name))
  })
  
  # Leaflet map
  output$usMap <- renderLeaflet({
    req(input$metric)  # Ensure input$metric is not NULL
    
    data <- filtered_data3()
    
    # Aggregate data by state
    state_data <- data %>%
      group_by(state) %>%
      summarise(value = mean(get(input$metric), na.rm = TRUE))
    
    # Merge with state shapefile
    states_sf_merged <- states_sf %>% left_join(state_data, by = c("STUSPS" = "state"))
    
    pal <- colorNumeric(rev(viridis::viridis(256)), domain = states_sf_merged$value, na.color = "transparent")
    
    leaflet(states_sf_merged) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -98.35, lat = 39.5, zoom = 4) %>%
      addPolygons(
        fillColor = ~pal(value),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste(NAME, ",", input$metric, ": ", round(value, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~value,
        title = input$metric,
        position = "bottomright"
      ) 
  })
  
  # Define pastel colors
  pastel_colors <- c(
    "#FAD02E", "#F28D35", "#F25F5C", "#D8336F", "#8E5C2F",
    "#6C5B7B", "#A5C9CA", "#9D9D9D", "#D4A5A5", "#F5B4B4"
  )
  
  # Common legend for both plots
  common_fill <- scale_fill_manual(name = "Airline", values = pastel_colors)
  
  # First Heatmap - Average Delays by City and Airline
  output$heatmap1 <- renderPlot({
    if (is.null(parse_month())) {
      filtered_data1 <- city_agg1
    } else {
      filtered_data1 <- city_agg1 %>%
        filter(format(as.Date(paste(year, month, "01", sep = "-")), "%Y-%m") == format(parse_month(), "%Y-%m"))
    }
    
    ggplot(filtered_data1, aes(x = city, y = name, fill = Average.Delays)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
      labs(x = "City", y = "Airline", fill = "Average Delay (Min)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Second Heatmap - Proportion of Cancelled Flights by City and Airline
  output$heatmap2 <- renderPlot({
    if (is.null(parse_month())) {
      filtered_data2 <- city_agg1
    } else {
      filtered_data2 <- city_agg1 %>%
        filter(format(as.Date(paste(year, month, "01", sep = "-")), "%Y-%m") == format(parse_month(), "%Y-%m"))
    }
    
    ggplot(filtered_data2, aes(x = city, y = name, fill = Proportion.of.Cancelled.Flights)) +
      geom_tile(color = "white") +
      scale_fill_gradient(low = "lightyellow", high = "darkred", na.value = "white") +
      labs(x = "City", y = "Airline", fill = "Proportion of Cancelled Flights") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

