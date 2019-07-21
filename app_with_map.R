library(shiny)
library(shinydashboard)
#library(readr)
library(ggplot2)
library(ggmap)
library(plotly)



food = readRDS(url("https://www.dropbox.com/s/fzjzrwl4uergltc/food_production_2012_2017.rds?raw=1"))
county_shapefiles = readRDS(url("https://www.dropbox.com/s/80n660ryzjk5uya/county_shapefiles.rds?raw=1"))

crop_categories <- sort(unique(food$crop_category))
counties <- sort(unique(food$county))
counties = counties[counties != "Others"]

shinyApp(
  ui = dashboardPage(
    skin = "green",
    dashboardHeader(title = "Food Production"),

    dashboardSidebar(disable = T),
    dashboardBody(
      tabBox(
        width = 12, id = "tabbox",
        tabPanel(
          title = "Crop Overview",
          width = 12,
          fluidRow(
            height = 80,
            box(
              width = 3, height = 80, solidHeader = T,
              selectInput("crop_category",
                label = "Crop Category",
                choices = crop_categories
              )
            ),
            box(
              width = 3, height = 80, solidHeader = T,
              uiOutput("crop_type")
            ),
            box(
              width = 3, height = 80, solidHeader = T,
              uiOutput("par3")
            )
          ),

          fluidRow(
            box(
              width = 12,
              height = 600,
              solidHeader = T,
              plotlyOutput("trend", height = 600)
            )
          )
        ),
        tabPanel(
          title = "Crop Map",
          width = 12,
          fluidRow(
            height = 80,
            box(
              width = 3, height = 80, solidHeader = T,
              selectInput("map_category",
                label = "Crop Category",
                choices = crop_categories
              )
            ),
            box(
              width = 3, height = 80, solidHeader = T,
              uiOutput("par4")
            ),
            box(
              width = 3, height = 80, solidHeader = T,
              uiOutput("par5")
            )
          ),
          fluidRow(
          box(
            solidHeader = T, width = 7, height = 600,
            #h5("The darker the shade, the higher the produce", style = "color:#006400"),
            plotlyOutput("map_plot", height = 600)
            
          ))
        ),
        tabPanel(
          title = "County Overview",
          fluidRow(
            height = 80,
            box(
              width = 3, height = 80, solidHeader = T,
              selectInput("county", label = "County", choices = c(counties, "All"))
            ),
            box(
              width = 3, height = 80, solidHeader = T,
              uiOutput("par2")
            )
          ),
          box(
            width = 12, height = 600, solidHeader = T,
            plotOutput("county_plot", height = 600)
          )
        )
      )
    )
  ),
  server = function(input, output) {
    # tab 1 
    output$crop_type = renderUI({
      selectInput("crop_name", label = "Crop Name",
                  choices = sort(unique(food$crop_type[food$crop_category == input$crop_category])))
    })
    
    output$par3 = renderUI({
      selectInput("crop_county", label = "County",
                  choices = sort(unique(food$county[food$crop_type == input$crop_name])))
    })
    
    crop_output_data <- reactive({
      if (input$crop_county == "All") {
        food[food$crop_type == input$crop_name, ]
      }
      else if (input$crop_county != "All") {
        food[food$crop_type == input$crop_name & food$county == input$crop_county, ]
      }
    })
    
    output$trend <- renderPlotly({
      output_data <- crop_output_data() %>%
        dplyr::rename("produce" = volume_in_mt) 
      
      g <- ggplot(output_data, aes(x = year, y = produce)) + geom_line(lwd = 2, col = "#006400") +
        ggtitle(label = paste(input$crop_name, "Annual Produce in Tonnes")) +
        theme(axis.ticks = element_blank(), 
              axis.title = element_blank(),
              axis.text = element_text(size = 14,colour = "#006400"), 
              plot.title = element_text(size = 16, colour = "#006400", face = "bold")) + 
        scale_x_continuous(breaks = seq(min(output_data$year, na.rm = T), max(output_data$year, na.rm = T), 1),
                           labels = seq(min(output_data$year, na.rm = T), max(output_data$year, na.rm = T), 1)) +
        scale_y_continuous(labels = scales::comma)
      ggplotly(g)
      
      #validate(need(nrow(crop_output_data) > 0, "Just a minute. Output loading"))
      
    })
    
    # tab2
    output$par4 <- renderUI({
      selectInput("map_name",
        label = "Crop Name",
       choices = sort(unique(food$crop_type[food$crop_category == input$map_category]))
      )
    })


    output$par5 <- renderUI({
      selectInput("map_year",
        label = "Year",
        choices = sort(unique(food$year[food$crop_type == input$map_name]), decreasing = T)
      )
    })

    crop_output_map <- reactive({
      food[food$crop_type == input$map_name & food$year == input$map_year, ]
    })


    output$map_plot <- renderPlotly({
      output_data <- crop_output_map() %>%
        dplyr::rename("produce" = volume_in_mt)
   
      county_shapefiles1 <- county_shapefiles %>%
        dplyr::left_join(output_data, by = "county")

      g <- ggplot(county_shapefiles1, aes(x = long, y = lat, group = group, label = county)) +
        geom_polygon(aes(fill = produce)) +
        theme_classic() + theme(
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          axis.line = element_blank(),
          plot.title = element_text(size = 16, colour = "#006400", face = "bold")) +
         guides(fill = guide_legend(title = "Produce")) +
        ggtitle(label = paste(input$map_year, input$map_name, "Produce in Tonnes")) +
        scale_fill_gradient(low = "#7CFC00", high = "#006400")
      ggplotly(g) 
      #layout(legend = list(orientation = "h", x = -0.5, y =-1))
     # g
      #validate(need(nrow(crop_output_map) > 0, "Just a minute. Map loading"))
    })
    
    output$par2 <- renderUI({
      if (input$county == "All"){
        selectInput("year",
                    label = "Year",
                    sort(unique(food$year), decreasing = T)
        )
        
      }
    else if (input$county != "All"){
      selectInput("year",
                  label = "Year",
                  sort(unique(food$year[food$county == input$county]), decreasing = T)
      )
    }
    
    })

    county_data <- reactive({
      if (input$county == "All"){
        food
      }
      else if (input$county != "All")
      {
      food %>%
        dplyr::filter(county == input$county, year == input$year)
      }
      
    })


    # tab3
    output$county_plot <- renderPlot({
      g <- ggplot(county_data(), aes(x = reorder(crop_type, -volume_in_mt), y = volume_in_mt)) +
        geom_col(aes(fill = crop_category)) +
        ggtitle(label = paste(input$year, "Crop Produce in Tonnes", input$county, sep = " ")) +
        theme_classic() + theme(
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_text(angle = 90, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_blank(),
          strip.text = element_text(size = 14),
          plot.title = element_text(size = 16, colour = "#006400", face = "bold"),
          legend.position = "none") +
        facet_grid(~crop_category, scales = "free_x") +
        scale_fill_manual(values = c("#006400", "#7CFC00", "#FFFF00", "#DAA520", "#FF8C00", "#D2691E", "#8B4513")) +
        scale_y_continuous(labels = scales::comma, limits =  c(0, max(county_data()$volume_in_mt, na.rm = T)))

      g
      
    })
  }
)
