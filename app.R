# library(rsconnect)
# rsconnect::deployApp()

# Load necessary packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(maps)
library(ggmap)
library(magick)
library(markdown)
library(leaflet)
library(geojson)
library(geojsonio)
library(rgdal)
library(dplyr)
library(Cairo)
library(dashboardthemes)
# library(c( shiny,shinydashboard,ggplot2,maps,ggmap,magick,markdown,leaflet,geojson,geojsonio,rgdal,dplyr,Cairo,dashboardthemes ))

######## try using data table instead of data frame
# Load access data
access_data <- read.csv('access_data.csv', header=TRUE, stringsAsFactors = F, na.strings="NA")
# Load map data and remove Antarctica
countries <- readOGR("countries.geojson")
countries <- countries[!countries$ADMIN == "Antarctica",]

# Load world map and fix virgin islands labels
worldmap <- map_data(map="world")
worldmap$region[worldmap$region == 'Virgin Islands'][1:23] <- "British Virgin Islands"
worldmap$region[worldmap$region == 'Virgin Islands'] <- "United States Virgin Islands"


# Get list of countries
countrylist <- cbind.data.frame(worldmap$region,worldmap$subregion,stringsAsFactors = F)
names(countrylist) <- c("region","subregion")
countrylist$region[countrylist$subregion == 'Hawaii'] <- "USA - Hawaii"
countrylist$region[countrylist$subregion == 'Alaska'] <- "USA - Alaska"
countrylist$region[countrylist$region == 'USA'] <- "USA - Continental"
countrylist <- data.frame(unique(countrylist$region))
countrylist <- data.frame(countrylist[order(countrylist$unique.countrylist.region.),])
names(countrylist) <- "Country/Region"

# Define data processing function with year, h, as input
YearMapDataFxn = function(h,MaxPerc=100){
    # Get access data for year h
    access_df <- access_data[access_data$Year == as.numeric(h),]
    access_df <- cbind.data.frame(access_df$country, as.numeric(access_df$Nat.Elec.Rate), stringsAsFactors=FALSE)
    names(access_df) <- c("Country","Access")
    # Replace electrification rates over given percentage with NA
    access_df$Access[access_df$Access > MaxPerc] <- NA
    
    # New shit to suppress left_join warning, convert access_df to factor and sort levels to match countries@data
    access_df$Country <- as.factor(access_df$Country)
    # access_df$Access <- as.factor(access_df$Access)
    combined <- sort(union(levels(access_df$Country), levels(countries@data$ADMIN)))
    countries@data <- left_join(mutate(countries@data, ADMIN=factor(ADMIN, levels=combined)), mutate(access_df, Country=factor(Country, levels=combined)),by=c('ADMIN' = 'Country'))
    # Merge with map data
    # countries@data <- left_join(countries@data, access_df, by = c('ADMIN' = 'Country'))
    return(countries)
}

# Define data processing function with year, h, as input
CountryMapDataFxn = function(c,h){
  
    # Get access data for year h, country c
    access_df <- access_data[access_data$country == c,]
    access_df <- access_df[access_df$Year == h,]
    # Merge access data with worldmap by country
    countrymap <- worldmap[worldmap$region == c,]
    df <- merge(countrymap, access_df, by.x = "region", by.y = "country")
    df2 <- df[order(df$Year),]
    names(df2)[8:10] <- c("National","Rural","Urban")
    return(df2)
  
}



ui <- dashboardPage(
  
    dashboardHeader(
        title = "Global Electricity Access", 
        titleWidth = 300
    ),
    
    dashboardSidebar(
        width = 300,
        hr(),
        sidebarMenu(id="tabs",
            menuItem(
                "ReadMe", 
                tabName = "readme", 
                icon=icon("info"), 
                selected=TRUE
            ),
            menuItem(
                "Global Access By Year", 
                tabName="by_year", 
                icon=icon("globe")
            ),
            menuItem(
                "Rural Versus Urban Access By Country", 
                tabName="by_country", 
                icon=icon("adjust")
            ),
            menuItem("Gifs",  tabName = "gifs", icon = icon("play"),
                menuSubItem("All Countries", 
                    tabName = "gif100", icon = icon("angle-right")),
                menuSubItem("High Impact Countries", 
                    tabName = "gif50", 
                    icon = icon("angle-right")
                )
            )
        ),
        hr()
    ),
    
    dashboardBody(
        ### changing theme
        # shinyDashboardThemes(
        #     theme = "grey_dark"
        # ),
        tabItems(
            tabItem(tabName = "readme", selected = T,
                box(
                    width = NULL,
                    title = "Welcome!",
                    solidHeader = TRUE,
                    status = "primary"
                    # withMathJax(),
                    # includeMarkdown("Readme.md")
                )
            ),
            tabItem(tabName = "by_year",
                fluidRow( # split panel in 2 columns: 1 for year slider, 1 for map output
                    column(width = 2,
                        box(
                            width = NULL, 
                            title = "Select a year:",
                            solidHeader = TRUE,
                            status = "primary",
                            sliderInput(
                                inputId="inputyear",
                                label="", 
                                value=1990, 
                                min=1990, max = 2014, 
                                step=1, 
                                sep ="",
                                ticks = F
                            )
                        )
                    ),
                    column(width = 10,
                         box(
                           height = NULL, 
                           width = NULL, 
                           solidHeader = TRUE,
                           status = "primary",
                           title = textOutput("MapTitle"), 
                           leafletOutput("YearMap")
                         )
                    )
                )
            ),
            tabItem(tabName = "by_country",
                fluidRow(
                    column(width = 4,
                        box( 
                            width = NULL,
                            title = "Select a country:",
                            solidHeader = TRUE,
                            status = "primary",
                            selectizeInput(
                                inputId="inputcountry", 
                                h4(""),
                                options = list(dropdownParent = 'body'),
                                choices = c(Choose='',countrylist)
                            ),
                            tags$head(
                                tags$style(
                                    ".selectize-control.single { width: 150px; z-index: 1; }"
                                )
                            )
                        ),
                        box( 
                            width = NULL, 
                            title = "Choose a region:",
                            solidHeader = TRUE,
                            status = "primary",
                            radioButtons(
                                inputId="inputregion",
                                "", 
                                c("National"="National","Urban"="Urban","Rural"="Rural")
                            )
                        ),
                        box(
                            width = NULL, 
                            title = "Select a year:",
                            solidHeader = TRUE,
                            status = "primary",
                            sliderInput(
                                inputId="inputyear_c",
                                label="", 
                                value=2014, 
                                min=1990, max = 2014, 
                                step=1, 
                                sep ="",
                                ticks = F
                            )
                        )
                    ),
                    column(width = 8,
                        box(
                            height = NULL, 
                            width = NULL, 
                            status = "primary",
                            solidHeader = TRUE,
                            title = textOutput("CountryMapTitle"), 
                            plotOutput("CountryMap")
                        )
                    )
                )
            ),
            tabItem(tabName = "gif100",
                box( 
                    width = NULL, 
                    status = "primary",
                    solidHeader = TRUE,
                    title="History of Global Electricity Access",
                    imageOutput("gif100"),
                    downloadButton('downloadGif100', 'Download')
                )
            ),
            tabItem(tabName = "gif50",
                box( 
                    width = NULL, 
                    status = "primary",
                    solidHeader = TRUE,
                    title="History of Electricity Access in High Impact Countries",
                    imageOutput("gif50"),
                    downloadButton('downloadGif50', 'Download')
                )
            )
        )
    )
)


server <- function(input, output) ({

    # Get mapping data
    countries2 <- reactive({ 
        YearMapDataFxn(input$inputyear) 
    })
    
    # Define color palette
    # pal <- reactive({ 
    #     colorNumeric(
    #         c("darkseagreen3","deepskyblue4"), 
    #         domain = countries2$Access
    #     )
    # })
    pal <- colorNumeric( c("darkseagreen3","deepskyblue4"), domain = c(0,100) )

    
    # Define interactive country labels
    labels <- reactive({ sprintf("<strong>%s</strong><br/>%g &#37;</sup>", countries2()$ADMIN,countries2()$Access) %>%
        lapply(htmltools::HTML) })

    # Base map
    # output$YearMap <- renderLeaflet({
    #   leaflet(countries) %>%
    #       addTiles() #%>%
          # tileOptions(minZoom = 1, maxZoom = 2)
          # addPolygons(
          #     options = leafletOptions(minZoom = 0, maxZoom = 3),
          #     fillColor = "gray",
          #     weight = 0.3,
          #     opacity = 1,
          #     color = "black",
          #     dashArray = "",
          #     fillOpacity = 1,
          #     highlight = highlightOptions(
          #         weight = 1,
          #         color = "white",
          #         dashArray = "",
          #         fillOpacity = 0.7,
          #         bringToFront = TRUE
          #     )
          # )
    # })
    
    # observe({
    # 
    #     leafletProxy("YearMap",data = countries2()) %>%
    #         clearShapes() %>%
    #         clearControls() %>%
    #         addPolygons(
    #             options = leafletOptions(minZoom = 1, maxZoom = 3),
    #             fillColor = pal,
    #             weight = 0.3,
    #             opacity = 1,
    #             color = "black",
    #             dashArray = "",
    #             fillOpacity = 0.7,
    #             highlight = highlightOptions(
    #                 weight = 1,
    #                 color = "white",
    #                 dashArray = "",
    #                 fillOpacity = 0,
    #                 bringToFront = TRUE
    #             )#,
    #             # label = labels(),
    #             # labelOptions = labelOptions(
    #             #     style = list("font-weight" = "normal", padding = "3px 8px"),
    #             #     textsize = "15px",
    #             #     direction = "auto"
    #             # )
    #         ) %>%
    #         addLegend(
    #             pal = pal,
    #             values = ~Access, 
    #             opacity = 0.7, 
    #             title = NULL,
    #             labFormat = labelFormat(suffix = "%"), 
    #             position = "bottomright"
    #         )
    # })    
        
    output$YearMap <- renderLeaflet({
        
        # Get input year
        h <- input$inputyear

        # Get mapping data
        countries <- YearMapDataFxn(h)
        
        # Remove Antarctica
        countries <- countries[!countries$ADMIN == "Antarctica",]

        # Define color palette
        pal <- colorNumeric(c("darkseagreen3","deepskyblue4"), domain = countries$Access)
        labels <- sprintf(
          "<strong>%s</strong><br/>%g &#37;</sup>",
          countries$ADMIN, countries$Access
        ) %>% lapply(htmltools::HTML)

        leaflet(countries) %>%
            # options = leafletOptions(minZoom = 0, maxZoom = 3) %>%
            # addTiles() %>%
            addPolygons(
                # options = leafletOptions(minZoom = 0, maxZoom = 3),
                fillColor = ~pal(Access),
                weight = 0.3,
                opacity = 1,
                color = "black",
                dashArray = "",
                fillOpacity = 1,
                highlight = highlightOptions(
                    weight = 1,
                    color = "white",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))  %>%
            addLegend(
                pal = pal,
                values = ~Access,
                opacity = 1,
                title = NULL,
                labFormat = labelFormat(suffix = "%"),
                position = "bottomright"
            )
    })
    
    output$MapTitle <- renderText(  paste0("Map of Electricity Access in ",input$inputyear)  )
    
    output$CountryMapTitle <- renderText({
      
        h <- input$inputyear_c
        c <- input$inputcountry
        r <- input$inputregion
        if(input$inputcountry == "USA - Continental"){
            c="Continental USA"
        }
        if(input$inputcountry == "USA - Hawaii"){
            c="Hawaii"
        }
        if(input$inputcountry == "USA - Alaska"){
            c="Alaska"
        }
        if(r == "National"){r <- ""}
        paste0("Electricity Access in ",r," ",c," in ",h)
    })
    
    output$CountryMap <- renderPlot({
      
        # Get input year and country
        h <- input$inputyear_c
        c <- input$inputcountry
        r <- input$inputregion
        
        if(c == ''){
            ggplot()
        }
        else{
            # Get mapping data
            if(c %in% c("USA - Hawaii","USA - Alaska","USA - Continental")){
              c="USA"
            }
            map2 <- CountryMapDataFxn(c,h)
            map <- map2[,1:6]
            if(input$inputcountry == "USA - Continental"){
              map <- map[!(map$subregion %in% c("Hawaii","Alaska")),]
              c="Continental USA"
            }
            if(input$inputcountry == "USA - Hawaii"){
              map <- map[map$subregion == "Hawaii",]
              c="Hawaii"
            }
            if(input$inputcountry == "USA - Alaska"){
              map <- map[map$subregion == "Alaska",]
              c="Alaska"
            }
  
            # Plot
            # Ammend for countries that do not have data
            test <- map2[names(map2) == r]
            if(dim(data.frame(test))[1]==0){
                map <- worldmap[worldmap$region==c,]
                gg = ggplot() +
                    ggtitle(
                        paste0(
                            "Data is unavailable for \n",
                            as.character(c)
                        )
                    ) +
                    geom_map(
                        data=map, 
                        map=map,
                        aes(x=long, y=lat, map_id=region),
                        fill = "gray"
                    ) +
                    theme(
                        plot.title = element_text(hjust = 0.5, size = 30),
                        plot.background = element_rect(fill = "transparent")
                        # panel.background = element_rect(fill = "black")
                    )
            }else if(T %in% is.na(test)){
                gg = ggplot() +
                    ggtitle(paste0("Data is unavailable for \n", c)) +
                    geom_map(
                        data=map, 
                        map=map,
                        aes(x=long, y=lat, map_id=region),
                        fill = "gray"
                ) +
                coord_equal()
                theme(
                    plot.title = element_text(hjust = 0.5, size = 30),
                    plot.background = element_rect(fill = "transparent")
                    # panel.background = element_rect(fill = "black")
                )
            }else{
                map$access <- max(map2[names(map2) == r])
                names(map)[ncol(map)] <- "access"
                gg = ggplot() +
                  ggtitle(paste0(max(map$access),"%")) +
                  theme(
                      plot.title = element_text(hjust = 0.5, size = 30),
                      plot.background = element_rect(fill = "transparent")
                      # panel.background = element_rect(fill = "black")
                  ) +
                  geom_map(
                      data=map, 
                      map=map,
                      aes(
                          x=long, 
                          y=lat, 
                          map_id=region, 
                          fill=access
                      )
                  ) + 
                  scale_fill_gradient(
                      low = "orange", 
                      high = "blue", 
                      guide = "colourbar", 
                      limits=c(0,100)
                  ) + 
                  coord_equal()
            }
            gg
        }
    }, bg="transparent")
    
    output$gif100 <- renderImage({

        # Create a Progress object
        progress <- shiny::Progress$new(style = "old")
        progress$set(message = "Computing data", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        # Create a closure to update progress.
        updateProgress <- function(value = NULL, detail = NULL) {
          if (is.null(value)) {
            value <- progress$getValue()
            value <- value + (progress$getMax() - value) / 5
          }
          progress$set(value = value, detail = detail)
        }
      
        tmpfile <- image_read("gif100.gif") %>% 
            # image_resize("90%") %>%
            image_animate(fps=4) %>%
            image_write(tempfile(fileext='gif'), format = 'gif')
        
        list(src = tmpfile, contentType = "image/gif")

    })
    
    output$gif50 <- renderImage({
      
      # Create a Progress object
      progress <- shiny::Progress$new(style = "old")
      progress$set(message = "Computing data", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      # Create a closure to update progress.
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 5
        }
        progress$set(value = value, detail = detail)
      }
      
      tmpfile <- image_read("gif50.gif") %>% 
        # image_resize("90%") %>%
        image_animate(fps=4) %>%
        image_write(tempfile(fileext='gif'), format = 'gif')
      
      list(src = tmpfile, contentType = "image/gif")
      
    })
    
    
    output$downloadGif100 <- downloadHandler(
      filename = "gif100.gif",
      content = function(file){
        image_write(im = image_read("gif100.gif"), path = file)
      }
    )
    
    
    output$downloadGif50 <- downloadHandler(
      filename = "gif50.gif",
      content = function(file){
          image_write(im = image_read("gif50.gif"), path = file)
      }
    )
  
})



shinyApp(
    ui = ui, 
    server = server
)



