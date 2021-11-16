trial <- vroom::vroom("C:/Users/TzeMin/Documents/occurrence-mapper/data/demo.csv", delim = ",", col_types = list(
  id = "c", # make sure to read 'id' values as characters instead of numbers
  sciname = "c",
  genus = "c",
  family = "c",
  order = "c",
  recorder = "c",
  date = col_date(format = "%m/%d/%Y"),
  latitude = "?",
  longitude = "?"
)) %>%
  dplyr::relocate(id, sciname, genus, family, order, date, recorder, longitude, latitude) %>%
  mutate_if(is.character, function(col) iconv(col, to = "UTF-8"))

inpt <- c("NA")
inpt <- replace(inpt, inpt == "NA", NA)

trial <- trial %>% replace(is.na(sciname), "none")
trial <- trial %>% replace_na(list(sciname = "Not Available", genus = "Not Available", family = "Not Available", order = "Not Available"))
trial %>% dplyr::filter(order %in% inpt)


# # Use iris dataset and add some coordinates
# data("iris")
# iris$longitude = rnorm(150) * 2 + 13
# iris$latitude = rnorm(150) + 48
# 
# ui <- dashboardPage(
#   header = dashboardHeader(title="Example"),
#   sidebar = dashboardSidebar(sidebarMenu(
#     menuItem("Summary", tabName = "summary")
#   )),
#   
#   body = dashboardBody(tabItems(
#     tabItem(
#       tabName = "summary",
#       div(class = "outer",
#           tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
#           leafletOutput("occmap", height = "100%", width = "100%")),
#       absolutePanel(id = "controls", 
#                     shinyWidgets::pickerInput(
#                       "species_select", label = "Species", choices = unique(iris$Species), multiple = TRUE, options = list(`actions-box` = TRUE))),
#       uiOutput("maptitle")
#     )
#   ))
# )
# 
# server <- function(input, output, session) {
#   
#   displayed <- reactiveVal()
#   
#   observeEvent(input$species_select, {
#     
#     displayed(input$species_select)
#     
#     output$occmap <- renderLeaflet({
#       df <- iris %>% dplyr::filter(Species %in% input$species_select)
#       leaflet(data = iris) %>%
#       setView(lng = 14.5501, lat = 47.5162, zoom = 3) %>%
#       addTiles(group = "Street") %>%
#       addCircleMarkers(data = df,
#                        ~longitude, ~latitude,
#                        radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
#                        fillOpacity = 1, fillColor = "black") %>%
#       addLayersControl(options = layersControlOptions(collapsed = FALSE))
#     })
#   }, ignoreNULL = FALSE)
#   
#   output$maptitle <- renderUI({
#     absolutePanel(id = "title", bottom = 20, fixed = TRUE, draggable = FALSE, tags$h4(HTML(paste(displayed(), collapse=", "))))
#   })
# }
# 
# shinyApp(ui, server)