library(dplyr)
library(bslib)
library(shiny)
library(clock)
library(lubridate)
library(thematic)
library(leaflet)
library(jardskjalftar)
library(metill)
library(glue)
library(shinyWidgets)

theme_set(theme_metill())

bs_global_theme(
    bootswatch = "flatly"
)

bs_global_add_variables(
    primary = "#484D6D",
    secondary = "#969696",
    success = "#969696",
    # danger = "#FF8CC6",
    # info = "#FF8CC6",
    light = "#faf9f9",
    dark = "#484D6D",
    bg = "#faf9f9",
    fg = "#737373",
    "body-bg" = "#faf9f9",
    base_font = "Lato",
    heading_font = "Segoe UI",
    "navbar-brand-font-family" = "Playfair Display",
    code_font = "SFMono-Regular"
)
thematic_on()

sidebar_info <- paste0(
    br(" "),
    h5("Höfundur:"),
    p("Brynjólfur Gauti Guðrúnar Jónsson"),
    HTML("<p>Fleiri greiningar á  <a href='https://metill.is'>Metill.is</a></p>"),
    p("Byggt á gögnum frá Veðurstofu Íslands.")
)

ui <- page_sidebar(
    title = "Fjöldi jarðskjálfta og landfræðileg dreifing þeirra á Íslandi",
    theme = bs_global_get(),
    sidebar = sidebar(
        width = "30%",
        dateRangeInput(
            inputId = "daterange",
            label = "Sýna jarðskjálfta frá tímabilinu",
            start = Sys.Date() - months(1),
            end = Sys.Date(),
            min = "2010-01-01",
            max = Sys.Date(),
            weekstart = 1,
            language = "is",
            separator = "til"
        ),
        sliderInput(
            inputId = "min_magnitude",
            label = "Sýna bara skjálfta sem eru öflugri en",
            value = 0,
            min = 0,
            max = 7,
            step = 0.5
        ),
        materialSwitch(
            "cluster",
            "Hópa saman nálægum skjálftum?",
            status = "primary"
        ),
        actionButton(
            inputId = "go",
            label = "Sækja gögn"
        ),
        br(),
        HTML(sidebar_info)
    ),
    p("Tölurnar innan hringanna tákna fjölda jarðskjálfta. Færið músina yfir punktana til að sjá svæðið sem fjöldatalan á við um. Smelltu svo á punktinn til að þysja inn að honum og sjá nánari dreifingu."),
    leafletOutput("p")
)

server <- function(input, output) {

    plot_data <- reactive({
        start_date <- input$daterange[1] + seconds(1)
        end_date <- input$daterange[2] + days(1) - seconds(1)

        validate(
            need(start_date <= end_date, "Vinstri dagsetningin má ekki vera nýrri en hægri dagsetningin")
        )

        download_skjalftalisa_data(start_date, end_date) |>
            mutate(
                label = glue(
                    paste0(
                        "Dagsetning: {time}<br>",
                        "Stærð: {magnitude}"
                    )
                )
            ) |>
            arrange(magnitude)
    }) |>
        bindEvent(
            input$go,
            ignoreNULL = FALSE
        )

    output$p <- renderLeaflet({
        d <- plot_data() |>
            filter(magnitude >= input$min_magnitude)

        validate(
            need(nrow(d) >= 1, "Enginn jarðskjálfti fannst með völdum leitarskilyrðum")
        )

        pal <- colorNumeric(
            "Reds",
            domain = c(0, 7),
            reverse = FALSE
        )

        if (input$cluster == TRUE) {
            clust_opt <- markerClusterOptions()
        } else {
            clust_opt <- NULL
        }

        d |>
            leaflet() |>
            addProviderTiles(providers$OpenStreetMap.HOT) |>
            addCircleMarkers(
                clusterOptions = clust_opt,
                fillColor = ~ pal(magnitude),
                color = "black",
                weight = 1,
                fillOpacity = ~ 1 + 2 * (magnitude),
                radius = ~ 1 + 2 * (magnitude),
                label = ~ lapply(label, HTML)
            )
    })
}

shinyApp(ui, server)
