library(shiny)
library(bs4Dash)
library(echarts4r)

library(tidyverse)

library(ggvis)

umap_data<-readRDS(paste0("data/umap_data.Rds"))


# echarts_dark_theme <- list(
#     options = '{
#     "color":["#6610f2", "#ffc107", "#e83e8c", "#ff851b", "#17a2b8", "#3d9970"], 
#     "backgroundColor": "#343a40", 
#     "textStyle": {
#         color: "#fff"
#     },
#     .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {color: red}
#   }',
#   name = "dark_theme"
# )

ui <- dashboardPage(
    #fullscreen = TRUE,
    header=dashboardHeader(
      tags$style(HTML('.js-irs-0 .irs-bar {border-top-color: #80ba24;border-bottom-color: #80ba24;} 
                        .js-irs-0 .irs-bar-edge {border-color: #80ba24;}
                        .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-from, .js-irs-0 .irs-to {background: #80ba24;}
                        /* body */.content-wrapper, .right-side {background-color: #ffffff;}
                        box-body {padding-top: 0px;}
                      '))),
    dashboardSidebar(disable = T),
    dashboardBody(
        skin = "light",
                    box(title="UMAP decomposition of documents",
                        width=12,
                        height="90vh",
                        overflow = F,
                        ggvisOutput("umap_plot"),
                        sidebar=boxSidebar(
                          skin = "light",
                          width = 25,
                          tabName = "Filter",
                          id="Filter",
                          sliderInput("year", "Year released", 1977, 2021, value = c(1977, 2021),sep = ""),
                          selectInput("type", "Type",c("All",unique(umap_data$type))),
                          selectInput("cb", "central bank",c("All",unique(umap_data$cb))),
                          selectInput("currency", "currency",c("All",unique(umap_data$currency))),
                          textInput("speaker", "Speaker name",placeholder = "e.g. Mario Draghi"),
                          selectInput("color", "Color",c("type","cb","currency","speaker","year"))
                        ),
                        p("UMAP decomposition based on: McInnes, L., Healy, J., & Melville, J. (2018). Umap: Uniform manifold approximation and projection for dimension reduction. arXiv preprint arXiv:1802.03426.")
                     )
        )
    )

server <- function(input, output) {
    tooltip<-function(x){
        if (is.null(x)) return(NULL)
        if (is.null(x$doc_id)) return(NULL)
        
        # Pick out the movie with this ID
        text_all <- isolate(umap_filter())
        text <- text_all[text_all$doc_id == x$doc_id, ]
        
        paste0("<b>", text$type, "</b><br>",
               text$currency, "<br>",
               text$speaker, "<br>",
               format(text$date, big.mark = ",", scientific = FALSE)
        )
    }
    umap_filter  <- reactive({
        minyear <- input$year[1]
        maxyear <- input$year[2]
        
        m <- umap_data %>%
            filter(year >= minyear,
                   year <= maxyear)
        
        if (input$type != "All") {
            Type <- input$type
            m <- m %>% filter(type == Type)
        }
        # Optional: filter by Speaker
        if (input$speaker!= "") {
            Speaker <- input$speaker
            m <- m %>% filter(speaker == Speaker)
        }
        # Optional: filter by Cb
        if (input$cb!= "All") {
            Cb <- input$cb
            m <- m %>% filter(cb == Cb)
        }
        # Optional: filter by Currency
        if (input$currency!= "All") {
            Currency <- input$currency
            m <- m %>% filter(currency == Currency)
        }
        
        m <- as.data.frame(m)
        
        m
    })
    vis <- reactive({
        fill_variable<- prop("fill",as.name(input$color))
        
        umap_filter%>% 
            ggvis(~V1,~V2) %>%
            layer_points(size := 50, size.hover := 200,
                         fillOpacity := 0.2, fillOpacity.hover := 0.5,
                         fill = fill_variable, key := ~doc_id)%>%
            add_tooltip(tooltip, "hover") %>% 
            ggvis::hide_legend('fill')%>%
            add_axis("x", title = "Dimension 1") %>%
            add_axis("y", title = "Dimension 2") %>%
            set_options(width = "auto", height = "auto")
    })
    vis %>% bind_shiny("umap_plot")
}

shinyApp(ui, server)
