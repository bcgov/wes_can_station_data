## R Shiny app to download processed and quality controlled stations data of western north america
## author: Aseem Raj Sharma aseem.sharma@gov.bc.ca
# Copyright 2023 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Required -------------------
library('shiny')
library('shinydashboard')
library('shinyWidgets')
library("shinythemes")
library("shinyjs")
library('shinyalert')
library('markdown')
library('rmarkdown')
library('knitr')
library('gt')

library('sf')
library('leaflet')

library('tidyverse')
library('magrittr')
library('plotly')

library('lubridate')

library('viridisLite')
library('cptcity')
library('RColorBrewer')
library('colorspace')

# Load and process input data -------
## Paths --
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
shp_fls_pth <- './shapefiles/'
data_path <-  './wes_can_stn_data/'

# Credit  -----
plt_wtrmrk <-
  "Created by Aseem Sharma (aseem.sharma@gov.bc.ca), BC Ministry of Forests."
plt_wtrmrk

## Shape files --------------
# List of shape files
list.files(path = shp_fls_pth,
           pattern = ".shp",
           full.names = T) -> shp_fls_lst
shp_fls_lst

# Western Canada
wca_shp <-
  st_read(shp_fls_lst[str_detect(shp_fls_lst, "western_can") == T])
sf_use_s2(FALSE)

# plot(st_geometry(wca_shp))
wca_shp <- sf::st_cast(wca_shp, "MULTIPOLYGON")

# Stations metadata  files -------
list.files(path = data_path,
           pattern = glob2rx('00_*.csv'),
           full.names = T) -> stn_mtdt_fls
stn_mtdt_fls

mtdt_1 <- read_csv(stn_mtdt_fls[[1]])
mtdt_1%<>%
  dplyr::select(prov,stn = station_id, stn_nam = station_name, lat,lon, ele=elev)%<>%
  mutate(agency = 'ECCC')%<>%
  mutate(stn = as.character(stn))

mtdt_2 <- read_csv(stn_mtdt_fls[[2]])
mtdt_2%<>%
  dplyr::select(stn,stn_nam, lat,lon,ele,agency =network)%<>%
  mutate(prov = "BC")%<>%
  mutate(stn = as.character(stn))

stn_mtdt <- bind_rows(mtdt_1,mtdt_2)
stn_mtdt %<>%
  distinct(stn, .keep_all = T)%<>%
  drop_na(ele)
stn_mtdt
tail(stn_mtdt)

stns <- stn_mtdt$stn
agency <- stn_mtdt$agency

# Create elevation interval
stn_mtdt%<>%
  mutate(
    ele_int = case_when(
      ele < 500 ~ "<500",
      (ele >= 500 & ele < 1000) ~ "500-1000",
      (ele >= 1000 &
         ele < 1500) ~ "1000-1500",
      (ele >= 1500 &
         ele < 2000) ~ "1500-2000",
      ele >= 2000 ~ ">2000",
      TRUE ~ NA
    )
  ) %>%
  arrange(ele) %>%
  mutate(ele_int = factor(
    ele_int,
    levels = c("<500", "500-1000", "1000-1500", "1500-2000", ">2000", NA)
  ))

# Metadata leaflet plot
ele_pal <- colorFactor(
  palette = 'Set1',
  domain = stn_mtdt$ele_int
)


## Stations data files  -----
list.files(path = data_path,
           pattern = "*.csv",
           full.names = T) -> stns_dt_fls_lst
stns_dt_fls_lst
head( stns_dt_fls_lst)

stns_dt_fls <- tibble(dt_pth = stns_dt_fls_lst)

stns_dt_fls %<>%
  mutate(fl_nam = basename(dt_pth)) %<>%
  mutate(stn = str_extract(fl_nam, paste(stns[order(-nchar(stns))], collapse = "|")),
         agency = str_extract(fl_nam, paste(agency[order(-nchar(agency))], collapse = "|")))%<>%
  dplyr::select(!fl_nam)%<>%
  drop_na()
stns_dt_fls%<>%
  mutate(stn_ag = paste0(stn,"-",agency))

# Merge data path with meta data file
stns_dt_mtdt_fl <- full_join(stns_dt_fls,stn_mtdt)

stns_dt_mtdt_fl%<>%
  drop_na(dt_pth)%<>%
  drop_na(ele)
stns_dt_mtdt_fl
tail(stns_dt_mtdt_fl)

#  UI ----
ui <- fluidPage(
  navbarPage(
    id = "wes_can_stn_data",
    title = "Western Canada Stations Data",
    theme = "bcgov.css",

    ## App page ----
    tabPanel(
      title = "Western Canada Stations Data",
      sidebarLayout(
        sidebarPanel(
          shinyjs::useShinyjs(),
          id = "selection-panel",
          # style = "position:fixed; width:24%; max-height: 100vh;",
          width = 4,
          # Background text  -----
          fluidRow(column(
            HTML("<h4><b>Western Canada Stations Data app  </b> </h4>"),
            helpText(HTML(" The <i> wes_can_stn_data_app </i>   provides access to preliminary
                          quality-controlled daily climate data (minimum and maximum temperatures and precipitation)
                          for 2,583 stations across Western Canada (BC, AB, NT and YK).
                          Sourced from Environment and Climate Change Canada (ECCC) and
                          other agencies (BC-specific),  the app offers daily data that has
                          at least 10 years of data availability. Users can explore, visualize,
                          and download station data streamlining data analysis and quality control processes.",)),
            width = 12,
          )),

          ##### filters -----
          # Location map for station selection and selection input -----
          fluidRow(column(
            HTML("<h4><b>Stations location </b> </h4>"),
            helpText(HTML("<h5> Please click on the station for data and plot. </h5>",)),
            title = "Map Location",
            width = 12,
            leafletOutput("loc_map", height = "50vh")
          )),
          br(),
          br(),
          br(),
          fluidRow(column(
            HTML("<h4><b>Description </b> </h4>"),
            helpText(HTML("<h5> Please read <i> Readme file </i>  below for details </h5>",)),
            title = "readmefile",
            width = 12,
            uiOutput("Readmedoc", height = "40vh")
          )),
          # Creation and code of the app  -----
          fluidRow(column(
            width = 12,
            HTML(
              "<h5> <u>App created by:</u>
             <br>
             <b>Aseem R. Sharma, PhD</b><br>
              Research Climatologist<br>
              FFEC, FCCSB, OCF, BC Ministry of Forests<br>
              <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> <br>
              <br>
              <h5><b>Code</b></h5>
              <h5> The code and data of this app are available through GitHub at <a href='XXXXXX'> XXXX.</a></h5>"
            )
          ),
          column(width = 12,
                 textOutput("deploymentDate"),))
        ),

        mainPanel(
          tags$head(tags$style(HTML(
            '.box {margin: 25px;}'
          ))),
          width = 8,
          fluidRow(column(
            HTML("<h4><b>Station's meta data </b> </h4>"),
            title = "Meta data",
            width = 12,
            gt_output("stn_mtdt_tbl")
          )),
          br(),

          ##### time series plots ----
          column(width = 12,
                 wellPanel(
                   HTML("<h4><b>Time series plot and data </b> </h4>")
                 )),
          fluidRow(column(
            width = 12,
            offset = 0.5,
            tabBox(
              width = 12,
              tabPanel(
                status = 'primary',
                title = "Daily time series plot",
                plotlyOutput("timeseriesdataplot", height = "60vh"),
                #### Download plot and data ----
                downloadButton(outputId = "download_dly_ts_plt",
                               label = "Download plot"),
                downloadButton(outputId = "download_qc_dly_ts_data",
                               label = "Download daily time series data"),
              ),
              # tabPanel(
              #   status = 'primary',
              #   title = "Timeseries line plot",
              #   echarts4rOutput("echarts_trn_plt", height = "50vh")
              # )
            )
          )),
        ),
      ),
      ##### footer -----
      HTML("<br>",
           "<br>"),
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;position:relative;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    ),

    ## Feedback and links ----
    tabPanel(
      title = "Feedback & Links",
      value = "feed_link",
      column(width = 12,
             wellPanel(HTML(
               "<h3><b>Feedback</h3>"
             )), fluidRow(
               box(
                 width = 12,
                 status = 'primary',
                 # title = "Note",
                 uiOutput("feedback_text"),
               )
             )),
      column(
        width = 12,
        wellPanel(HTML("<h4><b>Links to other app </h4>")),
        HTML(
          "<h5><b>Here are the links to other apps developed in FFEC.</b></h5>
            <a href= 'https://bcgov-env.shinyapps.io/bc_climate_extremes_app/'> bc_climate_extremes app </a>
           <br>
          <a href= 'https://bcgov-env.shinyapps.io/bc_climate_anomaly/'> bc_climate_anomaly app </a>
           <br>
          <a href= 'https://bcgov-env.shinyapps.io/cmip6-BC/'> CMIP6-BC </a>
                               <br>
          <br>"
        )
      ),
      ###### footer ----
      column(
        width = 12,
        style = "background-color:#003366; border-top:2px solid #fcba19;",
        tags$footer(
          class = "footer",
          tags$div(
            class = "container",
            style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
            tags$ul(
              style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home", "Home", style = "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/disclaimer", "Disclaimer", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/privacy", "Privacy", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/accessibility", "Accessibility", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/gov/content/home/copyright", "Copyright", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              ),
              tags$li(
                a(href = "https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html", "Contact", style =
                    "font-size:1em; font-weight:normal; color:white; padding-left:5px; padding-right:5px; border-right:1px solid #4b5e7e;")
              )
            )
          )
        )
      )
    )
  )
)


# Define server and reactive contents from data

# Server ----
server <- function(session, input, output) {
  options(warn = -1)
   # Location map with stations for selection ------------

  output$loc_map <- renderLeaflet({

  leaflet() %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
      addLayersControl(baseGroups = c("Default", "Satellite"), options = layersControlOptions(collapsed = FALSE)) %>%
      addProviderTiles("Esri.WorldTopoMap")%>%
      addPolygons(data= wca_shp,
                  color = 'grey',
                  fill=NA,
                  opacity = 1,
                  fillOpacity = 0.65,
                  weight = 1.5,
                  dashArray = '3',
                  smoothFactor = 1)%>%
      addCircleMarkers(
    data = stns_dt_mtdt_fl,
    ~lon, ~lat, ~stn,
    label = stns_dt_mtdt_fl$stn,
        radius = 8.0,
        fillColor = ~ ele_pal(ele_int),
        stroke = FALSE,
        fillOpacity = 0.8,
        popup = ~ paste(
          "Station:",
          stns_dt_mtdt_fl$stn,
          "<br>",
          "Ele:",
          stns_dt_mtdt_fl$ele,
          "<br>",
          "agency:",
          stns_dt_mtdt_fl$agency
        )
      )%>%
      addPolygons(data= wca_shp,
                  color = 'grey',
                  fill=NA,
                  opacity = 1,
                  fillOpacity = 0.65,
                  weight = 1.5,
                  dashArray = '3',
                  smoothFactor = 1)%>%
      addLegend(
        data = stns_dt_mtdt_fl,
        "bottomright",
        pal = ele_pal,
        values = ~ ele_int,
        labels = "Elevation",
        title = "Station elevation"
      )

  })

  # Generate ractive station data -----
  # Station data
  stn_data_dtval <- reactive({
    req(input$loc_map_marker_click$id)
    stn <- input$loc_map_marker_click$id
    # stn <- '1E08P'
    # stn <- '27118'
    sel_stn_mtdt <- stns_dt_mtdt_fl[stns_dt_mtdt_fl$stn %in% stn,]
    sel_stn_dt <- read_csv(sel_stn_mtdt$dt_pth)

    if ("stnid" %in% names(sel_stn_dt)) {
      names(sel_stn_dt)[names(sel_stn_dt) == "stnid"] <- "stn"
    }

    if ("per_na_tmax" %in% names(sel_stn_dt)) {
      names(sel_stn_dt)[names(sel_stn_dt) == "per_na_tmax"] <- "tmax_na_per"
    }

    if ("per_na_tmin" %in% names(sel_stn_dt)) {
      names(sel_stn_dt)[names(sel_stn_dt) == "per_na_tmin"] <- "tmin_na_per"
    }

    if ("per_na_prcp" %in% names(sel_stn_dt)) {
      names(sel_stn_dt)[names(sel_stn_dt) == "per_na_prcp"] <- "prcp_na_per"
    }

    sel_stn_dt%<>%
      dplyr::select(stn,date,tmin,tmax,prcp,tmin_na_per, tmax_na_per, prcp_na_per)
    sel_stn_dt

  })

  stn_data_dtinf <- reactive({
    req(input$loc_map_marker_click$id)
    stn <- input$loc_map_marker_click$id
    sel_stn_mtdt <- stns_dt_mtdt_fl[stns_dt_mtdt_fl$stn %in% stn,]
    sel_stn_mtdt

  })

  # Create a meta data table and  time series plot to show the data and missing gap
 # Metadata table ----------

  output$stn_mtdt_tbl <- render_gt({
    sel_stn_dt_val <- stn_data_dtval()
    sel_stn_dt_inf <- stn_data_dtinf()

    sel_stn_dt_val$stn <- as.character(sel_stn_dt_val$stn)


    # sel_stn_dt_val <- sel_stn_dt
    # sel_stn_dt_inf <- sel_stn_mtdt

    sel_stn_dt_inf$tmin_na_per <- unique(sel_stn_dt_val$tmin_na_per)
    sel_stn_dt_inf$tmax_na_per <- unique(sel_stn_dt_val$tmax_na_per)
    sel_stn_dt_inf$prcp_na_per <- unique(sel_stn_dt_val$prcp_na_per)
    sel_stn_dt_inf$str_dt <- min(sel_stn_dt_val$date)
    sel_stn_dt_inf$end_dt <- max(sel_stn_dt_val$date)
    sel_stn_dt_inf$no_yr <- year(max(sel_stn_dt_val$date)) - year(min(sel_stn_dt_val$date))


    sel_stn_dt_inf%<>%
      dplyr::select(stn,stn_nam,lat,lon,ele, agency,str_dt,end_dt,no_yr,prcp_na_per,tmin_na_per,tmax_na_per)
    sel_stn_dt_inf

    # Make a table
    stn_mtdt_tbl <-
      sel_stn_dt_inf |>
      gt()|>
      tab_spanner(
        label = "Missing percentage (%)",
        columns = c(prcp_na_per, tmin_na_per, tmax_na_per)
      ) |>
      cols_label(
        stn = "Station",
        lat = "Latitude",
        lon = "Longitude",
        ele = "Elevation (m)",
        agency = "Agency",
        str_dt = 'Start date',
        end_dt = 'End date',
        no_yr = "Number of years",
        stn_nam = "Station long name",
        prcp_na_per = "Precipitiaton",
        tmin_na_per = "Minimum temperature",
        tmax_na_per = "Maximum temperature"
      )|>
      tab_header(
        title = md("*Table: Stations metadata*")
      ) |>
      # tab_options(column_labels.background.color = "black")|>
      # data_color()|>
      tab_options(
        table_body.hlines.style = "none",
        column_labels.border.top.color = "blue",
        column_labels.border.bottom.color = "blue",
        table_body.border.bottom.color = "blue"
      )
    stn_mtdt_tbl

  })

   # Time series plot of the quality controlled prcp, tmin and tmax --------

  #Create a reactive plot
 qc_controlled_dly_ts_plt <- reactive({
   sel_stn_dt_val <- stn_data_dtval()
   sel_stn_dt_inf <- stn_data_dtinf()

   stnid <- sel_stn_dt_inf$stn
   lon <- sel_stn_dt_inf$lon
   lat <- sel_stn_dt_inf$lat
   ele <- sel_stn_dt_inf$ele
   agency <- sel_stn_dt_inf$agency
   end_dt <- max(sel_stn_dt_val$date)

   sel_stn_dt_val%>%
     pivot_longer(cols = c('tmin','tmax','prcp'),names_to = 'par',values_to = 'val') -> sel_stn_dt_val_l
   sel_stn_dt_val_l

   sel_stn_dt_val_l$temp <- ifelse(sel_stn_dt_val_l$par == 'tmin' |sel_stn_dt_val_l$par == 'tmax','temp',sel_stn_dt_val_l$par)

   annotation_data <- data.frame(
     par = 'tmin',  # Specify the panel where the text should appear
     date = end_dt,  # x-position for the annotation
     val = -Inf  # y-position for the annotation
   )

   qc_prcp_temp_plt <- sel_stn_dt_val_l %>%
     ggplot(.) +
     geom_text(
       data = annotation_data,
       aes(x = date, y = val, label = plt_wtrmrk),
       inherit.aes = FALSE,  # Ensure this layer doesn't use the main plot aesthetics
       hjust = 1,
       vjust = -0.5,
       color = 'gray80',
       size = 2.0
     ) +
     geom_line(aes(x = date, y = val), color = 'navy') +
     facet_wrap(. ~ par, scales = 'free_y', ncol = 1,
                labeller = as_labeller(c('prcp'='Precipitation (mm)',
                                         'tmin' ='Minimum temperature (°C)',
                                         'tmax' ='Maximum temperature (°C)'))) +
     theme_bw() +
     scale_x_date(
       date_breaks = "1 year",
       date_minor_breaks = "1 month",
       date_labels = "%Y-%m"
     ) +
     labs(title = paste0('StationID: ',stnid,' ',agency,'_daily_prcp_tmin_tmax',
                         ' [','Lon: ',lon,'°, ','Lat: ',lat,'°, ','Elev: ',ele,'m]'))+
     theme(axis.text.x = element_text(
       angle = 90,
       hjust=0.5,vjust=0.5),
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       plot.title = element_text(size = 12, face = "bold", margin = margin(b = 10)))
   qc_prcp_temp_plt

})

  output$timeseriesdataplot <- renderPlotly({
    qc_controlled_dly_ts_plt()
  })

  # Download plot and data -----
  # Data to download
  dwnld_data_fl <- reactive({

    sel_stn_dt_val <- stn_data_dtval()
    sel_stn_dt_inf <- stn_data_dtinf()
    # sel_stn_dt_val <-  sel_stn_dt

    sel_stn_dt_val$stn <- as.character(sel_stn_dt_val$stn)

    sel_stn_dt_val$station  <- sel_stn_dt_inf$stn_nam
    sel_stn_dt_val$agency  <- sel_stn_dt_inf$agency
    sel_stn_dt_val$lat  <- sel_stn_dt_inf$lat
    sel_stn_dt_val$long  <- sel_stn_dt_inf$lon
    sel_stn_dt_val$ele  <- sel_stn_dt_inf$ele
    sel_stn_dt_val$str_dt  <- min(sel_stn_dt_val$date)
    sel_stn_dt_val$end_dt  <- max(sel_stn_dt_val$date)
    sel_stn_dt_val$no_yr  <- year(max(sel_stn_dt_val$date)) - year(min(sel_stn_dt_val$date))

    sel_stn_dt_val%>%
      dplyr::select(stationID = stn,
                    station_name = station,
                    agency =agency,
                    lat = lat,
                    long = long,
                    elev = ele,
                    date,
                    tmax, tmin,prcp,
                    start_date = str_dt,
                    end_date = end_dt,
                    no_yr = no_yr,
                    mis_tmax_perc = tmax_na_per,
                    mis_tmin_per = tmin_na_per,
                    mis_prcp_per = prcp_na_per)

  })

# Download file name

  dwnld_file_nam_info <- reactive({

    sel_stn_dt_val <- stn_data_dtval()
    sel_stn_dt_inf <- stn_data_dtinf()

    stn_id <- unique(sel_stn_dt_inf$stn)
    stn_ag <- unique(sel_stn_dt_inf$agency)
    str_dt <-  unique(sel_stn_dt_inf$str_dt)
    end_dt <-  unique(sel_stn_dt_inf$end_dt)

    fl_nam <-
      paste0(stn_id,'_',stn_ag,'_daily_qc_prcp_tmin_tmax_data_',str_dt,'_',end_dt)
    fl_nam
  })

# Save daily time series data as a csv

  output$download_qc_dly_ts_data <- downloadHandler(
    filename = function(file) {
      paste0(dwnld_file_nam_info(),
             ".csv")
    },
    content = function(file) {
      write_csv(dwnld_data_fl(),
                file, append = FALSE)
    }
  )

  # Save daily time series plot as png

  output$download_dly_ts_plt <- downloadHandler(
    filename = function(file) {
      paste0(dwnld_file_nam_info(),
             ".png")
    },
    content = function(file) {
      ggsave(
        file,
        plot =  qc_controlled_dly_ts_plt(),
        width = 12,
        height =9,
        units = "in",
        dpi = 305,
        scale = 0.7,
        limitsize = F,
        device = "png"
      )
    }
  )

  # Readme file -----
  # HTML in the shiny www folder
  output$Readmedoc <- renderUI({
    a(
      "Readme.pdf",
      target = "_blank",
      style = "font-size:20px;",
      href = "read_me_wes_can_station_data_process_info.pdf",
      img(
        src = "pdf_logo.png",
        height = "4%",
        width = "4%",
        align = "center"
      )
    )
  })

  observeEvent(input$loc_map_marker_click$id, {
    stn <- input$loc_map_marker_click$id
    updateSelectInput(session, "stn", selected = stn)
  })

  # Feedback text -------
  output$feedback_text <- renderText({
    HTML(
      "This app provides daily data from mutiple sources as described in  readme file after some quality controlled.
      Should you have any inquiries or wish to provide feedback, please do not hesitate to use
      <a href=https://forms.office.com/r/TaS60ZtbCh'> this feedback form </a> or write to Aseem Sharma @ <a href= 'mailto: Aseem.Sharma@gov.bc.ca'>Aseem.Sharma@gov.bc.ca</a> . </p>"
    )
  })

  # Deployment date ----
  # App deployment date ----
  output$deploymentDate <- renderText({
    paste0("This app was last updated on ",
           readLines("deployment_history.txt"), '.'
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
