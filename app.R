rm(list = ls())
cat("\014")

library(shiny)
library(ggvis)
library(reshape)
library(tidyr)
library(plyr)
library(ggplot2)

#read in raw data
life <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip=3)[,1:59]
life <- subset(life,select=-c(Indicator.Name, Indicator.Code, Country.Name))
colnames(life) <- c("Country", seq(1960, 2014))

fertility<- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip=3)[,1:59]
fertility <- subset(fertility,select=-c(Indicator.Name, Indicator.Code,Country.Name))
colnames(fertility) <- c("Country", seq(1960, 2014))

pop <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv',skip=3)[,1:59]
pop <- subset(pop,select=-c(Indicator.Name, Indicator.Code,Country.Name))
colnames(pop) <- c("Country", seq(1960, 2014))

region <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')[,1:2]
colnames(region) <- c("Country","Region")

#melt 
life <- melt(life)
colnames(life) <- c("Country", "Year", "Life")

fertility <- melt(fertility)
colnames(fertility) <- c("Country", "Year", "Fertility")

pop <- melt(pop)
colnames(pop) <- c("Country", "Year", "Population")

#combine dfs together
df <- merge(life,fertility,by = c("Country", "Year"))
df <- merge(df,pop,by = c("Country", "Year"))
df <- merge(df,region,by = "Country")
df$Year <- as.integer(as.character(df$Year))

Region_names <- levels(region$Region)

limits <- c()
limits$lifemin = min(df$Life, na.rm=T)
limits$lifemax = max(df$Life, na.rm=T)
limits$fertilitymin = min(df$Fertility, na.rm=T)
limits$fertilitymax = max(df$Fertility, na.rm=T)

ui <- fluidPage(
  headerPanel('World Development Indicators Trend by Country '),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  ),
  sidebarPanel(
    sliderInput(inputId="year", label="Year", min=min(df$Year, na.rm=T), 
                max=max(df$Year, na.rm=T)-1, value=min(df$Year, na.rm=T), step=1, sep='', animate = T),
    checkboxGroupInput("regions", "Choose Regions to display:", choices = Region_names, selected = Region_names,
                       inline = FALSE)
  )
)

server <- function(input, output) {
  sub_df <- reactive({df[df$Year == input$year & df$Region %in% input$Region_names, ]})
  
  output$info <- renderText({
    paste0("year=", input$year, "\nnrow(sub_df)=", nrow(sub_df))
  })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[df$Life == x$Life & df$Fertility == x$Fertility & !is.na(df$Life) & !is.na(df$Fertility), ]
    paste0(row$Country)
  }
  
  sub_df %>% 
    ggvis(x = ~Life, y = ~Fertility, fill = ~factor(Region), size = ~Population) %>%
    layer_points() %>%
    add_axis("x", title = "Life Expectancy (years per human body)") %>%
    add_axis("y", title = "Fertility Rate (babies per woman)") %>%
    add_legend("fill", title="Region", properties = legend_props(legend = list(y = 150))) %>%
    add_legend("size", title="Population", properties = legend_props(legend = list(y = 50))) %>%
    add_tooltip(all_values, "hover") %>%
    scale_numeric("x", domain = c(limits$lifemin, limits$lifemax), nice = T) %>%
    scale_numeric("y", domain = c(limits$fertilitymin, limits$fertilitymax), nice = T) %>%
    set_options(duration=0) %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}

shinyApp(ui = ui, server = server)