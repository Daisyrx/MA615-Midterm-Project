#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
pacman::p_load(knitr,tidyverse, png, grid,reshape2,naniar,plotly,readxl)

######################################
# Read the dataset
climate <- read_excel("climate_change_download_0.xls",sheet = 1)
countries <- read_excel("climate_change_download_0.xls",sheet = 2)

# choose variables that I want to take a look at 
chosen <- climate %>% filter(`Series code` %in%  c("NY.GDP.MKTP.CD","NY.GNP.PCAP.CD","EG.USE.PCAP.KG.OE","SP.POP.TOTL","EN.ATM.CO2E.PC","EN.ATM.CO2E.KT","SP.POP.GROW"))
chosen <- chosen[,-c(5,6,27,28)]

#######################################
# Clean the data
chosen_melt <- melt(chosen,id=c("Country code","Country name","Series code","Series name"))
colnames(chosen_melt) <- c("CountryCode","CountryName","SeriesCode","SeriesName","Year","Value")

# replace all ".." cells with NA
chosen_melt <- chosen_melt %>% replace_with_na(replace = list(Value = ".."))
chosen_melt$Value <- as.numeric(chosen_melt$Value)%>%round(2)

# Make each variable as one column
chosen_spread <- spread(chosen_melt,key=SeriesName,value = Value)
chosen_spread <- chosen_spread[,-c(1,3)]

################################################
## Take a look at each variable one by one
################################################

# Energy use per capita (kilograms of oil equivalent)	
energy <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "Energy use per capita (kilograms of oil equivalent)")

# Take a look at the ggplot
ggplot(energy,aes(Year,Value,color = Year)) + geom_point() + ylab("Energy Use")

# Total World Energy use per capita (kilograms of oil equivalent)
energy_tot <- aggregate(energy[,4],by = list(energy$Year),FUN = sum,na.rm=TRUE)
colnames(energy_tot) <- c("Year","EnergyUse")

# Take a look at how energy usage changed over decades
energy_tot$Year <- as.numeric(as.character(energy_tot$Year))
energyplot <- plotly_build(ggplot(energy_tot) + geom_line(aes(Year,EnergyUse),stat = "identity") + geom_point(aes(Year,EnergyUse)) + xlab("Year") + ylab("Energy Use"))

##############################################
# CO2 emissions, total (KtCO2)
CO2Total <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "CO2 emissions, total (KtCO2)",Year != 2009)

# Take a look at the ggplot
ggplot(CO2Total,aes(Year,Value,color = Year)) + geom_point() + ylab("World CO2 Emissions(kt)")

# World CO2 emissions, total (KtCO2)
CO2_tot <- aggregate(CO2Total[,4],by = list(CO2Total$Year),FUN = sum,na.rm=TRUE)
colnames(CO2_tot) <- c("Year","World CO2 Emissions(kt)")

# Take a look at how total CO2 emissions changed over decades
CO2_tot$Year <- as.numeric(as.character(CO2_tot$Year))
CO2plot <- plotly_build(ggplot(CO2_tot) + geom_line(aes(Year,`World CO2 Emissions(kt)`),stat = "identity") + geom_point(aes(Year,`World CO2 Emissions(kt)`)) + xlab("Year") + ylab("World CO2 Emissions(kt)"))

##############################################
# CO2 emissions per capita (metric tons)
CO2_capita <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "CO2 emissions per capita (metric tons)",Year != 2009)

# Take a look at the ggplot
ggplot(CO2_capita,aes(Year,Value,color = Year)) + geom_point() + ylab("World CO2 Emissions per capita")

# World CO2 emissions per capita (metric tons)
CO2_capita_tot <- aggregate(CO2_capita[,4],by = list(CO2_capita$Year),FUN = sum,na.rm=TRUE)
colnames(CO2_capita_tot) <- c("Year","World CO2 Emissions per capita")

# Take a look at how world CO2 emissions per capita changed over decades
CO2_capita_tot$Year <- as.numeric(as.character(CO2_capita_tot$Year))
plotly_build(ggplot(CO2_capita_tot) + geom_line(aes(Year,`World CO2 Emissions per capita`),stat = "identity") + geom_point(aes(Year,`World CO2 Emissions per capita`)) + xlab("Year") + ylab("World CO2 Emissions per capita"))

###################################
# GDP ($)
GDP <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "GDP ($)")

# Take a look at the ggplot
ggplot(GDP,aes(Year,Value,color = Year)) + geom_point()

####################################
# GNI per capita (Atlas $)	

GNI <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "GNI per capita (Atlas $)")

# Take a look at the ggplot
ggplot(GNI,aes(Year,Value,color = Year)) + geom_point()

####################################
# Population growth (annual %)
popgrowth <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "Population growth (annual %)")

# Take a look at the ggplot
ggplot(popgrowth,aes(Year,Value,color = Year)) + geom_point() + ylab("Population Growth")

# World Population
popgrowth_tot <- aggregate(popgrowth[,4],by = list(popgrowth$Year),FUN = mean,na.rm=TRUE)
colnames(popgrowth_tot) <- c("Year","PopulationGrowth")

# Take a look at how world population changed over decades
popgrowth_tot$Year <- as.numeric(as.character(popgrowth_tot$Year))
plotly_build(ggplot(popgrowth_tot) + geom_line(aes(Year,`PopulationGrowth`),stat = "identity") + geom_point(aes(Year,`PopulationGrowth`))+ xlab("Year") + ylab("Population Growth"))

####################################
# Population
pop <- chosen_melt%>%select(CountryName,SeriesName,Year,Value)%>%filter(SeriesName == "Population")

# Take a look at the ggplot
ggplot(pop,aes(Year,Value,color = Year)) + geom_point() + ylab("Population")

# World Population
pop_tot <- aggregate(pop[,4],by = list(pop$Year),FUN = sum,na.rm=TRUE)
colnames(pop_tot) <- c("Year","Population")

# Take a look at how world population changed over decades
pop_tot$Year <- as.numeric(as.character(pop_tot$Year))
plotly_build(ggplot(pop_tot) + geom_line(aes(Year,`Population`),stat = "identity") + geom_point(aes(Year,`Population`)) + xlab("Year") + ylab("Population"))

# It is interesting finding that though the world population is growing every year, the annual population growth rate has a decreasing trend.

####################################################
#Take a look at population and CO2 emissions
pop_co2 <- chosen_melt%>%filter(SeriesName %in% c("CO2 emissions, total (KtCO2)","Population"),Year != 2009)%>%spread(key=SeriesName,value = Value)

pop_world <- aggregate(pop_co2[,5:6],by = list(pop_co2$Year,pop_co2$SeriesCode),FUN = sum,na.rm=TRUE)
colnames(pop_world) <- c("Year","SeriesCode","CO2","Population")

pop_world$`CO2` <- as.numeric(as.character(pop_world$CO2))
pop_world$Population <- as.numeric(as.character(pop_world$Population))

ggplot(data = pop_world) + stat_summary(mapping = aes(y = Population, x = Year),fun.ymin = min,fun.ymax = max,fun.y = median)

ggplot(data = pop_world) + stat_summary(mapping = aes(y = CO2, x = Year),fun.ymin = min,fun.ymax = max,fun.y = median)
RL <- chosen_melt%>%sample_n(10,replace = FALSE)%>%dplyr::select(`CountryName`)%>%pull()%>%unique()
#################################################
# prepare for shiny app
SeriesName <- colnames(chosen_spread[,3:9])

#################################################
ui <- fluidPage(

    tabPanel("Climate Change Data",
    titlePanel("Climate Change"),
    sidebarLayout(
        sidebarPanel(
            selectInput("SeriesName",label = "Series Name",choices = SeriesName),
            actionButton("resample","Click to show")
        ),

        mainPanel(
            plotlyOutput("Climateplot")
        )
    )
    )
)

server <- function(input, output) {
    RL <- eventReactive(input$resample,{
        RL <- chosen_melt%>%sample_n(10,replace = FALSE)%>%dplyr::select(`CountryName`)%>%pull()%>%unique()
        RL
    })
    
    PP <- reactive({
        PP <- chosen_melt%>%
            dplyr::filter(SeriesName %in% input$SeriesName,`CountryName` %in% RL())%>%
            dplyr::mutate(Year = as.numeric(as.character(Year)))
        PP
    })

    output$Climateplot <- renderPlotly({
        p <- ggplot(PP())+ 
            geom_line(aes(Year,Value,color = `CountryName`)) +
            xlab("Year") 
        
        plotly_build(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
