library(shiny)

## Load, format, subset, WQD File
ds2 <- read.csv(file="funding.csv", sep=",", stringsAsFactors=FALSE,
                header=T)

ds2$key <- paste(ds2$Lea_Name, ds2$year)
dsSlim <- ds2[!duplicated(ds2$key),]

## Fix names from funding to match names from Tigris
dsSlim$Lea_Name[dsSlim$Lea_Name=="Edgecombe County Public School"] <-
    "Edgecombe County Schools"
dsSlim$Lea_Name[dsSlim$Lea_Name=="Newton Conover City Schools"] <-
    "Newton-Conover City Schools"
dsSlim$Lea_Name[dsSlim$Lea_Name=="Lenoir County Public Schools"] <-
    "Lenoir County Schools"
dsSlim$Lea_Name[dsSlim$Lea_Name=="Carteret County Public Schools"] <-
    "Carteret County Schools"

dsSlim <<- dsSlim
##dsSlim <<- dsSlim[dsSlim$year==2015,]

## Load rds shapefile of school districts
sds <<- readRDS(file="sds.rds")


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="School App",
               tabPanel("Graphs",
                        fluidRow(
                            selectInput("field", "Data to plot:",
                                        names(dsSlim),
                                        selected="lea_total_expense_num"),
                            plotlyOutput("bar2"),
                            sliderInput("year", "Select Year",
                                        min=2002, max=2016, value=2015,
                                        animate=TRUE)
                            ),
                        fluidRow(
                            plotlyOutput("bar3")
                            )

                        ),
               tabPanel("Map",
                        leafletOutput("map1")
                        )
               )
    )




