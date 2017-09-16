library(shiny)

## Load, format, subset, WQD File
ds <- read.csv("test2.csv",
                 stringsAsFactors=FALSE, sep=",", header=TRUE)

ds <<- ds %>%
    mutate(SalFrac = Sal/(Sal+Ben+Inst),
           BenFrac = Ben/(Sal+Ben+Inst),
           InstFrac = Inst/(Sal+Ben+Inst))

ds2 <- read.csv(file="funding.csv", sep=",", stringsAsFactors=FALSE,
                header=T)

ds2$key <- paste(ds2$Lea_Name, ds2$year)
dsSlim <- ds2[!duplicated(ds2$key),]
dsSlim <<- dsSlim[dsSlim$year==2015,]


sds <<- readRDS(file="sds.rds")


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="School App",
               tabPanel("Graphs",
                        fluidRow(
                            selectInput("field", "Data to plot:",
                                        names(dsSlim),
                                        selected="lea_total_expense_num"),

                            plotlyOutput("bar2")
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




