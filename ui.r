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

dsSlim <<- dsSlim[dsSlim$Lea_Name!="Charter and Non-District Affiliated Schools",]
##dsSlim <<- dsSlim[dsSlim$year==2015,]

## Load rds shapefile of school districts
sds <- readRDS(file="sds.rds")
##sds@data$perPup <- NA
##sds <<- readRDS(file="sds.rds")

## for (district in 1:length(dsSlim$Lea_Name)) {
##     if (dsSlim$Lea_Name[district] %in% sds@data$NAME) {
##         matchI <- which(sds@data$NAME==dsSlim$Lea_Name[district])
##         sds@data$perPup[matchI] <- dsSlim$lea_state_perpupil[district]
##     }
## }

sds <<- sds


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="NC School Funding App",
               tabPanel("Percentage Graphs",
                        fluidRow(
                            column(5,
                                   sliderInput("year1", "Select Year",
                                               min=2002, max=2016, value=2016,
                                               animate=TRUE)
                                   ),
                            column(4,
                                   selectInput("sort1", "Sort By: ",
                                               c("Alphabetical" = "a",
                                                 "Salary Pct" = "b",
                                                 "Benefits Pct" = "c")
                                               )## ,
                                   ## selectizeInput(inputId="hl1",
                                   ##                label="Highlight: ",
                                   ##                choices=unique(dsSlim$Lea_Name),
                                   ##                multiple=TRUE
                                   ##             )
                                   ),
                            column(3,
                                   br(),
                                   checkboxInput("rev1", "Reverse?",
                                                 value=FALSE),
                                   align = "left"
                                   )
                            ),
                        fluidRow(
                            plotlyOutput("bar2", height=650)
                            )
                        ),
               tabPanel("Per Pupil Graphs",
                        fluidRow(
                            column(5,
                                   sliderInput("year2", "Select Year",
                                               min=2002, max=2016, value=2016,
                                               animate=TRUE)
                                   ),
                            column(6,
                                   selectInput("sort2", "Sort By: ",
                                               c("Alphabetical" = "a",
                                                 "Federal" = "b",
                                                 "State" = "c",
                                                 "Local" = "d")
                                               ),
                                    checkboxInput("rev2", "Reverse?",
                                                 value=FALSE)
                                   )
                            ),
                        fluidRow(
                            plotlyOutput("bar3", height=650)
                            )
                        ),
               tabPanel("Map",
                        fluidRow(
                            column(2),
                            column(6,
                                   selectizeInput("mapVar", "Map: ",
                                               choices=c("Local" = "lea_state_perpupil_num",
                                                   "State"= "sta",
                                                   "Federal" = "fed"),
                                                  selected="Local"
                                       )
                                   )
                            ),
                        h2("State"),
                        leafletOutput("map1"),
                        br(),
                        h2("Local"),
                        leafletOutput("map2"),
                        br(),
                        h3("Federal"),
                        leafletOutput("map3")
                        )
               )
    )




