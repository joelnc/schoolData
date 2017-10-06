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
sds@data$perPup <- NA
sds <<- readRDS(file="sds.rds")

for (district in 1:length(dsSlim$Lea_Name)) {
    if (dsSlim$Lea_Name[district] %in% sds@data$NAME) {
        matchI <- which(sds@data$NAME==dsSlim$Lea_Name[district])
        sds@data$perPup[matchI] <- dsSlim$lea_state_perpupil[district]
    }
}

sds <<- sds


## Define UI for applicaiton that draws a hist
shinyUI(
    navbarPage(title="NC School Funding App",
               tabPanel("Percentage Graphs",
                        fluidRow(
                            column(6,
                                   sliderInput("year1", "Select Year",
                                               min=2002, max=2016, value=2016,
                                               animate=TRUE)
                                   ),
                            column(6,
                                   selectInput("sort1", "Sort By: ",
                                               c("Alphabetical" = "a",
                                                 "Rev. Alpha." = "b",
                                                 "Salary Pct" = "c",
                                                 "Benefits Pct" = "d")
                                               )
                                   )
                            ),
                        fluidRow(
                            plotlyOutput("bar2")
                            )
                        ),
               tabPanel("Per Pupil Graphs",
                        sliderInput("year2", "Select Year",
                                    min=2002, max=2016, value=2016,
                                    animate=TRUE),
                        fluidRow(
                            plotlyOutput("bar3")
                            )
                        ),
               tabPanel("Map",
                        leafletOutput("map1")
                        )
               )
    )




