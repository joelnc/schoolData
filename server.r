library(shiny)

## Define server logic reqd to draw hist
shinyServer(
    function(input, output) {


        ## Bar plot reacvie daata
        fundingDataset1 <- reactive({
            dsSlim %>%
                filter(year == input$year1)
        })


        ## Data
        output$bar2 <- renderPlotly({

            if (input$sort1 == "a") {
                xform <- list(categoryorder = "array",
                              categoryarray = fundingDataset1()$Lea_Name[order(fundingDataset1()$Lea_Name)])
            } else if (input$sort1 == "b") {
                xform <- list(categoryorder = "array",
                              categoryarray = fundingDataset1()$Lea_Name[order(fundingDataset1()$Lea_Name,
                                  decreasing=TRUE)])

            } else if (input$sort1 == "c") {
                xform <- list(categoryorder = "array",
                              categoryarray = fundingDataset1()$Lea_Name[order(fundingDataset1()$lea_salary_expense_pct)])
            } else if (input$sort1 == "d") {
                xform <- list(categoryorder = "array",
                              categoryarray = fundingDataset1()$Lea_Name[order(fundingDataset1()$lea_benefits_expense_pct)])
            }


                p <- plot_ly(fundingDataset1(), x=~Lea_Name, y=~lea_salary_expense_pct,
                             name="Salary", type="bar") %>%
                                 add_trace(y=~lea_benefits_expense_pct,
                                           name="Benefits") %>%
                                 add_trace(y=~lea_services_expense_pct,
                                           name="Services") %>%
                                 add_trace(y=~lea_supplies_expense_pct,
                                           name="Supplies") %>%
                                 add_trace(y=~lea_instruct_equip_exp_pct,
                                           name="Intruc. Equip.") %>%
                                 add_trace(y=~lea_other_expense_pct,
                                           name="Other") %>%
                                 layout(barmode="stack",
                                        xaxis=xform)


            p

        })

        ## Bar plot reactive data
        fundingDataset2 <- reactive({
            dsSlim %>%
                filter(year == input$year2)
        })

        ## Data
        output$bar3 <- renderPlotly({

            p <- plot_ly(fundingDataset2(), x=~Lea_Name, y=~lea_federal_perpupil_num,
                             name="Fed", type="bar") %>%
                                 add_trace(y=~lea_state_perpupil_num,
                                           name="State") %>%
                                 add_trace(y=~lea_local_perpupil_num,
                                           name="Local") %>%
                     layout(barmode="stack")

            p

        })

        bins <- c(5000,6000,7000,8000,9000,10000,11000,12000,Inf)
        ##bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
        pal <- colorBin("YlOrRd", domain = sds$lea_state_perpupil_num, bins = bins)

        ## Map
        output$map1 <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers$OpenStreetMap.Mapnik, group="Streets") %>%
                addPolygons(data=sds, group="Districts",
                            weight=2, opacity=1, fillOpacity=0.5,
                            popup="test", fillColor=~pal(dsSlim$lea_state_perpupil_num))
        })



    }) ## Done

