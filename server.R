# Deployed to
# https://cschutten.shinyapps.io/project/

shinyServer(function(session, input, output) {
    
    # Change stations available to select based on pollutant selected
    observeEvent(input$pollutant, {
                 if (input$pollutant=="PM2.5") choice = PM2.5
                 else if (input$pollutant=="PM10") choice = PM10
                 else if (input$pollutant=="NO2") choice = NO2
                 else if (input$pollutant=="SO2") choice = SO2
                 updateSelectizeInput(session=session, inputId="stations", choices=choice)
                 })
    
    # Remove options from xaxis plotting if they don't make sense
    observeEvent(input$aggregation, {
                if (input$aggregation %in% c("daily_avg", "thresh_hours_per_day")) choice = xaxis[c(1,2,3)]
                else if (input$aggregation %in% c("yearly_avg", "thresh_hours_per_year", "thresh_days_per_year")) choice = xaxis[1]
                else choice = xaxis
                updateSelectizeInput(session=session, inputId="xaxis", choices=choice)
    })
    
    # Only calculate when button is pressed to reduce unecessary calculations
    observeEvent(input$calculate, {
        output$pollutionplot <- renderPlot( {
            
            pollutant_selected <- isolate(input$pollutant)
            stations_selected <- isolate(input$stations)
            aggregation <- isolate(input$aggregation)
            threshold <- isolate(input$threshold)
            xaxis <- isolate(input$xaxis)
            
            # create data frame based on stations selected
            data = data.frame()
            for (item in stations_selected) {
                print(str_c("Data/", stations$EoICode[stations$StationName==item], "_", pollutant_selected, ".csv"))
                data <- rbind(data,
                              read_csv(str_c("Data/", stations$EoICode[stations$StationName==item],
                                             "_", pollutant_selected, ".csv")))
            }
            
            # add variables to data frame for plotting
            data <- data %>%
                left_join(stations[,c(1,8)], by=c("AirQualityStationEoICode" = "EoICode")) %>%     # join station names
                transform(Date=as.Date(str_c(Day, Month, Year, sep="/"), "%d/%m/%Y")) %>%          # create date field
                transform(Month_Date=as.Date(paste0("2000-",format(Date, "%j")), "%Y-%j")) %>%     # create Month-Day field
                transform(DayOfWeek=factor(weekdays(Date),                                         # create day of the week field
                                           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
                transform(HourOfWeek=(as.integer(DayOfWeek)-1)*24+Hour) %>%                        # create hour of the week field
                group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek, HourOfWeek, Hour) %>%
                summarise(Concentration=mean(Concentration))                                       # gets the average for every date & hour (some hours have multiple readings)
            
            # modify data frame based on aggregation selected
            if (aggregation=="daily_avg") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Concentration=mean(Concentration))
            } else if (aggregation=="yearly_avg") {
                data <- data %>% group_by(StationName, Year) %>%
                    summarise(Concentration=mean(Concentration))
            } else if (aggregation=="daily_max") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    slice(which.max(Concentration))
            } else if (aggregation=="thresh_hours_per_day") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Count=n())
            } else if (aggregation=="thresh_hours_per_year") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
            } else if (aggregation=="thresh_days_per_year") {
                data <- data %>% group_by(StationName, Year, Month, Day) %>%
                    summarise(Concentration=mean(Concentration)) %>%
                    filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
            }
            
            
            plot <- ggplot()
            
            # plot displayed is dependant on xaxis type and aggregation selected
            if (xaxis=="calendar") {
                if (aggregation %in% c("none", "daily_avg", "daily_max")) {
                    plot <- plot + geom_line(data=data, aes(x=Date, y=Concentration, colour=StationName), size=0.8)
                } else if (aggregation=="yearly_avg") {
                    plot <- plot + geom_line(data=data, aes(x=Year, y=Concentration, colour=StationName), size=0.8)
                } else if (aggregation=="thresh_hours_per_day") {
                    plot <- plot + geom_line(data=data, aes(x=Date, y=Count, colour=StationName), size=0.8)
                } else if (aggregation %in% c("thresh_hours_per_year", "thresh_days_per_year")) {
                    plot <- plot + geom_line(data=data, aes(x=Year, y=Count, colour=StationName), size=0.8)
                }
            } else if (xaxis=="year") {
                if (aggregation %in% c("none", "daily_avg", "daily_max")) {
                    plot <- plot + 
                        geom_point(data=data, aes(x=Month_Date, y=Concentration, colour=StationName), size=3, alpha=0.5)
                } else if (aggregation %in% c("thresh_hours_per_day")) {
                    plot <- plot +
                        geom_jitter(data=data, aes(x=Month_Date, y=Count, colour=StationName), size=3, alpha=0.5, width=0.01)
                }
                plot <- plot + scale_x_date(labels = function(x) format(x, "%b"), date_breaks="1 month") + xlab("Seasonal Effect")
            } else if (xaxis=="week") {
                if (aggregation=="none") {
                    plot <- plot + 
                        geom_point(data=data, aes(x=HourOfWeek, y=Concentration, colour=StationName), size=3, alpha=0.5) +
                        xlab("Weekly Effect (Hour of Week)")
                } else if (aggregation %in% c("daily_avg", "daily_max")) {
                    plot <- plot + 
                        geom_jitter(data=data, aes(x=DayOfWeek, y=Concentration, colour=StationName), size=3, alpha=0.5, width=0.25) +
                        xlab("Weekly Effect")
                } else if (aggregation %in% c("thresh_hours_per_day")) {
                    plot <- plot + 
                        geom_jitter(data=data, aes(x=DayOfWeek, y=Count, colour=StationName), size=3, alpha=0.5, width=0.25) +
                        xlab("Weekly Effect")
                }
            } else if (xaxis=="day") {
                if (aggregation %in% c("none", "daily_max")) {
                    plot <- plot +
                        geom_jitter(data=data, aes(x=Hour, y=Concentration, colour=StationName), size=3, alpha=0.5, width=0.15) +
                        xlab("Daily Effect")
                }
            }
            
            # add y-axis labels and titles based on aggregation selected
            if (aggregation=="none") {
                plot <- plot + 
                    ylab(paste("Hourly ", pollutant_selected, " Concentration")) +
                    ggtitle(paste(pollutant_selected, " (hourly)"))
            } else if (aggregation=="daily_avg") {
                plot <- plot +
                    ylab(paste("Daily Average ", pollutant_selected, " Concentration")) +
                    ggtitle(paste(pollutant_selected, " (daily average)"))
            } else if (aggregation=="yearly_avg") {
                plot <- plot +
                    ylab(paste("Yearly Average ", pollutant_selected, " Concentration")) +
                    ggtitle(paste(pollutant_selected, " (yearly average)"))
            } else if (aggregation=="daily_max") {
                plot <- plot +
                    ylab(paste("Daily Maxima ", pollutant_selected, " Concentration")) +
                    ggtitle(paste(pollutant_selected, " (daily maxima)"))
            } else if (aggregation=="thresh_hours_per_day") {
                plot <- plot +
                    ylab(paste("Number of Hours per Day ", pollutant_selected, " above ", threshold, "µg/m^3")) +
                    ggtitle(pollutant_selected)
            } else if (aggregation=="thresh_hours_per_year") {
                plot <- plot +
                    ylab(paste("Number of Hours per Year ", pollutant_selected, " above ", threshold, "µg/m^3")) +
                    ggtitle(pollutant_selected)
            } else if (aggregation=="thresh_days_per_year") {
                plot <- plot +
                    ylab(paste("Number of Days per Year ", pollutant_selected, " above ", threshold, "µg/m^3")) +
                    ggtitle(pollutant_selected)
            }
            
            # show line for EU standard where applicable
            if (pollutant_selected=="PM2.5" & aggregation=="yearly_avg") {
                plot <- plot + geom_hline(aes(yintercept=25), linetype=2, size=1)
            } else if (pollutant_selected=="PM10" & aggregation=="daily_avg") {
                plot <- plot + geom_hline(aes(yintercept=50), linetype=2, size=1)
            } else if (pollutant_selected=="PM10" & aggregation=="thresh_days_per_year" & threshold==50) {
                plot <- plot + geom_hline(aes(yintercept=35), linetype=2, size=1)
            } else if (pollutant_selected=="PM10" & aggregation=="yearly_avg") {
                plot <- plot + geom_hline(aes(yintercept=40), linetype=2, size=1)
            } else if (pollutant_selected=="S02" & aggregation=="none") {
                plot <- plot + geom_hline(aes(yintercept=350), linetype=2, size=1)
            } else if (pollutant_selected=="S02" & aggregation=="thresh_hours_per_year" & threshold==350) {
                plot <- plot + geom_hline(aes(yintercept=24), linetype=2, size=1)
            } else if (pollutant_selected=="S02" & aggregation=="thresh_days_per_year" & threshold==125) {
                plot <- plot + geom_hline(aes(yintercept=3), linetype=2, size=1)
            } else if (pollutant_selected=="S02" & aggregation=="daily_avg") {
                plot <- plot + geom_hline(aes(yintercept=125), linetype=2, size=1)
            } else if (pollutant_selected=="NO2" & aggregation=="none") {
                plot <- plot + geom_hline(aes(yintercept=200), linetype=2, size=1)
            } else if (pollutant_selected=="N02" & aggregation=="thresh_hours_per_year" & threshold==200) {
                plot <- plot + geom_hline(aes(yintercept=18), linetype=2, size=1)
            } else if (pollutant_selected=="N02" & aggregation=="yearly_avg") {
                plot <- plot + geom_hline(aes(yintercept=40), linetype=2, size=1)
            }
            
            plot
        })
        
        # output map of Czech Republic with stations selected shown
        output$maplot <- renderPlot( {
            stations_selected <- isolate(input$stations)
            
            map.text("world","Czech Republic", cex=2)
            points(stations$Longitude[stations$StationName %in% stations_selected], 
                   stations$Latitude[stations$StationName %in% stations_selected], cex = 1.5, pch=19, col="red")
            text(stations$Longitude[stations$StationName %in% stations_selected], 
                 stations$Latitude[stations$StationName %in% stations_selected], 
                 stations$StationName[stations$StationName %in% stations_selected], cex= 0.7, pos=3)
        })
        
        # output data table
        output$table <- renderDataTable({
            pollutant_selected <- isolate(input$pollutant)
            stations_selected <- isolate(input$stations)
            aggregation <- isolate(input$aggregation)
            threshold <- isolate(input$threshold)
            xaxis <- isolate(input$xaxis)
            
            data = data.frame()
            for (item in stations_selected) {
                print(str_c("Data/", stations$EoICode[stations$StationName==item], "_", pollutant_selected, ".csv"))
                data <- rbind(data,
                              read_csv(str_c("Data/", stations$EoICode[stations$StationName==item],
                                             "_", pollutant_selected, ".csv")))
            }
            
            data <- data %>%
                left_join(stations[,c(1,8)], by=c("AirQualityStationEoICode" = "EoICode")) %>%     # join station names
                transform(Date=as.Date(str_c(Day, Month, Year, sep="/"), "%d/%m/%Y")) %>%          # create date field
                transform(Month_Date=as.Date(paste0("2000-",format(Date, "%j")), "%Y-%j")) %>%     # create Month-Day field
                transform(DayOfWeek=factor(weekdays(Date),                                         # create day of the week field
                                           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
                transform(HourOfWeek=(as.integer(DayOfWeek)-1)*24+Hour) %>%                        # create hour of the week field
                group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek, HourOfWeek, Hour) %>%
                summarise(Concentration=mean(Concentration))                                       # gets the average for every date & hour (some hours have multiple readings)
            
            if (aggregation=="none") {
                data[,c("StationName", "Date", "Hour", "Concentration")]
            } else if (aggregation=="daily_avg") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Concentration=mean(Concentration))
                data[,c("StationName", "Date", "Concentration")]
            } else if (aggregation=="yearly_avg") {
                data <- data %>% group_by(StationName, Year) %>%
                    summarise(Concentration=mean(Concentration))
                data[,c("StationName", "Year", "Concentration")]
            } else if (aggregation=="daily_max") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    slice(which.max(Concentration))
                data[,c("StationName", "Date", "Concentration")]
            } else if (aggregation=="thresh_hours_per_day") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Count=n())
                data[,c("StationName", "Date", "Count")]
            } else if (aggregation=="thresh_hours_per_year") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
                data[,c("StationName", "Year", "Count")]
            } else if (aggregation=="thresh_days_per_year") {
                data <- data %>% group_by(StationName, Year, Month, Day) %>%
                    summarise(Concentration=mean(Concentration)) %>%
                    filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
                data[,c("StationName", "Year", "Count")]
            }
        })
        
    }, ignoreNULL=TRUE)
    
    # creates the csv for download from the data used in the table
    output$download <- downloadHandler(
        filename = "pollution_data.csv",
        content = function(file) {
            
            pollutant_selected <- isolate(input$pollutant)
            stations_selected <- isolate(input$stations)
            aggregation <- isolate(input$aggregation)
            threshold <- isolate(input$threshold)
            xaxis <- isolate(input$xaxis)
            
            data = data.frame()
            for (item in stations_selected) {
                print(str_c("Data/", stations$EoICode[stations$StationName==item], "_", pollutant_selected, ".csv"))
                data <- rbind(data,
                              read_csv(str_c("Data/", stations$EoICode[stations$StationName==item],
                                             "_", pollutant_selected, ".csv")))
            }
            
            data <- data %>%
                left_join(stations[,c(1,8)], by=c("AirQualityStationEoICode" = "EoICode")) %>%     # join station names
                transform(Date=as.Date(str_c(Day, Month, Year, sep="/"), "%d/%m/%Y")) %>%          # create date field
                transform(Month_Date=as.Date(paste0("2000-",format(Date, "%j")), "%Y-%j")) %>%     # create Month-Day field
                transform(DayOfWeek=factor(weekdays(Date),                                         # create day of the week field
                                           c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
                transform(HourOfWeek=(as.integer(DayOfWeek)-1)*24+Hour) %>%                        # create hour of the week field
                group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek, HourOfWeek, Hour) %>%
                summarise(Concentration=mean(Concentration))                                       # gets the average for every date & hour (some hours have multiple readings)
            
            if (aggregation=="none") {
                write.csv(data[,c("StationName", "Date", "Hour", "Concentration")], file, row.names=FALSE)
            } else if (aggregation=="daily_avg") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Concentration=mean(Concentration))
                write.csv(data[,c("StationName", "Date", "Concentration")], file, row.names=FALSE)
            } else if (aggregation=="yearly_avg") {
                data <- data %>% group_by(StationName, Year) %>%
                    summarise(Concentration=mean(Concentration))
                write.csv(data[,c("StationName", "Year", "Concentration")], file, row.names=FALSE)
            } else if (aggregation=="daily_max") {
                data <- data %>% group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    slice(which.max(Concentration))
                write.csv(data[,c("StationName", "Date", "Concentration")], file, row.names=FALSE)
            } else if (aggregation=="thresh_hours_per_day") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek) %>%
                    summarise(Count=n())
                write.csv(data[,c("StationName", "Date", "Count")], file, row.names=FALSE)
            } else if (aggregation=="thresh_hours_per_year") {
                data <- data %>% filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
                write.csv(data[,c("StationName", "Year", "Count")], file, row.names=FALSE)
            } else if (aggregation=="thresh_days_per_year") {
                data <- data %>% group_by(StationName, Year, Month, Day) %>%
                    summarise(Concentration=mean(Concentration)) %>%
                    filter(Concentration>threshold) %>%
                    group_by(StationName, Year) %>%
                    summarise(Count=n())
                write.csv(data[,c("StationName", "Year", "Count")], file, row.names=FALSE)
            }
        }
    )
    
})