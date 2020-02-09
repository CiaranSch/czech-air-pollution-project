install.packages('rsconnect')
rsconnect::setAccountInfo(name='cschutten',
                          token='3CF6D457C8BDD62FE2E5D0E17D7B2949',
                          secret='1btt+P2GZKw401Cp1JoqIdne/eE3BU0k/RVWw2Fm')


setwd("C:/Users/cschu/Dropbox/Masters/R Programming/Project")

stations_selected <- c("Praha 10−Prumyslova", "Praha 10−Vrsovice", "Praha 2−Riegrovy sady", "Praha 5−Smichov")

map.text("world","Czech Republic", cex=2)
points(stations$Longitude[stations$StationName %in% stations_selected], 
       stations$Latitude[stations$StationName %in% stations_selected], cex = 1.5, pch=19, col="red")
text(stations$Longitude[stations$StationName %in% stations_selected], 
     stations$Latitude[stations$StationName %in% stations_selected], 
     stations$StationName[stations$StationName %in% stations_selected], cex= 0.7, pos=3)

labels=stations_selected
stations[stations$StationName %in% stations_selected,]



data = data.frame()
for (item in stations_selected) {
    print(str_c("Data/", stations$EoICode[stations$StationName==item], "_", "PM10", ".csv"))
    data <- rbind(data,
                  read_csv(str_c("Data/", stations$EoICode[stations$StationName==item],
                                 "_", "PM10", ".csv")))
}
data <- data %>%
    left_join(stations[,c(1,8)], by=c("AirQualityStationEoICode" = "EoICode")) %>%     # join station names
    transform(Date=as.Date(str_c(Day, Month, Year, sep="/"), "%d/%m/%Y")) %>%          # create date field
    transform(Month_Date=as.Date(paste0("2000-",format(Date, "%j")), "%Y-%j")) %>%     # create Month-Day field
    transform(DayOfWeek=factor(weekdays(Date),                                         # create day of the week field
                               c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
    transform(HourOfWeek=(as.integer(DayOfWeek)-1)*24+Hour) %>%                        # create hour of the week field
    group_by(StationName, Year, Month, Day, Month_Date, Date, DayOfWeek, HourOfWeek, Hour) %>%
    summarise(Concentration=mean(Concentration)) 

threshold = 70
data2 <- data %>% group_by(StationName, Year, Month, Day) %>%
    summarise(Concentration=mean(Concentration)) %>%
    filter(Concentration>threshold) %>%
    group_by(StationName, Year) %>%
    summarise(Count=n())
#### Calander ####
ggplot() +
    geom_line(data=data2, aes(x=Year, y=Count, colour=StationName), size=0.8)
#### Seasonal effect ####
ggplot() +
    geom_jitter(data=data2, aes(x=Month_Date, y=Count, colour=StationName), size=3, alpha=0.5, width=0.01) +
    scale_x_date(labels = function(x) format(x, "%b"), date_breaks="1 month") + 
    xlab("Seasonal Effect")
#### Weekly effect ####



