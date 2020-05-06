library(tidyverse)
library(covid19.analytics)
library(lubridate)
library(reshape)
library(gganimate)

library(leaflet)
library(ggmap)
library(leaflet.extras)
library(htmltools)
library(maps)
library(mapproj)

ag <- covid19.data(case="aggregated")
tsc <- covid19.data(case="ts-confirmed")
glimpse(data)

data <- read_csv("COVIDlatestdata.csv")

# Important Trends for Different Countries
plot.country <- function(country){
  tsc.long <- tsc %>%
    filter(Country.Region==country) %>%
    select(5:length(tsc)) %>%
    data.frame() 
    
  names(tsc.long) <- ymd(gsub('X', '', names(tsc.long)))
  
  tsc.long <- tsc.long %>%
    t() %>%
    data.frame() %>%
    rownames_to_column(var="date") 
  names(tsc.long) <- c('date', 'cases')
  tsc.long
}

tsc.italy = plot.country("Italy")
tsc.turkey = plot.country("Turkey")
tsc.germany = plot.country("Germany")
tsc.colombia = plot.country("Colombia")
tsc.thailand = plot.country("Thailand")
tsc.US = plot.country("US")

tsc.all <- tsc.italy %>%
  merge(tsc.turkey, by="date", all=TRUE) %>%
  merge(tsc.germany, by="date", all=TRUE) %>%
  merge(tsc.colombia, by="date", all=TRUE) %>%
  merge(tsc.thailand, by="date", all=TRUE) %>%
  merge(tsc.US, by="date", all=TRUE)

names(tsc.all) <- c("date", "Italy", "Turkey", "Germany", "Colombia", "Thailand", "US")


melt(tsc.five, id.vars="date", variable_name="country") %>%
  ggplot(aes(x=date(date), y=value, col=country)) +
  geom_line() +
  labs(x="Date", y="Cases", title=paste("Daily Total Cases"))


# 2
state_us <- filter(data, grepl("Cali", province) | grepl("New York", province) | grepl("Maryland", province) | grepl("New Jersey", province))
state_us["date_confirmation"] <- as.Date(sapply(state_us[["date_confirmation"]], gsub, pattern="\\.", replacement="/"), format = c("%d/%m/%Y"))

state_us %>%
  group_by(province, date_confirmation) %>%
  summarize(count=n()) %>%
  ggplot(aes(x=date_confirmation, y=count, col=province)) +
  geom_point() +
  labs(x="Date", y="Cases", title=paste("Daily Total Cases of US"))

data %>%
  filter(country %in% c("Finland", "Turkey", "United States", "Colombia", "Thailand")) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x=country, fill=sex)) +
  geom_bar(position="fill")


#3
datanew <- data %>% 
  mutate(date_confirmation = dmy(data$date_confirmation))

head(datanew$date_confirmation)

datanew %>% group_by(date_confirmation) %>%
  summarise(count=n()) %>%
  mutate(cum=cumsum(count)) %>%
  ggplot(aes(x=date_confirmation, y=cum)) +
  geom_line(color="red")+
  geom_point(size=1.5)+
  geom_area(fill="pink")+
  theme_bw() +
  ggtitle("Cumulative Covid Cases") +
  transition_reveal(cum)

anim_save()

# 4
usa <- data %>%
  filter(country=="United States")

usa <- usa %>% 
  group_by(province) %>%
  summarise(count=n())

data <- merge(s, usa,
              by.x="region",
              by.y="province")

usa$province <- tolower(usa$province)

data <- merge(s, usa,
              by.x="region",
              by.y="province")

ggplot(data, aes(x=long, y=lat, group=group, fill=count)) +
  geom_polygon(color="gray") +
  coord_map("polyconic") +
  scale_fill_gradient(low="white", high="blue")



usa <- data %>% filter(country == 'United States', province %in% c("California",  "Nevada"))
usa <- usa %>% group_by(city, province, longitude, latitude) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

mycolor <- colorFactor(palette = c('red', 'blue'),
                       levels = c('New Jersey', 'Delaware'))


map <- na.omit(usa) %>% leaflet() %>% 
  addProviderTiles('HERE') %>% 
  addCircleMarkers(radius = 5,
                   color = ~mycolor(province),
                   popup = ~paste0(city, "<br/>",
                                   count),
                   label = ~paste0(city, "(", province, ")"))
map


CA <- filter(usa, province == "California")
NV <- filter(usa, province == "Nevada")
leaflet() %>%  
  addTiles(group = 'OpenRailwayMap') %>% 
  addProviderTiles('HERE', group = 'terrainDay') %>% 
  addProviderTiles("HikeBike", group = "HikeBike") %>%
  addCircleMarkers(data = CA,
                   radius = 2,
                   popup = ~paste0(city, "<br/>",
                                   count),
                   label = ~paste0(city, "(", province, ")"),
                   group =  "California",
                   color = 'blue') %>% 
  addCircleMarkers(data = NV,
                   radius = 2,
                   group = "Nevada",
                   color = 'red',
                   label = ~paste0(city, "(", province, ")"),
                   popup = ~paste0(city, "<br/>",
                                   count)) %>%   
  addLayersControl(baseGroups = c('OpenRailwayMap', "terrainDay", 'HikeBike'),
                   overlayGroups = c("California", "Nevada") )%>% 
  setView(lng = -119, lat = 36.2,
          zoom = 5.5)


# datanew$day <- day(datanew$date_confirmation)
# datanew$month <- month(datanew$date_confirmation)
# 
# new <- datanew %>% filter(month==3) %>%
#   group_by(day, country) %>%
#   summarise(count=n())
# 
# new <- as.data.frame(complete(new, day, country, fill=list(count=0)))
# new %>% filter(country=="United States" | country == "France" | country == "United Kingdom") %>%
#   ggplot(aes(x=day, y = count)) +
#   geom_line() +
#   geom_point() +
#   theme_bw() +
#   transition_reveal(day)
# anim_save("test")