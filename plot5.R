## EDA week four project plot 5
## Mark Sucato

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
auto <- SCC %>% filter(str_detect(EI.Sector, "On-Road")) 
data <- NEI %>%
	mutate(Year = as.factor(year)) %>%
	filter(fips == "24510") %>%
	semi_join(auto) %>%
	left_join(auto) %>%
	rename(Source = EI.Sector) %>%
	group_by(Source, Year) %>%
	summarise(Total = sum(Emissions))
levels(data$Source)[49] <- "Diesel Heavy Duty Vehicles" 
levels(data$Source)[50] <- "Diesel Light Duty Vehicles" 
levels(data$Source)[51] <- "Gasoline Heavy Duty Vehicles"
levels(data$Source)[52] <- "Gasoline Light Duty Vehicles"

p <- ggplot(data, aes(Year, Total, fill = Source))
p + geom_bar(stat = "identity", position = "stack") +
	theme_light() + 
	scale_fill_brewer(palette = "Paired") +
	labs(title = "Baltimore City Motor Vehicle Emissions by Source and Year") +
	labs(x = "Year", y = "PM2.5 emissions (tons)")

ggsave("plot5.png", device = "png")
