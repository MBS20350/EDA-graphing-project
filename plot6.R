## EDA week four project plot 6
## Mark Sucato

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
library(DataCombine)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
auto <- SCC %>% filter(str_detect(EI.Sector, "On-Road")) 
data <- NEI %>%
	mutate(Year = as.factor(year)) %>%
	filter(fips == "24510" | fips == "06037") %>%
	semi_join(auto) %>%
	left_join(auto) %>%
	rename(Source = EI.Sector) %>%
	group_by(Source, Year, fips) %>%
	summarise(Total = sum(Emissions)) %>%
	arrange(fips) %>%
 	change(Var = "Total", GroupVar = "fips", NewVar = "Change")
levels(data$Source)[49] <- "Diesel Heavy Duty Vehicles" 
levels(data$Source)[50] <- "Diesel Light Duty Vehicles" 
levels(data$Source)[51] <- "Gasoline Heavy Duty Vehicles"
levels(data$Source)[52] <- "Gasoline Light Duty Vehicles"
for (i in 2:32) { if(data$Year[i] == "1999") {data$Change[i] = NA} }

p <- ggplot(data, aes(Year, Change, fill = Source))
p + geom_bar(stat = "identity", position = "dodge") +
	theme_light() + 
	scale_fill_brewer(palette = "Paired") +
	labs(title = "Percent Change in Motor Vehicle Emissions by Source and Year") +
	labs(subtitle = "Los Angeles County (06037) vs Baltimore City (24510)") +
	labs(x = "Year", y = "Percent Change in PM2.5 Emissions from Previous Year") +
	facet_grid(.~fips)

ggsave("plot6.png", device = "png")