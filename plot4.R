## EDA week four project plot 4
## Mark Sucato

library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
Coal <- SCC %>% filter(str_detect(EI.Sector, "Coal")) 
data <- NEI %>%
	mutate(Year = as.factor(year)) %>%
	semi_join(Coal) %>%
	left_join(Coal) %>%
  	mutate(SCC.Level.Two = recode(SCC.Level.Two, "Industrial" = "Commercial/Institutional")) %>%
	mutate(SCC.Level.Two = recode(SCC.Level.Two, "Electric Utility" = "Electric Generation")) %>%
	mutate(SCC.Level.Two = recode(SCC.Level.Two, "Space Heaters" = "Commercial/Institutional")) %>%
	mutate(SCC.Level.Two = recode(SCC.Level.Two, "Total Area Source Fuel Combustion" = "Commercial/Institutional")) %>%
	group_by(SCC.Level.Two, Year) %>%
	rename(Source = SCC.Level.Two) %>%
	summarise(Total = sum(Emissions)) 
levels(data$Source)[22] <- "Commercial/Institutional/Other"

p <- ggplot(data, aes(Year, Total, fill = Source))
p + geom_bar(stat = "identity", position = "stack") +
	theme_light() + 
	scale_fill_brewer(palette = "Set1") +
	labs(title = "United States Coal Combustion Emissions by Source and Year") +
	labs(x = "Year", y = "PM2.5 emissions (tons)")

ggsave("plot4.png", device = "png")
