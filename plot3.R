## EDA week four project plot 3
## Mark Sucato

library(dplyr)
library(ggplot2)
library(RColorBrewer)

NEI <- readRDS("summarySCC_PM25.rds")
data <- NEI %>%
	filter(fips == "24510") %>%
	mutate(Type = as.factor(type), Year = as.factor(year)) %>%
	group_by(Type, Year) %>%
	summarise(Total = sum(Emissions)) 

p <- ggplot(data, aes(Type, Total, fill = Year))
p + geom_bar(stat = "identity", position = "dodge") +
	theme_light() + 
	scale_fill_brewer(palette = "Dark2") +
	labs(title = "Baltimore City Emissions by Type and Year") +
	labs(x = "Emission Type", y = "PM2.5 emissions (tons)") 
	  
ggsave("plot3.png", device = "png")
