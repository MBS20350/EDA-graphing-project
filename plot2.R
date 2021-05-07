## EDA week four project plot 2
## Mark Sucato

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
data <- NEI %>%
	filter(fips == "24510") %>%
	group_by(year) %>%
	summarise(Total = sum(Emissions))

barplot(Total ~ year, data = data, col = "steelblue", xlab = "Year", 
	ylab = "PM2.5 Emissions (tons)",
	main = "Baltimore City Total Emissions per Year")

dev.copy(png, file = "plot2.png")
dev.off()