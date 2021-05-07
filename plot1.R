## EDA week four project plot 1
## Mark Sucato

library(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")
data <- NEI %>%
	group_by(year) %>%
	summarise(Total = sum(Emissions))

barplot(Total ~ year, data = data, col = "firebrick", xlab = "Year", 
	ylab = "PM2.5 Emissions (tons)",
	main = "United States Total Emissions per Year")

dev.copy(png, file = "plot1.png")
dev.off()