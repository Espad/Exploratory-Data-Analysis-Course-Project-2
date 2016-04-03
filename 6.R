# install or load required packages
if (!require("ggplot2")) {
  install.packages("ggplot2")
}
library(ggplot2)

download_data_set <- function (){
  # download dataset
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="./data/Dataset.zip")
  
  # unzip dataset to /data directory
  unzip(zipfile="./data/Dataset.zip",exdir="./data")
  
}

generate_six_plot <-function (){
# Load the NEI & SCC data frames.
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Gather the subset of the NEI data which corresponds to vehicles
vehicles <- grepl("vehicle", SCC$SCC.Level.Two, ignore.case=TRUE)
vehiclesSCC <- SCC[vehicles,]$SCC
vehiclesNEI <- NEI[NEI$SCC %in% vehiclesSCC,]

# Subset the vehicles NEI data by each city's fip and add city name.
vehiclesBaltimoreNEI <- vehiclesNEI[vehiclesNEI$fips=="24510",]
vehiclesBaltimoreNEI$city <- "Baltimore City"

vehiclesLANEI <- vehiclesNEI[vehiclesNEI$fips=="06037",]
vehiclesLANEI$city <- "Los Angeles County"

# Combine the two subsets with city name into one data frame
bothNEI <- rbind(vehiclesBaltimoreNEI,vehiclesLANEI)

png("plot6.png",width=480,height=480,units="px")

ggp <- ggplot(bothNEI, aes(x=factor(year), y=Emissions, fill=city)) +
  geom_bar(aes(fill=year),stat="identity", fill = "red") +
  facet_grid(scales="free", space="free", .~city) +
  guides(fill=FALSE) + theme_bw() +
  labs(x="year", y=expression("Total PM"[2.5]*" Emission (Kilo-Tons)")) + 
  labs(title=expression("PM"[2.5]*" Motor Vehicle Source Emissions in Baltimore & LA, 1999-2008"))

print(ggp)

dev.off()
}