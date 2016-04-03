download_data_set <- function (){
  # download dataset
  if(!file.exists("./data")){dir.create("./data")}
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(fileUrl,destfile="./data/Dataset.zip")
  
  # unzip dataset to /data directory
  unzip(zipfile="./data/Dataset.zip",exdir="./data")
  
}

generate_first_plot <- function (){
# Reading source data
# This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# Aggregate by sum the total emissions by year
aggTotals <- aggregate(Emissions ~ year,NEI, sum)

png("plot1.png",width=480,height=480,units="px")

barplot(
  (aggTotals$Emissions)/10^6,
  names.arg=aggTotals$year,
  xlab="Year",
  ylab="PM2.5 Emissions (10^6 Tons)",
  main=expression("Total PM" [2.5]* " Emissions From All US Sources")
)

dev.off()
}