###############################################################################
### plot1.R
###
###############################################################################


###############################################################################
### Some preparation
###############################################################################
# Set up a data directory.
if (!file.exists("data")) {
    print("Creating data directory.")
    dir.create("data")
}

# From the United States Environmental Protection Agency (EPA)
# "National Emissions Inventory" (NEI) [29MB].
dataURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipRL <- "./data/dataset.zip"
d1RL <- "./data/summarySCC_PM25.rds"
d2RL <- "./data/Source_Classification_Code.rds"

# Download if necessary.
if (!file.exists(zipRL)) {
    download.file(dataURL, destfile=zipRL, method="curl")
} else {
    cat("Data file:", zipRL, "exists\n")
}

# Unzip if necessary.
if (!file.exists(d1RL) | !file.exists(d2RL)) {
    unzip(zipRL, exdir="./data/.")
} else {
    print("Data already unzipped.")
}


# Read in data file.
if (!exists("NEI") | !exists("SCC")) {
    NEI <- readRDS(d1RL)
    SCC <- readRDS(d2RL)
} else {
    print("Skipping data reads.")
}


###############################################################################
### 1. Total emissions from all sources decreased from 1999 to 2008?
###############################################################################
total_emissions_by_year <- do.call(rbind,
                                   lapply(split(NEI$Emissions, NEI$year),
                                          sum))

png(file="plot1.png")
plot(rownames(total_emissions_by_year),
     total_emissions_by_year,
     type="l",
     xlab="Year",
     ylab="Emissions (tons)",
     main="Emissions From All Sources") 
dev.off()


###############################################################################
### 2. Total emissions fom all sources within Baltimore decreased?
###############################################################################
NEI_balt <- NEI[NEI$fips == "24510", ]
total_emissions_by_year <- do.call(rbind,
                                   lapply(split(NEI_balt$Emissions,
                                                NEI_balt$year),
                                          sum))

png(file="plot2.png")
plot(rownames(total_emissions_by_year),
     total_emissions_by_year,
     type="l",
     xlab="Year",
     ylab="Emissions (tons)",
     main="Emissions From All Sources (Baltimore)")
dev.off()


###############################################################################
### 3. Which types of sources of emissions have seen decrease in Baltimore?
###############################################################################
NEI_balt <- NEI[NEI$fips == "24510", ]
NEI_balt_type <- split(NEI_balt, NEI_balt$type)
NBT <- lapply(NEI_balt_type,
              function(x){
                  do.call(rbind,
                      lapply(split(x$Emissions, x$year),
                             sum))})
NBT <- as.data.frame(NBT)
NBT <- cbind(year=as.integer(as.character(rownames(NBT))), NBT)
NBT_long <- melt(NBT, id="year")

plot3 <- ggplot(data=NBT_long, aes(x=year, y=value, colour=variable)) +
             geom_line() +
             xlab("Year") +
             ylab("Emissions (tons)") +
             ggtitle("Emissions by type (Baltimore)")

png(file="plot3.png")
print(plot3)
dev.off()


##############################################################################
## 4. Emissions from coal combustion-related sources?
##############################################################################
cc <- SCC[grepl("Coal", SCC$EI.Sector), "SCC"]
ce <- NEI[NEI$SCC %in% cc, ]
ceby <- do.call(rbind, lapply(split(ce$Emissions, ce$year), sum))

png(file="plot4.png")
plot(rownames(ceby), ceby, type="l", xlab="Year", ylab="Emissions (tons)",
     main="Coal Combustion-related Sources")
dev.off()


###############################################################################
### 5. Motor vehicle sources in Baltimore City?
###############################################################################
mv <- SCC[grepl("Vehicle", SCC$EI.Sector), "SCC"]
ve <- NEI[NEI$SCC %in% mv & NEI$fips == "24510", ]
veby <- do.call(rbind, lapply(split(ve$Emissions, ve$year), sum))

png(file="plot5.png")
plot(rownames(veby),
     veby,
     type="l",
     xlab="Year",
     ylab="Emissions (tons)",
     main="Vehicle Emissions (Baltimore)")
dev.off()


###############################################################################
### 6. Motor vehicle sources in Baltimore City vs Los Angeles.
###############################################################################
mv <- SCC[grepl("Vehicle", SCC$EI.Sector), "SCC"]
veb <- NEI[NEI$SCC %in% mv & NEI$fips == "24510", ]
vel <- NEI[NEI$SCC %in% mv & NEI$fips == "06037", ]

vebby <- do.call(rbind, lapply(split(veb$Emissions, veb$year), sum))
velby <- do.call(rbind, lapply(split(vel$Emissions, vel$year), sum))

png(file="plot6.png", height=480, width=960)
par(mfrow=c(1, 2))
plot(rownames(vebby),
     vebby,
     type="l",
     xlab="Year",
     ylab="Emissions (tons)",
     main="Vehicle Emissions (Baltimore)")

plot(rownames(velby),
     velby,
     type="l",
     xlab="Year",
     ylab="Emissions (tons)",
     main="Vehicle Emissions (Los Angeles)")
dev.off()

zz <- function() {source("main.R")}
