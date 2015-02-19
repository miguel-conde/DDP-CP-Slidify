
library(ggplot2)
library(scales) 


# First, we read the file and load raw data:

dataDirectory <- file.path(".","data")
myExoURL <- "https://raw.githubusercontent.com/OpenExoplanetCatalogue/oec_tables/master/comma_separated/open_exoplanet_catalogue.txt"

exoFile <- file.path(dataDirectory, "open_exoplanet_catalogue.txt")


if(!file.exists(dataDirectory)) {
    dir.create(dataDirectory)
}

if (!file.exists(exoFile)) {
    download.file(url = myExoURL, destfile = exoFile)
}


#read the data
rawExoData <- read.table(exoFile, header = FALSE, sep = ",", 
                          stringsAsFactors = FALSE)

names(rawExoData) <- c("PrimId", "BinFlag", "PMassJ", "PRadiusJ", "Period", 
                        "SemiMajorAxis", "Eccentricity", "Periastron",
                        "Longitude", "AscNode","Inclination", "Temperature", 
                        "Age", "DiscoveryMethod", "DiscoveryYear", "LastUpdated",
                        "RA", "Dec", "DistanceFromSun", "HStarMassS", "HStarRadiusS", 
                        "HStarMetallicityS","HStarTemperature", "HStarAge")

selected_table <- c("PrimId", "PMassJ", "PRadiusJ", "Period", 
                     "SemiMajorAxis", "Eccentricity", "Temperature", 
                     "DiscoveryMethod", "DiscoveryYear","DistanceFromSun", 
                     "HStarMassS", "HStarTemperature")

names_plot <- c("PMassJ", "PRadiusJ", "Period", 
                 "SemiMajorAxis", "Eccentricity", "Inclination", "Temperature", 
                 "Age", "DiscoveryYear", 
                 "DistanceFromSun", "HStarMassS", "HStarRadiusS", 
                 "HStarMetallicityS","HStarTemperature", "HStarAge")

## RENDER Plot
drawPlot <- function() {
    
    dd <- rawExoData[, , drop = FALSE]
    xs <- "SemiMajorAxis"
    ys <- "PMassJ"
    dd <- dd[!is.na(dd[,xs]) & !is.na(dd[,ys]), ]
    
    p <- ggplot(data = dd,
                aes_string(x = xs, y = ys),
    ) + 
        geom_point(size = 4, 
                   aes(colour = DiscoveryMethod,
                       fill = DiscoveryMethod,
                       shape = DiscoveryMethod)) +
        scale_colour_hue(l = 40) + # Use a slightly darker palette than normal
        scale_fill_hue(l = 40) 
    

    p <- p + scale_x_continuous(trans = log10_trans()) + 
        scale_y_continuous(trans = log10_trans())

png(file = "assets/img/img0.png", 720, 480) 
print(p)

    dev.off()
}


