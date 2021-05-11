prepare_EucFACE_met_for_BiomeEES <- function() {
    
    ### read input
    myDF <- read.csv("~/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/half_hourly/EUC_met_observed_var_half_hourly_2012_2019.csv", 
                     skip=3, header=F)
    
    tmpDF <- read.csv("~/Documents/Research/Projects/EucFACE_Modeling/Met_data/output/observed/csv/half_hourly/EUC_met_observed_var_half_hourly_2012_2019.csv")
    
    colnames(myDF) <- colnames(tmpDF)
    
    
    ### convert into hourly
    myDF$HOUR <- as.numeric(myDF$HOUR)
    myDF$HOUR <- myDF$HOUR + 0.01
    myDF$Hour <- round(myDF$HOUR, 0)
    
    hourDF <- summaryBy(PAR+Tair+RH+Wind+PSurf+CO2ambient+SoilTemp~YEAR+DOY+Hour, FUN=mean, data=myDF,
                        keep.names=T, na.rm=T)
    
    hourDF2 <- summaryBy(Rain~YEAR+DOY+Hour, FUN=sum, data=myDF,
                        keep.names=T, na.rm=T)
    
    hourDF <- merge(hourDF, hourDF2, by=c("YEAR", "DOY", "Hour"))
    
    
    ### calculate radiation, based on PAR
    hourDF$rad_h <- hourDF$PAR / 2
    

    ### convert tair and tsoil units
    hourDF$tair <- hourDF$Tair - 273.15
    
    hourDF$Tsoil <- hourDF$SoilTemp - 273.15
    
    ### select output variables
    outDF <- hourDF[,c("YEAR", "DOY", "Hour", "PAR", "rad_h", "tair", "Tsoil", "RH",
                       "Rain", "Wind", "PSurf", "CO2ambient")]
    colnames(outDF) <- c("year", "doy", "hour", "PAR", "rad_h", "tair", "Tsoil", "RH",
                        "Rain", "WSPEED", "PRESSURE", "aCO2_AW")
    
    outDF <- outDF[order(outDF$year, outDF$doy, outDF$hour),]
    
    write.csv(outDF, "input/EucFACE_amb.csv", row.names=F)
    write.csv(outDF, "/Users/mingkaijiang/Downloads/EucFACE_amb.csv", row.names=F)
    
    ### end
}