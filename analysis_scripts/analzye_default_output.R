analyze_default_output <- function() {
    
    ### ecosystem annual
    eco_ann_DF <- read.csv("output/Ecosystem_yearlyKonza-shrub.csv")
    
    with(eco_ann_DF, plot(CAI~year))
    with(eco_ann_DF, plot(LAI~year))
    with(eco_ann_DF, plot(treecover~year))
    with(eco_ann_DF, points(grasscover~year, col="red"))
    
    
    ### ecosystem daily
    eco_dai_DF <- read.csv("output/Ecosystem_dailyKonza-shrub.csv")
    
    with(eco_dai_DF, plot(LAI~doy))
    
    
    ### photosynthesis
    photoDF <- read.csv("output/PhotosynthesisDynamicsKonza-shrub.csv")
    
    ### annual cohorts
    cor_ann_DF <- read.csv("output/Annual_cohortsKonza-shrub.csv")
    
    ### daily cohort
    cor_dai_DF <- read.csv("output/Cohorts_dailyKonza-shrub.csv")
    
    
    ## let's look at one cohort to get a feel
    require(doBy)

    annDF <- summaryBy(LAI+density+f_layer~year+cID+PFT+layer, FUN=mean, data=cor_dai_DF, keep.names=T,na.rm=T)
    
    annDF$PFT <- as.factor(annDF$PFT)
    annDF$cID <- as.factor(annDF$cID)
    annDF$layer <- as.factor(annDF$layer)
    
    require(ggplot2)
    p1 <- ggplot(annDF, aes(x=year, y=LAI, pch=PFT, col=layer))+
        geom_point()

    plot(p1)
    

    ### PFT 2 is the canopy trees, look at this
    subDF <- subset(annDF, PFT=="2")
    
    p1 <- ggplot(subDF, aes(x=year, y=LAI, pch=layer, col=layer))+
        geom_point()
    
    plot(p1)
    
    
    ### look at layer 1
    subDF2 <- subset(subDF, layer == "1")
    
    p1 <- ggplot(subDF2, aes(x=year, y=LAI, col=cID))+
        geom_point()
    
    plot(p1)
    
    ## subset one cID to look at temporal pattern
    subDF3 <- subset(subDF2, cID == "828")
    
    p1 <- ggplot(subDF3, aes(x=year, y=LAI))+
        geom_point()
    
    plot(p1)
    
    ### why this cohort disappear?
    
    
    subDF4 <- subset(subDF2, cID == "829")
    
    p1 <- ggplot(subDF4, aes(x=year, y=LAI))+
        geom_point()
    
    plot(p1)
    
    ### this cohort has continuous record. The interannual fluctuation should be
    ### indicative of climate. But why it increased rapidly over the first 5 years
    ### (from LAI = 1 to 3)?
    
    
    ### to modify the model code, to track indiviual cohort from year 1
    ### to look at what determines cohort death.
    ### Can possibly look at data to understand what determines establishment too.
    
    
}