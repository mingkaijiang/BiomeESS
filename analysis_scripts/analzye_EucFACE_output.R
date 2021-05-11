analyze_EucFACE_output <- function() {
    
    ### ecosystem annual
    eco_ann_DF <- read.csv("output/Ecosystem_yearlyEucFACE.csv")
    
    with(eco_ann_DF, plot(CAI~year))
    with(eco_ann_DF, plot(LAI~year))
    with(eco_ann_DF, plot(treecover~year, ylim=c(0,1)))
    with(eco_ann_DF, points(grasscover~year, col="red"))
    
    
    ### ecosystem daily
    eco_dai_DF <- read.csv("output/Ecosystem_dailyEucFACE.csv")
    
    with(eco_dai_DF, plot(LAI~doy))
    
    
    ### photosynthesis
    #photoDF <- read.csv("output/PhotosynthesisDynamicsEucFACE.csv")
    
    ### annual cohorts
    #cor_ann_DF <- read.csv("output/Annual_cohortsEucFACE.csv")
    
    ### daily cohort
    cor_dai_DF <- read.csv("output/Cohorts_dailyEucFACE.csv")
    
    
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
    

    ### PFT 4 is the evergreen canopy trees, look at this
    subDF <- subset(annDF, PFT=="4")
    
    p1 <- ggplot(subDF, aes(x=year, y=LAI, pch=layer, col=layer))+
        geom_point()
    
    plot(p1)
    
    
    ### look at layer 1
    subDF2 <- subset(subDF, layer == "1")
    
    p1 <- ggplot(subDF2, aes(x=year, y=LAI, col=cID))+
        geom_point()
    
    plot(p1)
    
    
    subDF2$overall_LAI <- with(subDF2, LAI*f_layer)
   
    p1 <- ggplot(subDF2, aes(x=year, y=overall_LAI, col=cID))+
        geom_point()
    
    plot(p1)
    
}