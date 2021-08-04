analyze_EucFACE_output <- function() {
    
    ### ecosystem annual
    eco_ann_DF <- read.csv("output/Ecosystem_yearlyEucFACE.csv")
    
    with(eco_ann_DF, plot(CAI~year))
    with(eco_ann_DF, plot(LAI~year))
    with(eco_ann_DF, plot(treecover~year, ylim=c(0,1)))
    with(eco_ann_DF, points(grasscover~year, col="red"))
    
    require(ggplot2)
    p1 <- ggplot()+
        geom_point(data=eco_ann_DF, aes(x=year, y=CAI))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5));p1
    
    p2 <- ggplot()+
        geom_point(data=eco_ann_DF, aes(x=year, y=treecover), col="green")+
        geom_point(data=eco_ann_DF, aes(x=year, y=grasscover), col="brown")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=16, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("tree/grass cover");p2
    
    require(cowplot)
    pdf(paste0("output/spinup_fractional_cover.pdf"), 
        width=6, height=8)
    plot_grid(p1,p2,
              ncol=1)
    
    dev.off()
    
    
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

    annDF <- summaryBy(LAI+density+f_layer~year+cID+PFT+layer, 
                       FUN=mean, data=cor_dai_DF, keep.names=T,na.rm=T)
    
    annDF$PFT <- as.factor(annDF$PFT)
    annDF$cID <- as.factor(annDF$cID)
    annDF$layer <- as.factor(annDF$layer)
    
    require(ggplot2)
    p1 <- ggplot(annDF, aes(x=year, y=LAI, pch=PFT, col=layer))+
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right"); p1

    p2 <- ggplot(annDF, aes(x=year, y=f_layer, pch=PFT, col=layer))+
        geom_point()+
        geom_point()+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="right"); p2

    
    pdf(paste0("output/simulation_period_fractional_cover.pdf"), 
        width=6, height=8)
    plot_grid(p1,p2,
              ncol=1)
    
    dev.off()
    
    
    #### PFT 4 is the evergreen canopy trees, look at this
    #subDF <- subset(annDF, PFT=="4")
    #
    #p1 <- ggplot(subDF, aes(x=year, y=LAI, pch=layer, col=layer))+
    #    geom_point()
    #
    #plot(p1)
    #
    #
    #### look at layer 1
    #subDF2 <- subset(subDF, layer == "1")
    #
    #p1 <- ggplot(subDF2, aes(x=year, y=LAI, col=cID))+
    #    geom_point()
    #
    #plot(p1)
    #
    #
    #subDF2$overall_LAI <- with(subDF2, LAI*f_layer)
   #
    #p1 <- ggplot(subDF2, aes(x=year, y=overall_LAI, col=cID))+
    #    geom_point()
    #
    #plot(p1)
    
}