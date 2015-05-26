##Make sure urban locality coverage is ok
Rural <- read_csv("Data-out/LocalitiesWithCoverage/RuralLocalities.csv")
Telcel <- tbl_df(Rural) %>% filter(CVE_ENT==11 & CVE_MUN==007) 
Telcel$Telcel3G <- ifelse(is.na(Telcel$Telcel3G)==TRUE,0,1)

##ReadShapes Municipios
Muns <- readOGR("Shapes/Estados","Municipios",stringsAsFactors = FALSE)
Muns <- fortify(Muns[Muns$CVE_ENT=="11" & Muns$CVE_MUN=="007",])

#Celaya Test Graph
dir.create("plots")
G1 <- ggplot(Telcel) + geom_path(aes(x=long,y=lat,group=group,order=order),data=Muns) + geom_point(aes(coords.x1,coords.x2,color=as.factor(Telcel3G)),size=4)
G1 <- G1 + theme(line = element_blank(),
                 axis.title = element_blank(),
                 axis.text = element_blank(),
                 line = element_blank(),
                 panel.background = element_blank(),
                 plot.title = element_text(lineheight=.8, face="bold"))
G1 <- G1 + scale_color_discrete(name="Cobertura:\nTelcel 3G",labels=c("No Cubierto","Cubiero")) + ggtitle("Celaya, Guanajuato: Localidades rurales")
svg(filename="plots/Celaya.svg", 
    width=10, 
    height=10, 
    pointsize=12)
plot(G1)
dev.off()
remove(list=ls())



