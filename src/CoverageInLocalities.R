####Generate Locality Level Data on Cellphone coverage

##Download Rural Locality Points
download.file("http://mapserver.inegi.org.mx/MGN/mglr2014v6_2.zip","Shapes/LocalidadesRurales.zip")
unzip("Shapes/LocalidadesRurales.zip",exdir = "Shapes/LocalidadesRurales")
file.remove("Shapes/LocalidadesRurales.zip")
##Download urban locality points
download.file("http://mapserver.inegi.org.mx/MGN/mglu2014v6_2.zip","Shapes/LocalidadesUrbanas.zip")
unzip("Shapes/LocalidadesUrbanas.zip",exdir = "Shapes/Localidadesurbanas")
file.remove("Shapes/LocalidadesUrbanas.zip")


##Read Coverage Shapefiles
Iusacel3G <- readOGR("Shapes/Iusacel3G/","Iusacel3G",stringsAsFactors = FALSE)
IusacelGSM <- readOGR("Shapes/IusacelGSM/","IusacelGSM",stringsAsFactors = FALSE)
Movistar <- readOGR("Shapes/Movistar/","Movistar",stringsAsFactors = FALSE)
Telcel3G <- readOGR("Shapes/Telcel3G/","Telcel3G",stringsAsFactors = FALSE)
TelcelGSM <- readOGR("Shapes/TelcelGSM/","TelcelGSM",stringsAsFactors = FALSE)

##Localidades
Rural <- readOGR("Shapes/LocalidadesRurales","mglr2013v6_2",stringsAsFactors = TRUE)
Rural <- data.frame(Rural@data, Rural@coords)
Rural$CVE <- paste(Rural$CVE_ENT,Rural$CVE_MUN,Rural$CVE_LOC,Rural$CVE_AGEB,sep="")

##Get Rural Locality coverage for each company
#Point in Polygon Function
TestInPolygon <- function(Company){
  TMP <- Company
  ManyPolygons <- function(Poly){
    TMP2 <- fortify(TMP[row.names(TMP)==Poly,])
    TMP1 <- point.in.polygon(point.x = Rural$coords.x1,point.y = Rural$coords.x2,pol.x =TMP2$long,pol.y = TMP2$lat)
    TMP1 <- data.frame(CVE=Rural[,8],TMP1)[TMP1==1,]
    return(TMP1)
  }
  TMP3 <- lapply(1:(length(TMP)-1),ManyPolygons)
  TMP3 <- rbind_all(TMP3)
  TMP3 <- unique(TMP3)
  return(TMP3)
}

#Run it
Telcel3GLR <- TestInPolygon(Telcel3G)
Telcel3GRL <- TestInPolygon(Telcel3G)
TelcelGSMRL <- TestInPolygon(TelcelGSM)
IusacelGSMRL <- TestInPolygon(IusacelGSM)
MovistarRL <- TestInPolygon(Movistar)
names(Telcel3GRL)[2] <- "Telcel3G"
names(IusacelGSMRL)[2] <- "IusacelGSM" 
names(MovistarRL)[2] <- "Movistar" 
names(TelcelGSMRL)[2] <- "TelcelGSM" 
names(Iusacel3GLR)[2] <- "Iusacel3G" 
Rural <- merge(Rural,Telcel3GRL,by="CVE",all=TRUE)
Rural <- merge(Rural,Iusacel3GLR,by="CVE",all=TRUE)
Rural <- merge(Rural,IusacelGSMRL,by="CVE",all=TRUE)
Rural <- merge(Rural,MovistarRL,by="CVE",all=TRUE)
Rural <- merge(Rural,TelcelGSMRL,by="CVE",all=TRUE)
remove(Iusacel3GLR,IusacelGSMRL,MovistarRL,Telcel3GRL,TelcelGSMRL,ManyPolygons,TestInPolygon)
dir.create(path = "Data-out/LocalitiesWithCoverage",recursive=TRUE)
write.csv(Rural,"Data-out/LocalitiesWithCoverage/RuralLocalities.csv",row.names=FALSE)
remove(Rural)

##Get Urban Locality coverage for each company
#Switch projeciton to wgs84
system("ogr2ogr -t_srs EPSG:4326 Shapes/Localidadesurbanas/localidadesurbanas.shp Shapes/Localidadesurbanas/mglu2013v6_2.shp")
##IntersectShapes
#Havent managed it with code, need to fix some interect points
#Download intersection of urban locality polygons and coverage points
remove(Iusacel3G,IusacelGSM,Movistar,Telcel3G,TelcelGSM)
download.file("http://s3.amazonaws.com/shapefilescobertura/LocalidadesUrbanasIntersection.zip",destfile = "Shapes/IntersectFiles.zip")
unzip("Shapes/IntersectFiles.zip",exdir = "Shapes/")
file.remove("Shapes/IntersectFiles.zip")

##Load Intersections and get database
remove(list=ls())
Iusacel3G <- readOGR("Shapes/LocalidadesUrbanasIntersection/","Iusacel3GIntersect",stringsAsFactors = FALSE)@data
IusacelGSM <- readOGR("Shapes/LocalidadesUrbanasIntersection/","IusacelGSMIntersect",stringsAsFactors = FALSE)@data
Movistar <- readOGR("Shapes/LocalidadesUrbanasIntersection/","MovistarIntersect",stringsAsFactors = FALSE)@data
Telcel3G <- readOGR("Shapes/LocalidadesUrbanasIntersection/","Telcel3GIntersect",stringsAsFactors = FALSE)@data
TelcelGSM <- readOGR("Shapes/LocalidadesUrbanasIntersection/","TelcelIntersect",stringsAsFactors = FALSE)@data
Proveedores <- ls()
CleanIntersect <- function(Proveedor){
  TMP <- tbl_df(get(Proveedor))
  TMP1 <- mutate(TMP,CVE=paste(CVE_ENT,CVE_MUN,CVE_LOC,sep = "")) %>% 
    select(CVE) %>% mutate(Coverage=1)
  names(TMP1)[2] <- Proveedor
  return(TMP1)
}
CoberturaUrbana <- rbind_all(lapply(Proveedores,CleanIntersect))
remove(Iusacel3G,IusacelGSM,Movistar,Telcel3G,TelcelGSM,Proveedores,CleanIntersect)
write.csv(CoberturaUrbana,"Data-out/LocalitiesWithCoverage/UrbanLocalities.csv",row.names=FALSE)

##Generate bigger locality level coverage
Rural <- read.csv("Data-out/LocalitiesWithCoverage//RuralLocalities.csv",stringsAsFactors=FALSE)
Rural <- Rural[,c(1,9:13)]
Rural$CVE <- substr(Rural$CVE,1,9)
Rural$Type <- "Rural"
CoberturaUrbana$Type <- "Urban"
Cobertura <- rbind(CoberturaUrbana,Rural)
remove(Rural,CoberturaUrbana)
Cobertura <- unique(Cobertura)
write.csv(Cobertura,"Data-out/LocalitiesWithCoverage/FinalCoverage.csv",row.names=FALSE)
remove(list=ls())