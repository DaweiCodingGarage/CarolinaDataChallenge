library(ggmap)
library(rgdal)
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
carolina_data <- read.csv("~/Downloads/NYPD_Complaint_Data_Historic_short.csv",header = TRUE,stringsAsFactors = FALSE)
#remove data with no coordinate 
carolina_w_cord <- carolina_data[!is.na(carolina_data$Longitude)&!is.na(carolina_data$Latitude)&!is.na(carolina_data$CMPLNT_FR_TM),]
##plot ggmap
nymap <- get_map(location = 'New York',maptype = "roadmap",zoom =10)
## read in shape file of precincts
precincts<-readOGR(dsn=path.expand("~/Downloads/Police_Precincts"),layer="geo_export_2be84057-ecfe-4573-bfd3-43d07a0be5ba")
#make shapefile a dataframe
shapefile_df <- fortify(precincts)
#Dictionary to change Precincts Id
Lst = list()
for(i in seq(1,77)){
  Lst[i] = sort(as.numeric(unique(carolina_w_cord$ADDR_PCT_CD)))[i]
}
shapefile_df$intgroup <- as.integer(as.character(shapefile_df$group)) +1
shapefile_df$intgroup <-sapply(shapefile_df$intgroup,function(x) {Lst[x]})
# extract hour from crime time
carolina_w_cord$hour <- sapply(carolina_w_cord$CMPLNT_FR_TM,function(x) as.integer(substr(as.character(x),start=1,stop = nchar(as.character(x))-6)))
## add hour later
carolina_w_cord$month <- month(as.Date(carolina_w_cord$CMPLNT_FR_DT, "%m/%d/%Y"))
carolina_w_cord$year <- year(as.Date(carolina_w_cord$CMPLNT_FR_DT, "%m/%d/%Y"))

carolina_summary <- carolina_w_cord %>%
  group_by(ADDR_PCT_CD) %>%
  summarise(count=n())

shapefile_merge <- merge(shapefile_df,carolina_summary,by.x ='intgroup', by.y = 'ADDR_PCT_CD',sort=FALSE)
#help points in the same coordinate system
coordinates(carolina_w_cord)<-~Longitude+Latitude
proj4string(carolina_w_cord)<-CRS("+proj=longlat +datum=NAD83")
carolina_w_cord<-spTransform(carolina_w_cord, CRS(proj4string(precincts)))
#identical(proj4string(shapefile_df),proj4string(precincts))

carolina_w_cord<-data.frame(carolina_w_cord)

map <- ggplotly(ggmap(nymap)+
  geom_polygon(data = shapefile_merge, 
            aes(x = long, y = lat, fill= count,group = group),
            alpha=0.6, colour = "black", size = .2)+
  scale_fill_gradient(low = "yellow", high = "blue", na.value=NA) + 
  
  labs(x="", y="", title="New York City Precincts")+ #labels
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), # get rid of x ticks/text
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), # get rid of y ticks/text
        plot.title = element_text(lineheight=.8, face="bold", vjust=1))+ # make title bold and add space
  
  coord_equal(ratio=1))

shapefile_merge$groupfac <- sapply(shapefile_merge$intgroup,as.factor)
precinctsList <-  split.data.frame(shapefile_merge[,c('groupfac','long','lat')],shapefile_merge$groupfac)


