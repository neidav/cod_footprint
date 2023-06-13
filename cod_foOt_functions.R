#Load Libraries
library(raster)
library(rgeos)
library(rgdal)
library(stringr)
library(sf)

# Micah's functions -- i dont think we need this line anymore  anymore :P 
source("/Users/neidavillanueva/Desktop/cod_footprint/micahs_functions.R")

###########################################################
#MAKE SURE ALL FILE PATHS HAVE BEEN MODIFIED TO YOUR SPECIFIC COMPUTER SETTINGS
# below are the variables which you will need to do that for: 
  # shp_path (line: )
  # gis_path (in plot1_funcion)(line )
  # load (should be a dem.rdat file)
############################################################
# Creating KML file

# Load project shape file
shp_path<-"/Users/neidavillanueva/Desktop/cod_footprint" # project shape file location
cod_poly<-readOGR(shp_path,"cod_poly_shapefile") #shape file we want to search
cod_poly<-spTransform(cod_poly,masp)  # Project to Mass state plane coordinates (meters)
cod_poly_data = data.frame(cod_poly@data$Dscrptn) #grab the information entered for all of the polygons

sort_func <- function(df, fish_type, year, season, abundance){ 
  #here we are making a function that lets us look through our df to create this list of
  #returned rows that meet our requirements. this lets us filter for things like
  #fish type, season, abundance, etc.
  
  # if another component were to be added, you would add the grepl line equivalent of it 
  # and also include the new component in the function values
  
  lst = c() #re clear list we are making so old filtering/conditions don't apply
  for (i in 1:(nrow(df))){ #for each row in data frame we give you do the below if statements. this makes it go through all our rows in the df
    #i is a way to say which row number is being searched. these numbers are the output to our list
    if (grepl('low',tolower(df[i,])) == TRUE) next
        
    if (grepl(tolower(abundance),tolower(df[i,])) == TRUE
        &
        grepl(tolower(season),tolower(df[i,])) == TRUE
        &
        grepl(year,(df[i,])) == TRUE
        &
        grepl(tolower(fish_type),tolower(df[i,])) == TRUE){
      lst<- c(lst,i) # if cod poly has an extra row modify here
      
    }
  }
  return(lst)
  
}

zplot<-function(r,p=0.99,col=rev(terrain.colors(255)),...){
  rv<-raster::values(r)
  q<-quantile(rv,p,na.rm=TRUE)
  brx<-c(seq(0,q,length=length(col)),max(rv,na.rm=TRUE))
  leg<-pretty(range(rv,na.rm=TRUE))
  plot(r,col=col,axis.args=list(at=leg),...)
  
}

plot_1 <- function(main_df, sort_lst){
  sorted_df = main_df[FALSE,]
 
  for (num in sort_lst){
    
    (subb<- subset(main_df[num,]))
    sorted_df <- rbind(sorted_df,subb)
  }
  
  plot(sorted_df,col=adjustcolor("purple",0.25))
  
  sorted_df$NFISHER<-1 # Need a data col to summarize when converting to raster
  rg<-refgrid(buffer(sorted_df,25e3),res=1000) #CREATE A REFENCE GRID @ 1 km resolution (buffer cod_poly by 25 km to pad the extent)
  gridded(rg)<-TRUE # telling R its a regular grid
  rgr<-raster(rg) #CONVERT TO A REFERENCE GRID RASTER (needed when rasterizing a polygon layer)
  r<-raster::rasterize(x=polygons(sorted_df),y=rgr,field=sorted_df$NFISHER,fun=sum) # Convert polygon to raster, where CELL VALUE = number of overlapping polygons
  plot(r)
  
  #'making pretty'
 
  gispath<-"Desktop/cod_footprint" # USA base layer location
  crs_use<-masp #Mass state plane meters projection
  usa<-readOGR(gispath,"usa") # Reading in USA shape file
  usa_ne<-gUnaryUnion(spTransform(subset(usa,STATE%in%c("ME","NH","MA","RI","CT","NY")),crs_use)) # Parsing through states
  load("/Users/neidavillanueva/Desktop/cod_footprint/dem.rdat")
  
  zplot(r,p=0.99)
  crp_bw<-colorRampPalette(c("lightgray","black")) #GRAYSCALE?
  crp_col<-colorRampPalette(c("yellow","red","purple")) #COLORFUL? (CHANGE TO WHATEVER YOU LIKE)
  par(mar=c(3,3,1,1)) #ADJUST THE MARGINS
  zplot(r,p=0.99,col=crp_col(maxValue(r)),bg="gray",axes=FALSE
        ,legend.args=list(text="# Fishers",adj=0,line=0.5)) #PLOT THE RASTER
  plot(usa_ne,add=TRUE,col="black") #ADD SHORELINE IN BLACK
  
  
  
  # SHOW THE AREA WHERE AT LEAST nmin FISHERS IDENTIFIED
  contour(abs(dem),levels=c(50,100),add=TRUE,col=adjustcolor("blue",0.5)) #ADD SOME DEPTH CONTOURS
  maptix(ytix=42:44,xtix=-(71:67),crx=masp) #ADD SOME LAT LON TIC MARKS ON MARGINS
  
  shp_path<-"/Users/neidavillanueva/Desktop/cod_footprint"
  closure_poly <- readOGR(shp_path,"closures")
  closure_poly<- spTransform(closure_poly,masp)
  plot(closure_poly,add = TRUE,col = "lightgrey")
  adjustcolor("lightgrey", alpha.f=0.5) 
  
  
  nmin<-2
  clx<-"red3"
  alf<-0.25 #TRANSPARENCY FOR SHADED REGIONS
  crp_dem<-colorRampPalette(c("white","midnightblue"))
  plot(sorted_df,border="transparent")
  contour(abs(dem),levels=c(50,100),add=TRUE,col=adjustcolor("blue",0.5)) #ADD SOME DEPTH CONTOURS
 
  
  
  plot(usa_ne,add=TRUE,col="black") #ADD SHORELINE IN BLACK
  if(nmin>1)contour(r,levels=nmin,add=TRUE,col=clx,drawlabels=FALSE,lwd=2) #FOOTPRINT BORDER
  if(nmin==1)plot(gUnaryUnion(sorted_df),add=TRUE,border=clx,lwd=2) #FOOTPRINT BORDER (contour doesn't work at min value)
  plot(r>=nmin,col=adjustcolor(c("transparent",clx),alf),add=TRUE,legend=FALSE) #FILL THE FOOTPRINT
  maptix(ytix=42:44,xtix=-(71:67),crx=masp) #ADD SOME LAT LON TIC MARKS ON MARGINS
  myscalebar(x=0.75,y=0.75,len=50,barunits="km",ntics=6) #ADD SCALEBAR
  plot(closure_poly,add = TRUE,col = "lightgrey")
  adjustcolor("lightgrey", alpha.f=0.5) 
  legend("right",paste(nmin,"+ Fishers",sep=""),fill=adjustcolor(clx,alf),border=clx)
  box()
 
}

# THIS IS THE SECTION WE WILL MODIFYTO GET THE PLOTS WE WANT !!!!
# each map will need the equivalent of these two lines of code i.e. using the 
# sort_func and the plot_1 function
spring_2004_lst = sort_func(cod_poly_data,'2013','cod|bycatch','fall|sep|sept|oct|nov|dec','bycatch')
spring_2004_plot1 = plot_1(cod_poly, spring_2004_lst)

