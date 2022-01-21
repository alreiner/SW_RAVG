###################THIS IS CODE TO BATCH PROCESS Sentinel RASTERS FOR SW RAVG PROJECT#####################

#####basic process is : import rasters->mask some->focalstats->attribute subsetted point shapefiles with rasters
#The process will avoid merging the rasters to avoid resampling b/c the origins are different

###########################################################################################
################Code below is for Sentinel EA, ran 7/29/2020##########
#note - if you rerun after today, change the names of the redondo rasters to REDI and RDEW in 'write to masked rasters' step.  Plot shapefile names were corrected after this was run.  both were backwards, but matched for extract in output when run 7/28.  

###SET WORKING DIRECTORY###
setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m"
library("raster")
library("rgdal")
library("gsubfn")
library("sf")
#note - stop here and clean file names, particularly 33SP to SP33

### Load mask shapefiles ###
BLUE_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BLUE_mask.shp")
BONI_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BONI_mask.shp")
DIEN_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/DIEN_mask.shp")
HIGH_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HIGH_mask.shp")
HOND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HOND_mask.shp")
SP33_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/SP33_mask.shp")
RedoInt_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/Redondo_mask_interior_EarlierPreImage.shp")
RedoWest_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/Redondo_mask_western7_LaterPreImage.shp")
TIND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/TIND_mask.shp")

###LOAD RASTERS###
rastlist <- list.files(path=path, pattern='tif$', full.names=TRUE)
rastlist

#load all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
#to check the index numbers of all imported raster list elements
allrasters

###MASKING###
allrast.BLUE_m<-lapply(allrasters[grep("BLUE",allrasters)],mask, BLUE_mask, updatevalue=NA, updateNA=FALSE)
allrast.BONI_m<-lapply(allrasters[grep("BONI",allrasters)],mask, BONI_mask, updatevalue=NA, updateNA=FALSE)
allrast.DIEN_m<-lapply(allrasters[grep("DIEN",allrasters)],mask, DIEN_mask, updatevalue=NA, updateNA=FALSE)
allrast.HIGH_m<-lapply(allrasters[grep("HIGH",allrasters)],mask, HIGH_mask, updatevalue=NA, updateNA=FALSE)
allrast.HOND_m<-lapply(allrasters[grep("HOND",allrasters)],mask, HOND_mask, updatevalue=NA, updateNA=FALSE)
allrast.RedoInt_m<-lapply(allrasters[grep("REDO_NM3518110814620180404_20170502_20190502",allrasters)],mask, RedoInt_mask, updatevalue=NA, updateNA=FALSE)
allrast.RedoWest_m<-lapply(allrasters[grep("REDO_NM3518110814620180404_20170502_20190427",allrasters)],mask, RedoWest_mask, updatevalue=NA, updateNA=FALSE)
allrast.SP33_m<-lapply(allrasters[grep("SP33",allrasters)],mask, SP33_mask, updatevalue=NA, updateNA=FALSE)
allrast.TIND_m<-lapply(allrasters[grep("TIND",allrasters)],mask, TIND_mask, updatevalue=NA, updateNA=FALSE)

###Write Masked Raster###
#Need to create 'masked' folder in Windows Explorer first
lapply(allrast.BLUE_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.BONI_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.DIEN_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HIGH_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HOND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.RedoInt_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.RedoWest_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.SP33_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.TIND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))

#stop here and replace original rasters with masked rasters!

### reload all rasters from the directory which now includes the masked rasters ###

#instead of loading all rasters at once, create separate lists grouped by index type to prep for looping
rastlist_dnbr_no_offset <- list.files(path=path, pattern='_dnbr_no_offset', full.names=TRUE)
rastlist_dnbr_with_offset <- list.files(path=path, pattern='_dnbr_with_offset', full.names=TRUE)
rastlist_rbr_no_offset <- list.files(path=path, pattern='_rbr_no_offset', full.names=TRUE)
rastlist_rbr_with_offset <- list.files(path=path, pattern='_rbr_with_offset', full.names=TRUE)
rastlist_rdnbr_no_offset <- list.files(path=path, pattern='_rdnbr_no_offset', full.names=TRUE)
rastlist_rdnbr_with_offset <- list.files(path=path, pattern='_rdnbr_with_offset', full.names=TRUE)

#use lapply to load all rasters in these lists
allrasters_dnbr_no_offset <- lapply(rastlist_dnbr_no_offset, raster)
allrasters_dnbr_with_offset <- lapply(rastlist_dnbr_with_offset, raster)
allrasters_rbr_no_offset <- lapply(rastlist_rbr_no_offset, raster)
allrasters_rbr_with_offset <- lapply(rastlist_rbr_with_offset, raster)
allrasters_rdnbr_no_offset <- lapply(rastlist_rdnbr_no_offset, raster)
allrasters_rdnbr_with_offset <- lapply(rastlist_rdnbr_with_offset, raster)

#apply a kernel to weight neighboring cells according to weights in kernel
#circlefilter20m below that starts with 0.077 is for 20m imagery-derived data
circlefilter20m<-matrix(c(0.0766,0.1377,0.0766,0.1377,0.1427,0.1377,0.0766,0.1377,0.0766),nc=3, nr=3)

### focal stats ###
allrasters_dnbr_no_offset_focal<-lapply(allrasters_dnbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_dnbr_with_offset_focal<-lapply(allrasters_dnbr_with_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rbr_no_offset_focal<-lapply(allrasters_rbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rbr_with_offset_focal<-lapply(allrasters_rbr_with_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rdnbr_no_offset_focal<-lapply(allrasters_rdnbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rdnbr_with_offset_focal<-lapply(allrasters_rdnbr_with_offset,focal,circlefilter20m,na.rm=TRUE)

#write focal rasters for posterity
#note - before run this, manually change name of redo rasters: 
# REDW <- the raster with 4/27 date in name
#REDI <- the raster with both dates 5/2

#need a list of the filenames, not a list of the data in the files to which to name the focal rasters
#need to recreate the raster lists by type now that I have changed the REDO names to be unique
rastlist_dnbr_no_offset <- list.files(path=path, pattern='_dnbr_no_offset', full.names=TRUE)
rastlist_dnbr_with_offset <- list.files(path=path, pattern='_dnbr_with_offset', full.names=TRUE)
rastlist_rbr_no_offset <- list.files(path=path, pattern='_rbr_no_offset', full.names=TRUE)
rastlist_rbr_with_offset <- list.files(path=path, pattern='_rbr_with_offset', full.names=TRUE)
rastlist_rdnbr_no_offset <- list.files(path=path, pattern='_rdnbr_no_offset', full.names=TRUE)
rastlist_rdnbr_with_offset <- list.files(path=path, pattern='_rdnbr_with_offset', full.names=TRUE)

#write focal rasters in loop

# dnbr_no_offset
for (i in 1:length(allrasters_dnbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_dnbr_no_offset[[i]],114,117)
  writeRaster(allrasters_dnbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_dnbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}

# dnbr_with_offset
for (i in 1:length(allrasters_dnbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_dnbr_with_offset[[i]],114,117)
  writeRaster(allrasters_dnbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_dnbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rbr_no_offset
for (i in 1:length(allrasters_rbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_rbr_no_offset[[i]],114,117)
  writeRaster(allrasters_rbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rbr_with_offset
for (i in 1:length(allrasters_rbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_rbr_with_offset[[i]],114,117)
  writeRaster(allrasters_rbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rdnbr_no_offset
for (i in 1:length(allrasters_rdnbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_rdnbr_no_offset[[i]],114,117)
  writeRaster(allrasters_rdnbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rdnbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}

# rdnbr_with_offset
for (i in 1:length(allrasters_rdnbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_rdnbr_with_offset[[i]],114,117)
  writeRaster(allrasters_rdnbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rdnbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}



### attribute plots from rasters ###
#list and import shapefiles
#list and import shapefiles
#note - for IA, use the REDO.shp which is all REDO plots; for EA, use REDI.shp and REDW.shp, split b/c of disturbance in one scene
pathplots<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/Plots" 
plotlist<-list.files(pathplots, pattern="\\.shp$", full.names=TRUE)
allplots<-lapply(plotlist, readOGR)

#look at a plotlist and a rasterlist to make sure they have the same number and plot/rasters in same order
allplots
allrasters_dnbr_no_offset_focal


### Extract over each index type ###

### dnbr_no_offset
dnbr_no.datalist<-list()
for (i in 1:length(allrasters_dnbr_no_offset_focal)) {
      extract.data.dnbr_no<-data.frame(allplots[[i]]$Plot,
        extract(allrasters_dnbr_no_offset_focal[[i]],allplots[[i]]))
      extract.data.dnbr_no$i<-i #keep track of which iteration produced it
      dnbr_no.datalist[[i]]<-extract.data.dnbr_no #add extracted data to the datalist
}
dnbr_no.datalist #check the list

#bind all the data in the list by rows into a dataframe and look at it
all.dnbr_no.data<-dplyr::bind_rows(dnbr_no.datalist)
fix(all.dnbr_no.data)

#above gives funny names, so change names - 
##############CHANGE EA/SENTINEL to match data  
names(all.dnbr_no.data)[1]<-"Plot"
names(all.dnbr_no.data)[2]<-"Sentinel_EA_dnbr_no_offset"
names(all.dnbr_no.data)[3]<-"i"
fix(all.dnbr_no.data)


### dnbr_with_offset
dnbr_with.datalist<-list()
for (i in 1:length(allrasters_dnbr_with_offset_focal)) {
  extract.data.dnbr_with<-data.frame(allplots[[i]]$Plot,
                                   extract(allrasters_dnbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.dnbr_with$i<-i #keep track of which iteration produced it
  dnbr_with.datalist[[i]]<-extract.data.dnbr_with #add data to the datalist
}
dnbr_with.datalist

all.dnbr_with.data<-dplyr::bind_rows(dnbr_with.datalist)
fix(all.dnbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.dnbr_with.data)[1]<-"Plot"
names(all.dnbr_with.data)[2]<-"Sentinel_EA_dnbr_with_offset"
names(all.dnbr_with.data)[3]<-"i"
fix(all.dnbr_with.data)


### rbr_no_offset
rbr_no.datalist<-list()
for (i in 1:length(allrasters_rbr_no_offset_focal)) {
  extract.data.rbr_no<-data.frame(allplots[[i]]$Plot,
                                   extract(allrasters_rbr_no_offset_focal[[i]],allplots[[i]]))
  extract.data.rbr_no$i<-i #keep track of which iteration produced it
  rbr_no.datalist[[i]]<-extract.data.rbr_no #add data to the datalist
}
rbr_no.datalist

all.rbr_no.data<-dplyr::bind_rows(rbr_no.datalist)
fix(all.rbr_no.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rbr_no.data)[1]<-"Plot"
names(all.rbr_no.data)[2]<-"Sentinel_EA_rbr_no_offset"
names(all.rbr_no.data)[3]<-"i"
fix(all.rbr_no.data)


### rbr_with_offset
rbr_with.datalist<-list()
for (i in 1:length(allrasters_rbr_with_offset_focal)) {
  extract.data.rbr_with<-data.frame(allplots[[i]]$Plot,
                                     extract(allrasters_rbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.rbr_with$i<-i #keep track of which iteration produced it
  rbr_with.datalist[[i]]<-extract.data.rbr_with #add data to the datalist
}
rbr_with.datalist

all.rbr_with.data<-dplyr::bind_rows(rbr_with.datalist)
fix(all.rbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rbr_with.data)[1]<-"Plot"
names(all.rbr_with.data)[2]<-"Sentinel_EA_rbr_with_offset"
names(all.rbr_with.data)[3]<-"i"
fix(all.rbr_with.data)


###rdnbr_no_offset
rdnbr_no.datalist<-list()
for (i in 1:length(allrasters_rdnbr_no_offset_focal)) {
  extract.data.rdnbr_no<-data.frame(allplots[[i]]$Plot,
                                   extract(allrasters_rdnbr_no_offset_focal[[i]],allplots[[i]]))
  extract.data.rdnbr_no$i<-i #keep track of which iteration produced it
  rdnbr_no.datalist[[i]]<-extract.data.rdnbr_no #add data to the datalist
}
rdnbr_no.datalist

all.rdnbr_no.data<-dplyr::bind_rows(rdnbr_no.datalist)
fix(all.rdnbr_no.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rdnbr_no.data)[1]<-"Plot"
names(all.rdnbr_no.data)[2]<-"Sentinel_EA_rdnbr_no_offset"
names(all.rdnbr_no.data)[3]<-"i"
fix(all.rdnbr_no.data)


### rdnbr_with_offset
rdnbr_with.datalist<-list()
for (i in 1:length(allrasters_rdnbr_with_offset_focal)) {
  extract.data.rdnbr_with<-data.frame(allplots[[i]]$Plot,
                                     extract(allrasters_rdnbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.rdnbr_with$i<-i #keep track of which iteration produced it
  rdnbr_with.datalist[[i]]<-extract.data.rdnbr_with #add data to the datalist
}
rdnbr_with.datalist

all.rdnbr_with.data<-dplyr::bind_rows(rdnbr_with.datalist)
fix(all.rdnbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rdnbr_with.data)[1]<-"Plot"
names(all.rdnbr_with.data)[2]<-"Sentinel_EA_rdnbr_with_offset"
names(all.rdnbr_with.data)[3]<-"i"
fix(all.rdnbr_with.data)


### combine all extract output and write to CSV ###
#Note - this doesn't join based on Plot, but does put it all into one dataframe and you can visuall check that all plot cols are same
all.Sentinel.EA.extract<-dplyr::bind_cols(all.dnbr_no.data, all.dnbr_with.data, all.rbr_no.data, all.rbr_with.data, all.rdnbr_no.data, all.rdnbr_with.data)
fix(all.Sentinel.EA.extract)
write.csv(all.Sentinel.EA.extract, file="Sentinel_EA_AllPlots_Attr.csv")
#writes to current directory






###########################################################################################
################Code below is for Sentinel IA ##########

###SET WORKING DIRECTORY###
setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m"
library("raster")
library("rgdal")
library("gsubfn")
library("sf")
#note - stop here and clean file names, particularly 33SP to SP33

### Load mask shapefiles ###
BLUE_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BLUE_mask.shp")
BONI_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BONI_mask.shp")
DIEN_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/DIEN_mask.shp")
HIGH_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HIGH_mask.shp")
HOND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HOND_mask.shp")
SP33_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/SP33_mask.shp")
RedoInt_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/Redondo_mask_interior_EarlierPreImage.shp")
RedoWest_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/Redondo_mask_western7_LaterPreImage.shp")
TIND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/TIND_mask.shp")

###LOAD RASTERS###
rastlist <- list.files(path=path, pattern='tif$', full.names=TRUE)
rastlist

#load all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)
#to check the index numbers of all imported raster list elements
allrasters

###MASKING###
#note - IA data does not need masking for Redondo
allrast.BLUE_m<-lapply(allrasters[grep("BLUE",allrasters)],mask, BLUE_mask, updatevalue=NA, updateNA=FALSE)
allrast.BONI_m<-lapply(allrasters[grep("BONI",allrasters)],mask, BONI_mask, updatevalue=NA, updateNA=FALSE)
allrast.DIEN_m<-lapply(allrasters[grep("DIEN",allrasters)],mask, DIEN_mask, updatevalue=NA, updateNA=FALSE)
allrast.HIGH_m<-lapply(allrasters[grep("HIGH",allrasters)],mask, HIGH_mask, updatevalue=NA, updateNA=FALSE)
allrast.HOND_m<-lapply(allrasters[grep("HOND",allrasters)],mask, HOND_mask, updatevalue=NA, updateNA=FALSE)
allrast.SP33_m<-lapply(allrasters[grep("SP33",allrasters)],mask, SP33_mask, updatevalue=NA, updateNA=FALSE)
allrast.TIND_m<-lapply(allrasters[grep("TIND",allrasters)],mask, TIND_mask, updatevalue=NA, updateNA=FALSE)

###Write Masked Raster###
#Need to create 'masked' folder in Windows Explorer first
lapply(allrast.BLUE_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.BONI_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.DIEN_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HIGH_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HOND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.SP33_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.TIND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))

#stop here and replace original rasters with masked rasters!

### reload all rasters from the directory which now includes the masked rasters ###

#instead of loading all rasters at once, create separate lists grouped by index type to prep for looping
rastlist_dnbr_no_offset <- list.files(path=path, pattern='_dnbr_no_offset', full.names=TRUE)
rastlist_dnbr_with_offset <- list.files(path=path, pattern='_dnbr_with_offset', full.names=TRUE)
rastlist_rbr_no_offset <- list.files(path=path, pattern='_rbr_no_offset', full.names=TRUE)
rastlist_rbr_with_offset <- list.files(path=path, pattern='_rbr_with_offset', full.names=TRUE)
rastlist_rdnbr_no_offset <- list.files(path=path, pattern='_rdnbr_no_offset', full.names=TRUE)
rastlist_rdnbr_with_offset <- list.files(path=path, pattern='_rdnbr_with_offset', full.names=TRUE)

#use lapply to load all rasters in these lists
allrasters_dnbr_no_offset <- lapply(rastlist_dnbr_no_offset, raster)
allrasters_dnbr_with_offset <- lapply(rastlist_dnbr_with_offset, raster)
allrasters_rbr_no_offset <- lapply(rastlist_rbr_no_offset, raster)
allrasters_rbr_with_offset <- lapply(rastlist_rbr_with_offset, raster)
allrasters_rdnbr_no_offset <- lapply(rastlist_rdnbr_no_offset, raster)
allrasters_rdnbr_with_offset <- lapply(rastlist_rdnbr_with_offset, raster)

#apply a kernel to weight neighboring cells according to weights in kernel
#circlefilter20m below that starts with 0.077 is for 20m imagery-derived data
circlefilter20m<-matrix(c(0.0766,0.1377,0.0766,0.1377,0.1427,0.1377,0.0766,0.1377,0.0766),nc=3, nr=3)

### focal stats ###
allrasters_dnbr_no_offset_focal<-lapply(allrasters_dnbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_dnbr_with_offset_focal<-lapply(allrasters_dnbr_with_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rbr_no_offset_focal<-lapply(allrasters_rbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rbr_with_offset_focal<-lapply(allrasters_rbr_with_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rdnbr_no_offset_focal<-lapply(allrasters_rdnbr_no_offset,focal,circlefilter20m,na.rm=TRUE)
allrasters_rdnbr_with_offset_focal<-lapply(allrasters_rdnbr_with_offset,focal,circlefilter20m,na.rm=TRUE)

#write focal rasters for posterity

type<-'SW_RAVG_IA_Sentinel'
firename<-vector(mode="character",length=24)
# dnbr_no_offset
for (i in 1:length(allrasters_dnbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_dnbr_no_offset[[i]],114,117)
  writeRaster(allrasters_dnbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_dnbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}

# dnbr_with_offset
for (i in 1:length(allrasters_dnbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_dnbr_with_offset[[i]],114,117)
  writeRaster(allrasters_dnbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_dnbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rbr_no_offset
for (i in 1:length(allrasters_rbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_rbr_no_offset[[i]],114,117)
  writeRaster(allrasters_rbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rbr_with_offset
for (i in 1:length(allrasters_rbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_rbr_with_offset[[i]],114,117)
  writeRaster(allrasters_rbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}


# rdnbr_no_offset
for (i in 1:length(allrasters_rdnbr_no_offset_focal)) {
  firename[[i]]<-substr(rastlist_rdnbr_no_offset[[i]],114,117)
  writeRaster(allrasters_rdnbr_no_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rdnbr_no_offset_focal"), format="GTiff", overwrite=TRUE)
}

# rdnbr_with_offset
for (i in 1:length(allrasters_rdnbr_with_offset_focal)) {
  firename[[i]]<-substr(rastlist_rdnbr_with_offset[[i]],114,117)
  writeRaster(allrasters_rdnbr_with_offset_focal[[i]], filename=paste0(path,"/focal/",type,"_",firename[[i]],"_rdnbr_with_offset_focal"), format="GTiff", overwrite=TRUE)
}



### attribute plots from rasters ###
#list and import shapefiles
#note - for IA, use the REDO.shp which is all REDO plots; for EA, use REDI.shp and REDW.shp, split b/c of disturbance in one scene
pathplots<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/Plots" 
plotlist<-list.files(pathplots, pattern="\\.shp$", full.names=TRUE)
allplots<-lapply(plotlist, readOGR)

#look at a plotlist and a rasterlist to make sure they have the same number and plot/rasters in same order
allplots
allrasters_dnbr_no_offset_focal


### Extract over each index type ###

### dnbr_no_offset
dnbr_no.datalist<-list()
for (i in 1:length(allrasters_dnbr_no_offset_focal)) {
  extract.data.dnbr_no<-data.frame(allplots[[i]]$Plot,
                                   extract(allrasters_dnbr_no_offset_focal[[i]],allplots[[i]]))
  extract.data.dnbr_no$i<-i #keep track of which iteration produced it
  dnbr_no.datalist[[i]]<-extract.data.dnbr_no #add extracted data to the datalist
}
dnbr_no.datalist #check the list

#bind all the data in the list by rows into a dataframe and look at it
all.dnbr_no.data<-dplyr::bind_rows(dnbr_no.datalist)
fix(all.dnbr_no.data)

#above gives funny names, so change names - 
##############CHANGE EA/SENTINEL to match data  
names(all.dnbr_no.data)[1]<-"Plot"
names(all.dnbr_no.data)[2]<-"Sentinel_IA_dnbr_no_offset"
names(all.dnbr_no.data)[3]<-"i"
fix(all.dnbr_no.data)


### dnbr_with_offset
dnbr_with.datalist<-list()
for (i in 1:length(allrasters_dnbr_with_offset_focal)) {
  extract.data.dnbr_with<-data.frame(allplots[[i]]$Plot,
                                     extract(allrasters_dnbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.dnbr_with$i<-i #keep track of which iteration produced it
  dnbr_with.datalist[[i]]<-extract.data.dnbr_with #add data to the datalist
}
dnbr_with.datalist

all.dnbr_with.data<-dplyr::bind_rows(dnbr_with.datalist)
fix(all.dnbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.dnbr_with.data)[1]<-"Plot"
names(all.dnbr_with.data)[2]<-"Sentinel_IA_dnbr_with_offset"
names(all.dnbr_with.data)[3]<-"i"
fix(all.dnbr_with.data)


### rbr_no_offset
rbr_no.datalist<-list()
for (i in 1:length(allrasters_rbr_no_offset_focal)) {
  extract.data.rbr_no<-data.frame(allplots[[i]]$Plot,
                                  extract(allrasters_rbr_no_offset_focal[[i]],allplots[[i]]))
  extract.data.rbr_no$i<-i #keep track of which iteration produced it
  rbr_no.datalist[[i]]<-extract.data.rbr_no #add data to the datalist
}
rbr_no.datalist

all.rbr_no.data<-dplyr::bind_rows(rbr_no.datalist)
fix(all.rbr_no.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rbr_no.data)[1]<-"Plot"
names(all.rbr_no.data)[2]<-"Sentinel_IA_rbr_no_offset"
names(all.rbr_no.data)[3]<-"i"
fix(all.rbr_no.data)


### rbr_with_offset
rbr_with.datalist<-list()
for (i in 1:length(allrasters_rbr_with_offset_focal)) {
  extract.data.rbr_with<-data.frame(allplots[[i]]$Plot,
                                    extract(allrasters_rbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.rbr_with$i<-i #keep track of which iteration produced it
  rbr_with.datalist[[i]]<-extract.data.rbr_with #add data to the datalist
}
rbr_with.datalist

all.rbr_with.data<-dplyr::bind_rows(rbr_with.datalist)
fix(all.rbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rbr_with.data)[1]<-"Plot"
names(all.rbr_with.data)[2]<-"Sentinel_IA_rbr_with_offset"
names(all.rbr_with.data)[3]<-"i"
fix(all.rbr_with.data)


###rdnbr_no_offset
rdnbr_no.datalist<-list()
for (i in 1:length(allrasters_rdnbr_no_offset_focal)) {
  extract.data.rdnbr_no<-data.frame(allplots[[i]]$Plot,
                                    extract(allrasters_rdnbr_no_offset_focal[[i]],allplots[[i]]))
  extract.data.rdnbr_no$i<-i #keep track of which iteration produced it
  rdnbr_no.datalist[[i]]<-extract.data.rdnbr_no #add data to the datalist
}
rdnbr_no.datalist

all.rdnbr_no.data<-dplyr::bind_rows(rdnbr_no.datalist)
fix(all.rdnbr_no.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rdnbr_no.data)[1]<-"Plot"
names(all.rdnbr_no.data)[2]<-"Sentinel_IA_rdnbr_no_offset"
names(all.rdnbr_no.data)[3]<-"i"
fix(all.rdnbr_no.data)


### rdnbr_with_offset
rdnbr_with.datalist<-list()
for (i in 1:length(allrasters_rdnbr_with_offset_focal)) {
  extract.data.rdnbr_with<-data.frame(allplots[[i]]$Plot,
                                      extract(allrasters_rdnbr_with_offset_focal[[i]],allplots[[i]]))
  extract.data.rdnbr_with$i<-i #keep track of which iteration produced it
  rdnbr_with.datalist[[i]]<-extract.data.rdnbr_with #add data to the datalist
}
rdnbr_with.datalist

all.rdnbr_with.data<-dplyr::bind_rows(rdnbr_with.datalist)
fix(all.rdnbr_with.data)

#gives funny names, so change them first - 
##############CHANGE EA/SENTINEL to match data  
names(all.rdnbr_with.data)[1]<-"Plot"
names(all.rdnbr_with.data)[2]<-"Sentinel_IA_rdnbr_with_offset"
names(all.rdnbr_with.data)[3]<-"i"
fix(all.rdnbr_with.data)


### combine all extract output and write to CSV ###
#Note - this doesn't join based on Plot, but does put it all into one dataframe and you can visuall check that all plot cols are same
all.Sentinel.IA.extract<-dplyr::bind_cols(all.dnbr_no.data, all.dnbr_with.data, all.rbr_no.data, all.rbr_with.data, all.rdnbr_no.data, all.rdnbr_with.data)
fix(all.Sentinel.IA.extract)
write.csv(all.Sentinel.IA.extract, file="Sentinel_IA_AllPlots_Attr.csv")
#writes to current directory

