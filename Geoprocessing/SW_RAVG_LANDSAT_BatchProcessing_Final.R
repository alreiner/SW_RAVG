###################THIS IS CODE TO BATCH PROCESS LANDSAT RASTERS FOR SW RAVG PROJECT#####################

#####basic process is : import rasters->mask some->merge rasters->focalstats->attribute subsetted point shapefiles with rasters


###SET WORKING DIRECTORY###

setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Landsat"

getwd()
library("raster")
library("rgdal")
library("gsubfn")
#library("stringr")

##renamed 33SP files to SP33

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

#load all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)


###MASKING###

allrast.BLUE_m<-lapply(allrasters[grep("BLUE",allrasters)],mask, BLUE_mask, updatevalue=NA, updateNA=FALSE)
allrast.BONI_m<-lapply(allrasters[grep("BONI",allrasters)],mask, BONI_mask, updatevalue=NA, updateNA=FALSE)
allrast.DIEN_m<-lapply(allrasters[grep("DIEN",allrasters)],mask, DIEN_mask, updatevalue=NA, updateNA=FALSE)
allrast.HIGH_m<-lapply(allrasters[grep("HIGH",allrasters)],mask, HIGH_mask, updatevalue=NA, updateNA=FALSE)
allrast.HOND_m<-lapply(allrasters[grep("HOND",allrasters)],mask, HOND_mask, updatevalue=NA, updateNA=FALSE)
allrast.REDI_m<-lapply(allrasters[grep("REDO_NM3518110814620180404_20150619_20190614",allrasters)],mask, RedoInt_mask, updatevalue=NA, updateNA=FALSE)
allrast.REDW_m<-lapply(allrasters[grep("REDO_NM3518110814620180404_20170421_20190418",allrasters)],mask, RedoWest_mask, updatevalue=NA, updateNA=FALSE)
allrast.SP33_m<-lapply(allrasters[grep("SP33",allrasters)],mask, SP33_mask, updatevalue=NA, updateNA=FALSE)
allrast.TIND_m<-lapply(allrasters[grep("TIND",allrasters)],mask, TIND_mask, updatevalue=NA, updateNA=FALSE)



###Write Masked Raster###
#Need to create 'masked' folder in Windows Explorer first
lapply(allrast.BLUE_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.BONI_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.DIEN_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HIGH_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HOND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.REDI_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.REDW_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.SP33_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.TIND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))


###Replace non-masked rasters with masked rasters before Mosaic###
#MOVE FILES BY HAND, REMOVE REDOONDO TO REPLACE WITH INTERIOR/WESTERN7 MASKED FILES


###MOSAIC###

#create list by searching pattern for each index type, and mosaic everything in that list
#Note that allrast.*FIRE*_m lists are in the Global Environment, however now I want to mosaic by *INdex type*
#also note that I have moved the masked rasters by hand to the main directory to replace those of the original extent
#so, at this point, allrasters still points to raster list without the masked rasters
#Need to first recreate a list with the newly masked/moved files

#LOAD RASTERS#
rastlist_m <- list.files(path=path, pattern='tif$', full.names=TRUE)
#load all raster files in folder using lapply
allrasters_m <- lapply(rastlist_m, raster)

#mosaicking rasters
allrasters_dnbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_dnbr_no_offset",allrasters_m)])
allrasters_dnbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_dnbr_with_offset",allrasters_m)])
allrasters_rbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_rbr_no_offset",allrasters_m)])
allrasters_rbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_rbr_with_offset",allrasters_m)])
allrasters_rdnbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_rdnbr_no_offset",allrasters_m)])
allrasters_rdnbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_rdnbr_with_offset",allrasters_m)])

#write these rasters 
writeRaster(allrasters_dnbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_dnbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_dnbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_dnbr_with_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rbr_with_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rdnbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rdnbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rdnbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rdnbr_with_offset_mosaic"), format="GTiff")


###FOCAL STATS###

#apply a kernel to weight neighboring cells according to weights in kernel
#kernel below was revised 2/5 to downweight center pixel more so that previous version
#and assign center pixel the new value, matrix fills by column as default
circlefilter=matrix(c(0.025,0.146,0.025,0.146,0.32,0.146,0.025,0.146,0.025),nc=3, nr=3)
allrasters_dnbr_no_offset_focal<-focal(allrasters_dnbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_dnbr_with_offset_focal<-focal(allrasters_dnbr_with_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rbr_no_offset_focal<-focal(allrasters_rbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rbr_with_offset_focal<-focal(allrasters_rbr_with_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rdnbr_no_offset_focal<-focal(allrasters_rdnbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rdnbr_with_offset_focal<-focal(allrasters_rdnbr_with_offset_mosaic,circlefilter,na.rm=TRUE)

#write raster
#change type for each type
#create 'focal' folder each time
type='SW_RAVG_EA_Landsat'
writeRaster(allrasters_dnbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_dnbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_dnbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_dnbr_with_offset_focal"), format="GTiff")
writeRaster(allrasters_rbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_rbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_rbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_rbr_with_offset_focal"), format="GTiff")
writeRaster(allrasters_rdnbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_rdnbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_rdnbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_rdnbr_with_offset_focal"), format="GTiff")


########extract raster data at plot locations###########

### Extract data from each mosaic for plot locations ###
#with LANDSAT data which is mosaicked, import the shapefile with all plots

AllPlots<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/AllPlots.shp")

#DNBR no offset
extract.data.dnbr.no<-data.frame(coordinates(AllPlots),
                    AllPlots$Plot,
                    extract(allrasters_dnbr_no_offset_focal,AllPlots))
names(extract.data.dnbr.no)<-c("x1","x2","x3","Plot","Landsat_EA_dnbr_no_offset")


#DNBR with offset
extract.data.dnbr.with<-data.frame(coordinates(AllPlots),
                                 AllPlots$Plot,
                                 extract(allrasters_dnbr_with_offset_focal,AllPlots))
names(extract.data.dnbr.with)<-c("x1","x2","x3","Plot","Landsat_EA_dnbr_with_offset")


#RBR no offset
extract.data.rbr.no<-data.frame(coordinates(AllPlots),
                                   AllPlots$Plot,
                                   extract(allrasters_rbr_no_offset_focal,AllPlots))
names(extract.data.rbr.no)<-c("x1","x2","x3","Plot","Landsat_EA_rbr_no_offset")


#RBR with offset
extract.data.rbr.with<-data.frame(coordinates(AllPlots),
                                AllPlots$Plot,
                                extract(allrasters_rbr_with_offset_focal,AllPlots))
names(extract.data.rbr.with)<-c("x1","x2","x3","Plot","Landsat_EA_rbr_with_offset")


#RDNBR no offset
extract.data.rdnbr.no<-data.frame(coordinates(AllPlots),
                                  AllPlots$Plot,
                                  extract(allrasters_rdnbr_no_offset_focal,AllPlots))
names(extract.data.rdnbr.no)<-c("x1","x2","x3","Plot","Landsat_EA_rdnbr_no_offset")


#RDNBR with offset
extract.data.rdnbr.with<-data.frame(coordinates(AllPlots),
                                  AllPlots$Plot,
                                  extract(allrasters_rdnbr_with_offset_focal,AllPlots))
names(extract.data.rdnbr.with)<-c("x1","x2","x3","Plot","Landsat_EA_rdnbr_with_offset")


### combine all extract output and write to CSV ###
all.Landsat.EA.extract<-dplyr::bind_cols(extract.data.dnbr.no, extract.data.dnbr.with, extract.data.rbr.no, extract.data.rbr.with, extract.data.rdnbr.no, extract.data.rdnbr.with)
fix(all.Sentinel.IA.extract)
write.csv(all.Landsat.EA.extract, file="Landsat_eA_AllPlots_Attr.csv")
#writes to current directory





######################### This code is for Landsat IA#####################

###SET WORKING DIRECTORY###

setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Landsat"

getwd()
library("raster")
library("rgdal")
library("gsubfn")
#library("stringr")

##renamed 33SP files to SP33

### Load mask shapefiles ###
BLUE_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BLUE_mask.shp")
BONI_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/BONI_mask.shp")
DIEN_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/DIEN_mask.shp")
HIGH_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HIGH_mask.shp")
HOND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/HOND_mask.shp")
SP33_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/SP33_mask.shp")
TIND_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/TIND_mask.shp")
REDO_mask<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/masks/REDO_ALL_mask.shp")

###LOAD RASTERS###
rastlist <- list.files(path=path, pattern='tif$', full.names=TRUE)

#load all raster files in folder using lapply
allrasters <- lapply(rastlist, raster)


###MASKING###

allrast.BLUE_m<-lapply(allrasters[grep("BLUE",allrasters)],mask, BLUE_mask, updatevalue=NA, updateNA=FALSE)
allrast.BONI_m<-lapply(allrasters[grep("BONI",allrasters)],mask, BONI_mask, updatevalue=NA, updateNA=FALSE)
allrast.DIEN_m<-lapply(allrasters[grep("DIEN",allrasters)],mask, DIEN_mask, updatevalue=NA, updateNA=FALSE)
allrast.HIGH_m<-lapply(allrasters[grep("HIGH",allrasters)],mask, HIGH_mask, updatevalue=NA, updateNA=FALSE)
allrast.HOND_m<-lapply(allrasters[grep("HOND",allrasters)],mask, HOND_mask, updatevalue=NA, updateNA=FALSE)
allrast.SP33_m<-lapply(allrasters[grep("SP33",allrasters)],mask, SP33_mask, updatevalue=NA, updateNA=FALSE)
allrast.TIND_m<-lapply(allrasters[grep("TIND",allrasters)],mask, TIND_mask, updatevalue=NA, updateNA=FALSE)
allrast.REDO_m<-lapply(allrasters[grep("REDO",allrasters)],mask, REDO_mask, updatevalue=NA, updateNA=FALSE)



###Write Masked Raster###
#Need to create 'masked' folder in Windows Explorer first
lapply(allrast.BLUE_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.BONI_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.DIEN_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HIGH_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.HOND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.REDO_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.SP33_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))
lapply(allrast.TIND_m, function (x) writeRaster(x, filename=paste0(path,"/masked/",names(x),"_m"), format="GTiff"))


###Replace non-masked rasters with masked rasters before Mosaic###
#MOVE FILES BY HAND, REMOVE REDOONDO TO REPLACE WITH INTERIOR/WESTERN7 MASKED FILES


###MOSAIC###

#create list by searching pattern for each index type, and mosaic everything in that list
#Note that allrast.*FIRE*_m lists are in the Global Environment, however now I want to mosaic by *INdex type*
#also note that I have moved the masked rasters by hand to the main directory to replace those of the original extent
#so, at this point, allrasters still points to raster list without the masked rasters
#Need to first recreate a list with the newly masked/moved files

#LOAD RASTERS#
rastlist_m <- list.files(path=path, pattern='tif$', full.names=TRUE)
#load all raster files in folder using lapply
allrasters_m <- lapply(rastlist_m, raster)

#mosaicking rasters
allrasters_dnbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_dnbr_no_offset",allrasters_m)])
allrasters_dnbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_dnbr_with_offset",allrasters_m)])
allrasters_rbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_rbr_no_offset",allrasters_m)])
allrasters_rbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_rbr_with_offset",allrasters_m)])
allrasters_rdnbr_no_offset_mosaic<-do.call(merge,allrasters_m[grep("_rdnbr_no_offset",allrasters_m)])
allrasters_rdnbr_with_offset_mosaic<-do.call(merge,allrasters_m[grep("_rdnbr_with_offset",allrasters_m)])

#write these rasters 
writeRaster(allrasters_dnbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_dnbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_dnbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_dnbr_with_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rbr_with_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rdnbr_no_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rdnbr_no_offset_mosaic"), format="GTiff")
writeRaster(allrasters_rdnbr_with_offset_mosaic, filename=paste0(path,"/mosaicked/","allrasters_rdnbr_with_offset_mosaic"), format="GTiff")


###FOCAL STATS###

#apply a kernel to weight neighboring cells according to weights in kernel
#kernel below was revised 2/5 to downweight center pixel more so that previous version
#and assign center pixel the new value, matrix fills by column as default
circlefilter=matrix(c(0.025,0.146,0.025,0.146,0.32,0.146,0.025,0.146,0.025),nc=3, nr=3)
allrasters_dnbr_no_offset_focal<-focal(allrasters_dnbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_dnbr_with_offset_focal<-focal(allrasters_dnbr_with_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rbr_no_offset_focal<-focal(allrasters_rbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rbr_with_offset_focal<-focal(allrasters_rbr_with_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rdnbr_no_offset_focal<-focal(allrasters_rdnbr_no_offset_mosaic,circlefilter,na.rm=TRUE)
allrasters_rdnbr_with_offset_focal<-focal(allrasters_rdnbr_with_offset_mosaic,circlefilter,na.rm=TRUE)

#write raster
#change type for each type
#create 'focal' folder each time
type='SW_RAVG_IA_Landsat'
writeRaster(allrasters_dnbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_dnbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_dnbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_dnbr_with_offset_focal"), format="GTiff")
writeRaster(allrasters_rbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_rbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_rbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_rbr_with_offset_focal"), format="GTiff")
writeRaster(allrasters_rdnbr_no_offset_focal, filename=paste0(path,"/focal/",type,"_rdnbr_no_offset_focal"), format="GTiff")
writeRaster(allrasters_rdnbr_with_offset_focal, filename=paste0(path,"/focal/",type,"_rdnbr_with_offset_focal"), format="GTiff")


########extract raster data at plot locations###########

### Extract data from each mosaic for plot locations ###
#with LANDSAT data which is mosaicked, import the shapefile with all plots

AllPlots<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/AllPlots.shp")

#DNBR no offset
extract.data.dnbr.no<-data.frame(coordinates(AllPlots),
                                 AllPlots$Plot,
                                 extract(allrasters_dnbr_no_offset_focal,AllPlots))
names(extract.data.dnbr.no)<-c("x1","x2","x3","Plot","Landsat_IA_dnbr_no_offset")


#DNBR with offset
extract.data.dnbr.with<-data.frame(coordinates(AllPlots),
                                   AllPlots$Plot,
                                   extract(allrasters_dnbr_with_offset_focal,AllPlots))
names(extract.data.dnbr.with)<-c("x1","x2","x3","Plot","Landsat_IA_dnbr_with_offset")


#RBR no offset
extract.data.rbr.no<-data.frame(coordinates(AllPlots),
                                AllPlots$Plot,
                                extract(allrasters_rbr_no_offset_focal,AllPlots))
names(extract.data.rbr.no)<-c("x1","x2","x3","Plot","Landsat_IA_rbr_no_offset")


#RBR with offset
extract.data.rbr.with<-data.frame(coordinates(AllPlots),
                                  AllPlots$Plot,
                                  extract(allrasters_rbr_with_offset_focal,AllPlots))
names(extract.data.rbr.with)<-c("x1","x2","x3","Plot","Landsat_IA_rbr_with_offset")


#RDNBR no offset
extract.data.rdnbr.no<-data.frame(coordinates(AllPlots),
                                  AllPlots$Plot,
                                  extract(allrasters_rdnbr_no_offset_focal,AllPlots))
names(extract.data.rdnbr.no)<-c("x1","x2","x3","Plot","Landsat_IA_rdnbr_no_offset")


#RDNBR with offset
extract.data.rdnbr.with<-data.frame(coordinates(AllPlots),
                                    AllPlots$Plot,
                                    extract(allrasters_rdnbr_with_offset_focal,AllPlots))
names(extract.data.rdnbr.with)<-c("x1","x2","x3","Plot","Landsat_IA_rdnbr_with_offset")


### combine all extract output and write to CSV ###
all.Landsat.IA.extract<-dplyr::bind_cols(extract.data.dnbr.no, extract.data.dnbr.with, extract.data.rbr.no, extract.data.rbr.with, extract.data.rdnbr.no, extract.data.rdnbr.with)
write.csv(all.Landsat.IA.extract, file="Landsat_IA_AllPlots_Attr.csv")
#writes to current directory


