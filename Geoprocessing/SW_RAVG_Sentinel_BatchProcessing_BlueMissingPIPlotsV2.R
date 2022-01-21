############Extra code to attribute just the Missing Blue PI plots 8/5/2020



###########Sentinel EA

setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal"
library("raster")
library("rgdal")
library("gsubfn")
library("sf")

#load focal stats rasters

dnbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_dnbr_no_offset_focal.tif")
dnbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_dnbr_with_offset_focal.tif")
rbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_rbr_no_offset_focal.tif")
rbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_rbr_with_offset_focal.tif")
rdnbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_rdnbr_no_offset_focal.tif")
rdnbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_EA_Sentinel2_20m/focal/SW_RAVG_EA_Sentinel_BLUE_rdnbr_with_offset_focal.tif")


### Extract data from each mosaic for plot locations ###

#read in just missing BLUE PI plots
BLUE<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/Plots/BLUE.shp")

#DNBR no offset
extract.data.dnbr.no<-data.frame(coordinates(BLUE),
                                 BLUE$Plot,
                                 extract(dnbr_no_offset,BLUE))
names(extract.data.dnbr.no)<-c("x1","x2","x3","Plot","Sentinel_EA_dnbr_no_offset")


#DNBR with offset
extract.data.dnbr.with<-data.frame(coordinates(BLUE),
                                   BLUE$Plot,
                                   extract(dnbr_with_offset,BLUE))
names(extract.data.dnbr.with)<-c("x1","x2","x3","Plot","Sentinel_EA_dnbr_with_offset")


#RBR no offset
extract.data.rbr.no<-data.frame(coordinates(BLUE),
                                BLUE$Plot,
                                extract(rbr_no_offset,BLUE))
names(extract.data.rbr.no)<-c("x1","x2","x3","Plot","Sentinel_EA_rbr_no_offset")


#RBR with offset
extract.data.rbr.with<-data.frame(coordinates(BLUE),
                                  BLUE$Plot,
                                  extract(rbr_with_offset,BLUE))
names(extract.data.rbr.with)<-c("x1","x2","x3","Plot","Sentinel_EA_rbr_with_offset")


#RDNBR no offset
extract.data.rdnbr.no<-data.frame(coordinates(BLUE),
                                  BLUE$Plot,
                                  extract(rdnbr_no_offset,BLUE))
names(extract.data.rdnbr.no)<-c("x1","x2","x3","Plot","Sentinel_EA_rdnbr_no_offset")


#RDNBR with offset
extract.data.rdnbr.with<-data.frame(coordinates(BLUE),
                                    BLUE$Plot,
                                    extract(rdnbr_with_offset,BLUE))
names(extract.data.rdnbr.with)<-c("x1","x2","x3","Plot","Sentinel_EA_rdnbr_with_offset")


### combine all extract output and write to CSV ###
all.Sentinel.EA.extract<-dplyr::bind_cols(extract.data.dnbr.no, extract.data.dnbr.with, extract.data.rbr.no, extract.data.rbr.with, extract.data.rdnbr.no, extract.data.rdnbr.with)
write.csv(all.Sentinel.EA.extract, file="Sentinel_EA_BLUEV2_Attr.csv")
#writes to current directory




###########Sentinel IA

setwd("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R")
path<-"T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal"
library("raster")
library("rgdal")
library("gsubfn")
library("sf")

#load focal stats rasters

dnbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_dnbr_no_offset_focal.tif")
dnbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_dnbr_with_offset_focal.tif")
rbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_rbr_no_offset_focal.tif")
rbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_rbr_with_offset_focal.tif")
rdnbr_no_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_rdnbr_no_offset_focal.tif")
rdnbr_with_offset<-raster("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/SW_RAVG_IA_Sentinel2_20m/focal/SW_RAVG_IA_Sentinel_BLUE_rdnbr_with_offset_focal.tif")


### Extract data from each mosaic for plot locations ###

#read in just missing BLUE PI plots
BLUE<-readOGR("T:/FS/BusOps/EnterpriseProgram/Project/R03/RegionalProjects/RAVG/Data/AReiner/Imagery_R/Plots/Blue.shp")

#DNBR no offset
extract.data.dnbr.no<-data.frame(coordinates(BLUE),
                                 BLUE$Plot,
                                 extract(dnbr_no_offset,BLUE))
names(extract.data.dnbr.no)<-c("x1","x2","x3","Plot","Sentinel_IA_dnbr_no_offset")


#DNBR with offset
extract.data.dnbr.with<-data.frame(coordinates(BLUE),
                                   BLUE$Plot,
                                   extract(dnbr_with_offset,BLUE))
names(extract.data.dnbr.with)<-c("x1","x2","x3","Plot","Sentinel_IA_dnbr_with_offset")


#RBR no offset
extract.data.rbr.no<-data.frame(coordinates(BLUE),
                                BLUE$Plot,
                                extract(rbr_no_offset,BLUE))
names(extract.data.rbr.no)<-c("x1","x2","x3","Plot","Sentinel_IA_rbr_no_offset")



#RBR with offset
extract.data.rbr.with<-data.frame(coordinates(BLUE),
                                  BLUE$Plot,
                                  extract(rbr_with_offset,BLUE))
names(extract.data.rbr.with)<-c("x1","x2","x3","Plot","Sentinel_IA_rbr_with_offset")


#RDNBR no offset
extract.data.rdnbr.no<-data.frame(coordinates(BLUE),
                                  BLUE$Plot,
                                  extract(rdnbr_no_offset,BLUE))
names(extract.data.rdnbr.no)<-c("x1","x2","x3","Plot","Sentinel_IA_rdnbr_no_offset")


#RDNBR with offset
extract.data.rdnbr.with<-data.frame(coordinates(BLUE),
                                    BLUE$Plot,
                                    extract(rdnbr_with_offset,BLUE))
names(extract.data.rdnbr.with)<-c("x1","x2","x3","Plot","Sentinel_IA_rdnbr_with_offset")


### combine all extract output and write to CSV ###
all.Sentinel.IA.extract<-dplyr::bind_cols(extract.data.dnbr.no, extract.data.dnbr.with, extract.data.rbr.no, extract.data.rbr.with, extract.data.rdnbr.no, extract.data.rdnbr.with)
write.csv(all.Sentinel.IA.extract, file="Sentinel_IA_BLUEV2_Attr.csv")
#writes to current directory




