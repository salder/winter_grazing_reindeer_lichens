#data collection for vegetation maps
#
#vorläufige Flechtenkarte fuer Gällivare sameby

#R packete
library(googledrive)
library(raster)
library(dplyr)
library(rgdal)

rasterOptions(tmpdir="L:/DATA/temp_raster/")
rasterOptions(tmpdir="//YKSI/13_Geodata/RBP_vinterbete_lavmodel/raster_temp/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"



###############################################################################################


drop_cont<-function(form=form,data_use=dat_mod,fam="quasibinomial",method="REML")
{
  var_all<-as.character(attr(terms(form),"variables"))[-1]
  response<-var_all[1]
  predictors<-var_all[-1]
  form0<-as.formula(paste(response,1,sep="~"))
  fit0<-gam(form0,data=data_use,family=fam)#,method=method)
  fit_tot<-gam(form,data=data_use,family=fam)#,method=method)
  d0<-deviance(fit0)
  d_tot<-deviance(fit_tot)
  var_explain<-predictor_explain<-prop_var_explain<-NA
  for (i in 1:length(predictors))
  {
    predictor_sub<-paste(predictors[-i],collapse="+")
    form1<-as.formula(paste(response,predictor_sub,sep="~"))
    fit_red<-gam(form1,data=data_use,family=fam,sp=fit_tot$sp[-i])#,method=method)
    predictor_explain[i]<-predictors[i]
    var_explain[i]<-(deviance(fit_red)-d_tot)/d0


  }
  prop_var_explain<-round(var_explain/sum(var_explain),3)
  #prop_var_explain<-prop_var_explain[order(prop_var_explain[,3],decreasing = F),]
  res<-data.frame(predictor_explain,var_explain,prop_var_explain)
  #res_a<-res[order(res$prop_var_explain,decreasing=T),]
  res<-list(fit_tot,summary(fit_tot),res)
}


###########################################################################################################



#1. Satellit daten von drive runterladen und mergen


#satellit images
#just one time
 drive_find()                                  #choose "2"   Einloggen erforderlich ueber Browser
 setwd("M:/reindder_lichen_map/GEE")             #hier werden die Sat daten gespeichert

 t1<-drive_find()                              #finden aller files auf dem drive
 files<-grep("lav_vinterbete",t1$name,value=TRUE)   #nur die Gallivare files auswählen
 View(files)

 #files<-files[c(42:53)]
 for (i in 111:length(files))                    # runterladen der files auf M:/reindder_lichen_map/GEE
   drive_download(file=files[i],overwrite=TRUE)


 scene_list<-dir(path="M:/reindder_lichen_map/GEE",pattern=".tif",full.names = TRUE)



 parameter_list<-c(

    "b2_lav_vinterbete"
   ,"b3_lav_vinterbete"
   ,"b4_lav_vinterbete"
   ,"b5_lav_vinterbete"
   ,"b6_lav_vinterbete"
   ,"b7_lav_vinterbete"
   ,"b8_lav_vinterbete"
   ,"b9_lav_vinterbete"
   ,"b10_lav_vinterbete"
   ,"b11_lav_vinterbete"
   ,"b12_lav_vinterbete"
   ,"ndvi_lav_vinterbete"
   ,"ndwi_lav_vinterbete"
   ,"ndci_lav_vinterbete"
   ,"land_water_lav_vinterbete"
   ,"gndvi_lav_vinterbete"
   ,"sipi_lav_vinterbete"
   ,"soil_lav_vinterbete"
   ,"savi_lav_vinterbete"
   
   #,
 )

 for (k in 1:(length(parameter_list)))
 {

   parameter<-parameter_list[k]
   #parameter="ndwi_m7_8"
   scene_list_red<-grep(parameter,scene_list,value = TRUE)
   file.copy(from=scene_list_red, to="F:/Geo-Data/temp_calc/",overwrite=TRUE)            #schneller
   scene_list_red<-dir(path="F:/Geo-Data/temp_calc/",pattern=".tif",full.names = TRUE)

   k1<-raster(scene_list_red[1])
   k2<-raster(scene_list_red[2])

   test_m<-list(k1,k2)

   if (length(scene_list_red)>2)
   {
     for (i in (3):length(scene_list_red))
     {
       k3<-raster(scene_list_red[i])
       test_m<-append(test_m,k3)
     }
   }

   test_m$filename <-"F:/Geo-Data/try1.tif"
   test_m$overwrite <- TRUE
   mm <- do.call(merge, test_m)
   projection(mm)<-projSWEREF
   file_name<-paste("M:/reindder_lichen_map/raster_files_combined/",parameter,".tif", sep="")
   writeRaster(mm, filename=file_name, format="GTiff", overwrite=TRUE)
   file.remove(scene_list_red)
 }


#satellit image for the Gallivare SB
#just one time
 
 
 
scene_list<-dir(path="M:/reindder_lichen_map/raster_files_combined/",pattern=".tif",full.names = TRUE)

b10<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 





#tax data -> update?
tax.data<-read.csv("M:/Härjedalen/r_analysis/ud1846vegdata.csv",sep=";")
tax_lav.data<-tax.data%>%filter(Tackningsart_latin=="Cladina spp.")
tax_lav.sp<-SpatialPointsDataFrame(coords=tax_lav.data[,c("Ostkoordinat","Nordkoordinat")],data=tax_lav.data,proj4string=CRS(projSWEREF))
# proj4string(hygge_new)<-projSWEREF




tax_lav.data$b2<-extract(b2,tax_lav.sp)
tax_lav.data$b3<-extract(b3,tax_lav.sp)
tax_lav.data$b4<-extract(b4,tax_lav.sp)
tax_lav.data$b5<-extract(b5,tax_lav.sp)
tax_lav.data$b6<-extract(b6,tax_lav.sp)
tax_lav.data$b7<-extract(b7,tax_lav.sp)
tax_lav.data$b8<-extract(b8,tax_lav.sp)
tax_lav.data$b9<-extract(b9,tax_lav.sp)
tax_lav.data$b10<-extract(b10,tax_lav.sp)
tax_lav.data$b11<-extract(b11,tax_lav.sp)
tax_lav.data$b12<-extract(b12,tax_lav.sp)
tax_lav.data$gndvi<-extract(gndvi,tax_lav.sp)
tax_lav.data$ndci<-extract(ndci,tax_lav.sp)
tax_lav.data$ndvi<-extract(ndvi,tax_lav.sp)
tax_lav.data$ndwi<-extract(ndwi,tax_lav.sp)
tax_lav.data$savi<-extract(savi,tax_lav.sp)
tax_lav.data$sipi<-extract(sipi,tax_lav.sp)
tax_lav.data$soil<-extract(soil,tax_lav.sp)


#correlation??
library(usdm)
predictors<-tax_lav.data%>%select(b3
                                      ,b4
                                      ,b5
                                      ,b6
                                      ,b7
                                      ,b8
                                      ,b9
                                      ,b10
                                      ,b11
                                      ,b12
                                      ,ndvi   
                                      ,gndvi
                                      ,ndci
                                      ,ndwi
                                      ,savi
                                      ,sipi
                                      ,soil)
                                      
predictors<-na.omit(predictors)                                     
vif(predictors)
vifcor(predictors,th=0.8)
vifstep(predictors)

library(mgcv)

form<-as.formula(Tackningsarea/100~s(b2)
                 +s(b3)
                 +s(b4)
                 +s(b5)
                 +s(b6)
                 +s(b7)
                 +s(b8)
                 +s(b9)
                 +s(b10)
                 +s(b11)
                 +s(b12)
                 +s(gndvi)
                 +s(ndci)
                +s(ndwi)
                 +s(savi)
                 +s(sipi)
                 +s(soil)
                 +s(ndvi)
                 
                  )

fit.lav<-gam(form,data= tax_lav.data,"quasibinomial",method="GCV.Cp") 
summary(fit)
gam.check(fit)
plot(fit,pages=1,shade=T,scale=F)

dropvar_all<-drop_cont(form,tax_lav.data,method="GCV.Cp",fam="quasibinomial")
dropvar_all






##############################################################################################
#prediction





b10_r<-raster("M:/reindder_lichen_map/raster_files_combined/b10_lav_vinterbete.tif") 
b11_r<-raster("M:/reindder_lichen_map/raster_files_combined/b11_lav_vinterbete.tif")  
b12_r<-raster("M:/reindder_lichen_map/raster_files_combined/b12_lav_vinterbete.tif")  
b2_r<-raster("M:/reindder_lichen_map/raster_files_combined/b2_lav_vinterbete.tif")   
b3_r<-raster("M:/reindder_lichen_map/raster_files_combined/b3_lav_vinterbete.tif")   
b4_r<-raster("M:/reindder_lichen_map/raster_files_combined/b4_lav_vinterbete.tif")   
b5_r<-raster("M:/reindder_lichen_map/raster_files_combined/b5_lav_vinterbete.tif")   
b6_r<-raster("M:/reindder_lichen_map/raster_files_combined/b6_lav_vinterbete.tif")   
b7_r<-raster("M:/reindder_lichen_map/raster_files_combined/b7_lav_vinterbete.tif")   
b8_r<-raster("M:/reindder_lichen_map/raster_files_combined/b8_lav_vinterbete.tif")   
b9_r<-raster("M:/reindder_lichen_map/raster_files_combined/b9_lav_vinterbete.tif")   
gndvi_r<-raster("M:/reindder_lichen_map/raster_files_combined/gndvi_lav_vinterbete.tif")
ndci_r<-raster("M:/reindder_lichen_map/raster_files_combined/ndci_lav_vinterbete.tif") 
ndvi_r<-raster("M:/reindder_lichen_map/raster_files_combined/ndvi_lav_vinterbete.tif") 
ndwi_r<-raster("M:/reindder_lichen_map/raster_files_combined/ndwi_lav_vinterbete.tif") 
savi_r<-raster("M:/reindder_lichen_map/raster_files_combined/savi_lav_vinterbete.tif") 
sipi_r<-raster("M:/reindder_lichen_map/raster_files_combined/sipi_lav_vinterbete.tif") 
soil_r<-raster("M:/reindder_lichen_map/raster_files_combined/soil_lav_vinterbete.tif") 









x<-extent(b2_r)
dif<-(x[4]-x[3])/40

for (i in 28:40)
{   
e1<-extent(x[1],x[2],x[3]+(i-1)*dif,x[3]+i*dif)

b2<-crop(b2_r,e1)
b3<-crop(b3_r,e1)
b4<-crop(b4_r,e1)
b5<-crop(b5_r,e1)
b6<-crop(b6_r,e1)
b7<-crop(b7_r,e1)
b8<-crop(b8_r,e1)
b9<-crop(b9_r,e1)
b10<-crop(b10_r,e1)
b11<-crop(b11_r,e1)
b12<-crop(b12_r,e1)
gndvi<-crop(gndvi_r,e1)
ndci<-crop(ndci_r,e1)
ndvi<-crop(ndvi_r,e1)
ndwi<-crop(ndwi_r,e1)
savi<-crop(savi_r,e1)
sipi<-crop(sipi_r,e1)
soil<-crop(soil_r,e1)

pred.brick<-brick(b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,gndvi,ndci,ndvi,ndwi,savi,sipi,soil)
names(pred.brick)<-c("b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","gndvi","ndci","ndvi","ndwi","savi","sipi","soil")
f_name<-paste("L:/Geo-Data/Satellit_vinterlav/vinterbete_block_",i,".rds",sep="")
saveRDS(pred.brick,file=f_name)
}




file.list<-dir("L:/Geo-Data/Satellit_vinterlav")

for (i in 1:30)
{file.rds<-paste("L:/Geo-Data/Satellit_vinterlav/",file.list[i],sep="")
pred.brick<-readRDS(file.rds)
names(pred.brick)<-c("b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","gndvi","ndci","ndvi","ndwi","savi","sipi","soil")

beginCluster(n=6)
x1<-clusterR(pred.brick,predict,args=list(fit.lav,type="response"))
endCluster()
t1<-getValues(x1)
t1<-ifelse(t1>0.9,NA,t1)
t1<-ifelse(t1>0.7,0.7,t1)
t1<-trunc(t1*100,0)
x1<-setValues(x1,t1)
save.file<-paste("//YKSI/13_Geodata/RBP_vinterbete_lavmodel/raster_prediction/lav_vinterbete_del_",i,".tif")
writeRaster(x1, filename=save.file, format="GTiff", overwrite=TRUE,datatype="FLT4S") 
}
