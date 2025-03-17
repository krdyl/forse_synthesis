
#all processed files
files.processed <- list.files("Z:/shares/forse/2_tls/metrics/all/new/", full.names = T)
scans.processed <- tools::file_path_sans_ext(basename(files.processed))
scans.processed <- gsub("all_allrxps_", "", files.processed)



files.folders <- list.dirs("Z:/shares/forse/2_tls/data", recursive = T)
files.folders<-unlist(files.folders)
files.folders <- files.folders[grepl(".RiSCAN/SCANS/", files.folders)]
files.folders <- files.folders[grepl("SINGLESCANS", files.folders)]

#list all rxp files
scans.files <- unlist(list.files(files.folders, ".rxp", full.names = T))
#drop residual files if any
scans.files <- scans.files[!grepl("residual|@", scans.files)]
#split the file paths into individual elements to create a data table
scans.files <- (strsplit(scans.files,"/"))


#create the data table from the scan files
dt.scans <- rbindlist(lapply(scans.files, function(x) as.list(x)), idcol = "row_id", use.names = F, fill = T)
#retain relevant columns
dt.scans<- dt.scans[, 7:ncol(dt.scans)]
#name the columns
colnames(dt.scans) <- c("country", "region1", "region2", "project", "type1", "scanposition", "type2", "file")



#for bosland data  
collectfiles<-function(project){
  files <- list.dirs(project, recursive = F)
  files <- files[grepl("SCNPOS", files)]
  files<- unlist(lapply(files, function(file) unlist(list.files(paste0(file, "/scans/"), pattern = "*.rxp", full.names = T))))
  files <- files[!grepl("residual|@|mon", files)]
  files <- (strsplit(files,"/"))
  dt <- rbindlist(lapply(files, function(x) as.list(x)), idcol = "row_id", use.names = F, fill = T)
  dt <- dt[, unname(which((apply(dt, 2, function(x) any(grepl(".PROJ", x)))))): ncol(dt)]
  dt[, `:=`(V8=sub(".PROJ", "", V8),
            V9=sub(".SCNPOS", "", V9),
            V11=sub(".rxp", "", V11))]
}

scans.all.bos<-rbind(collectfiles("Z:/shares/bosland/BOS/001/TL/2023/2023-09-14_Bosland_plot1.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/002/TL/2023/2023-08-21_Bosland_plot2.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/003/TL/2023/2023-08-11_Bosland_plot3.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/004/TL/2023/2023-08-09_Bosland_plot4.PROJ/"),
                     collectfiles("Z:/shares/bosland/BER/001_Eisberg/TL/2023/2023-07-09_Eisberg.PROJ/"),
                     collectfiles("Z:/shares/bosland/BER/002_Eisgraben/TL/2023/2023-06-29_Bartholoma_eisgraben.PROJ/"),
                     collectfiles("Z:/shares/bosland/BER/003_Endstal/TL/2023/2023-06-26_Endstal.PROJ/"),
                     collectfiles("Z:/shares/bosland/BER/004_Ofental/TL/2023/2023-06-27_Ofental.PROJ/"))



scans.processed.bos <- scans.all.bos[unlist(lapply(scans.processed, function(scan){
  which(scans.all.bos$V11%in%scan)
}))]

colnames(scans.processed.bos) <- c("project", "scanposition", "type1", "file")

dt.scans <- rbind(dt.scans, scans.processed.bos, fill = T)


dirs <- list.files(path = "Z:/shares/lidar_data_cavefornalab/ForSe/TLS/European_dataset/Cecilia Dahlsjo/TLS/", pattern = "\\.zip$", full.names = TRUE)

#remove directory with the matrices
dirs <- dirs[-31]


alldirs<-lapply(dirs, function(dir) {
  contents <- unzip(dir, list=T)
})

alldirs<- rbindlist(alldirs)
alldirs<- alldirs[grepl("ScanPos_C_2m", Name)]
alldirs<- alldirs[grepl(".rxp", Name)]
alldirs<- alldirs[!grepl("mon.rxp", Name)]
alldirs<-alldirs[,c("region1", "project", "scanposition", "file"):=tstrsplit(Name, "/")]
dt.scans <- rbind(dt.scans, alldirs[, c("project", "scanposition", "file")], fill = T)




# #landshut
# 
# land.dirs <- list.files("Z:/shares/forse/2_tls/data/germany/landshut/landshut/", recursive = T, pattern = ".rxp")
# land.dirs <-land.dirs[grepl(".RiSCAN", land.dirs)]
# land.dirs <-land.dirs[!grepl("residual|@", land.dirs)]
# land.dirs <- as.data.table(land.dirs)
# land.dirs <- land.dirs[, c("project", "type1", "scanposition", "type2", "file"):=tstrsplit(land.dirs, "/")]

#morpho
morpho.dirs <- list.files("Z:/shares/forse/2_tls/data/france/morpho/rxp/", recursive = T, pattern = ".rxp")
morpho.dirs <- as.data.table(morpho.dirs)
morpho.dirs[, file := sapply(morpho.dirs, function(x) {
  parts <- unlist(strsplit(x, "_"))
  paste(parts[1:2], collapse = "_")
})]
morpho.dirs <- morpho.dirs[,"country":="france"]
morpho.dirs <- morpho.dirs[,"region1":="morpho"]
morpho.dirs <- morpho.dirs[,"region2":="morpho"]

dt.scans <- rbind(dt.scans, morpho.dirs[, c("country", "region1", "region2", "file")], fill = T)


#wytham
ww.dirs <- list.files("Z:/shares/forse/2_tls/data/uk/wytham/wytham/rxp/", recursive = T, pattern = ".rxp")
ww.dirs <- as.data.table(ww.dirs)
ww.dirs <- ww.dirs[,file:=basename(ww.dirs)]
ww.dirs <- ww.dirs[,"country":="uk"]
ww.dirs <- ww.dirs[,"region1":="wytham"]
ww.dirs <- ww.dirs[,"region2":="wytham"]

dt.scans <- rbind(dt.scans, ww.dirs[, c("country", "region1", "region2", "file")], fill = T)
dt.scans1 <- dt.scans
dt.scans <- dt.scans1



dt.scans <- dt.scans[, file:=tools::file_path_sans_ext(file)]



dt.scans <- dt.scans[file%in%scans.processed]
dt.scans <- dt.scans[,-c("Name", "Length", "Date", "type1", "type2")]
dt.scans <- dt.scans[,country:=ifelse(grepl("BER",project),"germany",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("BER",project),"berchtesgaden",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("BER",project),"berchtesgaden",region2)]

dt.scans <- dt.scans[,country:=ifelse(grepl("BOS",project),"belgium",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("BOS",project),"bosland",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("BOS",project),"bosland",region2)]

dt.scans <- dt.scans[,country:=ifelse(grepl("ScanPos_C_2m",scanposition),"uk",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("ScanPos_C_2m",scanposition),"ash",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("ScanPos_C_2m",scanposition),"ash",region2)]
dt.scans <- dt.scans[scanposition=="ScanPos_C_2m",project2:= tstrsplit(project, "\\.")[[2]]]


dt.scans <- dt.scans[region1=="arville", project2:=gsub(".RiSCAN", "",project)]
dt.scans <- dt.scans[region1=="formica_transects", project2:=region2]

yyy<-dt.scans

dt.scans <- dt.scans[region2%in%c("CST1", "CST2", "CST3"), project2:=gsub("CS", "CSLO",region2)]
dt.scans <- dt.scans[region2%in%c("SST1", "SST2", "SST3"), project2:=gsub("SS", "SSLO",region2)]



setkeyv(dt.scans, c("project2", "scanposition"))
setkeyv(key, c("project", "scanpos"))

new <- dt.scans[key]

new[logger=="FR_FS_morfoHET30-C", file:="180911_131440"]
new[logger=="FR_FS_morfoHET30-F1", file:="180911_111601"]
new[logger=="FR_FS_morfoHET30-F2", file:="180911_174101"]
new[logger=="FR_FS_morfoHET30-F3", file:="180911_162251"]
new[logger=="FR_FS_morfoHET30-F4", file:="180911_125253"]
new[project2=="MORPHO", country:="france"]
new[project2=="MORPHO", region2:="morpho"]

new[project2%in%c("BOS001", "BOS002", "BOS003", "BOS004", "BER001", "BER002","BER003", "BER004"), region2:=project]


write.csv(new, "Z:/shares/forse/2_tls/metrics/all/key.csv")




xx<-dt.scans[,project:=ifelse(grepl("Bosland_plot1",project),"BOS001", project)]
xx<-xx[,project:=ifelse(grepl("Bosland_plot2",project),"BOS002", project)]
xx<-xx[,project:=ifelse(grepl("Bosland_plot3",project),"BOS003", project)]
xx<-xx[,project:=ifelse(grepl("Bosland_plot4",project),"BOS004", project)]

xx<-xx[,project:=ifelse(grepl("Eisberg",project),"BER001", project)]
xx<-xx[,project:=ifelse(grepl("eisgraben",project),"BER002", project)]
xx<-xx[,project:=ifelse(grepl("Endstal",project),"BER003", project)]
xx<-xx[,project:=ifelse(grepl("Ofental",project),"BER004", project)]

xx[,project2:=gsub(".RiSCAN|.riproject", "" ,project)]
xx[,project2:=ifelse(region1=="formica_transects", region2, project2)]
xx[scanposition=="ScanPos_C_2m",project2:= tstrsplit(project, "\\.")[[2]]]
xx[,project2:=ifelse(grepl("BOS|BER", project), project, project2)]
xx[,project2:=gsub(".RiSCAN|.riproject", "" ,project)]



dt.scans$project2 <- toupper(dt.scans$project2)
key$project <- toupper(key$project)

setkeyv(key, c("project", "scanpos"))
setkeyv(dt.scans, c("project2", "scanposition"))

new<-dt.scans[key]

dt.scans1 <- dt.scans[region1=="formica_transects"]
key.formicac

setkeyv(key.formicat, c("project", "scanpos"))
setkeyv(dt.scans1, c("project2", "scanposition"))


dt.scans1[key.formicat]


dt.scans1$project2
key.formicat$project
