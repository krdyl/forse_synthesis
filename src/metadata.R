
#all processed files
files.processed <- list.files("Z:/shares/forse/2_tls/metrics/all/new/", full.names = F)
scans.processed <- tools::file_path_sans_ext(basename(files.processed))
scans.processed <- gsub("all_allrxps_", "", scans.processed)
scans.processed <- gsub("all_allrxps2_", "", scans.processed)



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



#for WEAVE data  
library(data.table)

collectfiles <- function(project) {
  pos_dirs <- list.dirs(project, recursive = FALSE, full.names = TRUE)
  pos_dirs <- pos_dirs[grepl("SCNPOS", pos_dirs)]
  if (!length(pos_dirs)) return(data.table())
  
  rxp <- unlist(lapply(pos_dirs, function(d) {
    scans_dir <- file.path(d, "scans")
    if (!dir.exists(scans_dir)) return(character(0))
    list.files(scans_dir, pattern = "\\.rxp$", full.names = TRUE)
  }), use.names = FALSE)
  
  rxp <- rxp[!grepl("residual|@|mon", rxp)]
  if (!length(rxp)) return(data.table())
  
  rxp <- gsub("\\\\", "/", rxp)
  
  data.table(
    proj   = sub("^.*/([^/]+)\\.PROJ/.*$", "\\1", rxp),
    scnpos = sub("^.*/([^/]+)\\.SCNPOS/.*$", "\\1", rxp),
    scan   = sub("^.*/([^/]+)\\.rxp$", "\\1", rxp)
  )
}

scans.all.weave<-rbind(collectfiles("Z:/shares/bosland/BOS/001/TL/2023/2023-09-14_Bosland_plot1.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/002/TL/2023/2023-08-21_Bosland_plot2.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/003/TL/2023/2023-08-11_Bosland_plot3.PROJ"),
                     collectfiles("Z:/shares/bosland/BOS/004/TL/2023/2023-08-09_Bosland_plot4.PROJ/"),
                     collectfiles("Z:/shares/bosland/BGD/001_Eisberg/TL/2023/2023-07-09_Eisberg.PROJ/"),
                     collectfiles("Z:/shares/bosland/BGD/002_Eisgraben/TL/2023/2023-06-29_Bartholoma_eisgraben.PROJ/"),
                     collectfiles("Z:/shares/bosland/BGD/003_Endstal/TL/2023/2023-06-26_Endstal.PROJ/"),
                     collectfiles("Z:/shares/bosland/BGD/004_Ofental/TL/2023/2023-06-27_Ofental.PROJ/"))



scans.processed.weave <- scans.all.weave[scan %in% scans.processed]

#replace project names
scans.processed.weave <- scans.processed.bos[,project2:=ifelse(grepl("Bosland_plot1",project),"BOS001", 
                                     ifelse(grepl("Bosland_plot2",project),"BOS002", 
                                            ifelse(grepl("Bosland_plot3",project),"BOS003",
                                                   ifelse(grepl("Bosland_plot4",project),"BOS004",
                                                          ifelse(grepl("Eisberg",project),"BER001",
                                                                 ifelse(grepl("eisgraben",project),"BER002",
                                                                        ifelse(grepl("Endstal",project),"BER003",
                                                                               ifelse(grepl("Ofental",project),"BER004", project))))))))]



colnames(scans.processed.bos) <- c("project", "scanposition", "file")

dt.scans <- rbind(dt.scans, scans.processed.bos, fill = T)


dirs <- list.files(path = "Z:/shares/lidar_data_cavefornalab/ForSe/TLS/European_dataset/Cecilia Dahlsjo/TLS/", pattern = "\\.zip$", full.names = TRUE)

## 1) Drop the “matrices” zip(s) by pattern instead of position
# Example patterns – adjust to whatever uniquely identifies them
dirs <- dirs[!grepl("matrix|matrices", dirs, ignore.case = TRUE)]

## 2) List contents of all zips, with an id column so you can trace provenance
alldirs <- rbindlist(lapply(dirs, function(z) {
  x <- unzip(z, list = TRUE)
  
  # Ensure data.table
  setDT(x)
  
  # Keep track of which zip it came from (useful for debugging)
  x[, zipfile := z]
  x
}), fill = TRUE)

## 3) Filter to the files you actually want
alldirs <- alldirs[
  grepl("ScanPos_C_2m", Name, fixed = TRUE) &      # exact substring match
    grepl("\\.rxp$", Name) &                         # ends with .rxp
    !grepl("mon\\.rxp$", Name)                       # excludes mon.rxp at end
]

## 4) Split the path robustly
# If you truly know it's exactly 4 segments, keep this:
# alldirs[, c("region1", "project", "scanposition", "file") := tstrsplit(Name, "/", fixed = TRUE)]
#
# Safer: split into all parts then take the LAST 4 segments
parts <- tstrsplit(alldirs$Name, "/", fixed = TRUE, fill = NA_character_)

# Convert to DT just for easy indexing
parts_dt <- as.data.table(parts)

# Take last 4 columns, regardless of overall path depth
last4 <- parts_dt[, (ncol(parts_dt)-3L):ncol(parts_dt)]

setnames(last4, c("region1", "project", "scanposition", "file"))
alldirs <- cbind(alldirs, last4)

## 5) Keep only what you need, append into dt.scans
to_add <- unique(alldirs[, .(project, scanposition, file)])  # unique optional

# Ensure dt.scans exists and has the right columns
if (!exists("dt.scans")) {
  dt.scans <- to_add
} else {
  dt.scans <- rbindlist(list(dt.scans, to_add), use.names = TRUE, fill = TRUE)
}



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
morpho.dirs[, scanpos := sapply(morpho.dirs, function(x) {
  parts <- unlist(strsplit(x, "_"))
  tools::file_path_sans_ext(parts[4])
})]
morpho.dirs <- morpho.dirs[,"country":="france"]
morpho.dirs <- morpho.dirs[,"region1":="morpho"]
morpho.dirs <- morpho.dirs[,"region2":="morpho"]
colnames(morpho.dirs)[3] <- "scanposition"


dt.scans <- rbind(dt.scans, morpho.dirs[, c("country", "region1", "region2", "scanposition", "file")], fill = T)



#wytham
ww.dirs <- list.dirs("Z:/shares/kdayal/1_data/1_TLS/5_forse/P6.RiSCAN/SCANS", recursive = T)

ww.dirs <- basename(ww.dirs[grepl("T", ww.dirs)])
ww.scans <- tools::file_path_sans_ext(list.files("Z:/shares/forse/2_tls/data/allrxps2/", recursive = F, pattern =".rxp"))
ww.scans <- ww.scans[grepl("1506", ww.scans)]
ww.scans <- sort(ww.scans)

ww.dirs[grepl("1506", ww.scans)]

ww.dirs[unlist(ww.dirs) %in% unlist(ww.scans)]


ww.dirs <- data.table("scanposition"=ww.dirs,
                      "file"=ww.scans)
ww.dirs <- ww.dirs[,"country":="uk"]
ww.dirs <- ww.dirs[,"region1":="wytham"]
ww.dirs <- ww.dirs[,"region2":="wytham"]

#stack
dt.scans <- rbind(dt.scans, ww.dirs[, c("country", "region1", "region2", "file", "scanposition")], fill = T)

#drop .rxp 
dt.scans <- dt.scans[, file:=tools::file_path_sans_ext(file)]

#drop scans not processed
dt.scans <- dt.scans[file%in%scans.processed]


#fill empty columns manually
dt.scans <- dt.scans[,country:=ifelse(grepl("BER",project2),"germany",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("BER",project2),"berchtesgaden",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("BER",project2),"berchtesgaden",region2)]

dt.scans <- dt.scans[,country:=ifelse(grepl("BOS",project2),"belgium",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("BOS",project2),"bosland",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("BOS",project2),"bosland",region2)]

dt.scans <- dt.scans[,country:=ifelse(grepl("ScanPos_C_2m",scanposition),"uk",country)]
dt.scans <- dt.scans[,region1:=ifelse(grepl("ScanPos_C_2m",scanposition),"ash",region1)]
dt.scans <- dt.scans[,region2:=ifelse(grepl("ScanPos_C_2m",scanposition),"ash",region2)]
dt.scans <- dt.scans[scanposition=="ScanPos_C_2m", project2:= tstrsplit(project, "\\.")[[2]]]

dt.scans <- dt.scans[region1=="formica_transects", project2:=region2]
dt.scans <- dt.scans[region2%in%c("CST1", "CST2", "CST3"), project2:=gsub("CS", "CSLO",region2)]
dt.scans <- dt.scans[region2%in%c("SST1", "SST2", "SST3"), project2:=gsub("SS", "SSLO",region2)]

#FR is same as NF (probably)
dt.scans <- dt.scans[grepl("FrLoT", region2), project2:=gsub("FR", "NF", project2)]

dt.scans<-dt.scans[, project2:=ifelse(region1=="formica_transects", region2, project2)]
dt.scans<-dt.scans[scanposition=="ScanPos_C_2m",project2:= tstrsplit(project, "\\.")[[2]]]


dt.scans <- dt.scans[region2=="morpho", project2:="MORPHO"]
dt.scans <-dt.scans[region2=="wytham", project2:="WYTHAM"]

#drop extension
dt.scans<-dt.scans[, project2:=gsub(".RiSCAN|.riproject", "" , project2)]



# 
# new[logger=="FR_FS_morfoHET30-C", file:="180911_131440"]
# new[logger=="FR_FS_morfoHET30-F1", file:="180911_111601"]
# new[logger=="FR_FS_morfoHET30-F2", file:="180911_174101"]
# new[logger=="FR_FS_morfoHET30-F3", file:="180911_162251"]
# new[logger=="FR_FS_morfoHET30-F4", file:="180911_125253"]
# new[project2=="MORPHO", country:="france"]
# new[project2=="MORPHO", region2:="morpho"]
dt.scans$project2 <- toupper(dt.scans$project2)
key$project <- toupper(key$project)

setkeyv(key, c("project", "scanpos"))
setkeyv(dt.scans, c("project2", "scanposition"))

new<-dt.scans[key]

new <- na.omit(new, cols=c("file"))
