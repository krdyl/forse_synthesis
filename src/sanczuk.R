library(data.table)
library(ineq)  # For Gini coefficient
library(vegan) # For Shannon-Wiener index

files <- list.files("./data/synthesis/", full.names = T)

# create a data.table out if the list of file names containing country, project, scan name
files <- rbindlist(lapply(files, function(file){
  data <- fread(file)
  name <- tools::file_path_sans_ext(basename(file))
  name <- sub("formica_cities","formicacities", name)
  name <- sub("formica_transects","formicatransects", name)
  country <- unlist(strsplit(name, "_"))[1]
  project <- unlist(strsplit(name, "_"))[2]
  scan <- paste0(unlist(strsplit(name, "_"))[3], "_", unlist(strsplit(name, "_"))[4])
  data <- data.table(country, project, scan, data)
}))

#rename the serial number column to height as it is basically the same
colnames(files)[4]<-"height"
#divide by two to get 0.5m increments
files <- files[,height:=height/2]

#function 
get_relative_height <- function(target, data) {
  # Find rows that bound the target proportion
  lower_row <- data[cprop <= target][.N] # Last row below target
  upper_row <- data[cprop >= target][1]  # First row above target
  
  # If exact match, return the height
  if (upper_row$cprop == target) {
    return(upper_row$ht)
  }
  
  # Linear interpolation between bounding rows
  h_low <- lower_row$height
  h_high <- upper_row$height
  p_low <- lower_row$cprop
  p_high <- upper_row$cprop
  
  return(h_low + (target - p_low) / (p_high - p_low) * (h_high - h_low))
}

files <- files[, cPAVD := cumsum(weighted_pavd), by = .(country, project, scan)]
files <- files[, cprop := cPAVD / sum(weighted_pavd), by = .(country, project, scan) ]

profile_stats <- files[, .(pai = max(weighted_pai, na.rm = TRUE),
                           maxpavd = max(weighted_pavd, na.rm = TRUE),
                           hmaxpavd = height[which.max(weighted_pavd)],
                           hmax = get_relative_height(0.99, .SD),
                           rh25 = get_relative_height(0.25, .SD),
                           rh50 = get_relative_height(0.50, .SD),
                           rh75 = get_relative_height(0.75, .SD),
                           gini_coeff = ineq(weighted_pavd, type = "Gini"),
                           coeff_var = sd(weighted_pavd) / mean(weighted_pavd),
                           shannon_index = diversity(weighted_pavd, index = "shannon")), by = .(country, project, scan)]


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
#create the data table
dt.scans <- rbindlist(lapply(scans.files, function(x) as.list(x)), idcol = "row_id", use.names = F, fill = T)
#retain relevant columns
dt.scans<- dt.scans[, 7:ncol(dt.scans)]
#name the columns
colnames(dt.scans) <- c("country", "region", "region", "project", "type1", "scanposition", "type2", "file")



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


files.processed <- list.files("Z:/shares/forse/2_tls/metrics/all/synthesis/", full.names = T)

scans.processed<- unlist(lapply(files.processed, function(file){
  name <- tools::file_path_sans_ext(basename(file))
  name <- sub("formica_cities","formicacities", name)
  name <- sub("formica_transects","formicatransects", name)
  day <- unlist(strsplit(tools::file_path_sans_ext(name), "_"))[3]
  time <- unlist(strsplit(tools::file_path_sans_ext(name), "_"))[4]
  gsub(" ","", paste(day,"_",time))
}))

x <- scans.processed[1]

scans.processed.bos <- scans.all.bos[unlist(lapply(scans.processed, function(scan){
  which(scans.all.bos$V11%in%scan)
}))]


which(scans.all.bos$V11%in%scans.processed[53])


scans.processed.bos <- scans.processed[unlist(lapply(scans.processed, function (x) unlist(which(grepl(x, scans.all.bos$V11)))))]

dt.scans.bos <- 


x<-scans.all.bos[V11%in%scans.processed.bos]

scans.all.bos$V11 %in% scans.processed.bos

dt2 <- dt[x]

which(grepl(scans.processed[509],dt$V13))
scans.pro<-paste0(strsplit(tools::file_path_sans_ext(files), "_")[3], "_", strsplit(tools::file_path_sans_ext(files), "_")[4])
