library(data.table)
library(ineq)  # For Gini coefficient
library(vegan) # For Shannon-Wiener index

files <- list.files("./data/new/", full.names = T)

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

setkey(files, scan)
setkey(new, file)

files <- files[new]

profile_stats <- files[, .(pai = max(hinge_pai, na.rm = TRUE)), by = .(i.country, region2, scan, logger)]
write.csv(profile_stats, "Z:/shares/forse/2_tls/metrics/all/pais.csv")

