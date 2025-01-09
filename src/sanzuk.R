install.packages("data.table")
install.packages("ineq")
install.packages("vegan")


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


files <- list.dirs("S:/shares/forse/2_tls/data/")
