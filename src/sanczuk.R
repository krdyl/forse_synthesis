library(data.table)
library(ineq)  # For Gini coefficient
library(vegan) # For Shannon-Wiener index

files <- list.files("./data/synthesis/30092025/redone/new3/", full.names = T)



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

files[
  ,
  `:=`(
    # 1) relative height 0–1 (divide by max height in that scan)
    height_rel     = height / max(height),
    
    # 2) relative cumulative PAI 0–1 (divide by max cumulative PAI in that scan)
    hinge_pai_rel  = hinge_pai / max(hinge_pai)
  ),
  by = scan   # if scan IDs aren't unique across projects, use by = .(project, scan)
]

#function 
get_relative_height <- function(target, data) {
  # Optionally handle targets outside range
  if (target <= min(data$cprop)) return(data[which.min(cprop), height])
  if (target >= max(data$cprop)) return(data[which.max(cprop), height])
  
  lower_row <- data[cprop <= target][.N]
  upper_row <- data[cprop >= target][1]
  
  if (upper_row$cprop == target) {
    return(upper_row$height)  # <- fixed
  }
  
  h_low <- lower_row$height
  h_high <- upper_row$height
  p_low <- lower_row$cprop
  p_high <- upper_row$cprop
  
  h_low + (target - p_low) / (p_high - p_low) * (h_high - h_low)
}

#function 
get_relative_height_rel <- function(target, data) {
  # Find rows that bound the target proportion
  lower_row <- data[cprop <= target][.N] # Last row below target
  upper_row <- data[cprop >= target][1]  # First row above target
  
  # If exact match, return the height
  if (upper_row$cprop == target) {
    return(upper_row$ht)
  }
  
  # Linear interpolation between bounding rows
  h_low <- lower_row$height_rel
  h_high <- upper_row$height_rel
  p_low <- lower_row$cprop_rel
  p_high <- upper_row$cprop_rel
  
  return(h_low + (target - p_low) / (p_high - p_low) * (h_high - h_low))
}

files <- files[, cPAVD := cumsum(weighted_pavd), by = .(country, project, scan)]
files <- files[, cprop := cPAVD / sum(weighted_pavd), by = .(country, project, scan) ]

get_height_at_prop <- function(target, data, height_col = "height") {
  p <- data[["cprop"]]
  h <- data[[height_col]]
  
  # Drop NAs
  ok <- is.finite(p) & is.finite(h)
  p <- p[ok]
  h <- h[ok]
  
  # Need at least 2 points
  if (length(p) < 2L) return(NA_real_)
  
  # Ensure increasing order in p
  ord <- order(p)
  p <- p[ord]
  h <- h[ord]
  
  # Interpolate height at target proportion; rule = 2 clamps outside range
  approx(x = p, y = h, xout = target, rule = 2)$y
}

files[scan=="230616_090244"]

files1 <- files[hinge_pavd<=0, hinge_pavd:=0]

profile_stats <- files[, .(pai = max(weighted_pai, na.rm = TRUE),
                           maxpavd = max(hinge_pavd, na.rm = TRUE),
                           hmaxpavd = height[which.max(hinge_pavd)],
                           hmax = get_relative_height(0.99, .SD),
                           rh25 = get_relative_height(0.25, .SD),
                           rh50 = get_relative_height(0.50, .SD),
                           rh75 = get_relative_height(0.75, .SD),
                           gini_coeff = ineq(hinge_pavd, type = "Gini"),
                           coeff_var = sd(hinge_pavd) / mean(hinge_pavd),
                           shannon_index = diversity(hinge_pavd, index = "shannon")), by = .(country, project, scan)]

get_relative_height_rel  <- function(target, data) get_height_at_prop(target, data, "height_rel")


profile_stats_rel <- files[
  ,
  .(
    pai          = max(hinge_pai, na.rm = TRUE),
    maxpavd      = max(hinge_pavd, na.rm = TRUE),
    hmaxpavd     = height[which.max(hinge_pavd)],
    hmax         = get_relative_height_rel(0.99, .SD),  # returns height_rel
    rh25         = get_relative_height_rel(0.25, .SD),
    rh50         = get_relative_height_rel(0.50, .SD),
    rh75         = get_relative_height_rel(0.75, .SD),
    gini_coeff   = ineq(hinge_pavd, type = "Gini"),
    coeff_var    = sd(hinge_pavd) / mean(hinge_pavd),
    shannon_index = diversity(hinge_pavd, index = "shannon")
  ),
  by = .(country, project, scan)
]

#using first set of metrics as a key 
old_metrics <- fread("Z:/shares/forse/2_tls/metrics/all/final_metrics/all_metrics.csv")

old_metrics2<-old_metrics[,c(1:13)]

setkey(old_metrics2, "file")
setkey(profile_stats, "scan")

pai3 <- old_metrics2[profile_stats]
pai3.ss <- pai3[!is.na(pai3$logger)]
write.csv(pai3.ss, "pai3.csv")

setkey(files, scan)
setkey(new, file)
files1<- files1[new]
files1<- files1[,c(1:10,20,21)]

files1.plotting <- melt(files1, measure.vars = c(5:10))





files1<- files1[new]

profile_stats <- files[, .(pai = max(hinge_pai, na.rm = TRUE)), by = .(i.country, region2, scan, logger)]
write.csv(profile_stats, "Z:/shares/forse/2_tls/metrics/all/all_stats.csv")



files1.plotting1<-files1.plotting[files1.plotting$variable=="hinge_pavd",]

grouped <- split(files1.plotting1, files1.plotting1$logger)

# Generate plots
plots <- lapply(seq_along(grouped), function(i) {
  df <- as.data.table(grouped[[i]])
  name <- names(grouped)[i]  # e.g., "A.X"
  subset <- df[variable=="hinge_pavd"]$value
  idx = max(which(subset!=0))+5
  # print(df)
  


  ggplot(df[variable=="hinge_pavd"][0:idx], aes(y = value, x = height, colour=file)) +
    geom_line(color = "steelblue", size = 1) +
    coord_flip()
})

plotsss <- plots[[63]]

#save figures
for (i in seq_along(plots[[63]])) {
  plot = trimws(unique(grouped[[i]]$project2))
  logger =  trimws(names(grouped)[i])
  options(bitmapType = "cairo")
  ggsave(filename = paste0("./results/plot_", plot, "_", logger, ".png"),  # Name the file
    plot = plots[[i]],   # Extract the plot from the list
    width = 8, height = 6, dpi = 300  # High quality: 300 DPI
  )
}



