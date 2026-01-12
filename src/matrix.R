
library(data.table)
#this script is to link matrices to scan positions. 
#we are renaming the dat files with name of the scan, instead of ScanPos001.dat etc. 


old_metrics <- fread("Z:/shares/forse/2_tls/metrics/all/final_metrics/all_metrics.csv")



#formica transects
matrices <- as.data.table(list.files("Z:/shares/forse/2_tls/matrices/formica_transects/", recursive = T, pattern = "*.DAT", full.names = T))

# split path
parts <- tstrsplit(matrices$V1, "/", fixed = TRUE)

# convert to data.table
dt_parts <- as.data.table(parts)

# keep last 4 columns
matrices <- dt_parts[, (ncol(dt_parts)-2):ncol(dt_parts), with = FALSE]


colnames(matrices) <- c("region1", "region2", "matrix")

matrices[, scanposition:=tools::file_path_sans_ext(matrix)]

setkeyv(matrices, c("region1", "region2", "scanposition"))

old_metrics_formica_transects <- old_metrics[region1=="formica_transects", 
                                             c("region1", "region2", "scanposition", "file")]
setkeyv(old_metrics_formica_transects, c("region1", "region2", "scanposition")) 

combined_formica_transects <- old_metrics_formica_transects[matrices, nomatch = 0]

combined_formica_transects[, oldname:=file.path("Z:/shares/forse/2_tls/matrices", region1, region2, matrix)]
combined_formica_transects[, newname:=file.path("Z:/shares/forse/2_tls/matrices", region1, "_final_used", region2, paste0(file, ".DAT"))]

# Create dirs only for valid newname values
combined_formica_transects[
  !is.na(newname) & nzchar(newname),
  dir.create(dirname(newname), recursive = TRUE, showWarnings = FALSE),
  by = .I
]

combined_formica_transects[, ok := FALSE]

combined_formica_transects[
  file.exists(oldname),
  ok := file.copy(oldname, newname, overwrite = TRUE),
  by = .I
]


#berchtesgaden
matrices <- as.data.table(list.files("Z:/shares/forse/2_tls/matrices/berchtesgaden/", recursive = T, pattern = "*.DAT", full.names = T))

matrices <- matrices[!grepl("_final_used", V1)]

# split path
parts <- tstrsplit(matrices$V1, "/", fixed = TRUE)

# convert to data.table
dt_parts <- as.data.table(parts)

# keep last 4 columns
matrices.bgd <- dt_parts[, (ncol(dt_parts)-2):ncol(dt_parts), with = FALSE]


colnames(matrices.bgd) <- c("region1", "project2", "matrix")

matrices.bgd[, scanposition:=tools::file_path_sans_ext(matrix)]

setkeyv(matrices.bgd, c("region1", "project2", "scanposition"))

old_metrics_bgd <- old_metrics[region1=="berchtesgaden", 
                                             c("region1", "project2", "scanposition", "file")]
old_metrics_bgd[,project2:=sub("BER","BGD",project2)]
setkeyv(old_metrics_bgd, c("region1", "project2", "scanposition")) 


combined_bgd <- old_metrics_bgd[matrices.bgd, nomatch = 0]


combined_bgd[, oldname:=file.path("Z:/shares/forse/2_tls/matrices", region1, project2, matrix)]
combined_bgd[, newname:=file.path("Z:/shares/forse/2_tls/matrices", region1, "_final_used", project2, paste0(file, ".DAT"))]

# Create dirs only for valid newname values
combined_bgd[
  !is.na(newname) & nzchar(newname),
  dir.create(dirname(newname), recursive = TRUE, showWarnings = FALSE),
  by = .I
]

combined_bgd[, ok := FALSE]

combined_bgd[
  file.exists(oldname),
  ok := file.copy(oldname, newname, overwrite = TRUE),
  by = .I
]




#bosland
