library(data.table)

key.gontrode <- data.table(
  project = "2023-06-16_ForSe_Gontrode",
  logger = c(94253532, 94253533, 94253531, 94253557, 94253556, 94253555, 94253554, 94253553, 94253552, 94253551,
             94253530, 94253529, 94253528, 94253527, 94253526, 94253550, 94253549, 94253548, 94253547, 94253546),
  scanpos = c("ScanPos001", "ScanPos055", "ScanPos053", "ScanPos047", "ScanPos045", "ScanPos041", "ScanPos039", "ScanPos033", "ScanPos029", "ScanPos023",
              "ScanPos059", "ScanPos051", "ScanPos043", "ScanPos035", "ScanPos027", "ScanPos003", "ScanPos007", "ScanPos011", "ScanPos015", "ScanPos019"))


values = c(
  "BEHIT1P1", "BEHIT1P2", "BEHIT1P3", "BEHIT1P4", "BEHIT1P5",
  "BEHIT2P1", "BEHIT2P2", "BEHIT2P3", "BEHIT2P4", "BEHIT2P5",
  "BEHIT3P1", "BEHIT3P2", "BEHIT3P3", "BEHIT3P4", "BEHIT3P5",
  "BELOT1P1", "BELOT1P2", "BELOT1P3", "BELOT1P4", "BELOT1P5",
  "BELOT2P1", "BELOT2P2", "BELOT2P3", "BELOT2P4", "BELOT2P5",
  "BELOT3P1", "BELOT3P2", "BELOT3P3", "BELOT3P4", "BELOT3P5",
  "BEMET1P1", "BEMET1P2", "BEMET1P3", "BEMET1P4", "BEMET1P5",
  "BEMET2P1", "BEMET2P2", "BEMET2P3", "BEMET2P4", "BEMET2P5",
  "BEMET3P1", "BEMET3P2", "BEMET3P3", "BEMET3P4", "BEMET3P5",
  "CSLOT1P1", "CSLOT1P2", "CSLOT1P3", "CSLOT1P4", "CSLOT1P5",
  "CSLOT2P1", "CSLOT2P2", "CSLOT2P3", "CSLOT2P4", "CSLOT2P5",
  "CSLOT3P1", "CSLOT3P2", "CSLOT3P3", "CSLOT3P4", "CSLOT3P5",
  "GELOT1P1", "GELOT1P2", "GELOT1P3", "GELOT1P4", "GELOT1P5",
  "GELOT2P1", "GELOT2P2", "GELOT2P3", "GELOT2P4", "GELOT2P5",
  "GELOT3P1", "GELOT3P2", "GELOT3P3", "GELOT3P4", "GELOT3P5",
  "ITHIT1P1", "ITHIT1P2", "ITHIT1P3", "ITHIT1P4", "ITHIT1P5",
  "ITHIT2P1", "ITHIT2P2", "ITHIT2P3", "ITHIT2P4", "ITHIT2P5",
  "ITHIT3P1", "ITHIT3P2", "ITHIT3P3", "ITHIT3P4", "ITHIT3P5",
  "ITLOT1P1", "ITLOT1P2", "ITLOT1P3", "ITLOT1P4", "ITLOT1P5",
  "ITLOT2P1", "ITLOT2P2", "ITLOT2P3", "ITLOT2P4", "ITLOT2P5",
  "ITLOT3P1", "ITLOT3P2", "ITLOT3P3", "ITLOT3P4", "ITLOT3P5",
  "ITMET1P1", "ITMET1P2", "ITMET1P3", "ITMET1P4", "ITMET1P5",
  "ITMET2P1", "ITMET2P2", "ITMET2P3", "ITMET2P4", "ITMET2P5",
  "ITMET3P1", "ITMET3P2", "ITMET3P3", "ITMET3P4", "ITMET3P5",
  "NFLOT1P1", "NFLOT1P2", "NFLOT1P3", "NFLOT1P4", "NFLOT1P5",
  "NFLOT2P1", "NFLOT2P2", "NFLOT2P3", "NFLOT2P4", "NFLOT2P5",
  "NFLOT3P1", "NFLOT3P2", "NFLOT3P3", "NFLOT3P4", "NFLOT3P5",
  "NOHIT1P1", "NOHIT1P2", "NOHIT1P3", "NOHIT1P4", "NOHIT1P5",
  "NOHIT2P1", "NOHIT2P2", "NOHIT2P3", "NOHIT2P4", "NOHIT2P5",
  "NOHIT3P1", "NOHIT3P2", "NOHIT3P3", "NOHIT3P4", "NOHIT3P5",
  "NOLOT1P1", "NOLOT1P2", "NOLOT1P3", "NOLOT1P4", "NOLOT1P5",
  "NOLOT2P1", "NOLOT2P2", "NOLOT2P3", "NOLOT2P4", "NOLOT2P5",
  "NOLOT3P1", "NOLOT3P2", "NOLOT3P3", "NOLOT3P4", "NOLOT3P5",
  "NOMET1P1", "NOMET1P2", "NOMET1P3", "NOMET1P4", "NOMET1P5",
  "NOMET2P1", "NOMET2P2", "NOMET2P3", "NOMET2P4", "NOMET2P5",
  "NOMET3P1", "NOMET3P2", "NOMET3P3", "NOMET3P4", "NOMET3P5",
  "POLOT1P1", "POLOT1P2", "POLOT1P3", "POLOT1P4", "POLOT1P5",
  "POLOT2P1", "POLOT2P2", "POLOT2P3", "POLOT2P4", "POLOT2P5",
  "POLOT3P1", "POLOT3P2", "POLOT3P3", "POLOT3P4", "POLOT3P5",
  "SSLOT1P1", "SSLOT1P2", "SSLOT1P3", "SSLOT1P4", "SSLOT1P5",
  "SSLOT2P1", "SSLOT2P2", "SSLOT2P3", "SSLOT2P4", "SSLOT2P5",
  "SSLOT3P1", "SSLOT3P2", "SSLOT3P3", "SSLOT3P4", "SSLOT3P5",
  "SWLOT1P1", "SWLOT1P2", "SWLOT1P3", "SWLOT1P4", "SWLOT1P5",
  "SWLOT2P1", "SWLOT2P2", "SWLOT2P3", "SWLOT2P4", "SWLOT2P5",
  "SWLOT3P1", "SWLOT3P2", "SWLOT3P3", "SWLOT3P4", "SWLOT3P5")

# Function to replace values
convert_to_scanpos <- function(x) {
  # Extract numeric part after "P"
  num_part <- as.numeric(sub(".*P", "", x))
  
  # Replace based on conditions
  if (num_part < 10) {
    return(paste0("ScanPos00", num_part))
  } else if (num_part < 100) {
    return(paste0("ScanPos0", num_part))
  } else {
    return(paste0("ScanPos", num_part))
  }
}

# Apply function to each value
scanpos_values <- sapply(values, convert_to_scanpos)

# Apply function to each value
scanpos_values <- rep(c("ScanPos001", "ScanPos003", "ScanPos005", "ScanPos007", "ScanPos009"), 45)


# Create data.table
key.formicat <- data.table(project =sub("*P[0-9]+", "", values),  
                           scanpos = scanpos_values, 
                           logger = values)




library(stringr)

# Original vector of names
names <- c(
  "vpf.2020-08-15.001.ScanPos007.zen35",
  "vpf.2020-08-15.001.ScanPos001.zen35",
  "vpf.2020-08-15.001.ScanPos009.zen35",
  "vpf.2020-08-15.001.ScanPos003.zen35",
  "vpf.2020-08-15.001.ScanPos011.zen35",
  "vpf.2020-08-15.001.ScanPos005.zen35",
  "vpf.2020-08-15.001.ScanPos015.zen35",
  "vpf.2020-08-15.001.ScanPos013.zen35",
  "vpf.2020-08-15.001.ScanPos023.zen35",
  "vpf.2020-08-15.001.ScanPos021.zen35",
  "vpf.2020-08-15.001.ScanPos019.zen35",
  "vpf.2020-08-15.001.ScanPos017.zen35",
  "vpf.2020-08-10.001.ScanPos009.zen35",
  "vpf.2020-08-10.001.ScanPos001.zen35",
  "vpf.2020-08-10.001.ScanPos011.zen35",
  "vpf.2020-08-10.001.ScanPos003.zen35",
  "vpf.2020-08-10.001.ScanPos007.zen35",
  "vpf.2020-08-10.001.ScanPos005.zen35",
  "vpf.2020-08-10.001.ScanPos019.zen35",
  "vpf.2020-08-10.001.ScanPos017.zen35",
  "vpf.2020-08-10.001.ScanPos021.zen35",
  "vpf.2020-08-10.001.ScanPos023.zen35",
  "vpf.2020-08-10.001.ScanPos015.zen35",
  "vpf.2020-08-10.001.ScanPos013.zen35",
  "vpf.2021-08-08.001.ScanPos025.zen35",
  "vpf.2021-08-08.001.ScanPos001.zen35",
  "vpf.2021-08-08.001.ScanPos023.zen35",
  "vpf.2021-08-08.001.ScanPos005.zen35",
  "vpf.2021-08-08.001.ScanPos021.zen35",
  "vpf.2021-08-08.001.ScanPos007.zen35",
  "vpf.2021-08-08.001.ScanPos019.zen35",
  "vpf.2021-08-08.001.ScanPos009.zen35",
  "vpf.2021-08-08.001.ScanPos017.zen35",
  "vpf.2021-08-08.001.ScanPos011.zen35",
  "vpf.2021-08-08.001.ScanPos015.zen35",
  "vpf.2021-08-08.001.ScanPos013.zen35",
  "vpf.2020-09-08.001.ScanPos007.zen35",
  "vpf.2020-09-08.001.ScanPos005.zen35",
  "vpf.2020-09-08.001.ScanPos009.zen35",
  "vpf.2020-09-08.001.ScanPos003.zen35",
  "vpf.2020-09-08.001.ScanPos011.zen35",
  "vpf.2020-09-08.001.ScanPos001.zen35",
  "vpf.2020-09-08.001.ScanPos015.zen35",
  "vpf.2020-09-08.001.ScanPos013.zen35",
  "vpf.2020-09-08.001.ScanPos019.zen35",
  "vpf.2020-09-08.001.ScanPos017.zen35",
  "vpf.2020-09-08.001.ScanPos021.zen35",
  "vpf.2020-09-08.001.ScanPos023.zen35",
  "vpf.2020-08-22.001.ScanPos001.zen35",
  "vpf.2020-08-22.001.ScanPos003.zen35",
  "vpf.2020-08-22.001.ScanPos007.zen35",
  "vpf.2020-08-22.001.ScanPos005.zen35",
  "vpf.2020-08-22.001.ScanPos011.zen35",
  "vpf.2020-08-22.001.ScanPos009.zen35",
  "vpf.2020-08-22.001.ScanPos021.zen35",
  "vpf.2020-08-22.001.ScanPos023.zen35",
  "vpf.2020-08-22.001.ScanPos019.zen35",
  "vpf.2020-08-22.001.ScanPos017.zen35",
  "vpf.2020-08-22.001.ScanPos013.zen35",
  "vpf.2020-08-22.001.ScanPos015.zen35",
  "vpf.2021-07-07.001.ScanPos001.zen35",
  "vpf.2021-07-07.001.ScanPos011.zen35",
  "vpf.2021-07-07.001.ScanPos009.zen35",
  "vpf.2021-07-07.001.ScanPos003.zen35",
  "vpf.2021-07-07.001.ScanPos007.zen35",
  "vpf.2021-07-07.001.ScanPos005.zen35",
  "vpf.2021-07-07.001.ScanPos021.zen35",
  "vpf.2021-07-07.001.ScanPos013.zen35",
  "vpf.2021-07-07.001.ScanPos019.zen35",
  "vpf.2021-07-07.001.ScanPos017.zen35",
  "vpf.2021-07-07.001.ScanPos015.zen35"
)



logger <- c(
  "BRE_U1_COM", "BRE_U1_SIM", "BRE_U2_COM", "BRE_U2_SIM", "BRE_U3_COM", "BRE_U3_SIM",
  "BRE_U4_COM", "BRE_U4_SIM", "BRE_U5_COM", "BRE_U5_SIM", "BRE_U6_COM", "BRE_U6_SIM",
  "BRU_U1_COM", "BRU_U1_SIM", "BRU_U2_COM", "BRU_U2_SIM", "BRU_U3_COM", "BRU_U3_SIM",
  "BRU_U4_COM", "BRU_U4_SIM", "BRU_U5_COM", "BRU_U5_SIM", "BRU_U6_COM", "BRU_U6_SIM",
  "KAT_U1_COM", "KAT_U1_SIM", "KAT_U2_COM", "KAT_U2_SIM", "KAT_U3_COM", "KAT_U3_SIM",
  "KAT_U4_COM", "KAT_U4_SIM", "KAT_U5_COM", "KAT_U5_SIM", "KAT_U6_COM", "KAT_U6_SIM",
  "PAR_U1_COM", "PAR_U1_SIM", "PAR_U2_COM", "PAR_U2_SIM", "PAR_U3_COM", "PAR_U3_SIM",
  "PAR_U4_COM", "PAR_U4_SIM", "PAR_U5_COM", "PAR_U5_SIM", "PAR_U6_COM", "PAR_U6_SIM",
  "STO_U1_COM", "STO_U1_SIM", "STO_U2_COM", "STO_U2_SIM", "STO_U3_COM", "STO_U3_SIM",
  "STO_U4_COM", "STO_U4_SIM", "STO_U5_COM", "STO_U5_SIM", "STO_U6_COM", "STO_U6_SIM",
  "ZUR_U1_COM", "ZUR_U1_SIM", "ZUR_U2_COM", "ZUR_U2_SIM", "ZUR_U3_COM", "ZUR_U3_SIM",
  "ZUR_U4_SIM", "ZUR_U5_COM", "ZUR_U5_SIM", "ZUR_U6_COM", "ZUR_U6_SIM"
)
# Remove "vpf." at the beginning and ".zen35" at the end
cleaned_names <- sub("^vpf\\.", "", names)  # Remove "vpf."
cleaned_names <- sub("\\.zen35$", "", cleaned_names)  # Remove ".zen35"

# Print cleaned names
print(cleaned_names)




parts <- tstrsplit(cleaned_names, "(?=.ScanPos)", perl = TRUE)  # Split before "ScanPos"
key.formicac<-data.table(parts[[1]], parts[[3]], logger)
colnames(key.formicac)<-c("project", "scanpos", "logger")


#arville
names.arv <- list.files("Z:/shares/forse/2_tls/data/belgium/arville/arville/", pattern = "*.RiSCAN")
key.arville <- data.table(project = sub(".RiSCAN", "", names.arv), 
                          scanpos = "ScanPos001",
                          logger = paste0("Arville_", as.numeric(sapply(strsplit(sub(".RiSCAN", "", names.arv), "\\."), `[`, 2))))


#bosland
library(readxl)

names.bos <- as.data.table(read_excel("./data/tms_bos.xlsx"))
names.bos <- names.bos[,c("site", "sitecode", "manid", "Name_2")]
names.bos <-names.bos[,scanpos:=ifelse(as.numeric(Name_2)<100, 
                                       paste0("ScanPos0", Name_2), 
                                       paste0("ScanPos", Name_2))]

scan_positions_list <- list(
  "ScanPos049", "ScanPos053", "ScanPos057", "ScanPos061",
  "ScanPos093", "ScanPos097", "ScanPos101", "ScanPos105",
  "ScanPos137", "ScanPos141", "ScanPos145", "ScanPos149",
  "ScanPos181", "ScanPos185", "ScanPos189", "ScanPos193"
)

names.bos <- names.bos[names.bos$scanpos %in% scan_positions_list]
names.bos <- names.bos[, site:=paste0(site,sitecode)]
names.bos <- names.bos[, c("site", "scanpos", "manid")]

#berchtesgaden

names.ber <- as.data.table(read_excel("./data/tms_ber.xlsx"))
names.ber <- na.omit(names.ber, "tm_id")
names.ber <- names.ber[, scanpos:=ifelse(as.numeric(ScanPos_vert)<100, 
                                         paste0("ScanPos0", ScanPos_vert), 
                                         paste0("ScanPos", ScanPos_vert))]
names.ber <- names.ber[, c("site", "scanpos", "tm_id")]
colnames(names.ber)<-c("site", "scanpos", "manid")
names.ber <- names.ber[, site:=sub("BGD", "BER", site)]

key.weave <- rbind(names.bos, names.ber)
colnames(key.weave) <- c("project", "scanpos", "logger")



dirs <- list.files(path = "Z:/shares/lidar_data_cavefornalab/ForSe/TLS/European_dataset/Cecilia Dahlsjo/TLS/", pattern = "\\.zip$", full.names = TRUE)
dirs <- dirs[-31]


alldirs<-lapply(dirs, function(dir) {
  contents <- unzip(dir, list=T)
})

alldirs<- rbindlist(alldirs)
alldirs<- alldirs[grepl("ScanPos_C_2m", Name)]
alldirs<- alldirs[grepl(".rxp", Name)]
alldirs<- alldirs[!grepl("mon.rxp", Name)]
alldirs<-alldirs[,c("project", "proj1", "scanpos", "file"):=tstrsplit(Name, "/")]
key.ash<-alldirs[,logger:=paste0(gsub("WW","ASH", project), "2")]
key.ash<-key.ash[,c("logger","project", "scanpos")]


##maeda

# Create the data table
key.maeda <- data.table(
  logger = c("20858566", "20858577", "20858580", "20858591",
             "94200114", "94212631", "94212633", "94212637"),
  project = c("EA32", "UA52", "EA11", "UA21", "AF11", "AF19", "AF4", "AF8"),
  scanpos = c("ScanPos001", "ScanPos001", "ScanPos001", "ScanPos001",
              "ScanPos001", "ScanPos001", "ScanPos001", "ScanPos001"))

#landshut
key.landshut <- data.table(
  logger = c("SITE9", "SITE11", "SITE12", "SITE13", "SITE16", "SITE17", "SITE18", "SITE23"),
  project = c("2023-08-01_9", "2023-08-01_11", "2023-08-01_12", "2023-08-01_13", 
              "2023-08-01_16", "2023-08-01_17", "2023-08-01_18", "2023-08-01_23"),
  scanpos = c("ScanPos001", "ScanPos001", "ScanPos001", "ScanPos001",
              "ScanPos001", "ScanPos001", "ScanPos001", "ScanPos001"))
  
#morpho
  key.morpho <-data.table(
    logger=c("FR_FS_morfoHET30-C",
             "FR_FS_morfoHET30-F1",
             "FR_FS_morfoHET30-F2",
             "FR_FS_morfoHET30-F3",
             "FR_FS_morfoHET30-F4"),
    project=c(rep("morpho",5)),
    scanpos = c("ScanPos0017", "ScanPos006", "ScanPos042", "ScanPos029", "ScanPos015"))
  
  
##
key <- rbind(key.arville, key.formicac, key.formicat, key.gontrode, key.weave, key.maeda, key.morpho, key.ash, key.landshut)
