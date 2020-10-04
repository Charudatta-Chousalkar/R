#!/usr/bin/Rscript
# $Id: fan_master_file.r 139850 2020-09-15 14:29:22Z cchousal $
# Author: Charudatta (Sept2020)

#input files:
# 1. ow_meter_details (ow_meter)
# 2. ow_firmware_version (ow_fw)
# 3. lfr_7day (fan_story)
# 4. ow_comm_rates (ow_comm)
# 5. ow_cm_read_rates (ow_reads)
# 6. fnd_act (fnd_act)
# 7. fnd_bact (fnd_bact)
# 8. ow_cgr_details (ow_cgr)
# 9. fnd_cgr_details (fnd_cgr)

# Run as:
# Rscript fan_master_file_v2.r -a riva-vect-2020-09-12.raw.ow_meter_details.txt -b riva-vect-2020-09-12.raw.ow_firmware_version.txt -c riva-vect-2020-09-12.lfr_meter_detail_report_7_day.csv -d riva-vect-2020-09-12.raw.ow_comm_rates.txt -e riva-vect-2020-09-12.raw.ow_cm_read_rates.txt -f riva-vect-2020-09-12.raw.fnd_act.txt -g riva-vect-2020-09-12.raw.fnd_bact.txt -h riva-vect-2020-09-12.raw.ow_cgr_details.txt -i riva-vect-2020-09-12.raw.fnd_cgr_details.txt -z Master_File.csv

#start_time <- Sys.time()
library("optparse")
library("stringr")
library('dplyr')
#ibrary(data.table)

argv = commandArgs(trailingOnly=TRUE)
parser <- OptionParser(add_help_option = FALSE)

parser <- add_option(parser, c("-a", "--one"), action="store", dest="ow_meter",
                     default='a', help="path of ow_meter")
parser <- add_option(parser, c("-b", "--two"), action="store", dest="ow_fw",
                     default='b', help="path of ow_fw")
parser <- add_option(parser, c("-c", "--three"), action="store", dest="fan_story",
                     default='c', help="path of fan_story")
parser <- add_option(parser, c("-d", "--four"), action="store", dest="ow_comm",
                     default='d', help="path of ow_comm")
parser <- add_option(parser, c("-e", "--five"), action="store", dest="ow_reads",
                     default='e', help="path of ow_reads")
parser <- add_option(parser, c("-f", "--six"), action="store", dest="fnd_act",
                     default='f', help="path of fnd_act")
parser <- add_option(parser, c("-g", "--seven"), action="store", dest="fnd_bact",
                     default='g', help="path of fnd_bact")
parser <- add_option(parser, c("-h", "--eight"), action="store", dest="ow_cgr",
                     default='h', help="path of ow_cgr")
parser <- add_option(parser, c("-i", "--nine"), action="store", dest="fnd_cgr",
                     default='i', help="path of fnd_cgr")

parser <- add_option(parser, c("-z", "--output"), action="store", dest="Master_File",
                     default='i', help="path of output Master_File")

filenm <- parse_args(parser, args = argv)


###################################################################################### Merging ow_meter_details file

#ow_meter <- gzfile(filenm$ow_meter,'rt')
if( file.access(filenm$ow_meter) == -1) {
  stop(sprintf("Specified file ( %s ) does not exist", filenm$ow_meter))
} else {
  ow_meter <- read.csv(filenm$ow_meter,header=F, sep='')
}
colnames(ow_meter) <- c('ElectronicSerialNumber','Nodekey','RelayKey','DeviceClassKey','NA','PremiseKey','sectorKey','NA','NA','SectorName','Latitude','Longitude','StreetNumber','StreetAddress','City','StateProvince','PostalCode','NA','InstallDate','FirstRegistrationTime','IsRegistrationProcessComplete','IsRegistered','IsAuthenticated','LastReadTime','LastDeregistrationTime','RemovalDate','ServicePointID','PROVISIONEDTIME','REGISTEREDTIME','AUTHENTICATIONTIME','NativeAddress','Serialnumber','APTITLE')

Master_File <- ow_meter %>% select('ElectronicSerialNumber','Nodekey','ServicePointID','APTITLE','DeviceClassKey','SectorName','Latitude','Longitude','StreetNumber','StreetAddress','City','StateProvince','PostalCode','InstallDate','IsRegistered','IsAuthenticated','IsRegistrationProcessComplete','LastReadTime','LastDeregistrationTime','RemovalDate','RelayKey')

Master_File <- Master_File %>% filter(!Master_File$InstallDate %in% c('NULL','NA','') & Master_File$RemovalDate %in% c('NULL','NA',''))

###################################################################################### Merging ow_firmware_version file

#ow_fw <- gzfile(filenm$ow_fw,'rt')
if( file.access(filenm$ow_fw) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$ow_fw)
  ow_fw <- data.frame(matrix(NA, nrow=1, ncol=5))
} else {
  ow_fw <- read.csv(filenm$ow_fw,header=F, sep='')
}
colnames(ow_fw) <- c('ElectronicSerialNumber','Version','FirmwareType','Description','LastUpdated')

Reg_fw <- ow_fw %>% filter(ow_fw$FirmwareType == 'E')
Reg_fw <- Reg_fw %>% rename(Register_fw=Version, Reg_fw_Date=LastUpdated)
Reg_fw <- Reg_fw %>% select('ElectronicSerialNumber','Register_fw','Reg_fw_Date')
RF_fw <- ow_fw %>% filter(ow_fw$FirmwareType == 'C')
RF_fw <- RF_fw %>% rename(RF_fw=Version, RF_fw_Date=LastUpdated)
RF_fw <- RF_fw %>% select('ElectronicSerialNumber','RF_fw','RF_fw_Date')

Master_File <- merge(Master_File,Reg_fw, by.x = "ElectronicSerialNumber",
                     by.y = "ElectronicSerialNumber", all.x = TRUE, all.y = FALSE)

Master_File <- merge(Master_File,RF_fw, by.x = "ElectronicSerialNumber",
                     by.y = "ElectronicSerialNumber", all.x = TRUE, all.y = FALSE)

# ###################################################################################### Merging lfr_7D_fan_story file

#fan_story <- gzfile(filenm$fan_story,'rt')
if( file.access(filenm$fan_story) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$fan_story)
  fan_story <- data.frame(matrix(NA, nrow=1, ncol=33))
} else {
  fan_story <- read.csv(filenm$fan_story,header=F, sep=',')
}
colnames(fan_story) <- c('ESN','Serial_Number','RelayKey','Sector_name','install_date','ServicePoinID','IsInstalled_today','IsInstalled_-1D','IsInstalled_-2D','IsInstalled_-3D','IsInstalled_-4D','IsInstalled_-5D','IsInstalled_-6D','IsRegistered_today','IsRegistered_-1D','IsRegistered_-2D','IsRegistered_-3D','IsRegistered_-4D','IsRegistered_-5D','IsRegistered_-6D','IsAuthenticated_today','IsAuthenticated_-1D','IsAuthenticated_-2D','IsAuthenticated_-3D','IsAuthenticated_-4D','IsAuthenticated_-5D','IsAuthenticated_-6D','IsReading_today','IsReading_-1D','IsReading_-2D','IsReading_-3D','IsReading_-4D','IsReading_-5D','IsReading_-6D')

FAN_Read_1D <- (fan_story$'IsReading_today')
FAN_Reads_3D <- (fan_story$'IsReading_today'+fan_story$'IsReading_-1D'+fan_story$'IsReading_-2D')
FAN_Reads_5D <- (fan_story$'IsReading_today'+fan_story$'IsReading_-1D'+fan_story$'IsReading_-2D'+fan_story$'IsReading_-3D'+fan_story$'IsReading_-4D')
FAN_Reads_7D <- (fan_story$'IsReading_today'+fan_story$'IsReading_-1D'+fan_story$'IsReading_-2D'+fan_story$'IsReading_-3D'+fan_story$'IsReading_-4D'+fan_story$'IsReading_-5D'+fan_story$'IsReading_-6D')
fan_story <- cbind(fan_story,FAN_Read_1D,FAN_Reads_3D,FAN_Reads_5D,FAN_Reads_7D)
fan_story <- fan_story %>% select('ESN','FAN_Read_1D','FAN_Reads_3D','FAN_Reads_5D','FAN_Reads_7D')

Master_File <- merge(Master_File,fan_story, by.x = "ElectronicSerialNumber",
                     by.y = "ESN", all.x = TRUE, all.y = FALSE)

###################################################################################### Merging ow_comm_rates file

#ow_comm <- gzfile(filenm$ow_comm,'rt')
if( file.access(filenm$ow_comm) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$ow_comm)
  ow_comm <- data.frame(matrix(NA, nrow=1, ncol=9))
} else {
  ow_comm <- read.csv(filenm$ow_comm,header=F, sep='')
}
colnames(ow_comm) <- c('ELECTRONICSERIALNUMBER','AGGREGATEREADRATEKEY','REPORTINGDATE','SECTORKEY',
                       'ONEDAYCOMMRATE','THREEDAYCOMMRATE','SEVENDAYCOMMRATE','THIRTYDAYCOMMRATE','LASTREADTIME')
ow_comm <- ow_comm %>% select('ELECTRONICSERIALNUMBER','REPORTINGDATE',
                              'ONEDAYCOMMRATE','THREEDAYCOMMRATE','SEVENDAYCOMMRATE','THIRTYDAYCOMMRATE')

Master_File <- merge(Master_File,ow_comm, by.x = "ElectronicSerialNumber",
                     by.y = "ELECTRONICSERIALNUMBER", all.x = TRUE, all.y = FALSE)

# ###################################################################################### Merging ow_interval_register_rate file

#ow_reads <- gzfile(filenm$ow_reads,'rt')
if( file.access(filenm$ow_reads) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$ow_reads)
  ow_reads <- data.frame(matrix(NA, nrow=1, ncol=26))
} else {
  ow_reads <- read.csv(filenm$ow_reads,header=F, sep='')
}
colnames(ow_reads) <- c('Electronicserialnumber','Aggregatestoragereadratekey','Reportingdate','Sectorkey','Onedayintreadrate','Twodayintreadrate','Threedayintreadrate','Fourdayintreadrate','Fivedayintreadrate','Sixdayintreadrate','Sevendayintreadrate','Twentydayintreadrate','Thirtydayintreadrate','Fourtyfivedayintreadrate','Sixtydayintreadrate','Onedayregreadrate','Twodayregreadrate','Threedayregreadrate','Fourdayregreadrate','Fivedayregreadrate','Sixdayregreadrate','Sevendayregreadrate','Twentydayregreadrate','Thirtydayregreadrate','Fourtyfivedayregreadrate','Sixtydayregreadrate')
ow_reads <- ow_reads %>% select('Electronicserialnumber',
                                'Onedayintreadrate','Threedayintreadrate','Sevendayintreadrate','Thirtydayintreadrate',
                                'Onedayregreadrate','Threedayregreadrate','Sevendayregreadrate','Thirtydayregreadrate')

Master_File <- merge(Master_File,ow_reads, by.x = "ElectronicSerialNumber",
                     by.y = "Electronicserialnumber", all.x = TRUE, all.y = FALSE)

# ###################################################################################### Merging fnd act & bact files
# length of ESN varies as per endpoint_type
Master_File$SerialNumber <- as.numeric(lapply(strsplit(as.character(Master_File$ElectronicSerialNumber), split=".",fixed = TRUE), tail, n=1))

#fnd_act <- gzfile(filenm$fnd_act,'rt')
if( file.access(filenm$fnd_act) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$fnd_act)
  fnd_act <- data.frame(matrix(NA, nrow=1, nco=31))
} else {
  fnd_act <- read.csv(filenm$fnd_act,header=F, sep='')
}
colnames(fnd_act) <- c('SerialNumber','Last_IP_Address','Latitude_Net_elements','Longitude_Net_elements','Mesh_Hops','Mesh_Path_Cost','Mesh_Link_Cost','Mesh_RSSI','Mesh_Reverse_RSSI','Mesh_Rank','MESH_LOW_PAN_PHY_TX_SPEED','MESH_LOW_PAN_PHY_RX_SPEED','INTER_PAN_MIGRATION','MESH_QUEUE_JUMP','ACT_Metrics_Last','NEXTHOP_ADDRESS','NEXTHOP_ADDRESS_TYPE','DESTINATION_ADDRESS_','DESTINATION_ADDRESS_TYPE','MESH_CHILDREN','MESH_DECENDANTS','MESH_PARENTS','NET_ROUTES_Last_Update','NET_ELEMENT_STATUS_ID','Status','TXSPEED','RXSPEED','AVERAGECPUUSAGE','AVERAGEMEMUSAGE','MESHACTIVELINKTYPE','MESHMODULATIONTYPE')

fnd_act <- fnd_act %>% select('SerialNumber','Last_IP_Address','Mesh_Hops','Mesh_Path_Cost','Mesh_Link_Cost','Mesh_RSSI','Mesh_Rank',
                              'INTER_PAN_MIGRATION','NEXTHOP_ADDRESS','Status','MESHACTIVELINKTYPE','MESHMODULATIONTYPE')
fnd_act$SerialNumber <- as.numeric(fnd_act$SerialNumber)

#fnd_bact <- gzfile(filenm$fnd_bact,'rt')
if( file.access(filenm$fnd_bact) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$fnd_bact)
  fnd_bact <- data.frame(matrix(NA, nrow=1, ncol=29))
} else {
  fnd_bact <- read.csv(filenm$fnd_bact,header=F, sep='')
}
colnames(fnd_bact) <- c('SerialNumber','Last_IP_Address','Latitude_net_elements','Longetude_net_elements','Mesh_Hops','Mesh_Path_Cost','Mesh_Link_Cost','Mesh_RSSI','Mesh_Reverse_RSSI','Mesh_Rank','Mesh_Standard_Deviation_RSSI','Mesh_Active_Scans','Mesh_Light_Active_Scans','Mesh_Sync_Request','MESH_LOW_PAN_PHY_TX_SPEED','MESH_LOW_PAN_PHY_RX_SPEED','INTER_PAN_MIGRATION','MESH_QUEUE_JUMP_RATE','BACT_Metrics_Last_Update','NEXTHOP_ADDRESS','NEXTHOP_ADDRESS_TYPE','DESTINATION_ADDRESS','DESTINATION_ADDRESS_TYPE','MESH_CHILDREN','MESH_DECENDANTS','MESH_PARENTS','NET_ROUTES_Last_Update','NET_ELEMENT_STATUS_ID_code','Status')

# adding extra columns to match act bact col numbers
fnd_bact$MESHACTIVELINKTYPE <- 0
fnd_bact$MESHMODULATIONTYPE <- 0
fnd_bact <- fnd_bact %>% select('SerialNumber','Last_IP_Address','Mesh_Hops','Mesh_Path_Cost','Mesh_Link_Cost','Mesh_RSSI','Mesh_Rank',
                                'INTER_PAN_MIGRATION','NEXTHOP_ADDRESS','Status','MESHACTIVELINKTYPE','MESHMODULATIONTYPE')
fnd_bact$SerialNumber <- as.numeric(fnd_bact$SerialNumber)

fnd_meters <- rbind(fnd_act,fnd_bact)

Master_File <- merge(Master_File,fnd_meters, by.x = "SerialNumber",
                     by.y = "SerialNumber", all.x = TRUE, all.y = FALSE)

# ###################################################################################### Merging fnd-owoc cgr files

#ow_cgr <- gzfile(filenm$ow_cgr,'rt')
if( file.access(filenm$ow_cgr) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$ow_cgr)
  ow_cgr <- data.frame(matrix(NA, nrow=1, ncol=8))
} else {
  ow_cgr <- read.csv(filenm$ow_cgr,header=F, sep='')
}
colnames(ow_cgr) <- c('CGR_ESN','CGRRelaykey','RelayKey','Description','DeviceClassKey','Latitude','Longitude','SectorName')
ow_cgr <- ow_cgr %>% select('CGR_ESN','CGRRelaykey')


ow_cgr$CGR_ESN <- str_replace(ow_cgr$CGR_ESN, "(VR-)", "")
ow_cgr$CGR_ESN <- str_replace(ow_cgr$CGR_ESN, "\\[", "")
ow_cgr$CGR_ESN <- str_replace(ow_cgr$CGR_ESN, "\\]", "")

SUBNET <- data.frame(do.call("rbind", strsplit(as.character(ow_cgr$CGR_ESN), ":", fixed = TRUE)))
ow_cgr$SUBNET <- SUBNET %>% select(c(length(SUBNET)-1))
ow_cgr$SUBNET <- as.character(unlist(ow_cgr$SUBNET))

ow_cgr$CGR_ESN <- tolower(ow_cgr$CGR_ESN)
Master_File <- merge(Master_File,ow_cgr, by.x = "RelayKey",
                     by.y = "CGRRelaykey", all.x = TRUE, all.y = FALSE)

#fnd_cgr <- gzfile(filenm$fnd_cgr,'rt')
if( file.access(filenm$fnd_cgr) == -1) {
  sprintf("Specified file ( %s ) does not exist", filenm$fnd_cgr)
  fnd_cgr <- data.frame(matrix(NA, nrow=1, ncol=22))
} else {
  fnd_cgr <- read.csv(filenm$fnd_cgr,header=F, sep='')
}

colnames(fnd_cgr) <- c('PID', 'CGR_SerialNumber','mesh_addr_config','latitude','longitude','status_id','Status','LastHeard','last_ip','UpTime','PowerSource','carrier',' mesh_key_exp','mesh_key_refresh','eth_tx','eth_rx','cell_tx','cell_rx','cell_rssi','Bandwidth','MESHPANID','DOMAIN')
fnd_cgr <- fnd_cgr %>% select('CGR_SerialNumber','mesh_addr_config', 'latitude', 'longitude', 'Status', 'LastHeard','UpTime', 'PowerSource', 'carrier','MESHPANID','DOMAIN')
fnd_cgr_SN <- fnd_cgr %>% select('CGR_SerialNumber','mesh_addr_config','MESHPANID','DOMAIN')

fnd_cgr$mesh_addr_config <- tolower(fnd_cgr$mesh_addr_config)
fnd_cgr_SN$mesh_addr_config <- tolower(fnd_cgr$mesh_addr_config)

SUBNET <- data.frame(do.call("rbind", strsplit(as.character(fnd_cgr_SN$mesh_addr_config), ":", fixed = TRUE)))
fnd_cgr_SN$SUBNET <- SUBNET %>% select(c(length(SUBNET)-1))
fnd_cgr_SN$SUBNET <- as.character(unlist(fnd_cgr_SN$SUBNET))

Master_File <- merge(Master_File,fnd_cgr_SN, by.x = "SUBNET",
                     by.y = "SUBNET", all.x = TRUE, all.y = FALSE)

Master_File <- Master_File %>% select(ElectronicSerialNumber,SerialNumber,Nodekey,ServicePointID,APTITLE,DeviceClassKey,SectorName,CGR_SerialNumber,everything())

write.csv(Master_File, file = filenm$Master_File, row.names = FALSE)

# end_time <- Sys.time()
# end_time - start_time
