
## Required Packages

packages <- c("knitr","rstudioapi","lubridate","purrr",
                  "tidyverse","openxlsx","shiny","plotly")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# FUNCTIONS for processing spu data

read_spu_file_metadata <- function(filepath,filename, info = "short") {
  # DESCRIPTION: Reads first 9 text lines in .spu files 
  # INPUT: .spu file -- collected using PPSystems UnispecDC
  #         info = "short", the default returns only the spu_filename, DateTime, FileNum, Integration time
  #                "long", returns all the info in the 9 lines of header in .spu files
  # OUTPUT: dataframe with 15 columns on instrument status, scan settings, max/min value
  
  # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
  text <- read_lines(filepath, n_max=9)
  
  # Line 1: Extract the file name in the spu file as a check. Some file names have spaces 
  spu_filename <- str_replace(text[1],".*[\\\\]([A-z0-9.]\\s*)","\\1") %>% # extract filename
    str_replace("\"","") # removes trailing quote at end of line 
  FileNum <- str_extract(spu_filename, "\\d{5}") %>% as.numeric() # from 5 digits in filename
  
  # Line 2: 
  Remarks <- str_split(text[2], pattern = " ")[[1]] # split by "space"
  Type <- str_split(Remarks[7], pattern = "=")[[1]][2] # extract relevant part
  ScanType <- ifelse(grepl("DARKscan",Type, fixed=T), "DARKscan", # format
                     ifelse(grepl("Datascan,DC",Type, fixed=T), "Throwawayscan", 
                            Type))
  DarkscanID <- str_extract(text[2], "Dark=.+.spu")
  Remarks <- text[2]
  
  # Line 3: 
  DateTime <-  lubridate::mdy_hms(text[3], tz="America/Anchorage")
  
  # Line 4: Limits -- range of spectra measured
  Limits <- str_extract(text[4], "\\d+.\\d.+\\d") 
  
  # Line 5: 
  Temperature <- as.numeric(strsplit(strsplit(text[5], split = " ")[[1]][4], split="=")[[1]][2])
  Battery <- str_extract(text[5], "BattV=\\d+.\\d+")
  Aux <- str_extract(text[5], "A\\d=.+\\d")
  
  # Line 6-9: 
  Minimum <- str_extract(text[6], "\\d+.+\\d") # Wavelength, ChB min
  Minimum_wavelength <- str_split(Minimum, boundary("word"))[[1]][1] # Wavelength
  Minimum_value <- str_split(Minimum, boundary("word"))[[1]][2] # ChB AD
  
  Maximum <- str_extract(text[7], "\\d+.+\\d") # Wavelength, ChB max
  Maximum_wavelength <- str_split(Maximum, boundary("word"))[[1]][1]
  Maximum_value <- str_split(Maximum, boundary("word"))[[1]][2]
  
  Integration <- as.numeric(strsplit(text[8], split = " ")[[1]][3])
  NumberScans <- str_extract(text[9], "\\d+")
  
  # Truncated Filename - use as SCANID to join to other dataframes
  spu_filename <- unlist(str_split(filename, pattern = "/")) %>% last()
  
  # Metadata 
  metadata <- tibble(spu_filename, DateTime, FileNum, ScanType, Integration, NumberScans,Minimum_wavelength,Minimum_value, Maximum_wavelength, Maximum_value, Limits, Temperature, Battery, Aux, DarkscanID, Remarks)
  
  if(info == "short") {
    metadata <- metadata %>% select(spu_filename, DateTime, FileNum, Integration)
  }
  
  # Print filenames while reading 
  #print(spu_filename) # use for error checking
  
  return(metadata)
}


read_spu_file_spectra <- function(filename) {
  # DESCRIPTION: For a generic .spu file regardless of name, extract spectral data
  # INPUT: Unispec-DC .spu file
  # OUTPUT: dataframe of spectral data with 3 columns Wavelength, ChB, ChA
  
  # Read spectral intensity data into dataframe
  data <- read.table(file = filename, skip = 9, col.names = c("Wavelength", "ChB", "ChA"))
  
  #print(filename)
  
  return(data)
}


# Assign nearest reference value
assign_closest_ref <- function(data_int, ref_int) {
  # Get the nearest integration 
  
  #pick <- which(abs(ref_int-data_int) == min(abs(ref_int-data_int)))
  pick <- sapply(data_int, function(x) which.min(abs(ref_int - x)))
  REF_Integration <- ref_int[pick]
  
  return(REF_Integration)
}

# Color band definitions for calculate_indices function
band_defns <- tribble(
  ~definition, ~color, ~min, ~max,
  "ITEX", "red", 560, 600,
  "ITEX", "nir", 725, 1000,
  "MODIS", "red", 620, 670, 
  "MODIS", "nir", 841, 876,
  "MODIS", "blue", 459,479,
  "SKYE", "red", 620, 680,
  "SKYE", "nir", 830, 880,
  "SKYE", "blue", 455, 480,
  "ToolikGIS_Drone_2018", "red", 640, 680,
  "ToolikGIS_Drone_2018", "nir", 820, 890,
  "ToolikGIS_MicaSense_2019", "blue", 455, 495,
  "ToolikGIS_MicaSense_2019", "green", 540, 580,
  "ToolikGIS_MicaSense_2019", "red", 658, 678,
  "ToolikGIS_MicaSense_2019", "red_edge", 707, 727,
  "ToolikGIS_MicaSense_2019", "near_ir", 800, 880,
  "ToolikEDC", "red", 560, 680,
  "ToolikEDC", "nir", 725, 1000
)

calculate_indices <- function(spectra, band_defns, instrument = "MODIS", indices = "NDVI") {
  # Calculates NDVI, EVI, and EVI2 from dataframe including Wavelength : Spectra 
  ## inputs: spectra - dataframe with Wavelength, Reflectance columns
  ##         band_defns : dataframe defining wavelengths definining colors 
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indicies   : the index to return 
  ## output: Index - name of vegetation index
  ##         BandDefinition - name of "instrument" or spectral band definition used
  ##         Value - value of index, with the band definition used. 
  
  bands <- band_defns %>% 
    filter(definition == instrument) 
  
  blue <- bands %>% filter(color=="blue") %>% select(min, max) %>% as.numeric()
  nir <- bands %>% filter(color=="nir") %>% select(min, max) %>% as.numeric()
  red <- bands %>% filter(color=="red") %>% select(min, max) %>% as.numeric()
  
  spectra_bands <- spectra %>% 
    mutate(color = ifelse(Wavelength >= blue[1] & Wavelength <= blue[2], "blue",
                          ifelse(Wavelength >= red[1] & Wavelength <= red[2], "red",
                                 ifelse(Wavelength >= nir[1] & Wavelength <= nir[2], "nir",
                                        "other")))) %>% 
    group_by(color) %>% 
    summarize(Reflectance = mean(Reflectance))
  
  index_data <- spectra_bands %>%
    spread(color, Reflectance) %>% 
    
    ## INDEX DEFINITIONS
    mutate(NDVI = (nir-red)/(nir+red),
           EVI = 2.5*((nir-red)/(nir+6*red-7.5*blue + 1)),
           EVI2 = 2.5*((nir-red)/(nir+2.4*red + 1))) %>% 
    select_at(indices) %>% 
    gather(Index, Value, everything()) %>% 
    
    # Add Spectral Band Definition convention
    mutate(BandDefinition = instrument) %>% 
    select(Index, BandDefinition, Value)
  
  return(index_data) 
}


## SANITY CHECK FUNCTIONS

check_scan_times <- function(raw_data) {
  
  timedata <- raw_data %>% 
    select(Site, DateTime, FileNum) %>% 
    distinct(DateTime, .keep_all = T)
  
  # Calculate time between scans 
  timedata$diff <- timedata$DateTime - lag(timedata$DateTime)
  
  meta_timedata <- left_join(timedata, field_keys)
  
  time_check <- meta_timedata %>% select(Site, DateTime, Block, Treatment, Replicate, FileNum, diff, everything()) %>% ungroup()
  
  return(time_check)
}
# Check slected file extenstion

check_ext <- function(in_file,f_ext,ERR_message) {
  correct_ext <- all(tools::file_ext(in_file)==f_ext)
  shinyFeedback::feedbackWarning("key_file",!correct_ext,ERR_message)
  req(correct_ext, cancelOutput = TRUE)
}