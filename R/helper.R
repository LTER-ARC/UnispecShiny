## Required Packages

library(tidyverse)
library(data.table)
library(openxlsx)


# Color band definitions for calculate_indices function
# used only in calculate_indices and calculate_indices2.  A cut function
# is used in calculate_indices3
band_defns <- tribble(
  ~definition, ~color, ~min, ~max,
  "ITEX", "red", 560, 600,
  "ITEX", "nir", 725, 1000,
  "MODIS", "red", 620, 670,
  "MODIS", "nir", 841, 876,
  "MODIS", "blue", 459, 479,
  "SKYE", "red", 620, 680,
  "SKYE", "nir", 830, 880,
  "SKYE", "blue", 455, 480,
  "ToolikGIS_Drone_2018", "red", 640, 680,
  "ToolikGIS_Drone_2018", "nir", 820, 890,
  "ToolikGIS_MicaSense_2019", "blue", 455, 495,
  "ToolikGIS_MicaSense_2019", "green", 540, 580,
  "ToolikGIS_MicaSense_2019", "red", 658, 678,
  "ToolikGIS_MicaSense_2019", "red_edge", 707, 727,
  "ToolikGIS_MicaSense_2019", "nir", 800, 880,
  "ToolikEDC", "red", 560, 680,
  "ToolikEDC", "nir", 725, 1000
)
# Breaks and labels for the cut function.  Cut bins the data between break points.
# The breaks between the bands of interest are labeled "".
modis_bands_breaks <- c(459, 479, 620, 670, 841, 876)
modis_bands_labels <- c("blue", "", "red", "", "nir")
micasense_bands_breaks <- c(455, 495, 540, 580, 658, 678, 707, 727, 800, 880)
micasense_bands_breaks_labels <- c("blue", "", "green", "", "red", "", "red_edge", "", "nir")

# Indices equations
NDVI <- "signif((nir - red) / (nir + red), digits = 4)"
EVI <- "signif(2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1)), digits = 4)"
EVI2 <- "signif(2.5 * ((nir - red) / (nir + 2.4 * red + 1)), digis = 4)"
NDVIRE <- "signif((nir - red_edge) / (nir + red_edge), digits = 4)"
instruments <-tibble(sensor = c("modis","micasense"), indices = c("NDVI", "NDVI,NDVIRE") )

# FUNCTIONS for processing spu data

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Read the metadata from the .spu file
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
read_spu_file_metadata <- function(filepath, filename, info = "short") {
  # DESCRIPTION: Reads first 9 text lines in .spu files
  # INPUT: .spu file -- collected using PPSystems UnispecDC
  #         info = "short", the default returns only the spu_filename, DateTime, FileNum, Integration time
  #                "long", returns all the info in the 9 lines of header in .spu files
  # OUTPUT: dataframe with 15 columns on instrument status, scan settings, max/min value

  # Extract info from the file itself, reading metadata from first 9 lines. Create a dataframe
  text <- data.table::fread(filepath, sep = "", header = FALSE, nrows = 9)
  
  # Line 1: Extract the file name in the spu file as a check. Some file names have spaces
  spu_filename <- stringr::str_replace(text[1], ".*[\\\\]([A-z0-9.]\\s*)", "\\1") %>% # extract filename
    str_replace("\"", "") # removes trailing quote at end of line
  
  FileNum <- stringr::str_extract(filename, "(\\d{4,7})") %>% as.numeric() # from 4-7digits in filename

  # Line 2:
  Remarks <- text[2]
  # Identify the scan. Note: Datascan,DC can be a Throwaway or Reference.  The Key file will determine which one.
  ScanType<- case_when(
    str_detect(Remarks,"DARKscan") ~  "DARKscan",
    str_detect(Remarks,"Datascan,DC") ~  "Non-Datascan",
    str_detect(Remarks,"RCF") ~ "Unispec-Corrected",
    str_detect(Remarks,"Datascan") ~ "Datascan"                  
  )
 
  DarkscanID <- stringr::str_extract(text[2], "Dark=.+.spu")
  Remarks <- text[2]

  # Line 3:
  DateTime <- lubridate::mdy_hms(text[3], tz = "America/Anchorage")

  # Line 4: Limits -- range of spectra measured
  Limits <- stringr::str_extract(text[4], "\\d+.\\d.+\\d")

  # Line 5:
  Temperature <- as.numeric(str_split(str_split(text[5], pattern = " ")[[1]][4], pattern = "=")[[1]][2])
  Battery <- stringr::str_extract(text[5], "BattV=\\d+.\\d+")
  Aux <- stringr::str_extract(text[5], "A\\d=.+\\d")

  # Line 6-9:
  Minimum <- stringr::str_extract(text[6], "\\d+.+\\d") # Wavelength, ChB min
  Minimum_wavelength <- stringr::str_split(Minimum, boundary("word"))[[1]][1] # Wavelength
  Minimum_value <- stringr::str_split(Minimum, boundary("word"))[[1]][2] # ChB AD

  Maximum <- stringr::str_extract(text[7], "\\d+.+\\d") # Wavelength, ChB max
  Maximum_wavelength <- stringr::str_split(Maximum, boundary("word"))[[1]][1]
  Maximum_value <- stringr::str_split(Maximum, boundary("word"))[[1]][2]

  Integration <- as.numeric(stringr::str_split(text[8], pattern = " ")[[1]][3])
  NumberScans <- stringr::str_extract(text[9], "\\d+")

  # Truncated Filename - use as SCANID to join to other dataframes
  spu_filename <- unlist(stringr::str_split(filename, pattern = "/")) %>% last()
  # get site name based on file name and year of scans
  Site <- site_name(spu_filename,year(DateTime))
  # Metadata
  metadata <- tibble(spu_filename, Site, DateTime, FileNum, ScanType, Integration, NumberScans, 
                     Minimum_wavelength, Minimum_value, Maximum_wavelength, Maximum_value, Limits, 
                     Temperature, Battery, Aux, DarkscanID, Remarks) %>% 
    mutate(Date = date(DateTime)) %>%
    mutate(Date = unique(Date)[1]) %>% # Case where the time was off and some of the scans' times were the next day.
    relocate(Date, .after = DateTime)

  if (info == "short") {
    metadata <- metadata %>% select(spu_filename, Site, DateTime, Date, FileNum, Integration,ScanType)
  }

  # Print filenames while reading
  # print(spu_filename) # use for error checking

  return(metadata)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Read the spectra data from .spu file
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
read_spu_file_spectra <- function(filename) {
  # DESCRIPTION: For a generic .spu file regardless of name, extract spectral data
  # INPUT: Unispec-DC .spu file
  # OUTPUT: dataframe of spectral data with 3 columns Wavelength, ChB, ChA

  # Read spectral intensity data into dataframe
  data <- data.table::fread(file = filename, skip = 9, col.names = c("Wavelength", "ChB", "ChA")) %>% 
    filter(Wavelength > 400, Wavelength < 1000)  # remove edge wavelengths, instrument unreliable at extremes

  return(data)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Assign nearest reference value
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
assign_closest_ref <- function(data_int, ref_int) {
  # Get the nearest integration

  pick <- sapply(data_int, function(x) which.min(abs(ref_int - x)))
  REF_Integration <- ref_int[pick]

  return(REF_Integration)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#  Calculates indices, e.g. NDVI, EVI, and EVI2
# Using a list of wavelength and reflectance
# NOTE: Since each .spu' Spectra is mapped to this function this
# function is called for every .spu file. The mapping takes avery long to complete.
# The function calculate_indices2 which take an unnested spectra is about 25x faster.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
calculate_indices <- function(spectra, band_defns, instrument = "MODIS", indices = "NDVI") {
  # Calculates NDVI, EVI, and EVI2 from dataframe
  ## inputs: spectra - dataframe with a list Wavelength, Reflectance columns
  ##         band_defns : dataframe defining wavelengths defining colors
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indices   : the index to return
  ## output:A list named Index - name of vegetation index
  ##         BandDefinition - name of "instrument" or spectral band definition used
  ##         Value - value of index, with the band definition used.

  bands <- band_defns %>%
    filter(definition == instrument)

  blue <- bands %>%
    filter(color == "blue") %>%
    select(min, max) %>%
    as.numeric()
  nir <- bands %>%
    filter(color == "nir") %>%
    select(min, max) %>%
    as.numeric()
  red <- bands %>%
    filter(color == "red") %>%
    select(min, max) %>%
    as.numeric()
  ifelse(instrument == "ToolikGIS_MicaSense_2019",
    red_edge <- bands %>% filter(color == "red_edge") %>% select(min, max) %>% as.numeric()
  )

  spectra_bands <- spectra %>%
    mutate(color = ifelse(Wavelength >= blue[1] & Wavelength <= blue[2], "blue",
      ifelse(Wavelength >= red[1] & Wavelength <= red[2], "red",
        ifelse(Wavelength >= nir[1] & Wavelength <= nir[2], "nir",
          ifelse(Wavelength >= red_edge[1] & Wavelength <= red_edge[2], "red_edge",
            "other"
          )
        )
      )
    )) %>%
    group_by(color) %>%
    summarize(Reflectance = mean(Reflectance))

  index_data <- spectra_bands %>%
    spread(color, Reflectance) %>%
    ## INDEX DEFINITIONS
    dplyr::mutate(
      NDVI = if ("NDVI" %in% indices) signif((nir - red) / (nir + red), digits = 4),
      EVI = if ("EVI" %in% indices) signif(2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1)), digits = 4),
      EVI2 = if ("EVI2" %in% indices) signif(2.5 * ((nir - red) / (nir + 2.4 * red + 1)), digis = 4),
      NDVIRE = if ("NDVIRE" %in% indices) signif((nir - red_edge) / (nir + red_edge), digits = 4)
    ) %>%
    dplyr::select_at(indices) %>%
    gather(Index, Value, everything()) %>%
    # Add Spectral Band Definition convention
    dplyr::mutate(BandDefinition = instrument) %>%
    select(Index, BandDefinition, Value)

  return(index_data)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Calculates NDVI, EVI, and EVI2 from dataframe !!That is not nested!!
#  The dataframe' Spectra is unnested with Wavelength and Reflectance columns
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
calculate_indices2 <-
  ## inputs: spectra - dataframe with Wavelength, Reflectance columns
  ##         band_defns : dataframe defining wavelengths defining colors
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indices   : the index or indices to return
  ## output: returns the dataframe with a list named Index (name of vegetation index
  ##         BandDefinition - name of "instrument" or spectral band definition used
  ##         Value - value of index, with the band definition used).
  function(spectra,
           band_defns = band_defns,
           instrument = "MODIS",
           indices = "NDVI") {
    bands <- band_defns %>%
      filter(definition == instrument)

    blue <- bands %>%
      filter(color == "blue") %>%
      select(min, max) %>%
      as.numeric()
    nir <- bands %>%
      filter(color == "nir") %>%
      select(min, max) %>%
      as.numeric()
    red <- bands %>%
      filter(color == "red") %>%
      select(min, max) %>%
      as.numeric()
    if (instrument == "ToolikGIS_MicaSense_2019") {
      red_edge <- bands %>%
        filter(color == "red_edge") %>%
        select(min, max) %>%
        as.numeric()
    }
    spectra_bands <- spectra %>%
      dplyr::mutate(color = ifelse(
        Wavelength >= blue[1] & Wavelength <= blue[2],
        "blue",
        ifelse(
          Wavelength >= red[1] & Wavelength <= red[2],
          "red",
          ifelse(Wavelength >= nir[1] &
            Wavelength <= nir[2], "nir",
          "other"
          )
        )
      )) %>%
      group_by(
        Date,
        Site,
        Block,
        Treatment,
        Replicate,
        spu_filename,
        DateTime,
        color
      ) %>%
      summarize(Reflectance = mean(Reflectance), .groups = "drop")

    index_data <- spectra_bands %>%
      spread(color, Reflectance) %>%
      ## INDEX DEFINITIONS
      dplyr::mutate(
        NDVI = if ("NDVI" %in% indices) signif((nir - red) / (nir + red), digits = 4),
        EVI = if ("EVI" %in% indices) signif(2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1)), digits = 4),
        EVI2 = if ("EVI2" %in% indices) signif(2.5 * ((nir - red) / (nir + 2.4 * red + 1)), digis = 4),
        NDVIRE = if ("NDVIRE" %in% indices) signif((nir - red_edge) / (nir + red_edge), digits = 4)
      ) %>%
      # Add Spectral Band Definition convention
      select(-nir, -red, -other, -blue) %>%
      dplyr::mutate(BandDefinition = instrument) %>%
      pivot_longer(cols = indices, names_to = "Index", values_to = "Value") %>%
      nest(Indices = c("Index", "BandDefinition", "Value")) %>%
      dplyr::left_join(
        {
          spectra %>% nest(Spectra = c("Wavelength", "Reflectance"))
        },
        by = c(
          "Date", "Site", "Block", "Treatment", "Replicate",
          "spu_filename", "DateTime"
        )
      ) %>%
      relocate(Indices, .after = last_col())
  }

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
calculate_indices3 <-
  ## inputs: spectra - dataframe with Wavelength, 
  ##         instrument : tibble with sensor,e.g. MODIS, SKYE, ITEX, and 
  ##            indices the index or indices to return
  ## output: returns the dataframe with a list named Index (name of vegetation index
  ##         
  function(spectra,instruments = insruments) {
    
    df2 <- spectra %>%
      mutate(
        modis = cut(
          Wavelength,
          breaks = c(400, 459, 479, 620, 670, 841, 876, 1000),
          labels = c("other","blue", "other", "red", "other", "nir","other"),
        ),
        micasense = cut(
          Wavelength,
          breaks = c(400, 455, 495, 540, 580, 658, 678, 707, 727, 800, 880, 1000),
          labels = c("other","blue", "other", "green", "other", "red", "other", 
                     "red_edge", "other", "nir","other")
        )
      )
    
    index_data <-
      map2(instruments$sensor,instruments$indices, function(x,y) {
        indices <- str_split(y,",")[[1]]
        df2 %>%
          group_by(
            Date,
            Site,
            Block,
            Treatment,
            Replicate,
            spu_filename,
            DateTime,
            .data[[x]]
          ) %>%
          summarize(Reflectance = mean(Reflectance), .groups = "drop") %>%
          spread(.data[[x]], Reflectance) %>%
          dplyr::mutate(
            NDVI = if ( str_detect(y,"NDVI")) signif((nir - red) / (nir + red), digits = 4),
            EVI = if (str_detect(y,"EVI" )) signif(2.5 * ((nir - red) / (nir + 6 * red - 7.5 * blue + 1)), digits = 4),
            EVI2 = if (str_detect(y,"EVI2")) signif(2.5 * ((nir - red) / (nir + 2.4 * red + 1)), digis = 4),
            NDVIRE = if (str_detect(y,"NDVIRE")) signif((nir - red_edge) / (nir + red_edge), digits = 4)
          ) %>% 
          select(Date,Site,Block,Treatment,Replicate,spu_filename,DateTime,any_of(indices)) %>%
          rename_with(~paste0(x,.x,recycle0 = TRUE),.cols = any_of(indices)) 
      }) %>% 
      reduce(full_join, by = join_by(Date, Site, Block, Treatment, Replicate, spu_filename, DateTime)) %>% 
      nest(Indices = contains("VI"))
    return(index_data)
  }

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Function to rename the sites to standard names.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
standard_site_names <- function(unispec_file) {
  # Standardize Site names from 2019 version to 2020 onward
  unispec_file <- unispec_file %>%
    mutate(
      Site = case_when(
        Site %in% c("WSG1", "WSG23", "WSG", "WSG2", "WSGB","LWSG", "WSD", "WS","TLSE","OUTSE") ~ "WSG89",
        Site %in% c("DHT", "DH", "HTH", "HEATH", "LHTH","HTHB") ~ "DHT89",
        Site %in% c("MAT", "MAT-SH", "MATSL", "MATSH") ~ "MAT89",
        Site %in% c("LMAT", "LOF") ~ "MAT06",
        Site %in% c("HIST", "HIST81", "HST", "HIS") ~ "MAT81",
        Site %in% c("SHB2", "SHB1", "SHB", "SHBB", "LSHB", "SH", "SHRB") ~ "SHB89",
        Site %in% c("MNAT") ~ "MNT97",
        Site %in% c("NANT", "NNT97","NMNT") ~ "MNN97",
        Site %in% c("LMATEDC") ~ "EDC",
        .default = Site
      )
    )
  return(unispec_file)
}

# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## SANITY CHECK FUNCTIONS
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
check_scan_times <- function(raw_data) {
  timedata <- raw_data %>%
    select(Site, DateTime, FileNum) %>%
    distinct(DateTime, .keep_all = T)

  # Calculate time between scans
  timedata$diff <- timedata$DateTime - lag(timedata$DateTime)

  meta_timedata <- left_join(timedata, field_keys)

  time_check <- meta_timedata %>%
    select(Site, DateTime, Block, Treatment, Replicate, FileNum, diff, everything()) %>%
    ungroup()

  return(time_check)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Check selected file extension
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
check_ext <- function(input_id, in_file, f_ext, ERR_message) {
  correct_ext <- all(tools::file_ext(in_file) == f_ext)
  shinyFeedback::feedbackWarning(input_id, !correct_ext, ERR_message)
  req(correct_ext, cancelOutput = TRUE)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Check the excel header row for required column name of "Fi
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
find_sheet <- function(key_file, name_check) {
  wb_sheets <- openxlsx::getSheetNames(key_file)
  key_sheet <- NULL
  for (i in wb_sheets) {
    column_names <- openxlsx::read.xlsx(key_file, sheet = i, colNames = T, rows = 1)
    if (all(name_check %in% names(column_names))) {
      key_sheet <- i
      break
    }
  }
  shinyFeedback::feedbackWarning("key_file", is.null(key_sheet), "Check Excel file column names.")
  if (is.null(key_sheet)) {
    showNotification(paste("Column names should match ", toString(headers)), duration = 15, type = "error")
  }
  req(key_sheet, cancelOutput = TRUE)
  return(key_sheet)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Get current index in form for plotting
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
current_index_data_ndvi <- function(index_data) {
  index_data %>%
    select(-Spectra) %>%
    unnest(Indices) %>%
    select(-BandDefinition) %>%
    spread(Index, Value) %>%
    # Select useful columns
    select(
      DateTime,
      Date,
      Site,
      Block,
      Treatment,
      Replicate,
      FileNum,
      NDVI
    ) %>%
    # Create additional columns
    mutate(
      Year = lubridate::year(DateTime),
      DOY = lubridate::yday(DateTime)
    ) %>%
    mutate(Block = as.numeric(str_extract(Block, "\\d"))) %>%
    mutate(Replicate = as.character(Replicate)) %>%
    mutate(Site = as.character(Site)) %>%
    mutate(collection_year = "Current") %>%
    # remove non-data (ref, dark, throwaway) scans
    filter(!str_detect(Treatment, "REF|DARK|THROWAWAY")) %>%
    # Standardize Site names from 2019 version onward
    standard_site_names()
    
}
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# get the site name from the .spu file name and year of scans
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
site_name <- function(spu_filename, site_year) {
  if (site_year > "2016") {
    Site <- toupper(str_extract(spu_filename, "^([a-zA-Z]+)([0-9]+)?((-[a-zA-Z]+)([0-9]+)?)*"))
  } else {
    # For 2016 file names are MMMDDsiteFilenumber, e.g. JUL10LOF00006.spu and sometimes with B1 or B2 added.
    Site <- toupper(str_replace(spu_filename, "(^.*?\\d{1,2})\\s*([a-zA-Z]*)(\\d{5,7}\\.spu$)", "\\2"))
    # For 2012 and 2013 the spu filenames have ddmmmsite format; need to remove the 3 letter month 
    # which was extracted with the site.
    if (str_length(Site) > 5) {
      pattern <- c("MAY", "JUN", "JUL", "AUG")
      for (i in 1:4) {
        Site <- sub(pattern[i], "", Site)
      }
    }
  }
  return(Site)
}
keys_2_spu <- function(key_file) {
  # Check for required column names
  name_check <- "FileNum"
  wb_sheets <- openxlsx::getSheetNames(key_file)
  key_sheet <- NULL
  for (i in wb_sheets) {
    column_names <- openxlsx::read.xlsx(key_file, sheet = i, colNames = T, rows = 1)
    if (name_check %in% names(column_names)) {
      key_sheet <- i
      break
    }
  }
  # Read in key data and join in the spu_filename
  df <- tryCatch({
    
  key_file %>%
    purrr::map(function(file_name) {
      as_tibble(openxlsx::read.xlsx(file_name, sheet = key_sheet, detectDates = T, cols = c(1:8)))
    }) %>%
    reduce(rbind) %>%
    mutate(across(where(is.character), str_trim)) %>%
    standard_site_names() %>%
    separate_rows(Site, sep = "-") %>% 
    mutate(Location = as.character(Location),
           Notes = as.character(Notes))
  }, warning = function(war) {
    print(key_file)
  }, error = function(err) {
    # Is executed if error encountered
  })

  return(df)
}
