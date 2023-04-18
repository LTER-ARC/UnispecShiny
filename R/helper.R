## Required Packages

library(tidyverse)
library(data.table)
library(openxlsx)


# Color band definitions for calculate_indices function
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
  FileNum <- stringr::str_extract(spu_filename, "\\d{4,5}") %>% as.numeric() # from 5 digits in filename

  # Line 2:
  Remarks <- stringr::str_split(text[2], pattern = " ")[[1]] # split by "space"
  Type <- stringr::str_split(Remarks[7], pattern = "=")[[1]][2] # extract relevant part
  ScanType <- ifelse(grepl("DARKscan", Type, fixed = T), "DARKscan", # format
    ifelse(grepl("Datascan,DC", Type, fixed = T), "Throwawayscan",
      Type
    )
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
  FileNum <- stringr::str_extract(spu_filename, "\\d{4,5}") %>% as.numeric() # from 5 digits in filename
  # Metadata
  metadata <- tibble(spu_filename, DateTime, FileNum, ScanType, Integration, NumberScans, Minimum_wavelength, Minimum_value, Maximum_wavelength, Maximum_value, Limits, Temperature, Battery, Aux, DarkscanID, Remarks)

  if (info == "short") {
    metadata <- metadata %>% select(spu_filename, DateTime, FileNum, Integration)
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
  data <- data.table::fread(file = filename, skip = 9, col.names = c("Wavelength", "ChB", "ChA"))

  # print(filename)

  return(data)
}
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Assign nearest reference value
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
assign_closest_ref <- function(data_int, ref_int) {
  # Get the nearest integration

  # pick <- which(abs(ref_int-data_int) == min(abs(ref_int-data_int)))
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
  ##         band_defns : dataframe defining wavelengths definining colors
  ##         instrument : e.g. MODIS, SKYE, ITEX
  ##         indicies   : the index or indices to return
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
    # ifelse(instrument == "ToolikGIS_MicaSense_2019",
    #        red_edge <- bands %>%filter(color=="red_edge") %>% select(min, max) %>% as.numeric(),
    #        red_edge <- NULL)
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
          # ifelse(Wavelength >= red_edge[1] & Wavelength <= red_edge[2],
          #        "red_edge",
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
# Function to rename the sites to standard names.
# @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
standard_site_names <- function(unispec_file) {
  # Standardize Site names from 2019 version to 2020 onward
  unispec_file <- unispec_file %>%
    mutate(Site = ifelse(Site %in% c("WSG1", "WSG23", "WSG", "WSG2"), "WSG89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("DHT", "HTH", "HEATH"), "DHT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MAT", "MAT-SH"), "MAT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("LMAT", "LOF"), "MAT06", Site)) %>%
    mutate(Site = ifelse(Site %in% c("HIST", "HIST81", "HST"), "MAT81", Site)) %>%
    mutate(Site = ifelse(Site %in% c("SHB2", "SHB1", "SHB"), "SHB89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MNAT"), "MNT97", Site)) %>%
    mutate(Site = ifelse(Site %in% c("NANT", "NNT97"), "MNN97", Site))
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
    if (name_check %in% names(column_names)) {
      key_sheet <- i
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
    # Standardize Site names from 2019 version to 2020 onwards
    mutate(Site = ifelse(Site %in% c("WSG1", "WSG23", "WSG"), "WSG89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("DHT", "HTH", "HEATH"), "DHT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MAT", "MAT-SH"), "MAT89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("LMAT"), "MAT06", Site)) %>%
    mutate(Site = ifelse(Site %in% c("HIST", "HIST81"), "MAT81", Site)) %>%
    mutate(Site = ifelse(Site %in% c("SHB2", "SHB"), "SHB89", Site)) %>%
    mutate(Site = ifelse(Site %in% c("MNAT"), "MNT97", Site)) %>%
    mutate(Site = ifelse(Site %in% c("NANT", "NNT97"), "MNN97", Site))
}
