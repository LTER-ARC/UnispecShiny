#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
# ---
# title: "UnispecShiny app"
# author: "Jim Laundre"
# date: "2022-08-04"
# Description:  A shiny app to display raw Unispec scans, error checks and
# label raw data with plot ids, reference correction, calculate indices and plot
# with past years scans.
# Files and file format:
# Past data file in ./data with columns: "Year" "Date" "DateTime" "DOY" "Site" "Block"
#        "Treatment" "Replicate" "FileNum" "modisNDVI" "modisEVI" "modisEVI2" "collection_year"
# Note: A different past data file can be read in in the ui of the app.
#
# Other files read in using the app ui:
#  raw .spu files
#  excel file with the key to the .spu files. See Unispec documentation for file format.
# Files calculated:
# keys() the info from the Excel key file
# key_spu_data(): A combined raw .spu and the excel key file information.
# ref_correction_factors(): Correction factors (chA/chB) for the REF scans. chA is the up facing sensor
# ref_int_values(): integration times of ref data for each site
# data_corrected_ref_integration(): Corrected reflectance
#
# indices_data(): Calculate Indices
# past_all_data(): Join past years indices data with indexes processed

# Data for Plots tabs
# processed_spectra(): Corrected Processed Spectra
# indices_data(): Corrected Processed indices
#
#
# ---

## Required Packages -----

library(shiny)
library(rstudioapi)
library(tidyverse)
library(openxlsx)
library(plotly)
library(data.table)
library(DT)
library(shinyjs)
library(viridis)
# Notes ---------------------------------------------------------------------------------

# Allow up to 10 mb file upload.
options(shiny.maxRequestSize = 10 * 1024^2)

# 2023-03-30 Changed to input file UI to selecting the past indices file.
# Added in the files tab where an updated past indices file can be saved.
# Note: Past data have been cleaned and sites standardized

# The default past indices file loaded is "data/indices_2014-2022.rds"

# Define UI for application ---------------------------------------------------

ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Setup for Error messages feedback
  shinyFeedback::useShinyFeedback(),
  tags$head(tags$style(
    HTML(
      ".shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }"
    )
  )),
  tags$head(tags$style(
    HTML(
      ".shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
             }"
    )
  )),
  titlePanel("Arctic LTER Spectral Reflectance Data"),
  sidebarLayout(
    sidebarPanel(
      #helpText("Default Multi-year file loaded = indices_2014-current.rds."),
      # Note input$multiyear_indices_file is a data frame - name, size, and datapath (path and file name)
      fileInput(
        "multiyear_indices_file",
        "Default file:indices_2014-current.rds. Upload a new multiyear indices.rds file?",
        placeholder = "indices_2014-current.rds",
        multiple = FALSE,
        accept = c(".rds")
      ),
      helpText("Select Unispec .spu files. Note: Cloud drive files can take a long time to load"),
      # Note input$spu_file is a data frame - name, size, and data path (path and file name)
      fileInput(
        "spu_file",
        "Upload .spu files",
        multiple = TRUE,
        accept = c(".spu")
      ),
      helpText("Default max. file size is 5MB"),
      fileInput(
        "key_file",
        "Upload Field Key Excel files",
        multiple = TRUE,
        accept = c(".xlsx")
      ),
      uiOutput("keyfiles_loaded"),
      hr(),
      conditionalPanel(
        condition = "input.tabselected != 'Files'",
        selectInput(
          "choice_site",
          "Sites",
          choices = NULL,
          multiple = FALSE
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 'Spectra Plot'",
        selectInput(
          "selectfile",
          "Select File",
          choices = NULL,
          size = 10,
          selectize = FALSE,
          multiple = FALSE
        ),
        selectInput( # Select for treatment to filter the list of spu files
          "treatments",
          "Treatments",
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 'Plot Graphs' || input.tabselected == 'Compare to Past Years'",
        checkboxGroupInput("choice_treatment", "Treatment",
          choices = NULL
        )
      ),
      conditionalPanel(
        condition = "input.tabselected == 'Plot Graphs'",
        checkboxGroupInput("choice_block", "Block",
          choices = NULL
        )
      ),
    ),
    mainPanel(uiOutput("QAQC_tb"))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # Helper functions -----------------
  source("R/helper.R", local = TRUE)
  # Default variables. instruments is used to calculate the indices
  instruments <- tibble(sensor = c("modis", "micasense"), indices = c("NDVI", "NDVI,NDVIRE"))
  # index2plot id used in the output plots
  index2plot <- "modisNDVI"
  # Template sheet name to use as a check for correct sheet
  # !!!Change if the template file changes!!!!
  header_check <- "FileNum"
  sheet_check <- "LongFormat"
  # @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  past_indices_data <- reactive({
    if (is.null(input$multiyear_indices_file)) {
      readRDS("data/indices_2014-2022.rds") %>% 
      mutate(across(where(is.factor), as.character)) %>%  # Remove factors for joining to current data
        mutate(Replicate = as.numeric(Replicate))
    } else {
      # Get the multiyear indices file but first check extension of file
      check_ext("multiyear_indices_file", input$multiyear_indices_file$name, "rds", "Invalid file; Please select a .rds file")
      readRDS(input$multiyear_indices_file$datapath) %>% 
      mutate(across(where(is.factor), as.character)) # Remove factors for joining to current data
    }
  })

  # Sidebar updates of SelectInputs ---------------------------------------------

  ## Update of Select input widgets ----
  # Choices are update depending on site selected and treatment

  # Update site input based on the key file loaded
  observeEvent(c(keys(), input$spu_file), {
    updateSelectInput(session, inputId = "choice_site", choices = unique(key_spu_data()$Site))
  })

  # Update Treatments based on selected Site
  site_subset <- reactive({
    req(key_spu_data(), input$choice_site)
    filter(key_spu_data(), Site %in% input$choice_site) %>% 
    mutate(across(where(is.factor), as.character)) # Convert factors so we can filter  
  })
  observeEvent(c(input$choice_site, input$key_file, input$spu_file), {
    req(site_subset())
    choices <- c("All", "Spectra_maxed", unique(site_subset()$Treatment))
    updateSelectInput(inputId = "treatments", choices = choices, selected = "All")
  })

  # update spu_filename based on selected site and treatments
  # Note using head to get 1st element; leaving as NULL didn't work),
  observeEvent(c(input$choice_site, input$treatments, input$key_file, input$spu_file), {
  choices <- switch(input$treatments,
    All = {
      site_subset() %>% select(spu_filename)
    },
    Spectra_maxed = {
      maxed_files()$spu_filename
    },
    {
      unique(filter(site_subset(), Treatment %in% input$treatments)$spu_filename)
    }
  )
  updateSelectInput(inputId = "selectfile", choices = choices, selected = head(choices, 1))
})

  ## Update input choices for Plot Graphs and Compare to Past Years Tabs ----

  # Update the treatment and block choices
  observeEvent(c(input$choice_site, input$key_file, input$spu_file), {
    # Get all the treatments for a site
    selected_t <- unique(processed_spectra() %>%
      filter(Site == input$choice_site) %>%
      select(Treatment))$Treatment %>%
      factor() %>%
      levels()
    # Check if any previous selected treatments are in the new selected site
    before_selected <- if (any(input$choice_treatment %in% selected_t)) {
      input$choice_treatment
    } else {
      selected_t[1]
    }
    updateCheckboxGroupInput(session,
      inputId = "choice_treatment", choices = selected_t,
      selected = before_selected
    )
    # Update the block selection
    selected_b <- unique(processed_spectra() %>%
      filter(Site == input$choice_site) %>%
      select(Block))$Block %>%
      factor() %>%
      levels()
    before_selected <- if (is.null(input$choice_block)) {
      selected_b
    } else {
      input$choice_block
    }
    updateCheckboxGroupInput(session,
      inputId = "choice_block", choices = selected_b,
      selected = before_selected
    )
  })
  # __________________________________________________________________________

  # Reactive outputs ----

  ## Read in the Excel key file ---------------------------------------
  # and output a combined table if more then one file. Join with the .spu filename
  # based on the file number.
  #  Using shinyFeedback to warn about incorrect file selection
  keys <- reactive({
    req(input$key_file, input$spu_file, spu_df())
    # Check extension of file
    check_ext("key_file", input$key_file$name, "xlsx", "Invalid file; Please select a .xlsx file")
    # Check for required column names
    key_sheet <- find_sheet(input$key_file$datapath, header_check)
    # Read in key data and join in the spu_filename
    df <- input$key_file$datapath %>%
      purrr::map(function(file_name) {
        as_tibble(openxlsx::read.xlsx(file_name, sheet = key_sheet, detectDates = T, cols = c(1:7)))
      }) %>%
      reduce(rbind) %>%
      mutate(across(where(is.character), str_trim),
                    FileNum = as.character(FileNum)) %>% # Change to character; REF will be concatenated to file number
      filter(Date %in% spu_df()$Date, Site %in% spu_df()$Site)
    #arrange(Site, spu_filename) %>%
     

    if (nrow(df) == 0) {
      shinyFeedback::feedbackWarning(
        inputId = "key_file",
        any(nrow(df) == 0),
        text = "The keys dataframe has 0 rows. Check Key file name or the file for missing information."
      )
      NULL -> fd # Remove fd data object
      return()
    }

    return(df)
  })
  ## Read the .spu data ---------------------------------------------------
  spu_df <- reactive({
    req(input$spu_file)
    # Check extension of file
    check_ext("spu_file", input$spu_file$name, "spu", "Invalid file; Please select a .spu file")
    id <- showNotification("Reading spu files' data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Read metadata text lines (9) from the spu files
    spu_filedata <- pmap_dfr(list(input$spu_file$datapath, input$spu_file$name), read_spu_file_metadata) %>%
      # Read in spectra data
      mutate(Spectra = map(input$spu_file$datapath, function(x) read_spu_file_spectra(x))) %>%
      mutate(Date = date(DateTime)) %>%
      relocate(Date, .after = DateTime) %>%
      mutate(Date = unique(Date)[1],# Case where the time was off and some of the scans' times were the next day.
             FileNum = as.character(FileNum)) # Change to character; REF will be concatenated to file number)
      #
    return(spu_filedata)
  })

  ## Combine the key file info with the spu data ------------------------------

  key_spu_data <- reactive({
    req(keys())
    id <- showNotification("Combining data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)

    fd <- left_join(spu_df(),keys(),  by = c("Date", "Site", "FileNum")) %>%
      #filter(!is.na(Treatment)) %>% 
      arrange(DateTime) %>%
      mutate_at(.vars = vars(Site, Block, Treatment), .funs = factor) %>% 
      # Give References a unique File Number for cases where a reference scan from a different site is used.
      mutate(FileNum = ifelse(Treatment %in% c("REF","DARK"),paste0(Treatment,FileNum),FileNum)) %>% 
      # Separate any multi site reference scans, e.g. MNT97-NNT97
      separate_rows(Site, sep = "-") %>% 
      standard_site_names()

    shinyFeedback::feedbackWarning(
      inputId = "spu_file",
      all(is.na(fd$spu_filename)),
      text = "No matchs between key and .spu files."
    )
    req(!all(is.na(fd$spu_filename)), cancelOutput = TRUE)
    return(fd)
  })

  ## Reference corrected data  -----------------------------------------------------------
  # Calculate the correction factor (chA/chB) for all the wavelengths of the REF files
  # Using the integration times to join the correction factor to the treatment scans, i.e.
  # the REF integration time nearest to data scan integration time.

  ### 1) Get mean correction factor for each Date, Site, Integration, and Wavelength ----
  ref_correction_factors <- reactive({
    req(key_spu_data())
    # Check for reference issues
    invalid_ref <- any(is.na(key_spu_data() %>% 
                  filter(Treatment == "REF") %>%.$spu_filename))|
                  !nrow(key_spu_data() %>% filter(Treatment == "REF"))
    shinyFeedback::feedbackWarning(
      inputId = "key_file",
      invalid_ref,
      text = "A reference has no FileNum or there are no REF files. Check Key File."
    )
    req(!invalid_ref,
      cancelOutput = TRUE
    )

    refdata <- key_spu_data() %>%
      ## Get the spectra for reference files
      filter(Treatment == "REF") %>%
      # Separate any multi site reference scans, e.g. MNT97-NNT97
      separate_rows(Site, sep = "-") %>%
      ### Unnest Spectra & calculate correction factor
      unnest(Spectra) %>%
      filter(Wavelength > 400, Wavelength < 1000) %>% # remove edge wavelengths, instrument unreliable at extremes
      mutate(correction_factor = ChA / ChB) %>%
      ### Group repeated REF measurements based on site
      group_by(Date, Site, Integration, Wavelength) %>%
      summarize(
        correction_factor = mean(correction_factor),
        # Add all the ref spu filenames to variable ref_filename
        ref_filenames = str_c(spu_filename, collapse = ", "), .groups = "keep"
      ) %>%
      rename(Integration.ref = Integration)

    shinyFeedback::feedbackWarning(
      inputId = "key_file",
      any(is.na(refdata$correction_factor)),
      text = "A reference file has a NA for correction factor."
    )
    req(!any(is.na(refdata$correction_factor)), cancelOutput = TRUE)

    return(refdata)
  })

  ### 2) Get integration times of ref correction factors for each site ----------
  ref_int_values <- reactive({
    ref_correction_factors() %>%
      group_by(Date, Site) %>%
      select(Date, Site, Integration.ref) %>%
      distinct() %>%
      # Separate any multi site reference scans, e.g. MNT97-NNT97
      separate_rows(Site, sep = "-") %>%
      rename(Integration = Integration.ref) # rename for joining
  })
  ###  3) Join reference scan integration time     ------------------------
  #     to the nearest data scan integration time. If several integration times
  #     there will be a many-to-many relationship. After assigning a integration
  #     reference time, unique() is used to get rid of the duplicates. 
  data_corrected_ref_integration <- reactive({
   req(key_spu_data(), input$key_file)
   full_join(key_spu_data(), ref_int_values(),
     by = c("Date", "Site"),
     suffix = c(".data", ".ref"), relationship = "many-to-many"
   ) %>%
     group_by(Date, Site, FileNum) %>%
     ## assign the "Integration.ref" value closest to data scan integration time
     mutate(Integration.ref = ifelse(is.na(Integration.ref), Integration.ref,
       Integration.ref[which.min(abs(Integration.data - Integration.ref))]
     )) %>%
     unique() %>% 
     unnest(Spectra) %>%
     rowwise() %>%
     group_by(Date, Site, Integration.ref) %>%
     # join data with ref correction factors
     left_join(., ref_correction_factors(),
       by = c("Date", "Site", "Wavelength", "Integration.ref")
     ) %>%
     ungroup() %>%
     # calculate raw and corrected reflectances.
     # And find maxed out scans and maxed out red bands.
     mutate(
       raw_reflectance = ChB / ChA, # the raw reflectance
       corrected_reflectance = ifelse(ScanType == "Unispec-Corrected",
         raw_reflectance,
         raw_reflectance * correction_factor
       ),
       Maxout = ChA > 65000 | ChB > 65000,
       Red_band_maxed = Maxout & Wavelength > 615
     ) 
 })

  ## Calculate Indices -----------------------------------------------------
  indices_data <- reactive({
   id <- showNotification("Please wait. Calculating Indices...", duration = NULL, closeButton = FALSE)
   on.exit(removeNotification(id), add = TRUE)

   data_corrected_ref_integration() %>%
     select(-ChB, -ChA, -raw_reflectance, -correction_factor) %>%
     filter(!toupper(Treatment) %in% c("DARK", "REF", "IGNOR") | !is.na(Treatment)) %>%
     rename(Reflectance = corrected_reflectance) %>%
     # Calculate Indices as defined in helper.R
     calculate_indices3(instruments) %>%
     unnest(cols = c(Indices)) %>%
     mutate(across(where(is.factor), as.character)) %>% # Convert factors so we can filter
     dplyr::filter(!toupper(Treatment) %in% c("DARK", "REF", "IGNOR", "Throwawayscan")) %>%
     mutate(
       Block = as.numeric(str_extract(Block, "\\d")), # convert "B1", etc  to numeric
       Year = lubridate::year(DateTime),
       DOY = lubridate::yday(DateTime)
     ) %>%
     relocate(Year, DOY, .after = Date)
 })
  ## Join past years indices data with indexes processed ----
  past_data_all <- reactive({
    req(indices_data(), past_indices_data())

    indices_data() %>%
      mutate(collection_year = "Current") %>%
      # remove non-data (ref, dark, throwaway) scans
      filter(!str_detect(Treatment, "REF|DARK|THROWAWAY")|!is.na(Treatment)) %>%
      full_join(past_indices_data(), by = c(
        "Year", "Date", "DateTime", "Site", "Treatment",
        "DOY", "collection_year", "Replicate", "Block", index2plot
      )) %>%
      arrange(Date) %>%
      unique()
  })
  
  
  ## Data for Plots tabs ----
  ### 1) Corrected Processed Spectra ----
  processed_spectra <- reactive({
    data_corrected_ref_integration() %>%
      mutate(across(where(is.factor), as.character)) %>% # Convert factors so we can filter
      dplyr::filter(!toupper(Treatment) %in% c("DARK", "REF", "IGNOR")) %>%
      mutate(Block = as.numeric(str_extract(Block, "\\d"))) # convert "B1", etc to numeric
  })

  # Checks on the data --------------------------------------------------------

  ## Check for missing information --------------------------------------------
  missing_info <- reactive({
    req(key_spu_data())
    key_spu_data() %>%
      filter(Site %in% input$choice_site) %>% # Only show checks on selected site.
      filter(
        is.na(spu_filename) |
          is.na(Site) |
          # Block with NA's should always be REFS or EXTRA
          is.na(Block) & !str_detect(Treatment, "REF|DARK|VEG") |
          # Check for replicates with NA's that aren't REF
          is.na(Replicate) & !str_detect(Treatment, "REF|DARK|THROWAWAY"),
        # don't care about EXTRA, VEG, or REF scans
        Treatment != "EXTRA|VEG|REF"
      ) %>%
      # reorder columns to see output better
      select(spu_filename, FileNum, Site, Block, Treatment, Replicate)
  })
  ## Check for number of scans that are not 5 or 10.----
  treatment_odd_number <- reactive({
    filter(key_spu_data(), key_spu_data()$Site %in% input$choice_site) %>%
      group_by(Site, Block, Treatment) %>%
      summarise(Total_count = n(), .groups = "keep") %>%
      filter(!Total_count %in% c(5, 10), Treatment != "DARK")
  })
  ## Table of checks ------------------------------------------------------
  checks_table <- reactive({
    spu_df_selected_site <- spu_df() %>% filter(Site %in% input$choice_site)
    spu_sites <- str_c(str_replace_na(unique(spu_df_selected_site$Site)), collapse = ",")
    key_sites <- str_c(str_replace_na(unique(keys()$Site)), collapse = ",")
    file_check <- paste(
      "Does number of .spu files for selected site -", nrow(spu_df_selected_site),
      ", match what's entered in key file -",
      nrow(filter(keys(), keys()$Site %in% input$choice_site)), "?"
    )
    site_check <- paste("Site used in spu files' names,", spu_sites, ", should match site in key file:", input$choice_site)
    maxedspectra <- if (nrow(maxed_files()) == 0) { # {
      paste("No maxed out specra.")
    }
    maxed_dt <- datatable(maxed_files(), caption = "Maxed out spu files.")
    txt_s <- datatable(tibble(Checks = c(site_check, file_check)),
      options = list(dom = "t")
    )
    return(txt_s)
  })

  ## Identify any files that aren't listed in the field key--------------------
  files_not_in_key <- reactive({
    data.frame(
      spuFiles_Not_in_Keys = anti_join(spu_df(), key_spu_data(),
        by = c("Date", "FileNum", "Site")
      ) %>%
        filter(Site %in% input$choice_site) %>%
        pull(spu_filename)
    )
  })

  ## Maxed out spectra --------------------------------------------------------
  maxed_files <- reactive({
    req(data_corrected_ref_integration()) 
    data_corrected_ref_integration() %>% 
    filter(Maxout) %>%
    mutate (Red_band_OK = !Red_band_maxed) %>% 
    select(Site, Block, Treatment, spu_filename, Maxout, Red_band_OK) %>%
      unique() %>%
      ungroup() %>%
      {
        if (nrow(.) == 0) add_row(., spu_filename = "No maxed out specra.") else .
      }
  })

  # Plot Spectra Tab ----------------------------------------------------

  #   This reactivate output contains the raw spectra in an x-y line plot format
  observeEvent(input$selectfile, ignoreNULL = TRUE, {
    # Only output plot if there is a file selected.
    output$specplot <- renderPlot({
      req(input$selectfile)
      if (is.null(input$selectfile) |
        !str_detect(input$selectfile, ".spu")) {
        return()
      }

      df <- data_corrected_ref_integration() %>%
        filter(spu_filename == input$selectfile, Site == input$choice_site) %>%
        tidyr::gather(key = Channel, value = Intensity, ChB, ChA) %>%
        tidyr::gather(
          key = ref_part,
          value = Reflectance_Intensity,
          Intensity,
          raw_reflectance
        )
      if (nrow(df) == 0) {
        return()
      }
      ## viz
      ggplot(df, mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
        geom_line(aes(color = Channel)) +
        scale_size_manual( values = c(2,1) ) +
        facet_wrap("ref_part", scales = "free") +
        labs(
          title = "Refelectance Intensity",
          subtitle = "Check for data scans over 65000. ChA-Incoming, ChB-Reflectance"
        )
    })

    # Tables below the plots
    # Selected file key file information
    output$key_selected <- renderTable({
      req(input$spu_file, input$key_file, keys())
      if (is.null(input$selectfile)) {
        return()
      }

      key_info <-
        key_spu_data() %>%
        dplyr::filter(spu_filename == input$selectfile) %>%
        mutate(Date = as.character(Date)) %>% 
        select(-Spectra)
      validate(need(nrow(key_info) > 0, "No Key information found."))
      return(key_info)
    })

    # Output the file's first 9 rows of metadata
    output$metatable <- renderTable({
      req(input$spu_file, input$key_file)
      if (is.null(input$selectfile)) {
        return()
      }
      if (!(input$selectfile %in% input$spu_file$name)) {
        return()
      }

      read.table(
        file = input$spu_file$datapath[input$spu_file$name == input$selectfile],
        col.names = "Instrument_Metadata_from_.spu_file",
        # first 9 rows of .spu file are metadata
        nrows = 9
      )
    })
  })
  ## ---- Files  Tab ---------------------------------------------------

  # Output tables ---------------------------------------------------------------
  
  ## Key file with .spu filenames
  output$key_table <- DT::renderDataTable({
    req(input$key_file)
    keys()
  })
  ## Combined Key and spu data
  output$all_data <- DT::renderDataTable({
    req(input$key_file)
    key_spu_data() %>% select(-Spectra)
  })
  ## Calculated indices 
  output$indices <- DT::renderDataTable({
    req(input$key_file)
    indices_data() #%>%
      #unnest(Indices)
  })

  ## ------Checks Tab -------------------------------------------------------------

  output$missing_data <- DT::renderDataTable({
    req(input$key_file)

    DT::datatable(
      missing_info(),
      caption = "Checks on missing information in data",
      options =
        list(
          searching = FALSE, paging = FALSE,
          language = list(
            zeroRecords = "No Missing Data"
          )
        )
    ) %>%
      DT::formatStyle(names(missing_info()), backgroundColor = styleEqual(NA, "red"))
  })
  output$treatment_odd_no <- DT::renderDataTable(
    treatment_odd_number(),
    caption = h3("Treatments with number of scans not equal 5 or 10")
  )
  output$checks_table <- DT::renderDataTable(checks_table())

  output$not_in_key <- DT::renderDataTable({
    if (nrow(files_not_in_key()) < 1) {
      return()
    }
    files_not_in_key()
  })
  output$maxedfiles <- DT::renderDataTable({
    datatable(maxed_files(), caption = h3("Maxed out spu files."))
  })

  ## --- Plot References Tab-------------------------------------------------------

  output$ref_plot1 <- renderPlotly({
    plotly::ggplotly(
      data_corrected_ref_integration() %>%
        filter(Site == input$choice_site, Treatment == "REF") %>%
        ggplot(mapping = aes(
          x = Wavelength, y = raw_reflectance,
          group_by = spu_filename
        )) +
        theme(legend.position = "left") +
        geom_line(aes(color = factor(FileNum))) +
        scale_color_discrete(name = "File Number") +
        facet_grid(Site ~ Treatment) +
        # Formatting
        labs(
          title = "White Reference Raw Scan",
          subtitle = "They all should be very similar.",
          x = "Wavelength (nm)",
          y = "Raw Reflectance"
        )
    )
  })

  output$ref_plot2 <- renderPlotly({
    plotly::ggplotly(
      data_corrected_ref_integration() %>%
        filter(Site == input$choice_site, Treatment == "REF") %>%
        ggplot(
          mapping =
            aes(
              x = Wavelength,
              y = correction_factor,
              group_by = spu_filename
            )
        ) +
        theme(legend.position = "left") +
        geom_line(aes(color = factor(Integration.ref))) +
        facet_grid(Site ~ Treatment) +
        # Formatting
        labs(
          title = "White Reference Correction Factor",
          subtitle = "Make sure correction factor isn't a data scan and between 0.5 and 1.5",
          x = "Wavelength (nm)",
          y = "Correction Factor"
        ) +
        scale_linetype_discrete(name = "Files") +
        scale_color_discrete(name = "Integration Time") +
        geom_hline(yintercept = 0.5, lty = 2) +
        geom_hline(yintercept = 1.5, lty = 2) +
        theme_light()
    )
  })
  # Plots Graphs Tab ------------------------------------------------------------

  #  TODO use plotly to plot instead of just converting plot
  output$plot_reflec <- renderPlotly({
    sub_df <- processed_spectra() %>%
      filter(
        Site %in% input$choice_site,
        Block %in% input$choice_block,
        Treatment %in% input$choice_treatment
      ) %>%
      group_by(Date, Site, Block, Treatment) %>%
      mutate(
        Replicate = as.character(Replicate),
        Block = as.factor(Block),
        Reflectance = raw_reflectance
      )
    if (nrow(sub_df) == 0) {
      return()
    }
    ### Plot
    plotly::ggplotly(
      ggplot(sub_df, mapping = aes(x = Wavelength, y = Reflectance)) +
        ggtitle(paste0("Scan date: ", sub_df$Date[1])) +
        geom_line(aes(color = Replicate, linetype = Block)) +
        facet_grid(Site ~ Treatment) +
        theme_light() +
        scale_color_viridis(discrete = TRUE, option = "D") +
        theme(legend.position = "left")
    )
  })
  output$plot_indices <- renderPlotly({
    req(input$choice_treatment, input$choice_block)
    sub_df <- indices_data() %>%
      filter(
        Site %in% input$choice_site,
        Block %in% input$choice_block,
        Treatment %in% input$choice_treatment
      ) %>%
      group_by(Date, Site, Block, Treatment) %>%
      mutate(Replicate = as.character(Replicate))
    if (nrow(sub_df) == 0) {
      return()
    }

    ### Plot
    plotly::ggplotly(
      ggplot(sub_df, mapping = aes(x = Block, y = .data[[index2plot]])) +
        geom_point(aes(color = Replicate)) +
        facet_grid(Site ~ Treatment) +
        theme_light() +
        scale_color_viridis(discrete = TRUE, option = "D") +
        theme(legend.position = "left")
    )
  })
  
  # Past Years Plot Tab-------------------------------------------

  output$plot_past_years <- renderPlotly({
    # Summarize the data by the index to plot. Note the .data[[ ]] is used to
    # get the data of the variable index2plot and !!()function is used to
    # get the name of the column
    sub_df <- past_data_all() %>%
      select(Site, Year, Date, DOY, Treatment, any_of(index2plot), collection_year) %>%
      group_by(collection_year, Site, Year, Date, DOY, Treatment) %>%
      filter(Site %in% input$choice_site, Treatment %in% input$choice_treatment) %>%
      summarize(
        sd = sd(.data[[index2plot]], na.rm = T),
        !!(index2plot) := mean(.data[[index2plot]], na.rm = T), .groups = "keep"
      )

    if (nrow(sub_df) == 0) {
      return()
    }
    ## Plot current and past data----
    plotly::ggplotly(
      ggplot(data = sub_df, aes(x = DOY, y = .data[[index2plot]], customdata = collection_year)) +
        geom_point(data = sub_df %>% filter(collection_year == "Past"), show.legend = FALSE) +
        geom_line(data = sub_df) +
        aes(color = factor(Year), linetype = factor(Year)) +
        geom_point(data = sub_df %>% filter(collection_year == "Current"), size = 4) +
        # scale_size_manual(values=c(4,2))+
        facet_grid(Site ~ Treatment) +
        # formatting
        theme_minimal() +
        scale_color_viridis(discrete = TRUE, option = "D")
    )
  })
  # Get click data
  output$click <- renderPlotly({
    d <- event_data("plotly_click")
    click_data <- past_data_all() %>%
      filter(Site %in% input$choice_site, Treatment %in% input$choice_treatment) %>%
      select(Site, Year, Date, DOY, Treatment, Block, Replicate, any_of(index2plot), collection_year) %>%
      filter(collection_year %in% d$customdata) %>%
      filter(DOY %in% d$x)
    if (nrow(click_data) == 0) {
      return()
    }

    ###  Plot mouse click data of index2plot by block----
    plotly::ggplotly(
      ggplot(click_data, mapping = aes(x = Block, y = .data[[index2plot]])) +
        ggtitle(paste0("Scan date: ", click_data$Date[1], " Day of Year: ", click_data$DOY[1])) +
        geom_point(aes(color = as.character(Replicate))) +
        labs(color = "Replicate") +
        facet_grid(Site ~ Treatment) +
        theme_light() +
        scale_color_viridis(discrete = TRUE, option = "D") +
        theme(legend.position = "left")
    )
  })

  # Save Files buttons ------------------------------------------------------------
  output$download_corrected_spectra_data <- downloadHandler(
    filename = function() {
      paste0(data_corrected_ref_integration()$Date[1], "_corrected_spectra.rds")
    },
    content = function(file) {
      write_rds(data_corrected_ref_integration(), file)
    }
  )
  output$download_key_spu_data <- downloadHandler(
    filename = function() {
      paste0(key_spu_data()$Date[1], "_combined.rds")
    },
    content = function(file) {
      write_rds(key_spu_data(), file)
    }
  )
  output$download_indices_data <- downloadHandler(
    filename = function() {
      paste0(indices_data()$Date[1], "_indices.rds")
    },
    content = function(file) {
      write_rds(indices_data(), file)
    }
  )
  output$update_all_data <- downloadHandler(
    filename = function() {
      paste0("indices_2014-current.rds")
    },
    content = function(file) {
      write_rds(past_data_all(), file)
    }
  )
  # ---- MainPanel tabset renderUI code-------------------------
  # generate the tabsets when input files are loaded.

  output$QAQC_tb <- renderUI({
    req(input$spu_file, input$key_file)
    ## Spectra Plot ----
    tabsetPanel(
      tabPanel("Spectra Plot", plotOutput("specplot"),
        div(hr(), p("Key infromation from key template file"),
          tableOutput("key_selected"),
          hr(),
          tableOutput("metatable"),
          style = "font-size:80%"
        ),
        id = "spec_plot"
      ),
      ## Checks ----
      tabPanel("Checks", h4("Checks on key and .spu files "),
        h5("Review the below tables for missing or incorrect information and for maxed out spectra."),
        div(DT::dataTableOutput("missing_data"),
          DT::dataTableOutput("not_in_key"),
          DT::dataTableOutput("checks_table"),
          DT::dataTableOutput("maxedfiles"),
          DT::dataTableOutput("treatment_odd_no"),
          style = "font-size:80%"
        ),
        id = "checks"
      ),
      ## References Plots ----
      tabPanel("References Plots", plotlyOutput("ref_plot1"),
        plotlyOutput("ref_plot2"),
        id = "ref_plot"
      ),
      ## Plot Graphs ----
      tabPanel(
        "Plot Graphs",
        plotlyOutput("plot_reflec"),
        plotlyOutput("plot_indices")
      ),
      ## Compare to Past Years ----
      tabPanel(
        "Compare to Past Years",
        plotlyOutput("plot_past_years"),
        h3("Click on a point to see data points by block"),
        plotlyOutput("click")
      ),
      ##Files ----
      tabPanel(
        "Files",
        tabsetPanel(
          tabPanel("Field Keys", DT::dataTableOutput("key_table")),
          tabPanel(
            "Combined keys and .spu data",
            p("Note: Table dosen't display the corrected spectra. The Save button will include it."),
            DT::dataTableOutput("all_data"),
            downloadButton("download_corrected_spectra_data", "Save corrected spectra as .rds")
          ),
          tabPanel(
            index2plot,
            DT::dataTableOutput("indices"),
            h3("Save indices button saves a .rds file with all the calculated indices."),
            h3("Update the all data file button will update the multiyear indices file with the current data."),
            downloadButton("download_indices_data", "Save indices as .rds"),
            downloadButton("update_all_data", "Update the all data file")
          )
        )
      ),
      id = "tabselected",
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
