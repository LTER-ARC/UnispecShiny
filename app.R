#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


## Required Packages -----

packages <- c("shiny","rstudioapi","tidyverse","lubridate",
              "openxlsx","plotly","data.table", "DT")
#source("R/helper.R",local = TRUE)
library(shinyjs)
library("viridis",warn.conflicts = FALSE)
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# ---------------------------------------------------------------------------------
# Before lunching the shiny app, read in data from past years and any newer 
#   index.rds files. Note: Past data have been cleaned and sites standardized
#  These data objects are scoped across all sessions

past_indices_data <- readRDS("data/indices_2014-2021.rds")

# Define UI for application ----

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
  navbarPage(
    "Arctic LTER Spectral Reflectance Data",
    tabPanel("1.Unispec QAQC ",
             sidebarLayout(
               sidebarPanel(
                 helpText("Select Unispec .spu files"),
                 fileInput(
                   "spu_file",           # Note input$spu_file is a data frame:
                   "Upload .spu files",  # name, size, and datapath (path and file name).
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
                selectInput(
                   "selectsite",
                   "Sites",
                    choices = NULL,
                    multiple = FALSE),
                selectInput(
                   "selectfile",
                   "Select File",
                   choices = NULL,
                   size = 10,
                   selectize = FALSE,
                   multiple = FALSE
                 ),
                 selectInput(
                   "treatments",
                   "Treatments",
                   choices = NULL, ) #Select for treatment to filter the list of spu files
               ),
               
               mainPanel(uiOutput("QAQC_tb"))
             )),
    tabPanel("2. NDVI Plots",
             sidebarLayout(
               sidebarPanel(
                 # Input: Checkboxes ----
                 checkboxGroupInput("choice_site", "Site",
                                    choices= NULL),
                 checkboxGroupInput("choice_treatment", "Treatment",
                                    choices= NULL),
                 checkboxGroupInput("choice_block", "Block",
                                    choices= NULL),
                 hr(),
               ),
               mainPanel(uiOutput("past_tb"))
             ))
  )
)

# Define server logic ----
server <- function(input, output, session) {
  # Helper functions and required libraries
  source("R/helper.R",local = TRUE)
  
  # Column names in temple file to use as a check for correct sheet
  header_check <- "FileNum" 
  
##--------Sidebar updates of SelectInputs ---------------------------------------------
  
  # Update Select input widgets ----
  # Choices are update depending on site selected and treatment
 
  #Update the select input based on the key file loaded
  observeEvent(keys(),{
    updateSelectInput(session, inputId = "selectsite",choices = unique(keys()$Site))
  })
  
  #Update Treatments based on selected Site
  site_subset <- reactive({
    req(input$selectsite)
    filter(keys(), Site %in% input$selectsite)
  })
  observeEvent(input$selectsite, {
     req(site_subset())
     choices <- c("All","Spectra_maxed",unique(site_subset()$Treatment))
     updateSelectInput(inputId = "treatments",choices = choices, selected ="All")
   })

 #update spu_filename based on selected site and treatments
 #Used head to get 1st element; leaving as NULL didn't work),
   observeEvent(c(input$selectsite, input$treatments), {
     choices <-  switch(input$treatments,
                        All = {site_subset() %>% select(spu_filename)},
                        Spectra_maxed = {maxed_files()$spu_filename},
                        {unique(filter(site_subset(), Treatment %in% input$treatments)$spu_filename)}
                        )
     updateSelectInput(inputId = "selectfile", choices = choices, selected = head(choices,1))
   })
  #__________________________________________________________________________
   
#----Reactive outputs ----
  
  # * Read in the Excel key files ---------------------------------------
  # and output a combined table if more then one file.  Using shinyFeedback
  # to warn about incorrect file selection
  keys <- reactive({
    req(input$key_file, input$spu_file,spu_df())
  # Check extension of file  
    check_ext("key_file",input$key_file$name,"xlsx","Invalid file; Please select a .xlsx file")
  # Check for required column names
    key_sheet <- find_sheet(input$key_file$datapath,header_check)
  # Read in key data and join in the spu_filename
     input$key_file$datapath %>% purrr::map(function(file_name)
      as_tibble(openxlsx::read.xlsx(file_name, sheet = key_sheet, detectDates = T,cols = c(1:7)))) %>%
      reduce(rbind) %>%
      mutate(across(where(is.character), str_trim)) %>%
      left_join(
        input$spu_file %>% 
          select(spu_filename = name) %>%
          mutate(Site = toupper(str_extract(spu_filename, "^[:alnum:]{3,}"))) %>%
          mutate(FileNum = str_extract(spu_filename, "\\d{5}") %>% as.numeric()),
        by = c("Site", "FileNum")
      ) %>%
       filter(Date %in% spu_df()$Date) %>%
       arrange(Site, spu_filename)
  })
  # * Read the .spu data ---------------------------------------------------
  spu_df <- reactive({
    req(input$spu_file)
    id <- showNotification("Reading spu files' data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    # Read metadata text lines (9) from the spu files
    spu_filedata <- pmap_dfr(list(input$spu_file$datapath,input$spu_file$name), read_spu_file_metadata) %>%
    mutate(Site = toupper(str_extract(spu_filename, "^[:alnum:]{3,}"))) %>%
    mutate(Spectra=map(input$spu_file$datapath, function(x) read_spu_file_spectra(x))) %>% 
    mutate(Date = date(DateTime)) %>%
    relocate(Date, .after = DateTime)
    return(spu_filedata)
  })
  
  # * Combine the key file info with the spu data -------------------------------
  full_data <- reactive({
    id <- showNotification("Combining data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    fd <- left_join(keys(), spu_df(),by = c("Date", "Site", "FileNum","spu_filename")) %>%
      arrange(DateTime) %>%
      mutate_at(.vars = vars(Site, Block, Treatment), .funs = factor)
    shinyFeedback::feedbackWarning(
          inputId = "spu_file",
          all(is.na(fd$spu_filename)),
          text = "No matchs between key and .spu files.")
    req(!all(is.na(fd$spu_filename)),cancelOutput = TRUE)
    return(fd)
  })
   
  # * Reference data  -----------------------------------------------------------
   
   ref_data <- reactive ({
     req(full_data())
     refdata<-full_data() %>%
       ## Get the spectra for reference files
       filter(Treatment == "REF") %>%
       ### Unnest Spectra & calculate correction factor
       unnest(Spectra) %>%
       filter(Wavelength > 400, Wavelength < 1000) %>% # remove edge wavelengths, instrument unreliable at extremes
       mutate(correction_factor = ChA / ChB) %>%
       ### Group repeated REF measurements based on your plot set-up (choose Block or NOT)
       group_by(Date, Site, Integration, Wavelength)
     shinyFeedback::feedbackWarning(
       inputId = "key_file",
       any(is.na(refdata$correction_factor)),
       text = "A reference file has a NA for correction factor.")
     req(!any(is.na(refdata$correction_factor)),cancelOutput = TRUE)
     
     return(refdata)
   })
   correction_factors <- reactive ({
     ref_data() %>% 
       summarize(correction_factor = mean(correction_factor), 
                 ref_filenames = str_c(spu_filename,collapse = ", "), .groups = "keep") %>% 
       rename(Integration.ref = Integration)
   })
   # * Get integration times of ref data for each site --------------------------
   ref_int_values <- reactive({ 
     full_data() %>%
       filter(Treatment == "REF") %>%
       group_by(Date, Site) %>% 
       select(Date,Site,Integration) %>%
       distinct()
   })
   # * Join reference scan integration time -----------------------------------
   # to the nearest data scan integration time
   data_corrected_ref_integration <- reactive ({
     req(full_data, input$key_file)
     full_join(full_data(), ref_int_values(), by = c("Date","Site"),suffix = c(".data",".ref")) %>%
       group_by(Date,Site,FileNum) %>%
       mutate(Integration.ref = ifelse(is.na(Integration.ref),Integration.ref, 
                                       Integration.ref[which.min(abs(Integration.data-Integration.ref))])) %>%
       distinct(FileNum, .keep_all = TRUE) %>%
       unnest(Spectra) %>% 
       filter(Wavelength > 400, Wavelength < 1000) %>%
       # # assign the "Integration.ref" value closest to data scan integration time
       rowwise() %>%
       group_by(Date, Site, Integration.ref) %>%
       # join data with ref data
       left_join(.,correction_factors(), 
                 by = c("Date", "Site", "Wavelength", "Integration.ref")) %>%
       ungroup() %>% 
       # calculate reflectances
       mutate(raw_reflectance = ChB/ChA) %>% # the raw reflectance
       mutate(corrected_reflectance = raw_reflectance*correction_factor)
   })
   # Get data ready for plots
   processed_spectra <- reactive({  
     data_corrected_ref_integration() %>% 
       mutate(across(where(is.factor), as.character)) %>%  # Convert factors so we can filter
       dplyr::filter(!Treatment %in% c("DARK", "REF")) %>%
       mutate(Block = as.numeric(str_extract(Block, "\\d"))) %>% # convert "B1", etc  to numeric
       standard_site_names()
   })
   # And unnest the indices 
   processed_indices <- reactive({
     req(index_data()) %>% 
       unnest(cols = c(Indices)) %>%
       mutate(across(where(is.factor), as.character)) %>%  # Convert factors so we can filter
       dplyr::filter(!Treatment %in% c("DARK", "REF")) %>%
       mutate(Block = as.numeric(str_extract(Block, "\\d"))) %>% # convert "B1", etc  to numeric
       dplyr::filter(Index %in% c("NDVI"))%>%
       rename(NDVI = Value) %>% 
       standard_site_names()
   })
   
   # * Calculate Indices ----------------------------------------------------- 
   index_data <- reactive ({
     id <- showNotification("Please wait. Calculating Indices...", duration = NULL, closeButton = FALSE)
     on.exit(removeNotification(id), add = TRUE)
     
     data_corrected_ref_integration() %>%
       select(-ChB, -ChA, -raw_reflectance, -correction_factor) %>%
       rename(Reflectance = corrected_reflectance) %>%
       nest(Spectra = c(Wavelength, Reflectance)) %>%
       # Calculate NDVI
       mutate(Indices = map(Spectra, function(x) calculate_indices(x, 
                                                                   band_defns = band_defns, instrument = "MODIS", indices = "NDVI"))
       )
   })
   # * Join past years indices data with indexes processed ----
   past_data_all <- reactive({
     req(processed_indices(),input$choice_site,input$choice_treatment)
     current_index_data_ndvi(index_data()) %>%
     full_join(past_indices_data,by = c("Year", "Date", "Site", "Treatment", "NDVI",
                                          "DOY","collection_year","Replicate","Block"))
   })
   
  # Checks on the data --------------------------------------------------------
   
  # * Check for missing information --------------------------------------------
  missing_info <- reactive ({
    req(full_data())
    full_data() %>% 
      filter(Site %in% input$selectsite) %>%  # Only show checks on selected site.
      filter(is.na(spu_filename) | 
               is.na(Site) |
               # Block with NA's should always be REFS or EXTRA 
               is.na(Block) & !str_detect(Treatment, "REF|DARK|VEG") | 
               # Check for replicates with NA's that aren't REF
               is.na(Replicate) & !str_detect(Treatment, "REF|DARK|THROWAWAY"),
             # don't care about EXTRA, VEG, or REF scans
             Treatment != "EXTRA|VEG|REF") %>%
      # reorder columns to see output better
      select(spu_filename, FileNum, Site, Block, Treatment, Replicate)
  })
  
 # * Table of checks ------------------------------------------------------
  checks_table<- reactive({
    spu_df_selected_site <-spu_df() %>% filter(Site %in% input$selectsite)
    spu_sites <- str_c(str_replace_na(unique(spu_df_selected_site$Site)), collapse = ",")
    key_sites <- str_c(str_replace_na(unique(keys()$Site)), collapse = ",")
    file_check <- paste("Does number of .spu files for selected site -", nrow(spu_df_selected_site),
                        ", match what's entered in key file -", 
                        nrow(filter(keys(), keys()$Site %in% input$selectsite)),"?")
    site_check <- paste("Site used in spu files' names,", spu_sites, ", should match site in key file:", input$selectsite)
    maxedspectra <- if(nrow(maxed_files()) == 0) #{
     paste("No maxed out specra.")
     maxed_dt <- datatable(maxed_files(), caption = "Maxed out spu files.")
    txt_s <- datatable(tibble(Checks = c(site_check,file_check)),
                       options = list(dom = 't'))
    return(txt_s)  
  }) 
  
  # * Identify any files that aren't listed in the field key--------------------
  files_not_in_key <- reactive({
    data.frame(
     spuFiles_Not_in_Keys = anti_join(spu_df(), keys(),
                                      by = c("Date", "FileNum", "Site"))%>%
      filter(Site %in% input$selectsite) %>%
      pull(spu_filename)
    )
  })

  # * Maxed out spectra --------------------------------------------------------
  maxed_files <-  reactive ({
   req(full_data())
   full_data() %>% 
    unnest(Spectra) %>% 
    filter(ChA > 65000 | ChB > 65000, Site %in% input$selectsite) %>% 
    group_by(spu_filename)%>%
    mutate(NDVI_OK = all(Wavelength < 600)) %>%
    select(spu_filename, NDVI_OK) %>%
    unique() %>%
    ungroup() %>% 
    {if(nrow(.) == 0) add_row(., spu_filename = "No maxed out specra.") else . }
  })
  
  #___________________________________________________________________________ 
  #* Select box updates.  When a file is loaded, get the sites and blocks ----
  #* and select the first ones
  observeEvent(index_data(),{
    
    selected_b <-unique(processed_spectra() %>% select(Block))$Block
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = selected_b[1])
    
    selected_s <-unique(processed_spectra() %>% select(Site))$Site
    updateCheckboxGroupInput(session, inputId = "choice_site", choices = selected_s,
                             selected = selected_s[1])
    
  })
  
  #  When a site is selected update the treatment and block choices 
  
  observeEvent(input$choice_site,{
    
    before_selected <-input$choice_treatment
    selected_t <-unique(processed_spectra() %>% 
                          filter(Site == input$choice_site) %>%
                          select(Treatment))$Treatment %>% factor() %>% levels()
    updateCheckboxGroupInput(session, inputId = "choice_treatment", choices = selected_t,
                             selected = before_selected)
    
    before_selected <-input$choice_block
    selected_b <-unique(processed_spectra() %>% 
                          filter(Site == input$choice_site) %>%
                          select(Block))$Block %>% factor() %>% levels()
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = before_selected)    
  })
  #  Create the reactive data for each plots based on the selected choices
  #
  sub_data_reflec <- reactive({
    req(processed_spectra(), input$choice_site, input$choice_block,
        input$choice_treatment )
    
    processed_spectra() %>%
      filter(Site %in% input$choice_site,Block %in% input$choice_block,Treatment %in% input$choice_treatment) %>%
      group_by(Date, Site, Block, Treatment) %>%
      mutate(Replicate = as.character(Replicate), Block = as.factor(Block),
             Reflectance = raw_reflectance)
  })
  
  sub_data_indices <- reactive({
    req(processed_indices(), input$choice_site, input$choice_block, input$choice_treatment )
    
    processed_indices() %>%
      filter(Site %in% input$choice_site,Block %in% input$choice_block,Treatment %in% input$choice_treatment) %>%
      group_by(Date, Site, Block, Treatment)%>%
      mutate(Replicate = as.character(Replicate))
  })
  
  sub_past_plot_data <- reactive({
    req(processed_indices(),input$choice_site,input$choice_treatment)
    past_data_all() %>%
    select(Site,Year,Date,DOY,Treatment,NDVI,collection_year) %>%
    group_by(Site, Year, DOY, Date, Treatment, collection_year) %>% 
    filter(Site %in% input$choice_site,Treatment %in% input$choice_treatment)%>%
    summarise(sd = sd(NDVI,na.rm = T),
              NDVI = mean(NDVI, na.rm = T), .groups = "keep")
  })
  
##----Plot Spectra Tab ----------------------------------------------------
  
  #   This reactivate output contains the raw spectra in an x-y line plot format
  observeEvent(input$selectfile,ignoreNULL = TRUE, {  # Only output plot if there is a file selected.
  output$specplot <- renderPlot({
    req(input$selectfile)
    if(is.null(input$selectfile) | !str_detect(input$selectfile,".spu")) {return()}

    data_corrected_ref_integration() %>%
      filter(spu_filename == input$selectfile) %>%  
      tidyr::gather(key = Channel, value = Intensity, ChB, ChA) %>%
      tidyr::gather(key = ref_part, value = Reflectance_Intensity, Intensity, raw_reflectance) %>% 
      ## viz
      ggplot(mapping = aes(x = Wavelength, y = Reflectance_Intensity)) +
      geom_line(aes(color=Channel)) +
      facet_wrap("ref_part", scales = "free")+
      labs(title = "Refelectance Intensity",
           subtitle = "Check for data scans over 65000"
      )
  })
  
 # Tables below the plots
 # Selected file key file information 
  output$key_selected <- renderTable({
    req(input$spu_file,input$key_file)
    if(is.null(input$selectfile)){return()}
    
    input_file_num <- as.integer(str_extract(input$selectfile, "\\d{5}"))
    input_site = toupper(str_extract(input$selectfile, "^[:alnum:]{3,}"))
    key_info <- keys() %>% dplyr::filter(FileNum == input_file_num & Site == input_site)
    validate(need(nrow(key_info) > 0, "No Key information found."))
    return(key_info)
  })
  
  # Output the file's first 9 rows of metadata
  output$metatable <- renderTable({ 
    req(input$spu_file,input$key_file)
    if(is.null(input$selectfile)){return()}
    if(!(input$selectfile %in% input$spu_file$name)) {return()}
    
    read.table(file=input$spu_file$datapath[input$spu_file$name==input$selectfile], 
               col.names = "Instrument_Metadata_from_.spu_file", # first 9 rows of .spu file are metadata
               nrows=9) 
  })
  }) #Closing of observeEvent
  
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ##---- Key file table  Tab ---------------------------------------------------

  # Output tables ----- 
  
  output$key_table <- DT::renderDataTable({
    req(input$key_file)
    keys()
    })
## Combined Key and spu data Tab
  output$all_data <- DT::renderDataTable({
    req(input$key_file)
    full_data() %>% select(-Spectra)
    })
  
  output$indices <- DT::renderDataTable({
    req(input$key_file)
    index_data() %>% 
      select(-Spectra,-ref_filenames, 
             -Integration.data, -Integration.ref) %>%
      unnest(Indices)
  })
  
## ------Checks Tab ------------------------------------------
  
  output$missing_data <- DT::renderDataTable({
    req(input$key_file)
    
    DT::datatable(
      missing_info(),
      caption = "Checks on missing information in data",
      options = 
        list(searching = FALSE,paging = FALSE,
             language = list(
               zeroRecords = "No Missing Data")              
        )) %>%
      DT::formatStyle(names(missing_info()), backgroundColor = styleEqual(NA, "red"))
    })
  
  output$checks_table <- DT::renderDataTable(checks_table())
  
  output$not_in_key <- DT::renderDataTable({
    if(nrow(files_not_in_key())<1){return()}
    files_not_in_key()
    })
  output$maxedfiles <- DT::renderDataTable({
    datatable(maxed_files(), caption = "Maxed out spu files")
  })
    
##--- Plot References Tab----------------------------------------
  
  output$ref_plot1 <- renderPlotly({
    plotly::ggplotly(
      ref_data() %>% filter(Site == input$selectsite) %>% 
       ggplot(mapping = aes(x = Wavelength, y = correction_factor,
                                       group_by = spu_filename)) + 
         theme(legend.position="left") + 
         geom_line(aes(color=factor(FileNum))) +
         scale_color_discrete(name ="File Number") +
         facet_grid(Site ~ Treatment) +
         # Formatting
         labs(title = "White Reference Raw Scan",
              subtitle = "They all should be very similar.",
              x = "Wavelength (nm)", 
              y = "Raw Reflectance")
     )
  })
  
  output$ref_plot2 <- renderPlotly({
    plotly::ggplotly( 
      ref_data() %>% filter(Site == input$selectsite) %>%  
    ggplot(mapping = 
         aes(x = Wavelength, 
             y = correction_factor,
             group_by = spu_filename)) + 
    theme(legend.position="left") + 
    geom_line(aes(color=factor(Integration))) + 
    facet_grid(Site ~ Treatment) +
    # Formatting
    labs(title = "White Reference Correction Factor",
         subtitle = "Make sure correction factor isn't a data scan and between 0.5 and 1.5",
         x = "Wavelength (nm)", 
         y = "Correction Factor") + 
    scale_linetype_discrete(name = "Files") + 
    scale_color_discrete(name = "Integration Time") + 
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_hline(yintercept = 1.5, lty = 2) +
    theme_light()
    )
  })
## ---- Block plots ----------------------------------------------------------
 
  #  TODO use plotly to plot instead of just converting plot
  output$plot_reflec <- renderPlotly({
    req(sub_data_reflec()) 
    ### Plot
    plotly::ggplotly(
      ggplot(sub_data_reflec(),mapping = aes(x = Wavelength, y = Reflectance )) + 
        ggtitle(paste0("Scan date: ",sub_data_reflec()$Date[1])) +
        geom_line(aes(color = Replicate, linetype = Block)) +
        facet_grid(Site ~ Treatment) +
        theme_light()+
        scale_color_viridis(discrete = TRUE, option = "D")+
        theme(legend.position = "left"))
  })
  output$plot_indices <- renderPlotly({
    req(sub_data_indices()) 
    ### Plot
    plotly::ggplotly(
      ggplot(sub_data_indices(),mapping = aes(x = Block, y = NDVI )) + 
        geom_point(aes(color = Replicate)) +
        facet_grid(Site ~ Treatment) +
        theme_light()+
        scale_color_viridis(discrete = TRUE, option = "D")+
        theme(legend.position = "left"))
  })
  #* Past Years Plot ----  
  output$plot_past_years <- renderPlotly({
    req(sub_past_plot_data())
    # Select all the blocks since it is an average of all the blocks. TODO hide blocks choices
    selected_b <-unique(processed_spectra() %>% select(Block))$Block
    updateCheckboxGroupInput(session, inputId = "choice_block", choices = selected_b,
                             selected = selected_b)
    plotly::ggplotly(
      ggplot(data =  sub_past_plot_data(), aes(x = DOY,y = NDVI, color = factor(Year), customdata = collection_year)) +
        geom_point(aes(alpha = 0.5, shape = factor(collection_year), size = factor(collection_year))) +
        scale_size_manual(values=c(4,2))+
        geom_line() +
        facet_grid(Site ~ Treatment) +
        #formatting
        theme_minimal() +
        scale_color_viridis(discrete = TRUE, option = "D")
    )
  })
  # output$click <- renderDataTable({
  #   d <- event_data("plotly_click")
  #   
  #   past_data_all() %>% filter(Site %in% input$choice_site,Treatment %in% input$choice_treatment)%>% 
  #     select(Site,Year,Date,DOY,Treatment,Replicate,NDVI,collection_year) %>%
  #     filter(collection_year %in% d$customdata) %>%
  #     filter(DOY %in% d$x)
  # })
  output$click <- renderPlotly({
    d <- event_data("plotly_click")
    click_data <- past_data_all() %>% filter(Site %in% input$choice_site,Treatment %in% input$choice_treatment)%>% 
          select(Site,Year,Date,DOY,Treatment,Block,Replicate,NDVI,collection_year) %>%
          filter(collection_year %in% d$customdata) %>%
          filter(DOY %in% d$x)
    if (nrow(click_data)== 0) return()
    
    ### Plot
    plotly::ggplotly(
      ggplot(click_data,mapping = aes(x = Block, y = NDVI )) + 
        ggtitle(paste0("Scan date: ",click_data$Date[1]," Day of Year: ",click_data$DOY[1])) +
        geom_point(aes(color = Replicate)) +
        facet_grid(Site ~ Treatment) +
        theme_light()+
        scale_color_viridis(discrete = TRUE, option = "D")+
        theme(legend.position = "left"))
  })
##--- Save Files Tab ----------------------------------------------------------
  output$corrected_data <-  DT::renderDataTable({
    
  })
  output$download_corrected_data <- downloadHandler(
    filename = function() {
      paste0(data_corrected_ref_integration()$Date[1], "_corrected.rds")
    },
    content = function(file) {
      write_rds(data_corrected_ref_integration(), file)
    }
  )
  output$download_full_data <- downloadHandler(
    filename = function() {
      paste0(full_data()$Date[1], "_combined.rds")
    },
    content = function(file) {
      write_rds(full_data(), file)
    }
  )
  output$download_indices_data <- downloadHandler(
    filename = function() {
      paste0(index_data()$Date[1], "_index.rds")
    },
    content = function(file) {
      write_rds(index_data(), file)
    }
  )
# ---- QAQC MainPanel tabset renderUI code-------------------------
# generate the tabsets when the file is loaded. 
# Hide selectinputs on tabs where they are not relative.
  observeEvent(input$tabselected, {
    if (input$tabselected =="Checks" | input$tabselected == "References Plots") {
      hideElement("selectfile")
      hideElement("treatments")
    } else {
      showElement("selectfile")
      showElement("treatments")
    }})
  output$QAQC_tb <- renderUI({
    req(input$spu_file, input$key_file)
    tabsetPanel(
      
        tabPanel("Spectra Plot", plotOutput("specplot"),
                 div( hr(),p("Key infromation from key template file"),  
                   tableOutput("key_selected"),
                   hr(),
                   tableOutput("metatable"),
                   style = "font-size:80%"),
                 id = "spec_plot"
                 ),
        tabPanel("Checks",h4("Checks on key and .spu files "),
                h5("Review the below tables for missing or incorrect information and for maxed out spectra."),
                div(DT::dataTableOutput("missing_data"),
                DT::dataTableOutput("not_in_key"),
                DT::dataTableOutput("checks_table"),
                DT::dataTableOutput("maxedfiles"),
                style = "font-size:80%"),
                id = "checks"
                ),
        tabPanel("References Plots", plotlyOutput("ref_plot1"),
                 plotlyOutput("ref_plot2"),
                 id = "ref_plot"
                 ),
        tabPanel("Files", 
                 tabsetPanel(
                   tabPanel("Field Keys",DT::dataTableOutput("key_table")
                   ),
                   tabPanel("All data combined",
                            DT::dataTableOutput("all_data"),
                            downloadButton("download_corrected_data", "Save corrected spectra as .rds")
                   ),
                   tabPanel("NDVI",
                            DT::dataTableOutput("indices"),
                            downloadButton("download_indices_data", "Save indices as .rds"))
                 )
                 # downloadButton("download_corrected_data", "Save corrected spectra as .rds"),
                 # downloadButton("download_full_data", "Save uncorrected spectra as .rds"),
                 # downloadButton("download_indices_data", "Save indices as .rds")
                 ),
        id ="tabselected"
        )
  })
  
  output$files_tb <- renderUI({
    req(input$spu_file, input$key_file)
    tabsetPanel(
      tabPanel("Field Keys",DT::dataTableOutput("key_table")
      ),
      tabPanel("All data combined",
               downloadButton("download_corrected_data", "Save corrected spectra as .rds"),
               DT::dataTableOutput("all_data")
      )
    )
  })
  #--- Plot with last years -------------------------------------------------
  output$past_tb <- renderUI({
    tabsetPanel(
      tabPanel("Plot Graphs",
               plotlyOutput("plot_reflec"),
               plotlyOutput("plot_indices")),
      tabPanel("Compare to Past Years",
               plotlyOutput("plot_past_years"),
               h3("Click on a point to see data points by block"),
               plotlyOutput("click"))
    )
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
