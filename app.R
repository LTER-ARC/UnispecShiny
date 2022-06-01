#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)


# Define UI for application that ----
ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
    ),
    titlePanel("Unispec QAQC"),
    sidebarLayout(
      sidebarPanel(
        fileInput("file","Upload .spu files", multiple = TRUE,
                  accept = c(".spu")), 
        # Note input$file is a data frame (name, size, and temporary file path of the files uploaded)
        helpText("Default max. file size is 5MB"),
        fileInput("key_file","Upload Field Key Excel files", multiple = TRUE,
                  accept = c(".xlsx")),
        uiOutput("keyfiles_loaded"),
        hr(),
        uiOutput("selectfile"),
        uiOutput("treatments") #Select for treatment to filter the list of spu files
      ),
      mainPanel(
        uiOutput("tb")
      )
    )
)

# Define server logic 
server <- function(input, output, session) {
  # Helper functions and required libraries
  source("R/helper.R",local = TRUE)
  
  
##--------Sidebar additional UIs ---------------------------------------------
  
  # * Select input widget ----
  # with the list of file loaded input$file but filtered by files_trtment_choice
  output$selectfile <- renderUI({
   req(input$file, input$key_file, input$choice_input)
    select_input <- as_tibble(
      switch(input$choice_input,
             All = {input$file %>% select(spu_filename=name)},
             Spectra_maxed = {maxed_files()},
             {keys() %>% subset(Treatment %in% input$choice_input)%>%
                 select(spu_filename)}) %>%
        replace_na(list(data.spu_filename ="No spu file!!! Check Key file."))
    )#TO DO map site,blk to replaced NA
    if(nrow(select_input) ==0) {select_input <- as.null()}
    list(
      helpText("Select a file for spectra plot"),
      selectInput(
        "Select",
        "Select file",
        choices = unique(select_input), 
        selectize = F,
        size = 10,
        multiple = FALSE,
        selected = head(select_input,1) #Had to use head to get 1st element; leaving as NULL didn't work
      )
    )
  })
  # * Table to show key files uploaded ----
  output$keyfiles_loaded <- renderUI({
    req(input$key_file)
    div(
    renderTable(input$key_file%>% select(name)),
    style = "font-size:80%")
  })
  # * Select box for treatments and maxed out files.----
  output$treatments <- renderUI({
    req(input$key_file)
    selectInput("choice_input", "Treament",
                choices= c("All","Spectra_maxed",as.vector(keys() %>% select(Treatment))),
                selected = "All")
  }) 
  # Will need to work on using updateSelectInput since it may help with reactive diagram
  # observeEvent(input$choice_input,
  #              {updateSelectInput(session,
  #                                 inputId="Select",
  #                                 choices=files_trtment_choice(),
  #                                 selected = files_trtment_choice()[1])
  #              }
  # )
  # observeEvent(input$choice_input,{
  #   files_trtment_choice <- reactive  ({
  # # req(input$key_file)
  #  # if(is.null(input$choice_input)){return()}
  #   select_input <- as_tibble(
  #     switch(input$choice_input,
  #         All = {input$file %>% select(spu_filename=name)},
  #         Spectra_maxed = {maxed_files()},
  #         {keys() %>% subset(Treatment %in% input$choice_input)%>%
  #             select(spu_filename)}) %>%
  #        replace_na(list(data.spu_filename ="No spu file!!! Check Key file."))
  #     )#TO DO map site,blk to replaced NA
  #   return(select_input)
  #   })
  #   })
 #__________________________________________________________________________
#----Reactive outputs ----
  
  # * Read in the Excel key files ---------------------------------------
  #and output a combined table if more then one file.  Using shinyFeedback
  # to warn about incorrect file selection
  keys <- reactive({
    req(input$key_file)
    check_ext(input$key_file$name,"xlsx","Invalid file; Please select a .xlsx file")
    input$key_file$datapath %>% purrr::map(function(file_name)
      as_tibble(openxlsx::read.xlsx(file_name, sheet = 1, detectDates = T,cols = c(1:7)))) %>%
      reduce(rbind) %>%
      mutate(across(where(is.character), str_trim)) %>%
      left_join(
        input$file %>% 
          select(spu_filename = name) %>%
          mutate(Site = toupper(str_extract(spu_filename, "[A-Za-z]{3,}[0-9]{1,2}(?=_)"))) %>%
          mutate(FileNum = str_extract(spu_filename, "\\d{5}") %>% as.numeric()),
        by = c("Site", "FileNum")
      )
  })
  # * Read the .spu data ---------------------------------------------------
  spu_df <- reactive({
    req(input$file)
    # Read metadata text lines (9) from the spu files
    spu_filedata <- pmap_dfr(list(input$file$datapath,input$file$name), read_spu_file_metadata) %>%
    mutate(Site = toupper(str_extract(spu_filename, "[A-Za-z]{3,}[0-9]{1,2}(?=_)"))) %>%
    mutate(Spectra=map(input$file$datapath, function(x) read_spu_file_spectra(x))) %>% 
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
          inputId = "file",
          all(is.na(fd$spu_filename)),
          text = "No matchs between key and .spu files.")
    req(!all(is.na(fd$spu_filename)),cancelOutput = TRUE)
    return(fd)
  })
  
  # * Check for missing information --------------------------------------------
  missing_info <- reactive ({
    id <- showNotification("Checking for missing data...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    req(full_data())
    M_data <- full_data() %>% 
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
    return(M_data)
  })
  
 # * Table of checks ------------------------------------------------------
  checks_table<- reactive({
    spu_sites <-str_c(str_replace_na(unique( spu_df()$Site)), collapse = ",")
    key_sites <- str_c(str_replace_na(unique(keys()$Site)), collapse = ",")
    file_check <- paste("Does number of .spu files -", nrow(spu_df()), ", match what's entered in key file -", nrow(keys()),"?")
    site_check <- paste("Sites from names of spu files:", spu_sites, "should match sites in key file(s):", key_sites)
    maxedspectra <- paste("Maxed out spectra",maxed_files())
    txt_s <- datatable(tibble(Checks = c(site_check,file_check,maxedspectra)),options = list(dom = 't'))
    return(txt_s)  
  }) 
  
  # * Identify any files that aren't listed in the field key--------------------
  files_not_in_key <- reactive({
    data.frame(
     spuFiles_Not_in_Keys = anti_join(spu_df(), keys(),by = c("Date", "FileNum", "Site"))%>%
              pull(spu_filename)
    )
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
    return(refdata)
    })
  correction_factors <- reactive ({
    ref_data() %>% 
    summarize(correction_factor = mean(ChA/ChB), 
              ref_filenames = str_c(spu_filename,collapse = ", "))
    }) 
  # * Maxed out spectra --------------------------------------------------------
  maxed_files <-  reactive ({
   req(full_data()) #,all(!is.na(full_data()$spu_filename)))
   full_data() %>% 
    unnest(Spectra) %>% 
    filter(ChA > 65000 | ChB > 65000) %>% 
    select(spu_filename) %>%
    unique() 
  })
  # * Get integration times of ref data for each site --------------------------
  ref_int_values <- reactive({ 
  full_data() %>%
    filter((Treatment == "REF")) %>%
    select(Date,Site,Integration) %>%
    distinct()
  })
  # * Join reference scan integration time -----------------------------------
  # to the nearest data scan integration time
  data_corrected_ref_integration <- reactive ({
    inner_join(full_data(), ref_int_values(), by = c("Date","Site"),suffix = c(".data",".ref")) %>%
    group_by(Date,Site,FileNum) %>%
    mutate(Integration.ref = Integration.ref[which.min(abs(Integration.data-Integration.ref))]) %>%
    distinct(FileNum, .keep_all = TRUE) %>%
    unnest(Spectra) %>% 
    filter(Wavelength > 400, Wavelength < 1000) %>%
      filter(!Treatment %in% c("DARK", "REF")) %>%
    # # assign the "REF_Integration" value closest to data scan integration time
    rowwise() %>%
    group_by(Date, Site, Integration.ref) %>%
    # join data with ref data
    left_join(.,correction_factors() %>% rename(Integration.ref = Integration), 
              by = c("Date", "Site", "Wavelength", "Integration.ref")) %>%
    ungroup() %>% 
    # calculate reflectances
    mutate(raw_reflectance = ChB/ChA) %>% # the raw reflectance
    mutate(corrected_reflectance = raw_reflectance*correction_factor)
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
                band_defns = band_defns, instrument = "MODIS", indices = c("NDVI", "EVI", "EVI2")))
           )
    })
  
##----Plot Spectra Tab ----------------------------------------------------
  
  #   This reactivate output contains the raw spectra in an x-y line plot format
  observeEvent(input$Select,ignoreNULL = TRUE, {  # Only output plot if there is a file selected.
  output$specplot <- renderPlot({
    if(is.null(input$Select)) {return()}
      ## read data
    file_spu <- input$file$datapath[input$file$name==input$Select]
    df<-read.table(file=file_spu, 
               skip = 9,col.names = c("Wavelength", "ChB", "ChA"))   
      ## tidy
    df %>%
      mutate(Reflectance = ChB/ChA) %>% 
      filter(Wavelength > 400, Wavelength < 1000) %>% 
      tidyr::gather(key = Channel, value = Intensity, ChB, ChA) %>%
      tidyr::gather(key = ref_part, value = Reflectance_Intensity, Intensity, Reflectance) %>% 
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
    req(input$file,input$key_file)
    if(is.null(input$Select)){return()}
    input_file_num <- as.integer(str_extract(input$Select, "\\d{5}"))
    input_site = toupper(str_extract(input$Select, "[A-Za-z]{3,}[0-9]{1,2}(?=_)"))
    key_info <- keys() %>% dplyr::filter(FileNum == input_file_num & Site == input_site)
    validate(need(nrow(key_info) > 0, "No Key information found for this .spu file."))
    return(key_info)
  })
  
  # Output the file's first 9 rows of metadata
  output$metatable <- renderTable({ 
    req(input$file,input$key_file)
    if(is.null(input$Select)){return()}
    if(!(input$Select %in% input$file$name)) {return()}
    read.table(file=input$file$datapath[input$file$name==input$Select], 
               col.names = "Instrument_Metadata_from_.spu_file", # first 9 rows of .spu file are metadata
               nrows=9) 
  })
  }) #Closing of observeEvent
  
##---- Key file table  Tab ---------------------------------------------------

  # Output a table of all the information in the key file(s) 
  output$key_table <- DT::renderDataTable({
    req(input$key_file)
    keys()
    })
## ------Combined Key and spu data Tab
  output$all_data <- DT::renderDataTable({
    req(input$key_file)
    full_data() %>% select(-Spectra)
  })
  
## ------Checks Tab ------------------------------------------
  
  output$missing_data <- DT::renderDataTable({
    req(input$key_file)
    DT::datatable(
      missing_info(),
      caption= "Checks for missing information") %>%
      DT::formatStyle(names(missing_info()), backgroundColor = styleEqual(NA, "red"))
    })
  
  output$checks_table <- DT::renderDataTable(checks_table())
  
  output$not_in_key <- DT::renderDataTable({
    if(nrow(files_not_in_key())<1){return()}
    files_not_in_key()
    })
    
##--- Plot References Tab----------------------------------------
  
  output$ref_plot1 <- renderPlotly({
    plotly::ggplotly(
       ggplot(ref_data(),aes(x = Wavelength, y = correction_factor,
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
    ggplot(ref_data(),
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
# ---- MainPanel tabset renderUI code-------------------------
# generate the tabsets when the file is loaded. 
# Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    req(input$file, input$key_file)
    tabsetPanel(
      
        tabPanel("Spectra Plot", plotOutput("specplot"),
                 div( hr(),p("Key infromation from key template file"),  
                   tableOutput("key_selected"),
                   hr(),
                   tableOutput("metatable"),
                   style = "font-size:80%")),
        tabPanel("Field Keys",DT::dataTableOutput("key_table")
                 ),
        tabPanel("All data combined",DT::dataTableOutput("all_data")
                 ),
        tabPanel("Checks",h5("Checks on key and .spu files "),
                h6("Review the below tables for missing or incorrect information and for maxed out spectra."),
                div(DT::dataTableOutput("missing_data"),
                DT::dataTableOutput("not_in_key"),
                DT::dataTableOutput("checks_table"),
                style = "font-size:80%")
                ),
        tabPanel("References Plots", plotlyOutput("ref_plot1"),
                 plotlyOutput("ref_plot2")
                 ),
        tabPanel("Save Files", downloadButton("download_corrected_data", "Save corrected spectra as .rds"),
                 downloadButton("download_full_data", "Save uncorrected spectra as .rds"),
                 downloadButton("download_indices_data", "Save indices as .rds")
                 )
        )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
