#' Shiny app to aggregate block-level population data from the decennial Census to a chosen grid definition
#'
#' @description
#' This function runs PopGrid_v2 via a shiny app UI, offering the same functionality as [PopGrid_v2::run_aggregation] but with a simple way to provide inputs.
#'
#' @return [PopGrid_v2::PopGridApp] does not return a value but instead saves three output files: a shapefile of the selected grid with population data, a CSV with gridded population for each of the race-ethnicity-gender-age demographic groups included in BenMAP, and a CSV with county weights for each of the eight race-ethnic groups for each grid cell.
#' @export
#'
#' @examples
#' PopGridApp()
#'

PopGridApp <- function(){
  ui <- fluidPage(
    titlePanel("PopGrid_v2"),
    sidebarLayout(

      # Sidebar input pane UI
      sidebarPanel(
        HTML("<h3>Inputs</h3>"),
        selectInput("mode",
                    "Select a mode",
                    choices = list("Shapefile" = "shapefile", "County" = "county", "Tract" = "tract"),
                    width = "100%"),
        selectInput("year",
                    "Select a year",
                    choices = 2020,#c(2010, 2020),
                    selected = 2020,
                    width = "100%"),
        #selectInput("method", "Select an aggregation method", choices = c("Area weighting", "Centroids"), selected = "Area weighting", width = "100%"),
        selectInput("states",
                    "States to include",
                    choices = c("All CONUS states", "Select a subset of states"),
                    selected = "All CONUS states",
                    width = "100%"),
        HTML("<p><strong>Provide the file path where you would like to save outputs</strong></p>"),
        shinyDirButton("output_path",
                       label = "Browse",
                       title = "Select output path",
                       multiple = FALSE),
        textOutput("selected_output_path"),
        HTML("<br>"),
        textInput("outfile_name",
                  "Provide a name for the output files (without file extension)",
                  placeholder = "Output file name",
                  width = "100%"),
        checkboxInput("overwrite",
                      "Overwrite existing output files of the same name in the output folder?",
                      value = FALSE,
                      width = "100%"),
        actionButton("run", "Run PopGrid_v2")
      ),

      # Main panel tabset is rendered conditionally by code in the server below based on inputs
      mainPanel(
        uiOutput("conditionalTabs")
      )
    )
    )

  server <- function(input, output, session){

    # This code renders the main panel tabset in the UI
    output$conditionalTabs <- renderUI({
      tabsetPanel(
        tabPanel(
          "Notes",
          HTML("<h4>Mode</h4>
                <p><strong>Shapefile</strong> mode allocates population to a user-provided shapefile. If selected, choose an input shapefile on the <em>Shapefile Selection</em> tab.</p>
                <p><strong>County</strong> and <strong>Tract</strong> modes instead use U.S. Census-defined county and tract shapes, respectively. These options do not require a shapefile.</p>
                <br>",
               "<h4>Year</h4>
                <p>Indicates the U.S. decennial Census year to use for population allocation. The only year currently supported is <strong>2020</strong>.</p>
                <br>",
               #"<h4>Aggregation Method</h4>
                #<p>The <strong>Area Weighting</strong> approach allocates block population data to grid cells based on area proportion. This is the recommended approach.</p>
                #<p>The <strong>Centroid</strong> approach allocates block population data to grid cells based on block centroids.</p>
                #<br>",
               "<h4>States</h4>
                <p>Allocation can be done for <strong>All CONUS states</strong>, which includes all of the contiguous U.S. If you only need population data for a subset of states, you can <strong>Select a subset of states</strong> on the <em>State Selection</em> tab instead.</p>
                <br>",
               "<h4>Processing Time</h4>
                <p>In shapefile mode, multi-state analyses may take many hours to run. Runtime depends primarily on download speed and the number of cores available on your computer. The app is designed to run in parallel automatically when multiple cores are available, and using a computer with more cores will decrease runtime.</p>
                <br>",
               "<h4>Census API</h4>
                <p>This app downloads data directly from the U.S. Census Bureau, which requires an API key. If you have not already done so, request a key from https://api.census.gov/data/key_signup.html and set the key in your .Renviron file by running census_api_key('my_key_here', install=TRUE, overwrite=TRUE) in the R file you used to launch this app. You only need to do this one time. Restart R or reload your environment with readRenviron('~/.Renviron') for this change to take effect.</p>")
        ),
        if (input$mode == "shapefile") {
          tabPanel(
            "Shapefile Selection",
            "Select input grid shapefile:",
            shinyFilesButton("input_file",
                             label = "Browse",
                             title = "Select input grid shapefile",
                             multiple = FALSE),
            textOutput("selected_input_file")
            )
          },
        if (input$states == "Select a subset of states") {
          tabPanel(
            "State Selection",
            selectInput("state_select",
                        "Select all states to include",
                        choices = c("Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware", "District of Columbia",
                                    "Florida","Georgia","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky",
                                    "Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                                    "Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
                                    "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania",
                                    "Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont",
                                    "Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
                        multiple = TRUE)
            )
          }
      )
    })

    shinyDirChoose(input,
                   "output_path",
                   roots = c(wd = ".", home = "~"))
    observeEvent(input$output_path, {
      outPath <- parseDirPath(roots = c(wd = getwd(), home = "~"), input$output_path)

      if (exists("outPath")){
        output$selected_output_path <- renderText({
          paste0("Selected folder: ", outPath)
        })
      }
    })

    # Inputs collected on the conditional tabs are stored in as reactive values so that values are retained when UI changes
    cond_tab_values <- reactiveValues(input_file = NULL, input_states = NULL)

    # Update these reactive values as needed
    observeEvent(input$input_file, {
      cond_tab_values$input_file <- parseFilePaths(roots = c(wd = getwd(), home = "~"), input$input_file)$datapath
    })
    observeEvent(input$state_select, {
      cond_tab_values$input_states <- input$state_select
    })

    shinyFileChoose(input, "input_file",
                    roots= c(wd = ".", home = "~"),
                    filetypes = c("", "shp"))
    observeEvent(input$input_file, {
      shinyFile <- parseFilePaths(roots = c(wd = getwd(), home = "~"), input$input_file)$datapath

      if (exists("shinyFile")){
        output$selected_input_file <- renderText({
          paste0("Selected file: ", shinyFile)
        })
        }
    })

    # All input values are stored as reactive values here for clarity
    run_inputs <- reactiveValues(mode = NULL,
                                 input_file = NULL,
                                 year = NULL,
                                 area_weight = NULL,
                                 input_states = NULL,
                                 out_path = NULL,
                                 outfile = NULL,
                                 overwrite = NULL
                                 )

    # When run button is clicked, update all input values to be compatible with run_aggregation
    observeEvent(input$run, {
      run_inputs$mode <- input$mode
      if (input$mode == "shapefile" & !is.null(cond_tab_values$input_file)) {
        run_inputs$grid_path <- cond_tab_values$input_file %>% dirname()
        run_inputs$grid_name <- cond_tab_values$input_file %>% basename() %>% str_remove("\\.[^.]*$")
      } else {
        run_inputs$grid_path <- NULL
        run_inputs$grid_name <- NULL
      }
      run_inputs$year <- input$year
      run_inputs$area_weight <- TRUE
      if (input$states == "Select a subset of states") {
        run_inputs$input_states <- cond_tab_values$input_states
      } else {
        run_inputs$input_states <- NULL
      }
      run_inputs$out_path <- parseDirPath(roots = c(wd = getwd(), home = "~"), input$output_path)
      run_inputs$outfile <- input$outfile_name
      run_inputs$overwrite <- input$overwrite

      # if required inputs are missing, create pop-ups with a descriptive error messages
      if ((is.character(run_inputs$out_path) & length(run_inputs$out_path) == 0) | is.null(run_inputs$out_path)) {
        showModal(
          modalDialog(
            title = "Error",
            HTML("In the <em>Inputs</em> pane, please provide a file path to save output files."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }

      if ((is.character(run_inputs$outfile) & run_inputs$outfile == "") | is.null(run_inputs$outfile)) {
        showModal(
          modalDialog(
            title = "Error",
            HTML("In the <em>Inputs</em> pane, please provide a name to use for output files."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }

      if (input$states == "Select a subset of states" & is.null(cond_tab_values$input_states)) {
        showModal(
          modalDialog(
            title = "Error",
            HTML("Please choose state(s) on the <em>Select States</em> tab or change States to include input to <strong>All CONUS states</strong>."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }

      if (input$mode == "shapefile" & (((is.character(cond_tab_values$input_file)) & length(cond_tab_values$input_file) == 0) | is.null(cond_tab_values$input_file))) {
        showModal(
          modalDialog(
            title = "Error",
            HTML("Please select input grid on the <em>Shapefile Selection</em> tab or change Mode input to <strong>County</strong> or <strong>Tract</strong>."),
            easyClose = TRUE,
            scrollable = FALSE
          )
        )
      }

      # Check that all required inputs are provided, and if so, call run_aggregation with inputs
      if (!((is.character(run_inputs$outfile) & run_inputs$outfile == "") | is.null(run_inputs$outfile)) &
          !((is.character(run_inputs$out_path) & length(run_inputs$out_path) == 0) | is.null(run_inputs$out_path)) &
          ((run_inputs$mode == "shapefile" & !is.null(run_inputs$grid_path) & !is.null(run_inputs$grid_name)) | run_inputs$mode != "shapefile") &
          (input$states == "Select a subset of states" & !is.null(run_inputs$input_states) | input$states != "Select a subset of states")
      ) {
        # message("running with inputs:")
        # message("mode = ", run_inputs$mode)
        # message("grid_path = ", run_inputs$grid_path)
        # message("grid_name = ", run_inputs$grid_name)
        # message("year = ", run_inputs$year)
        # message("area_weight = ",  run_inputs$area_weight)
        # message("states = ",  run_inputs$input_states)
        # message("out_path = ", run_inputs$out_path)
        # message("out_name = ", run_inputs$outfile)
        # message("overwrite = ",  run_inputs$overwrite)

        withProgress(message = "Aggregating population", value = 0, {
          run_aggregation(
            mode = isolate(run_inputs$mode),
            grid_path = isolate(run_inputs$grid_path),
            grid_name = isolate(run_inputs$grid_name),
            year = isolate(run_inputs$year),
            #area_weight = isolate(run_inputs$area_weight),
            states = isolate(run_inputs$input_states),
            output_path = isolate(run_inputs$out_path),
            output_name = isolate(run_inputs$outfile),
            overwrite = isolate(run_inputs$overwrite),
            shiny = TRUE
          )
        })
      }
    })
  }

  shinyApp(ui, server)
}
