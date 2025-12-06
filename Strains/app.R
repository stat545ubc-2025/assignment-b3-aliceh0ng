library(shiny)
library(shinyjs)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(ggplot2)

# Global Functions - 96-well layout
well_layout <- expand.grid(Row = LETTERS[1:8], Col = 1:12) %>%
  mutate(Well = paste0(Row, Col)) %>%
  arrange(Row, Col)

initial_plate <- well_layout %>%
  select(Well) %>%
  mutate(Strain    = NA_character_, Condition = NA_character_)

# Define UI

ui <- fluidPage(

  useShinyjs(),

  tags$head(
    tags$style(HTML("
    /* 96-well plate */
    .plate-wrapper {overflow-x: auto;}

    .plate-grid {display: grid;
      grid-template-columns: repeat(12, 65px);  /* wider wells, 12 fixed columns */
      grid-auto-rows: 65px;
      grid-gap: 4px;
      justify-content: start;}

    .plate-well-btn {width: 65px;
      height: 65px;
      font-size: 10px;
      padding: 2px;
      white-space: pre-line;}

    /* Well design table: narrow & scrollable */
    .design-table-wrapper {max-height: 535px;
      overflow-y: auto;
      border: 1px solid #ddd;
      padding: 4px;}

    .design-table-wrapper table {width: 100%;
      table-layout: fixed;   /* squish columns */
      font-size: 11px;}

    .design-table-wrapper th,
    .design-table-wrapper td {padding: 2px 4px;
      white-space: nowrap;}   /* keep columns tight */
  "))),

  titlePanel("Plate Reader Data Viewer"),

  fluidRow(
    column(
      width = 12,
      wellPanel(
        h3("How to use this plate reader data visualizer:"),
        p("This application allows you to define a plate layout and visualize data from a platereader experiment over time. Common use cases include
          visualizing a growth curve (OD600), or flourescence (AU) over time. It is mandatory that you provide at least the time and OD600 data to
          use this application. The sheets containing this data should be titled exactly 'Time' and 'OD600'."),
        tags$ol(
          tags$li("Choose whether to visualize example data or upload your own file."),
          tags$li("If using your own file, upload a plate reader Excel file with separate sheets for Time, OD600, and any fluorescent channels (e.g. GFP, BFP, RFP)."),
          tags$li("Select which sheets to import using the checkboxes in the sidebar."),
          tags$li("Use the 96-well plate grid to assign each well to a strain, and an experimental conditions (e.g. osmolality)."),
          tags$li("Adjust the slider below to restrict the data to a given time window."),
          tags$li("Use the strain selector / Prev / Next buttons to switch between strains. Each plot shows all conditions for the current strain.")
        )
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(

      # User uses sample file
      h4("Visualize example data"),
      checkboxInput("use_demo",
        "Use sample data ('osmE_antisense_08022023.xlsx')",
        value = FALSE),

      # User uploads own file
      h4("Upload your own data"),
      fileInput("file",
        "Select plate reader Excel file",
        accept = c(".xlsx", ".xls")),
      uiOutput("sheet_selector"),     # Time/OD600 mandatory, GFP/BFP/RFP optional
      tags$hr(),
      uiOutput("channel_ui"),         # pick which measurement to plot
      sliderInput("max_time_index",
        "Select how many rows of data you'd like to visualize:",
        min = 1, max = 10, value = 10, step = 1),
      tags$hr(),
      h4("Plate layout: assign strain & condition"),
      textInput("current_strain", "Current strain label for clicked wells:", ""),
      textInput("current_condition", "Current condition label for clicked wells:", ""),
      actionButton("clear_assignments", "Clear all assignments")),

    mainPanel(
      # Row: plate (wide, left) + table (narrow, right)
      fluidRow(
        column(
          width = 9,    # <- wider plate area
          h4("96-well plate layout"),
          div(class = "plate-wrapper", uiOutput("plate_ui"))),
        column(width = 3,    # <- narrower table area
          h4("Well Assignments"),
          div(class = "design-table-wrapper", tableOutput("design_table")))),

      tags$hr(),

      h4("Time-course plot"),
      uiOutput("strain_nav_ui"),
      plotOutput("timeplot", height = "500px")
      )
  )
)

# Define server logic

server <- function(input, output, session) {

  # Disable/enable fileInput based on "use_demo"
  observeEvent(input$use_demo, {if (isTRUE(input$use_demo)) {shinyjs::disable("file")}
    else {shinyjs::enable("file")}})

  demo_path <- "osmE_antisense_08022023.xlsx"

  excel_path <- reactive({
    if (isTRUE(input$use_demo)) {
      validate(need(file.exists(demo_path), paste("Demo file not found at", demo_path)))
      demo_path}
    else {req(input$file)
      input$file$datapath}
  })

  required_sheets  <- c("Time", "OD600")
  optional_sheets  <- c("GFP", "BFP", "RFP")

  # Plate mapping (Well -> Strain, Condition)
  plate_map <- reactiveVal(initial_plate)

  # Sheet selection UI (Time & OD600 are mandatory)
  # sheets_available <- reactive({req(input$file)
  #   all_sheets <- excel_sheets(input$file$datapath)
  #   # ignore the first sheet
  #   if (length(all_sheets) > 1) all_sheets[-1] else character(0)})
  sheets_available <- reactive({
    path <- excel_path()
    all_sheets <- excel_sheets(path)
    if (length(all_sheets) > 1) all_sheets[-1] else character(0)
  })

  output$sheet_selector <- renderUI({
    sheets <- sheets_available()
    req(length(sheets) > 0)

    validate(need(all(required_sheets %in% sheets),
        paste0("Your file must contain sheets named: ",
               paste(required_sheets, collapse = ", "))))

    optional_present <- intersect(optional_sheets, sheets)

    tagList(strong("Required sheets (always loaded):"),
      tags$ul(lapply(required_sheets, tags$li)),
      if (length(optional_present) > 0) {
        checkboxGroupInput("optional_sheets",
                           "Optional fluorescent sheets to import:",
                           choices  = optional_present,
                           selected = optional_present)}
      else {helpText("No optional sheets (GFP/BFP/RFP) found in this file.")})
  })

  # Read sheets into list
  # data_list <- reactive({
  #   req(input$file)
  #   sheets <- sheets_available()
  #
  #   validate(need(all(required_sheets %in% sheets),
  #       paste0("Your file must contain sheets named: ",
  #         paste(required_sheets, collapse = ", "))))
  #
  #   opt_sel <- input$optional_sheets
  #   if (is.null(opt_sel)) opt_sel <- character(0)
  #
  #   sheets_to_read <- c(required_sheets, opt_sel)
  #
  #   map(sheets_to_read, ~ read_excel(input$file$datapath, sheet = .x)) |>
  #     set_names(sheets_to_read)
  # })
  data_list <- reactive({
    path   <- excel_path()
    sheets <- sheets_available()

    validate(need(all(required_sheets %in% sheets),
                  paste0("Your file must contain sheets named: ",
                         paste(required_sheets, collapse = ", "))))

    opt_sel <- input$optional_sheets
    if (is.null(opt_sel)) opt_sel <- character(0)

    sheets_to_read <- c(required_sheets, opt_sel)

    map(sheets_to_read, ~ read_excel(path, sheet = .x)) |>
      set_names(sheets_to_read)
  })

  # Build tidy time-course data
  tidy_data <- reactive({
    dl <- data_list()
    req("Time" %in% names(dl))

    time_df <- dl[["Time"]]

    # Time conversion
    time_vec <- time_df$Time

    if (inherits(time_vec, "POSIXt")) {
      # already date-time
      time_min <- as.numeric(difftime(time_vec, time_vec[1], units = "mins"))
    } else if (inherits(time_vec, "hms")) {
      # already an hms object
      time_min <- as.numeric(time_vec) / 60
    } else {
      # treat as string like "00:05:31"
      time_min <- as.numeric(lubridate::hms(as.character(time_vec))) / 60
    }

    time_df <- time_df %>%
      dplyr::mutate(
        Time_min   = time_min,
        Time_index = dplyr::row_number()
      )

    # Pivot measurements
    meas_sheets <- setdiff(names(dl), "Time")
    validate(need(length(meas_sheets) > 0, "No measurement sheets found."))

    all_meas <- purrr::map_dfr(meas_sheets, function(ch) {
      m <- dl[[ch]]
      stopifnot(nrow(m) == nrow(time_df))

      m2 <- m %>%
        dplyr::mutate(Time = time_df$Time,
          Time_min = time_df$Time_min,
          Time_index = time_df$Time_index)

      m2 %>%
        tidyr::pivot_longer(cols = setdiff(colnames(m2),
                            c("Time", "Time_min", "Time_index")),
                            names_to  = "Well",
                            values_to = "Value") %>%
        dplyr::mutate(Channel = ch, Value   = as.numeric(Value))
    })

    all_meas %>% tidyr::drop_na(Time_min, Value)
  })

  # Channel chooser UI
  output$channel_ui <- renderUI({td <- tidy_data()
    chans <- sort(unique(td$Channel))

    default <- if ("GFP" %in% chans) "GFP" else chans[1]

    selectInput("channel", "Measurement (channel) to plot:", choices  = chans, selected = default)
  })

  # Update time slider limits once we know how many rows
  observeEvent(tidy_data(), {td <- tidy_data()
    max_idx <- max(td$Time_index, na.rm = TRUE)
    updateSliderInput(session, "max_time_index",
      min   = 1, max   = max_idx, value = max_idx)
    })

  # Plate UI (96-well grid)
  output$plate_ui <- renderUI({
    pm <- plate_map()

    div(class = "plate-grid",
        lapply(well_layout$Well, function(well) {
        info   <- pm[pm$Well == well, ]
        strain <- info$Strain
        cond   <- info$Condition

        label <- if (!is.na(strain) || !is.na(cond)) {
          paste0(well, "\n",
                 ifelse(is.na(strain), "", strain),
                 ifelse(is.na(cond) | cond == "", "", paste0("\n", cond)))}
        else {well}

        highlight_style <- if (!is.na(strain) || (!is.na(cond) && cond != "")) {
          "background-color:#e0f3ff;"}
        else {""}

        actionButton(inputId = paste0("well_", well),
                     label = label,
                     class = "plate-well-btn",
                     style = highlight_style)
      }))
  })

  # Click handlers for each well
  # Assign strain and condition independently (don’t overwrite with NA)
  for (this_well in well_layout$Well) {
    local({w <- this_well
    observeEvent(input[[paste0("well_", w)]], {

        pm  <- plate_map()
        idx <- which(pm$Well == w)
        s   <- input$current_strain
        cnd <- input$current_condition

        # If BOTH fields are empty -> clear this well
        if ((is.null(s)  || s  == "") &&
            (is.null(cnd) || cnd == "")) {
          pm$Strain[idx]    <- NA_character_
          pm$Condition[idx] <- NA_character_
        } else {
          # Otherwise, update only the non-empty fields
          if (!is.null(s) && s != "") {
            pm$Strain[idx] <- s
          }
          if (!is.null(cnd) && cnd != "") {
            pm$Condition[idx] <- cnd
          }
        }

        plate_map(pm)
      }, ignoreInit = TRUE)
    })
  }

  # Clear assignments
  observeEvent(input$clear_assignments, {plate_map(initial_plate)})

  # Show design table (any well with strain/condition)
  output$design_table <- renderTable({plate_map() %>%
      filter(!is.na(Strain) | !is.na(Condition), Strain != "" | Condition != "")})

  # Plot time-course
  # output$timeplot <- renderPlot({
  #   td <- tidy_data()
  #   pm <- plate_map()
  #   req(input$channel)
  #
  #   design <- pm %>% dplyr::filter(!is.na(Strain), Strain != "")
  #
  #   validate(need(nrow(design) > 0, "Assign at least one well a strain (Condition optional)."))
  #
  #   strains <- design %>%
  #     dplyr::pull(Strain) %>%
  #     unique() %>%
  #     sort()
  #
  #   req(length(strains) > 0)
  #
  #   # which strain to show
  #   chosen_strain <- input$strain_select %||% strains[ current_strain_idx() ]
  #
  #   dat <- td %>%
  #     dplyr::inner_join(design, by = "Well") %>%
  #     dplyr::filter(Channel == input$channel,
  #                   Time_index <= input$max_time_index,
  #                   Strain == chosen_strain) %>%
  #     tidyr::drop_na(Time_min, Value)
  #
  #   validate(need(nrow(dat) > 0, "No finite data to plot for this strain / settings."))
  #
  #   ggplot(dat, aes(x = Time_min, y = Value,
  #                   colour = Condition, fill = Condition)) +
  #     stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.3) +
  #     stat_summary(geom = "line",   fun = mean, linewidth = 0.7) +
  #     labs(title  = paste("Strain", chosen_strain),
  #          x = "Time (min)",
  #          y = input$channel,
  #          colour = "Condition",
  #          fill = "Condition") +
  #     theme_light() +
  #     theme(panel.border = element_rect(color = "black"),
  #           strip.background = element_blank())
  # })

  # Plot time-course
  output$timeplot <- renderPlot({
    td <- tidy_data()
    pm <- plate_map()
    req(input$channel)

    design <- pm %>% dplyr::filter(!is.na(Strain), Strain != "")

    validate(need(nrow(design) > 0,
                  "Assign at least one well a strain (Condition optional)."))

    strains <- design %>%
      dplyr::pull(Strain) %>%
      unique() %>%
      sort()

    req(length(strains) > 0)

    # which strain to show
    chosen_strain <- input$strain_select %||% strains[current_strain_idx()]

    dat <- td %>%
      dplyr::inner_join(design, by = "Well") %>%
      dplyr::filter(
        Channel    == input$channel,
        Time_index <= input$max_time_index,
        Strain     == chosen_strain
      ) %>%
      tidyr::drop_na(Time_min, Value) %>%
      dplyr::mutate(
        Condition = as.character(Condition),
        Condition = trimws(Condition),
        Condition = ifelse(Condition == "", NA, Condition)
      ) %>%
      tidyr::drop_na(Condition) %>%
      dplyr::mutate(
        Condition = factor(Condition)
      )

    validate(need(nrow(dat) > 0,
                  "No finite data with non-empty conditions to plot for this strain / settings."))

    ggplot(dat, aes(x = Time_min, y = Value,
                    colour = Condition, fill = Condition)) +
      stat_summary(geom = "ribbon", fun.data = mean_se, alpha = 0.3) +
      stat_summary(geom = "line",   fun = mean, linewidth = 0.7) +
      labs(
        title  = paste("Strain", chosen_strain),
        x      = "Time (min)",
        y      = input$channel,
        colour = "Condition",
        fill   = "Condition"
      ) +
      theme_light() +
      theme(
        panel.border     = element_rect(color = "black"),
        strip.background = element_blank()
      )
  })


  # index for Prev / Next buttons
  current_strain_idx <- reactiveVal(1)

  # UI to choose / step through strains
  output$strain_nav_ui <- renderUI({pm <- plate_map()

  strains <- pm %>%
      dplyr::filter(!is.na(Strain), Strain != "") %>%
      dplyr::pull(Strain) %>%
      unique() %>%
      sort()

    if (length(strains) == 0) {return(helpText("Assign at least one strain to wells to enable plotting."))}

    # keep index in range
    idx <- current_strain_idx()
    if (idx > length(strains)) {idx <- length(strains)
    current_strain_idx(idx)}

    fluidRow(column(8,selectInput("strain_select",
                                  "Strain to display:",
                                  choices  = strains,
                                  selected = strains[idx])),
      column(4, div(style = "margin-top: 25px;",
                    actionButton("prev_strain", "Prev"),
                    actionButton("next_strain", "Next"))))
  })

  observeEvent(input$prev_strain, {pm <- plate_map()

    strains <- pm %>%
      dplyr::filter(!is.na(Strain), Strain != "") %>%
      dplyr::pull(Strain) %>%
      unique() %>%
      sort()
    if (length(strains) == 0) return()

    idx <- current_strain_idx()
    idx <- if (idx <= 1) length(strains) else idx - 1
    current_strain_idx(idx)
    updateSelectInput(session, "strain_select", selected = strains[idx])
  })

  observeEvent(input$next_strain, {pm <- plate_map()
    strains <- pm %>%
      dplyr::filter(!is.na(Strain), Strain != "") %>%
      dplyr::pull(Strain) %>%
      unique() %>%
      sort()
    if (length(strains) == 0) return()

    idx <- current_strain_idx()
    idx <- if (idx >= length(strains)) 1 else idx + 1
    current_strain_idx(idx)
    updateSelectInput(session, "strain_select", selected = strains[idx])
  })

  # keep index in sync if user changes dropdown directly
  observeEvent(input$strain_select, {pm <- plate_map()
    strains <- pm %>%
      dplyr::filter(!is.na(Strain), Strain != "") %>%
      dplyr::pull(Strain) %>%
      unique() %>%
      sort()
    if (length(strains) == 0) return()
    idx <- which(strains == input$strain_select)[1]
    if (!is.na(idx)) current_strain_idx(idx)
  })

}

# Run the application
shinyApp(ui, server)

