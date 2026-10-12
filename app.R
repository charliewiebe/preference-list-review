library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
library(readr)

APP_TITLE    <- "Preference List Review"
APP_SUBTITLE <- "High School Follow Reports"

PREF_ORDER <- c(
  "George Ferguson",
  "Carter Hadnot",
  "Jack Leeper",
  "Cullen Scott",
  "Sebastian Wilson",
  "Chayce Formosa",
  "DJ Lowery",
  "Stephen Bobo",
  "Aiden Jones",
  "Trip Ostergard",
  "Gavin Williams",
  "Striker Pence",
  "Calum Andersen",
  "Brady Guzzie",
  "Wyatt McElree",
  "Trent Lutz",
  "Jake Ivey",
  "Cole Kuhn",
  "Chris Grasso",
  "Jaxon Climer",
  "Greyson Parker",
  "Justin Rodrigues",
  "Quincy Pratt",
  "Anderson Lambert",
  "Elyjah Mason",
  "Parker Lipp",
  "Banks Addison",
  "Ka'alekahi Kuhaulua",
  "Carson Kinnick",
  "Quinton Hill",
  "Shea Corona",
  "Kadyn Hines",
  "Aaron Minton",
  "Gavyn Dulaney",
  "Will Haggerty",
  "Gabe Fink",
  "Caleb Rutledge",
  "Enmanuel Acevedo",
  "Jett Little"
)

find_reports_csv <- function() {
  for (f in list.files(pattern = "\\.csv$", ignore.case = TRUE)) {
    cols <- names(readr::read_csv(f, n_max = 0, show_col_types = FALSE))
    if (all(c("player_name", "report") %in% cols)) return(f)
  }
  stop("No CSV with player_name and report columns in the app folder (", getwd(), ").")
}

build_player_df <- function() {
  rep <- readr::read_csv(find_reports_csv(), show_col_types = FALSE, locale = locale(encoding = "UTF-8")) |>
    rename(name = player_name) |>
    mutate(name = str_squish(name)) |>
    distinct(name, .keep_all = TRUE)
  if (!"school" %in% names(rep)) rep$school <- NA_character_
  missing <- setdiff(PREF_ORDER, rep$name)
  if (length(missing)) warning("No report found for: ", paste(missing, collapse = ", "))
  extra <- setdiff(rep$name, PREF_ORDER)                      # in the CSV but not the list: appended at the bottom
  if (length(extra)) warning("Not in the ordered list, added at the bottom: ", paste(extra, collapse = ", "))
  tibble(name = c(PREF_ORDER, extra)) |>
    mutate(original_rank = row_number()) |>
    left_join(rep, by = "name") |>
    mutate(school = coalesce(na_if(str_squish(school), ""), "Unknown"),
           report = coalesce(report, "No report on file.")) |>
    select(original_rank, name, school, report)
}

PLAYERS <- build_player_df()

# pair helpers
make_compared_key <- function(a, b) paste(sort(c(a, b)), collapse = "|||")

# Random adjacent pair from the current working list that has not been asked yet; NULL when none remain
next_random_pair <- function(working_list, compared_set) {
  n <- length(working_list)
  for (i in sample(seq_len(n - 1L))) {
    a <- working_list[i]; b <- working_list[i + 1L]
    if (!make_compared_key(a, b) %in% compared_set) return(c(a, b))
  }
  NULL
}

compute_rankings <- function(player_df, working_list) {
  tibble(name = working_list, new_rank = seq_along(working_list)) |>
    left_join(player_df |> select(name, original_rank, school), by = "name") |>
    mutate(rank_change = original_rank - new_rank) |>
    select(new_rank, name, school, original_rank, rank_change)
}

# theme
theme_args <- list(version = 5, bg = "#0d1117", fg = "#e6edf3", primary = "#ff8c42", secondary = "#30363d",
                   success = "#3fb950", info = "#ff8c42", warning = "#d29922", danger = "#f85149",
                   font_scale = 0.95, `border-radius` = "6px", `border-color` = "#30363d")
THEME <- do.call(bs_theme, c(theme_args, list(
  base_font    = font_google("IBM Plex Mono", local = FALSE),
  heading_font = font_google("Space Grotesk", wght = "700", local = FALSE))))

CSS <- "
  body { background:#0d1117; color:#e6edf3; min-height:100vh; }
  .app-header { padding:2.2rem 0 1.3rem; text-align:center; border-bottom:1px solid #21262d; margin-bottom:1.6rem; }
  .app-header h1 { font-family:'Space Grotesk',sans-serif; font-weight:700; font-size:2rem; letter-spacing:-0.02em; color:#e6edf3; margin:0; }
  .app-header .subtitle { color:#8b949e; font-size:0.85rem; margin-top:0.4rem; font-family:'IBM Plex Mono',monospace; }
  .landing-card { background:#161b22; border:1px solid #30363d; border-radius:10px; padding:2.2rem; max-width:520px; margin:0 auto; }
  .landing-card h2 { font-family:'Space Grotesk',sans-serif; font-size:1.2rem; margin-bottom:0.8rem; color:#ff8c42; }
  .landing-card p { color:#8b949e; font-size:0.85rem; }
  .progress-wrap { background:#21262d; border-radius:20px; height:6px; margin-bottom:0.5rem; overflow:hidden; }
  .progress-fill { background:linear-gradient(90deg,#ff8c42,#ffb347); height:100%; border-radius:20px; transition:width 0.4s ease; }
  .progress-label { font-size:0.78rem; color:#8b949e; font-family:'IBM Plex Mono',monospace; }
  .toolbar { display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:0.5rem; margin-bottom:1rem; }
  .toolbar .form-check { margin:0; } .toolbar .form-group { margin:0; }
  .toolbar .form-check-label { color:#8b949e; font-size:0.8rem; font-family:'IBM Plex Mono',monospace; }
  .key-hint { color:#6e7681; font-size:0.75rem; font-family:'IBM Plex Mono',monospace; }
  .card-col { display:flex; }
  .player-card { background:#161b22; border:2px solid #30363d; border-radius:10px; padding:1.6rem 1.4rem; width:100%;
                 display:flex; flex-direction:column; gap:0.9rem; transition:border-color 0.2s, box-shadow 0.2s; }
  .player-card:hover { border-color:#ff8c42; box-shadow:0 8px 30px rgba(255,140,66,0.14); }
  .card-top { text-align:center; }
  .player-name { font-family:'Space Grotesk',sans-serif; font-size:1.45rem; font-weight:700; color:#e6edf3; line-height:1.2; }
  .player-school { font-size:0.8rem; color:#8b949e; font-family:'IBM Plex Mono',monospace; letter-spacing:0.04em; text-transform:uppercase; margin-top:0.35rem; }
  .player-report { font-size:0.84rem; line-height:1.55; color:#c9d1d9; text-align:left; border-top:1px solid #21262d; padding-top:0.9rem;
                   font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Helvetica,Arial,sans-serif; }
  body.hide-reports .player-report { display:none; }
  .card-actions { margin-top:auto; display:flex; flex-direction:column; gap:0.6rem; align-items:center; }
  .vs-wrap { display:flex; align-items:center; justify-content:center; height:100%; min-height:120px; }
  .vs-badge { font-family:'Space Grotesk',sans-serif; font-size:1.1rem; font-weight:700; color:#ff8c42; background:#21262d; border-radius:50%;
              width:48px; height:48px; display:flex; align-items:center; justify-content:center; border:2px solid #30363d; }
  .btn-choose { background:#c75000; border:1px solid #ff8c42; color:#fff; font-family:'IBM Plex Mono',monospace; font-size:0.88rem;
                padding:0.55rem 1.4rem; border-radius:6px; width:100%; cursor:pointer; transition:background 0.15s; letter-spacing:0.02em; }
  .btn-choose:hover { background:#ff8c42; color:#fff; } .btn-choose:disabled { opacity:0.5; }
  .btn-start { background:#c75000; border:1px solid #ff8c42; color:#fff; font-family:'IBM Plex Mono',monospace; font-size:0.9rem;
               padding:0.65rem 2rem; border-radius:6px; width:100%; margin-top:1rem; }
  .btn-start:hover { background:#ff8c42; color:#fff; }
  .btn-quit { background:transparent; border:1px solid #30363d; color:#8b949e; font-family:'IBM Plex Mono',monospace; font-size:0.78rem;
              padding:0.4rem 1.2rem; border-radius:6px; }
  .btn-quit:hover { border-color:#8b949e; color:#e6edf3; }
  .results-section { background:#161b22; border:1px solid #30363d; border-radius:10px; padding:1.75rem; margin-bottom:1.5rem; }
  .results-section h3 { font-family:'Space Grotesk',sans-serif; font-size:1.05rem; color:#ff8c42; margin-bottom:1rem;
                        border-bottom:1px solid #21262d; padding-bottom:0.6rem; }
  .dataTables_wrapper, .dataTables_wrapper .dataTables_filter input, .dataTables_wrapper .dataTables_length select { color:#e6edf3 !important; background:transparent !important; }
  table.dataTable tbody tr { background:#0d1117 !important; color:#e6edf3 !important; }
  table.dataTable tbody tr:hover { background:#161b22 !important; }
  table.dataTable thead th { background:#21262d !important; color:#8b949e !important; font-family:'IBM Plex Mono',monospace !important;
                             font-size:0.75rem !important; text-transform:uppercase; letter-spacing:0.05em; border-bottom:1px solid #30363d !important; }
  .dataTables_paginate .paginate_button.current { background:#c75000 !important; border-color:#ff8c42 !important; color:#fff !important; border-radius:4px; }
  .dataTables_info { color:#8b949e !important; font-size:0.78rem; }
  .dataTables_paginate .paginate_button { color:#8b949e !important; }
  .session-info { font-family:'IBM Plex Mono',monospace; font-size:0.8rem; color:#8b949e; text-align:center; margin-bottom:0.5rem; }
  .btn-dl { background:#21262d; border:1px solid #30363d; color:#ff8c42; font-family:'IBM Plex Mono',monospace; font-size:0.85rem;
            padding:0.55rem 1.5rem; border-radius:6px; text-decoration:none; display:inline-block; }
  .btn-dl:hover { background:#30363d; border-color:#ff8c42; color:#ffb347; }
"


JS <- "
  window.plLast = 0;
  document.addEventListener('click', function(e) {
    var b = e.target.closest && e.target.closest('.btn-choose'); if (!b) return;
    if (Date.now() - window.plLast < 450) { e.stopImmediatePropagation(); e.preventDefault(); return; }
    window.plLast = Date.now(); setTimeout(function() { $('.btn-choose').prop('disabled', true); }, 0);
  }, true);
  $(document).on('change', '#show_reports', function() { $('body').toggleClass('hide-reports', !this.checked); });
  $(document).on('keydown', function(e) {
    if ($(e.target).is('input[type=text], textarea, select')) return;
    var id = e.key === 'ArrowLeft' ? 'btn_left' : (e.key === 'ArrowRight' ? 'btn_right' : null);
    var el = id && document.getElementById(id);
    if (el && !el.disabled) { e.preventDefault(); el.click(); }
  });
"

# ui
ui <- page_fluid(
  theme = THEME,
  tags$head(tags$title(APP_TITLE), tags$style(HTML(CSS)), tags$script(HTML(JS))),
  div(class = "app-header",
      h1(APP_TITLE),
      div(class = "subtitle", APP_SUBTITLE)),
  div(style = "max-width:1100px; margin:0 auto; padding:0 1rem 3rem;",
      uiOutput("screen_landing"),
      conditionalPanel("output.on_comparison",
        div(class = "toolbar",
            input_switch("show_reports", "Show scouting reports", value = TRUE),
            span(class = "key-hint", "← / → keys pick left / right"))),
      uiOutput("screen_comparison"),
      uiOutput("screen_results"))
)

# server
server <- function(input, output, session) {

  rv <- reactiveValues(screen = "landing", working_list = NULL, compared_set = character(0), mode = "pool",
                       bubble_name = NULL, bubble_down_name = NULL, current_pair = NULL, left_is_first = TRUE,
                       n_compared = 0L, rankings = NULL)

  output$on_comparison <- reactive(rv$screen == "comparison")
  outputOptions(output, "on_comparison", suspendWhenHidden = FALSE)

  set_matchup <- function(name_a, name_b) {
    rv$current_pair  <- c(which(PLAYERS$name == name_a), which(PLAYERS$name == name_b))
    rv$left_is_first <- sample(c(TRUE, FALSE), 1L)
  }
  do_swap <- function(pos_a, pos_b) { wl <- rv$working_list; wl[c(pos_a, pos_b)] <- wl[c(pos_b, pos_a)]; rv$working_list <- wl }
  record_compared <- function(a, b) rv$compared_set <- c(rv$compared_set, make_compared_key(a, b))

  advance <- function(winner_name, loser_name) {
    record_compared(winner_name, loser_name)
    wl <- rv$working_list

    if (rv$mode == "bubble_up") {
      bubble <- rv$bubble_name; pos <- which(wl == bubble)
      if (winner_name == bubble) {
        do_swap(pos - 1L, pos); new_pos <- pos - 1L
        if (new_pos == 1L) start_bubble_down() else set_matchup(bubble, rv$working_list[new_pos - 1L])
      } else start_bubble_down()
      return(invisible())
    }

    if (rv$mode == "bubble_down") {
      bubble <- rv$bubble_name; pos <- which(wl == bubble); n <- length(wl)
      if (loser_name == bubble) {
        do_swap(pos, pos + 1L); new_pos <- pos + 1L
        if (new_pos == n) finish_bubble() else set_matchup(bubble, rv$working_list[new_pos + 1L])
      } else finish_bubble()
      return(invisible())
    }

    winner_pos <- which(wl == winner_name); loser_pos <- which(wl == loser_name)
    if (winner_pos > loser_pos) {                               
      do_swap(loser_pos, winner_pos)
      rv$bubble_down_name <- loser_name
      start_bubble_up(winner_name)
    } else pick_next_pair()                                      
  }

  start_bubble_up <- function(name) {
    rv$mode <- "bubble_up"; rv$bubble_name <- name; pos <- which(rv$working_list == name)
    if (pos == 1L) start_bubble_down() else set_matchup(name, rv$working_list[pos - 1L])
  }
  start_bubble_down <- function() {
    name <- rv$bubble_down_name; rv$mode <- "bubble_down"; rv$bubble_name <- name
    pos <- which(rv$working_list == name)
    if (pos == length(rv$working_list)) finish_bubble() else set_matchup(name, rv$working_list[pos + 1L])
  }
  finish_bubble <- function() { rv$mode <- "pool"; rv$bubble_name <- NULL; rv$bubble_down_name <- NULL; pick_next_pair() }
  pick_next_pair <- function() {
    pair <- next_random_pair(rv$working_list, rv$compared_set)
    if (is.null(pair)) finalise_results() else set_matchup(pair[1], pair[2])
  }
  finalise_results <- function() { rv$rankings <- compute_rankings(PLAYERS, rv$working_list); rv$screen <- "results" }

  start_review <- function() {
    rv$working_list <- PLAYERS$name; rv$compared_set <- character(0); rv$mode <- "pool"
    rv$bubble_name <- NULL; rv$bubble_down_name <- NULL; rv$current_pair <- NULL; rv$left_is_first <- TRUE
    rv$n_compared <- 0L; rv$rankings <- NULL; rv$screen <- "comparison"
    pick_next_pair()
  }

  # landing
  output$screen_landing <- renderUI({
    req(rv$screen == "landing")
    div(class = "landing-card",
        p("Battle Royale-style player comparison. ",
          "Complete the review, selecting your preferred players, to update the preference list.;"),
        actionButton("btn_start", "Start Review", class = "btn btn-start"))
  })
  observeEvent(input$btn_start, start_review())

  # ── Comparison ──
  output$screen_comparison <- renderUI({
    req(rv$screen == "comparison", !is.null(rv$current_pair))
    pair <- rv$current_pair; wl <- rv$working_list
    adj_keys <- vapply(seq_len(length(wl) - 1L), function(i) make_compared_key(wl[i], wl[i + 1L]), character(1))
    total <- length(adj_keys); asked <- sum(adj_keys %in% rv$compared_set)
    pct <- min(100L, round(100 * asked / max(1L, total)))
    mode_label <- switch(rv$mode, pool = "Head-to-head",
                         bubble_up = paste0("↑ Placing ", rv$bubble_name), bubble_down = paste0("↓ Placing ", rv$bubble_name))
    sides <- if (rv$left_is_first) pair else rev(pair)

    make_card <- function(i, btn_id) {
      p <- PLAYERS[i, ]
      div(class = "player-card",
          div(class = "card-top",
              div(class = "player-name", p$name),
              div(class = "player-school", p$school)),
          div(class = "player-report", p$report),
          div(class = "card-actions", actionButton(btn_id, "Prefer This Player", class = "btn-choose")))
    }

    tagList(
      div(class = "mb-4",
          div(class = "session-info", paste0(rv$n_compared, " comparisons  ·  ", mode_label)),
          div(class = "progress-wrap", div(class = "progress-fill", style = paste0("width:", pct, "%"))),
          div(class = "progress-label text-center mt-1", paste0(asked, " / ", total, " matchups"))),
      fluidRow(
        column(5, class = "card-col", make_card(sides[1], "btn_left")),
        column(2, div(class = "vs-wrap", div(class = "vs-badge", "VS"))),
        column(5, class = "card-col", make_card(sides[2], "btn_right"))),
      div(class = "text-center mt-4", actionButton("btn_quit", "Finish Early & See Results", class = "btn btn-quit")))
  })

  resolve_choice <- function(picked_left) {
    req(rv$screen == "comparison", !is.null(rv$current_pair))
    first <- PLAYERS$name[rv$current_pair[1]]; second <- PLAYERS$name[rv$current_pair[2]]
    left_name <- if (rv$left_is_first) first else second; right_name <- if (rv$left_is_first) second else first
    rv$n_compared <- rv$n_compared + 1L
    if (picked_left) advance(left_name, right_name) else advance(right_name, left_name)
  }
  observeEvent(input$btn_left,  resolve_choice(TRUE))
  observeEvent(input$btn_right, resolve_choice(FALSE))
  observeEvent(input$btn_quit,  { req(rv$screen == "comparison"); finalise_results() })

  # ── Results ──
  output$screen_results <- renderUI({
    req(rv$screen == "results")
    tagList(
      div(class = "results-section", h3("✓ Review Complete"),
          div(class = "session-info", paste0(rv$n_compared, " comparisons"))),
      div(class = "results-section", h3("Updated Preference List"), DTOutput("tbl_rankings")),
      div(class = "results-section", h3("Biggest Movers"), DTOutput("tbl_movers")),
      div(class = "mt-3", downloadButton("btn_download", "Download Updated List", class = "btn-dl")),
      div(class = "mt-3", actionButton("btn_restart", "Start New Review", class = "btn btn-secondary btn-sm")))
  })

  output$tbl_rankings <- renderDT({
    req(rv$screen == "results", !is.null(rv$rankings))
    rv$rankings |>
      arrange(new_rank) |>
      transmute(Rank = new_rank,
                Player = htmltools::htmlEscape(name), School = htmltools::htmlEscape(school), Orig = original_rank,
                Change = case_when(rank_change > 0L ~ paste0('<span style="color:#3fb950">▲ ', rank_change, "</span>"),
                                   rank_change < 0L ~ paste0('<span style="color:#f85149">▼ ', abs(rank_change), "</span>"),
                                   TRUE ~ "–")) |>
      datatable(rownames = FALSE, escape = FALSE, options = list(pageLength = 50, dom = "tp", ordering = FALSE), class = "compact hover")
  })

  output$tbl_movers <- renderDT({
    req(rv$screen == "results", !is.null(rv$rankings))
    movers <- rv$rankings |>
      filter(rank_change != 0L) |>
      arrange(desc(abs(rank_change))) |>
      transmute(Player = name, `Original Rank` = original_rank, `New Rank` = new_rank, `Rank Change` = rank_change)
    if (nrow(movers) == 0L)
      return(datatable(tibble(Message = "No rank changes — original list confirmed."), rownames = FALSE, options = list(dom = "t"), class = "compact"))
    datatable(movers, rownames = FALSE, options = list(pageLength = 15, dom = "tp", ordering = FALSE), class = "compact hover") |>
      formatStyle("Rank Change", color = styleInterval(c(-0.5, 0.5), c("#f85149", "#8b949e", "#3fb950")), fontWeight = "bold")
  })

  output$btn_download <- downloadHandler(
    filename = function() "follow_reports_pref_list_updated.csv",
    content = function(file) {
      req(!is.null(rv$rankings))
      rv$rankings |>
        arrange(new_rank) |>
        transmute(`New Rank` = new_rank, Player = name, School = school, `Original Rank` = original_rank, `Rank Change` = rank_change) |>
        readr::write_csv(file)
    })

  observeEvent(input$btn_restart, { rv$screen <- "landing"; rv$rankings <- NULL; rv$current_pair <- NULL })
}

shinyApp(ui, server)