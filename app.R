library("shiny")
source("global.R")

ui <- fluidPage(
  
  tags$head(
    tags$link(href="styles.css", rel="stylesheet", type="text/css"),
    tags$script(src = "http://platform.twitter.com/widgets.js"),
    includeHTML(path = "www/google-analytics.html")
  ),
  
  tags$div(
    class = "title-app",
    tags$h1("Jogo da Memória"),
    tags$h4("Encontre a foto correspontente!")
  ),
  tags$br(),
  
  # verbatimTextOutput("test_res_show"),
  
  tags$div(
    style = "width: 650px; margin: auto;",
    time_UI("timer"),
    tags$br(),
    lapply(
      X = seq_len(n_hex * 2),
      FUN = function(x) {
        hex_UI(id = paste0("module", x))
      }
    )#,
    # verbatimTextOutput("test_res")
  )
  
)

server <- function(input, output, session) {
  
  start <- callModule(module = welcome, id = "Bem feito !")
  timer <- callModule(module = time, id = "timer", start = start)
  
  hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), n_hex)
  hex_png <- sample(rep(hex_png, 2))
  
  results_mods <- reactiveValues()
  results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)
  reset <- reactiveValues(x = NULL)
  block <- reactiveValues(x = NULL)
  
  lapply(
    X = seq_len(n_hex * 2),
    FUN = function(x) {
      results_mods[[paste0("module", x)]] <- callModule(
        module = hex,
        id = paste0("module", x),
        hex_logo = hex_png[x],
        reset = reset,
        block = block
      )
    }
  )
  
  observe({
    res_mod <- lapply(
      X = reactiveValuesToList(results_mods), 
      FUN = reactiveValuesToList
    )
    results_mods_parse$all <- res_mod
    results_mods_parse$show1 <- which_show(res_mod, 1)
    results_mods_parse$show2 <- which_show(res_mod, 2)
    results_mods_parse$show3 <- which_show(res_mod, 3)
  })
  
  observeEvent(results_mods_parse$show2, {
    hex1 <- which_hex(results_mods_parse$all, results_mods_parse$show1)
    hex2 <- which_hex(results_mods_parse$all, results_mods_parse$show2)
    if (identical(hex1, hex2)) {
      block$x <- hex1
      showNotification(
        ui = tags$div(
          style = "font-size: 160%; font-weight: bold;",
          sample(
            x = c("Muito bem!", "Bravo!", "Ótimo!", "Bom trabalho!",
                  "Incrível!", "Combina!", "Viva!"),
            size = 1
          )
        ), type = "message"
      )
    }
  })
  
  observeEvent(results_mods_parse$show3, {
    reset$x <- which_hex(
      results_mods_parse$all,
      c(results_mods_parse$show1, results_mods_parse$show2)
    )
    results_mods_parse$show1 <- NULL
    results_mods_parse$show2 <- NULL
    results_mods_parse$show1 <- results_mods_parse$show3
    results_mods_parse$show3 <- NULL
  })
  
  
  observe({
    allfound <- all_found(results_mods_parse$all)
    if (isTRUE(allfound)) {
      showModal(modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h2(
            tags$span(icon("trophy"), style = "color: #F7E32F;"),
            "Muito bem!",
            tags$span(icon("trophy"), style = "color: #F7E32F;")
          ),
          tags$h4("Você encontrou todas as fotos"),
          tags$h1(isolate(timer()), "segundos!"),
          tags$br(), tags$br(),
          tags$a(
            href = glue(shareurl, time = isolate(timer())),
            icon("twitter"), "Tweet sua pontuação ", 
            class = "btn btn-info btn-lg"
          ),
          tags$br(), tags$br(),
          
          tags$p("Este aplicativo é o nosso envio para o",
                 tags$a(href = "https://community.rstudio.com/t/shiny-contest-submission-hex-memory-game/25336", "Concurso Shiny !")),
          
          tags$br(), tags$br(),
          actionButton(
            inputId = "reload",
            label = "Jogar novamente !",
            style = "width: 100%;"
          )
        ),
        footer = NULL,
        easyClose = FALSE
      ))
    }
  })
  
  
  observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
}

shinyApp(ui = ui, server = server)
