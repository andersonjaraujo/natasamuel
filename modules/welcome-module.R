

# Welcome module ----------------------------------------------------------


welcome_UI <- function(id) {
  ns <- NS(id)
  modalDialog(
    title = tags$h1(
      style = "text-align: center;",
      "Bem-vindo ao jogo da memoria !"
    ),
    tags$div(
      style = "text-align: center;",
      tags$p("Encontre todos fotos correspondentes o mais rapido possivel!"),
      tags$p("Clique em uma foto para retorna"),
      tags$p("Voce pode ver apenas duas fotos ao mesmo tempo"),
      tags$p("Quando estiver pronto, clique no botao abaixo para jogar!")
    ), 
    footer = actionButton(
      inputId = ns("play"),
      label = "Jogar!",
      icon = icon("play"),
      style = "width: 100%"
    )
  )
}

welcome <- function(input, output, session) {
  
  id <- gsub("-$", "", session$ns(""))
  showModal(ui = welcome_UI(id))
  
  observeEvent(input$play, {
    removeModal()
  })
  
  return(reactive(input$play))
}

