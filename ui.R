library(shiny)
library(readxl)
library(rmarkdown)
library(pagedown)
library(shinybusy)
library(shinyalert)
library(shinyjs)
library(httr)
library(stringr)
library(rvest)
library(emojifont)
library(rhandsontable)
library(dplyr)

addResourcePath("temp_pdf", tempdir())
file.copy("www/topo.png", tempdir(), overwrite = TRUE)
file.copy("www/rodape.png", tempdir(), overwrite = TRUE)

chrome_extra_args = function(default_args = c("--disable-gpu")) {
  args = default_args
  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    args = c(args,
             "--no-sandbox", # required because we are in a container
             "--disable-dev-shm-usage") # in case of low available memory
  }
  args
}

geturl = function(url,handle) {
  curl::curl_fetch_memory(url, handle = handle)$content
}

ui = fluidPage(
  useShinyjs(),
  tags$script(
    "
    Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
      navigator.clipboard.writeText(text).then(function() {
          console.log('Texto copiado com sucesso');
      }, function(err) {
          console.error('Erro ao copiar: ', err);
      });
    });
    "
  ),
  titlePanel("Produtos DOU"),
  tabsetPanel(
    id = "abas",
    tabPanel("Gerar relatório",
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Selecione o arquivo Excel com os links do DOU no formato do site (HTML), o arquivo deve conter 
                    apenas o link dos atos na primeira coluna do excel, não coloque nenhum outro dado."),
                 tags$br(),
                 tags$h4("Link de exemplo:"),
                 tags$h4("    https://www.in.gov.br/web/dou/-/decreto-n-12.433-de-14-de-abril-de-2025-624225151"),
                 fileInput("excel5", "Selecione um arquivo Excel", accept = c(".xlsx", ".xls")),
                 tags$h4("Aperte o botão abaixo para gerar o relatório"),
                 actionButton("gerar", "Gerar Relatório"), width = 3
               ),
               mainPanel(
                 uiOutput("pdf_viewer")
               )
             )),
    tabPanel("Gerar texto para Whatsapp",
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Selecione o arquivo Excel com os links do DOU no formato do site (HTML), o arquivo deve conter 
                    apenas o link dos atos na primeira coluna do excel, não coloque nenhum outro dado."),
                 tags$br(),
                 tags$h4("Link de exemplo:"),
                 tags$h4("    https://www.in.gov.br/web/dou/-/decreto-n-12.433-de-14-de-abril-de-2025-624225151"),
                 fileInput("excel", "Selecione um arquivo Excel", accept = c(".xlsx", ".xls")), width = 3),
               mainPanel(
                 h4("Texto formatado para envio do Whatsapp:"),
                 div(style = "max-height: 700px; overflow-y: auto; background-color: #f9f9f9; padding: 10px; border: 1px solid #ccc;",
                     id = "texto", verbatimTextOutput("output_text")),
                 uiOutput("btn_copy")
               )
             )
    ),
    tabPanel("Consulta pública",
             HTML("<br><p>Coloque as informações sobre as <strong>Consultas Públicas </strong> na tabela abaixo.</p>"),
             dateRangeInput(
               inputId = "data_intervalo",
               label = "Selecione o intervalo de datas das consultas:",
               start = Sys.Date() - 7,
               end = Sys.Date(),
               format = "dd/mm/yyyy",
               language = "pt-BR"
             ),
             h5(paste0(emoji(search_emoji("rotating_light")),"Dica: Utilize o Control + Shift + V ao invés do Control + V")),
             rHandsontableOutput('tabela'),
             actionButton("add", "Adicionar Linha"),
             h4("Texto formatado para envio do Whatsapp:"),
             div(style = "max-height: 300px; overflow-y: auto; background-color: #f9f9f9; padding: 10px; border: 1px solid #ccc;",
                 verbatimTextOutput("output_text2")),
             tags$br(),
             uiOutput("btn_copy2"),
             tags$br()
    )
  )
)