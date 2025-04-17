server = function(input, output, session) {
  
  tabela = reactiveVal(data.frame(Orgao = c(rep("",10)),
                                  "Sub-Órgão" = c(rep("",10)),
                                  "Sub-Órgão2" = c(rep("",10)),
                                  Titulo = c(rep("",10)),
                                  Assinatura = c(rep("",10)), 
                                  Ementa = c(rep("",10)),
                                  Tipos = c(rep("",10)),
                                  Urls = c(rep("",10)),
                                  Prazo = c(rep("",10)),
                                  Ponto_focal  = c(rep("",10)),
                                  Area_tematica = c(rep("",10)),
                                  Tema_resumo = c(rep("",10)),
                                  stringsAsFactors = FALSE))
  
  output$tabela = renderRHandsontable({
    rhandsontable(tabela(), rowHeaders = NULL, stretchH = "all") %>% 
      hot_table(autoColumnSize = FALSE) %>%
      hot_cols(colWidths = 150,
               renderer = htmlwidgets::JS(
                 "function(instance, td, row, col, prop, value, cellProperties) {
           Handsontable.renderers.TextRenderer.apply(this, arguments);
           td.style.whiteSpace = 'nowrap';  // Impede quebra de linha
         }"
               )
      )
  })
  
  observeEvent(input$add, {
    df = rbind(tabela(), data.frame(Orgao = "",
                                    "Sub-Órgão" = "",
                                    "Sub-Órgão2" = "",
                                    Titulo = "",
                                    Assinatura = "", 
                                    Ementa = "",
                                    Tipos = "",
                                    Urls = "",
                                    Prazo = "",
                                    Ponto_focal  = "",
                                    Area_tematica = "",
                                    Tema_resumo = "", stringsAsFactors = FALSE))
    tabela(df)
  })
  
  
  
  texto3 = reactive({
    if (nrow(hot_to_r(input$tabela) %>% 
             filter(Ponto_focal != "",
                    Area_tematica != "",
                    Titulo != "",
                    Tema_resumo != "",
                    Urls != "",
                    Orgao != "",
                    Prazo != "")) == 0) {
      saida_texto = ""
    }else{
      saida_texto = paste0(emoji(search_emoji("rotating_light"))," *MECANISMOS DE PARTICIPAÇÃO PÚBLICA – SEMANA DE ",format(input$data_intervalo[1],"%d/%m")," A ",format(input$data_intervalo[2],"%d/%m/%Y"),"*\n")
      saida_texto = paste0(saida_texto, "Abaixo, lista de consultas, audiências públicas e tomadas de subsídios abertas esta semana por órgãos do governo federal.  Para detalhes ou suporte, entre em contato:\n")
      saida_texto = paste0(saida_texto, "(61) 3317-9066 | suex-cni@cni.com.br \n\n")
      
      req(input$tabela)  # Garante que input$tabela não é NULL
      df <- hot_to_r(input$tabela) %>% 
        filter(Ponto_focal != "",
               Area_tematica != "",
               Titulo != "",
               Tema_resumo != "",
               Urls != "",
               Orgao != "",
               Prazo != "") %>% 
        arrange(Area_tematica)
      print(df)
      
      for(i in 1:nrow(df)){
        if(i == 1){
          
        }else{ if (df$Orgao[i] != df$Orgao[i-1]) {
          saida_texto = paste0(saida_texto,"\n\n")
        }}
        saida_texto = paste0(saida_texto, "*",toupper(trimws(df$Area_tematica[i])),"*\n")
        saida_texto = paste0(saida_texto, "Ponto focal: ",trimws(df$Ponto_focal[i]),"\n")
        saida_texto = paste0(saida_texto,"\n> ", df$Orgao[i])
        saida_texto = paste0(saida_texto,"\n",  str_to_sentence(trimws(df$Titulo[i])))
        saida_texto = paste0(saida_texto,"\n_Tema: ", str_to_sentence(trimws(df$Tema_resumo[i])), "_")
        saida_texto = paste0(saida_texto,"\nPrazo: ", trimws(df$Prazo[i]))
        saida_texto = paste0(saida_texto,"\nLink: ", trimws(df$Urls[i]),"\n")
      }
      print(saida_texto)
    }
    saida_texto
  })
  
  output$output_text2 = renderText({
    if (nchar(trimws(paste(texto3(), collapse = "\n"))) > 0) {
      saida = texto3()
    }else{
      saida = ""
    }
    saida
  })
  
  
  
  
  output$btn_copy2 = renderUI({
    if (nchar(trimws(paste(texto3(), collapse = "\n"))) > 0) {
      actionButton("copy5", "Copiar Texto")
    }
  })
  
  observeEvent(input$copy5, {
    session$sendCustomMessage("copyToClipboard", texto3())
    showNotification("Texto copiado para a área de transferência!", type = "message")
  })
  
  
  
  dados = reactiveVal(NULL)
  pdf_path = reactiveVal(NULL)
  
  observeEvent(input$gerar, {
    req(input$excel5)
    dados(readxl::read_excel(input$excel5$datapath))
    
    shinyalert(
      title = "Aguarde",
      text = "Gerando relatório...",
      type = "info",
      showConfirmButton = FALSE,
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE
    )
    
    # Define caminhos temporários
    tempReport = file.path(tempdir(), "Extrato_DOU.Rmd")
    file.copy("Extrato_DOU.Rmd", tempReport, overwrite = TRUE)
    
    
    tempHtml = file.path(tempdir(), "report.html")
    
    # Defina um nome para o arquivo PDF final (será copiado para a pasta www)
    pdf_filename = paste0("Extrato_DOU_", format(Sys.Date(), "%d_%m_%Y"), ".pdf")
    pdf_temp_path = file.path(tempdir(), pdf_filename)
    
    params = list(data = dados())
    
    rmarkdown::render(
      input = tempReport,
      output_file = tempHtml,
      params = params,
      envir = new.env(parent = globalenv())
    )
    Sys.sleep(5)  
    # Gera o PDF de forma assíncrona
    pagedown::chrome_print(
      input = tempHtml,
      output = pdf_temp_path,
      extra_args = chrome_extra_args(),
      verbose = 1,
      async = TRUE,
      wait = 5 
    )$then(
      onFulfilled = function(generated_pdf) {
        pdf_path(pdf_filename)
        
        closeAlert()
        showNotification("PDF gerado com sucesso!", type = "message")
        
        # Atualiza o pdf_viewer com o caminho do PDF
        output$pdf_viewer = renderUI({
          req(pdf_path())
          tags$iframe(style = "height:800px; width:100%;", src = file.path("temp_pdf", pdf_path()))
        })
      },
      onRejected = function(error) {
        showNotification(error$message, duration = NULL, type = "error")
      }
    )
    
  })
  
  output$downloadReport = downloadHandler(
    filename = function() {
      paste("Extrato_DOU_", format(Sys.Date(), "%d%m%Y"), ".pdf", sep = "")
    },
    content = function(file) {
      req(pdf_path())
      file.copy(from = file.path("www", pdf_path()), to = file)
    },
    contentType = "application/pdf",
    closeAlert()
  )
  
  df_data = reactive({
    req(input$excel)
    read_excel(input$excel$datapath, col_names = FALSE)
  })
  
  texto = reactive({
    req(df_data())
    data = df_data()
    shinyalert(
      title = "Aguarde",
      text = "Gerando texto...",
      type = "info",
      showConfirmButton = FALSE,
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE
    )
    secao = rep("",nrow(data[,1]))
    texto = rep("",nrow(data[,1]))
    orgao = rep("",nrow(data[,1]))
    titulo = rep("",nrow(data[,1]))
    url = rep("",nrow(data[,1]))
    
    for (i in 1:nrow(data[,1])) {
      url2 = gsub("http://","https://",as.character(data[i,1]))
      url[i] = url2
      h = curl::new_handle()
      curl::handle_setopt(h, ssl_verifypeer = 0)
      pag = read_html(geturl(url2,h))
      rm(h)
      #while(response$status_code != 200){
      #  response = GET(as.character(data[i,1]))
      #}
      secao[i] = str_sub(str_remove(html_text(html_nodes(pag,css=".secao-dou"))[1],"Seção: "),1,1)
      orgao[i] = html_text(html_nodes(pag,css=".orgao-dou-data"))
      titulo[i] = html_text(html_nodes(pag,css=".border-bottom-0"))
      if(length(html_text(html_nodes(pag,css=".ementa-dou, .ementa"))) > 0){
        texto[i] = html_text(html_nodes(pag,css=".ementa-dou, .ementa"))
      } else{
        texto[i] = substr(paste(html_text(html_nodes(pag,css=".texto-dou .dou-paragraph")),
                                " ",collapse = " "), 1, 300)
      }
    }
    closeAlert()
    data = data.frame(secao,orgao,titulo,texto,url) 
    
  })
  
  texto2 = reactive({
    saida_texto = "--- *Diário Oficial da União*---"
    saida_texto = paste0(saida_texto, "\n--- *Dia ", format(Sys.Date(),format = "%d/%m/%Y"),"* ---")
    num_secao = texto()$secao %>% 
      unique()
    for(i in 1:length(num_secao)){
      secao_selecionada = texto() %>% filter(secao == num_secao[i])
      saida_texto = paste0(saida_texto, "\n--- *Seção ",num_secao[i],"* ---\n")
      orgao_secao = secao_selecionada$orgao %>% unique()
      
      for(j in 1:length(orgao_secao)){
        orgao_selecionado = secao_selecionada %>% filter(orgao == orgao_secao[j])
        saida_texto = paste0(saida_texto,"\n> ", orgao_secao[j])
        for(k in 1:nrow(orgao_selecionado)){
          saida_texto = paste0(saida_texto,"\n- *",orgao_selecionado$titulo[k],"*: ",orgao_selecionado$texto[k]," ",orgao_selecionado$url[k],"\n\n")
        }
      }
    }
    saida_texto
  })
  
  output$output_text = renderText({
    texto2()
  })
  output$btn_copy = renderUI({
    if (nchar(trimws(paste(texto2(), collapse = "\n"))) > 0) {
      actionButton("copy", "Copiar Texto")
    }
  })
  
  observeEvent(input$copy, {
    session$sendCustomMessage("copyToClipboard", texto2())
    showNotification("Texto copiado para a área de transferência!", type = "message")
  })
  
  
}
