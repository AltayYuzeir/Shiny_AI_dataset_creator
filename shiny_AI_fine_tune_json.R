library(jsonlite)
library(DT)
library(shiny)
library(uuid)
library(tools)
library(xlsx)

# from https://stackoverflow.com/questions/75236102/read-more-buttons-for-long-text-in-r-shiny-dt-datatables
js <- "
function(cell) {
  var $cell = $(cell);
  $cell.contents().wrapAll('<div class=\\\"content\\\"></div>');
  var $content = $cell.find('.content');
  $cell.append($('<button>Read more</button>'));
  $btn = $cell.find('button');
  $content.css({
    height: '50px',
    overflow: 'hidden'
  });
  $cell.data('isLess', true);
  $btn.click(function () {
    var isLess = $cell.data('isLess');
    $content.css('height', isLess ? 'auto' : '50px');
    $(this).text(isLess ? 'Read less' : 'Read more');
    $cell.data('isLess', !isLess);
  });
}
"

ui <- fluidPage(
  style = "background:#404040;color:white;",
  br(),
  fluidRow(
  column(width = 4, offset = 1,
         radioButtons("selectInput", "Select file type to upload or '+ New File' :",
                      choices = c("JSON", "CSV", "XLSX", "+ New File"), 
                      selected = "+ New File", inline = T,
                      )),
  column(width = 4, offset = 0,
  conditionalPanel(
  condition = "input.selectInput == 'JSON'",
  fileInput(
    inputId = "jsonInput",
    label = "Please provide JSON file:",
    multiple = FALSE,
    accept = ".json",
    width = NULL,
    buttonLabel = "Browse...",
    placeholder = "No file selected",
    capture = NULL
    )),
  conditionalPanel(
    condition = "input.selectInput == 'CSV'",
    fileInput(
      inputId = "csvInput",
      label = "Please provide CSV file:",
      multiple = FALSE,
      accept = c(".csv"),
      width = NULL,
      buttonLabel = "Browse...",
      placeholder = "No file selected",
      capture = NULL
    )),
  conditionalPanel(
    condition = "input.selectInput == 'XLSX'",
    fileInput(
      inputId = "xlsxInput",
      label = "Please provide XLSX file:",
      multiple = FALSE,
      accept = ".xlsx",
      width = NULL,
      buttonLabel = "Browse...",
      placeholder = "No file selected",
      capture = NULL
    ))
  ), 
  column(width = 3, 
         div(style="text-align:center; color: #80b3ff", tags$b("Copyright"),icon("copyright"),
             tags$b("2023-present"),br(), tags$b("Altay Yuzeir"),
             tags$a(href ="https://github.com/AltayYuzeir/Shiny_AI_dataset_creator",
                    tags$b(tags$span(style = "color: #80b3ff", icon("github"), "GitHub")),
                    target = "_blank")))
  ),
  hr(),
  fluidRow(
  column(width = 4, offset = 1, 
  textAreaInput("humanInput", "Human prompt:", height = "150px",
                width = "100%", resize = "vertical")),
  column(width = 4, offset = 1, 
  textAreaInput("gptInput", "GPT response:",  height = "150px",
                width = "100%", resize = "vertical"))
  ),
  tags$head(tags$style("#humanInput {background:#a6a6a6;color:black;font-size:15px}")),
  tags$head(tags$style("#gptInput {background:#a6a6a6;color:black;font-size:15px}")),
  br(),
  fluidRow(
  column(width = 3, offset = 1,
         textInput("sourceInput", "Source:")),
  column(width = 3, offset = 0,
         textInput("topicInput", "Topic:")),
  column(width = 3, offset = 0,
         textInput("system_promptInput", "System prompt:")),
  
  ),
  hr(),
  fluidRow(
    style = "text-align:center",
  column(width = 2, offset = 3,
         actionButton("submitEntry", "Sudmit to dataset", 
                      style = "background:#00cc99",
                      icon = icon("database"))),
  column(width = 2, offset = 2,
         actionButton("removeDuplicates", 
                      style = "background:#ff5050",
                      "Purge duplicates", icon = icon("trash-can")))
  ),
  
  hr(),
  # future update ?? a delete option for each entry
  # from https://community.rstudio.com/t/dt-remove-row/111741
  tags$head(
    tags$script(
      "function deleteRow(el){
				$('#datasetTable')
					.data('datatable')
					.row($(el).parents('tr'))
					.remove()
					.draw();
			};"
    )
  ),
  DT::dataTableOutput("datasetTable"),
  tags$head(tags$style("#datasetTable .dataTables_filter input {
          background:white;color:#4d4d4d;}")),
  tags$head(tags$style("#datasetTable {color: white;}")),
  hr(),
  fluidRow(
    style = "text-align:center",
    
    column(width = 4, offset = 0, 
           downloadButton("downloadJson", 
                          style = "background:#ff9900",
                          label = "Download JSON")),
    
    column(width = 4, offset = 0, 
           downloadButton("downloadCsv", 
                          style = "background:#3399ff",
                          label = "Download CSV")),
    
    column(width = 4, offset = 0, 
           downloadButton("downloadXlsx", 
                          style = "background:#107c41",
                          label = "Download XLSX"))
  ),
  br(),
  br()
  
)

server <- function(input, output, session) {
  
  observeEvent(input$submitEntry,{
    if(!isTruthy(input$humanInput) | !isTruthy(input$gptInput) | 
       !isTruthy(input$sourceInput) | !isTruthy(input$topicInput) |
       !isTruthy(input$system_promptInput)) 
      showNotification("Please fill out all fields", type = "error")
    else {
      record = c(input$humanInput, input$gptInput, input$sourceInput, 
                 input$topicInput, input$system_promptInput)
      data_global <<- rbind(data_global, record)
      colnames(data_global) <<- c("human", "gpt", "source", "topic", "system_prompt")
      
      output$datasetTable <- DT::renderDataTable({
        DT::datatable(data_global, rownames = FALSE, selection = "none",
                      options = list(
                        "columnDefs" = list(
                          list(
                            "targets" = c(0,1),
                            "createdCell" = JS(js)
                          ) ) ))%>% DT::formatStyle(columns = names(data), color="white")
      })
    }
  })
  
  observeEvent(input$removeDuplicates,{
    data_global <<- unique(data_global)
    output$datasetTable <- DT::renderDataTable({
      DT::datatable(data_global, rownames = FALSE, selection = "none",
                    options = list(
                      "columnDefs" = list(
                        list(
                          "targets" = c(0,1),
                          "createdCell" = JS(js)
                        ) ) ))%>% DT::formatStyle(columns = names(data), color="white")
    })
  })
  
  # Render initial Data table ----
  output$datasetTable <- DT::renderDataTable({
    if (input$selectInput == "JSON") {
      
      file <- input$jsonInput
      ext <- tools::file_ext(file$datapath)
      
      req(file)
      shiny::validate(need(ext == "json", "Please upload a JSON file"))
      
      data = jsonlite::fromJSON(file$datapath)
      
      for(i in 1:nrow(data))
      {
        data$human[i] = data$conversations[[i]][["value"]][1]
        data$gpt[i] = data$conversations[[i]][["value"]][2]
      }
      
      col_order <- c("human", "gpt", "source",
                     "topic", "system_prompt")
      data <- data[, col_order]
      
      #data$delete <- "<a onclick='deleteRow(this);'>delete</a>"
      ###### GLOBAL VARIABLE 
      data_global <<- data
      
        DT::datatable(data, rownames = FALSE, selection = "none",
                      #escape = FALSE, 
                      options = list(
                        "columnDefs" = list(
                          list(
                            "targets" = c(0,1),
                             className = 'dt-justify',
                            "createdCell" = JS(js)
                          ) ) )) %>% DT::formatStyle(columns = names(data), color="white")
      
      
    } else {
      if (input$selectInput == "CSV") {
        file <- input$csvInput
        ext <- tools::file_ext(file$datapath)
        
        req(file)
        shiny::validate(need(ext == "csv", "Please upload a CSV file"))
        
        data = read.csv(file$datapath)
        
        ###### GLOBAL VARIABLE 
        data_global <<- data
       
          DT::datatable(data, rownames = FALSE, selection = "none",
                        options = list(
                          "columnDefs" = list(
                            list(
                              "targets" = c(0,1),
                              "createdCell" = JS(js)
                            ) ) ))%>% DT::formatStyle(columns = names(data), color="white")
       
      } 
      else {
        if (input$selectInput == "XLSX"){
          file <- input$xlsxInput
          ext <- tools::file_ext(file$datapath)
          
          req(file)
          shiny::validate(need(ext == "xlsx", "Please upload an XLSX file"))
          
          data = read.xlsx(file$datapath, sheetIndex = 1)
          
          ###### GLOBAL VARIABLE 
          data_global <<- data
          
            DT::datatable(data, rownames = FALSE, selection = "none",
                          options = list(
                            "columnDefs" = list(
                              list(
                                "targets" = c(0,1),
                                "createdCell" = JS(js)
                              ) ) ))%>% DT::formatStyle(columns = names(data), color="white")
        } else {
          data = as.data.frame(matrix(nrow = 0, ncol = 5))
          colnames(data) = c("human", "gpt", "source", "topic", "system_prompt")
          data_global <<- data
          
            DT::datatable(data, rownames = FALSE, selection = "none",
                          options = list(
                            "columnDefs" = list(
                              list(
                                "targets" = c(0,1),
                                "createdCell" = JS(js)
                              ) ) ))%>% DT::formatStyle(columns = names(data), color="white")
         
        }
      }
    }
    
  }#, server = FALSE
  )
  
  
  #### Download JSON -----
  output$downloadJson <- downloadHandler(
       filename = function() {
        paste('dataset-', format(Sys.time(), "%d-%b-%Y_%Hh-%Mmin"), '.json', sep='')
       },
       content = function(con) {
        if(!exists("data_global") | nrow(data_global)==0) 
          showNotification("Dataset is Empty !", type = "error")
         else  
            {   # GLOBAL VARIABLE
                json = data_global
                json$id = uuid::UUIDgenerate(n = nrow(json))
                
                df = data.frame(matrix(nrow = 2, ncol = 2))
                colnames(df) = c("from", "value")
                df[,1] = c("human", "gpt")
                
                for(i in 1:nrow(json))
                {
                  
                  df[1,2] = json$human[i]  
                  df[2,2] = json$gpt[i]
                  json$conversations[[i]] = as.data.frame(df)
                }
                col_names = c("id","conversations", "source",
                              "topic", "system_prompt")
                json = json[,col_names]
                json_file = jsonlite::toJSON(json, dataframe = "rows")
                write(x = json_file, con)
            }
         })
  
  #### Download CSV -----
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste('dataset-', format(Sys.time(), "%d-%b-%Y_%Hh-%Mmin"), '.csv', sep='')
    },
    content = function(con) {
      if(!exists("data_global")) return()
      else  #write.csv(data_global, row.names = F)
      {   # GLOBAL VARIABLE
        json = data_global
        
        csv_file = json
        write.csv(x = csv_file, con, row.names = F)
      }
    })
  
  #### Download XLSX -----
  
  output$downloadXlsx <- downloadHandler(
    filename = function() {
      paste('dataset-', format(Sys.time(), "%d-%b-%Y_%Hh-%Mmin"), '.xlsx', sep='')
    },
    content = function(con) {
      if(!exists("data_global")) return()
      else  #write.csv(data_global, row.names = F)
      {   # GLOBAL VARIABLE
        json = data_global
        
        xlsx_file = json
        write.xlsx(x = xlsx_file, con, row.names = F)
      }
    })
  
}

shinyApp(ui, server)
