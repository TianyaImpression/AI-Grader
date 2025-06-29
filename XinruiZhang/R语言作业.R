# 加载必要的库
library(shiny)
library(httr)
library(jsonlite)
library(readtext)
library(DT)
library(shinybusy)
library(pdftools)
library(officer)
library(readxl)
library(tidyverse)
library(purrr)
library(tesseract) # 用于OCR识别图像中的文本

# 定义UI界面
ui <- fluidPage(
  add_busy_spinner(spin = "fading-circle"),
  titlePanel("多格式作业AI评分系统"),
  sidebarLayout(
    sidebarPanel(
      # 数字人助手放在侧边栏顶部
      div(
        style = "text-align: center; margin-bottom: 20px;",
        h4("AI助手"),
        div(class = "digital-human-container",
            div(class = "digital-human",  
                # 头部 - 小女孩
                div(class = "head",
                    div(class = "hair"),
                    div(class = "bow"),
                    div(class = "eye left"),
                    div(class = "eye right"),
                    div(class = "blush left"),
                    div(class = "blush right"),
                    div(class = "mouth")
                ),
                # 身体 - 连衣裙
                div(class = "body",
                    div(class = "dress-detail"),
                    div(class = "arm left", div(class = "hand")),
                    div(class = "arm right", div(class = "hand"))
                ),
                # 腿部
                div(class = "legs",
                    div(class = "leg left", div(class = "foot")),
                    div(class = "leg right", div(class = "foot"))
                )
            )
        ),
        p("智能评分助手，随时为您服务", style = "margin-top: 10px; font-style: italic;")
      ),
      
      radioButtons("mode", "批改模式",
                   choices = c("单个文件批改" = "single", 
                               "多个文件批改" = "batch"),
                   selected = "single"),
      
      conditionalPanel(
        condition = "input.mode == 'single'",
        fileInput("single_file", "上传单个文件", 
                  multiple = FALSE,
                  accept = c(
                    # 文本类
                    ".txt", ".csv", ".json", ".xml", ".yaml", ".yml", ".md", ".log", ".ini", ".cfg", ".conf",
                    # 文档类
                    ".pdf", ".doc", ".docx", ".ppt", ".pptx", ".xls", ".xlsx", ".odt", ".rtf",
                    # 图像类
                    ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".svg", ".webp",
                    # 代码文件
                    ".R", ".rmd", ".qmd", ".py", ".js", ".html", ".css", ".java", ".c", ".cpp", ".php", ".sh", ".sql"
                  ))
      ),
      
      conditionalPanel(
        condition = "input.mode == 'batch'",
        fileInput("batch_files", "上传多个文件", 
                  multiple = TRUE,
                  accept = c(
                    # 文本类
                    ".txt", ".csv", ".json", ".xml", ".yaml", ".yml", ".md", ".log", ".ini", ".cfg", ".conf",
                    # 文档类
                    ".pdf", ".doc", ".docx", ".ppt", ".pptx", ".xls", ".xlsx", ".odt", ".rtf",  
                    # 图像类
                    ".jpg", ".jpeg", ".png", ".gif", ".bmp", ".tiff", ".svg", ".webp",
                    # 代码文件
                    ".R", ".rmd", ".qmd", ".py", ".js", ".html", ".css", ".java", ".c", ".cpp", ".php", ".sh", ".sql"
                  ))
      ),
      
      h4("通用作业评分标准"),
      # 内容质量标准
      wellPanel(
        h5("1. 内容质量 (40分)"),
        sliderInput("content_completeness", "内容完整性", 
                    min = 0, max = 15, value = 15),
        textInput("content_completeness_desc", "内容完整性描述", 
                  value = "评估是否完整回答了所有问题/任务要求"),
        sliderInput("correctness", "内容正确性", 
                    min = 0, max = 15, value = 15),
        textInput("correctness_desc", "内容正确性描述", 
                  value = "评估内容/结果的准确性和可靠性"),
        sliderInput("depth_analysis", "分析深度", 
                    min = 0, max = 10, value = 10),
        textInput("depth_analysis_desc", "分析深度描述", 
                  value = "评估分析的深度和洞察力")
      ),
      
      # 技术实现标准
      wellPanel(
        h5("2. 技术实现 (30分)"),
        sliderInput("method_appropriateness", "方法适当性", 
                    min = 0, max = 10, value = 10),
        textInput("method_appropriateness_desc", "方法适当性描述", 
                  value = "评估所用方法/技术的适用性"),
        sliderInput("technical_execution", "技术执行", 
                    min = 0, max = 10, value = 10),
        textInput("technical_execution_desc", "技术执行描述", 
                  value = "评估技术实现的质量（代码、公式等）"),
        sliderInput("problem_solving", "问题解决", 
                    min = 0, max = 10, value = 10),  
        textInput("problem_solving_desc", "问题解决描述", 
                  value = "评估问题解决方法的有效性")
      ),
      
      # 创新思维标准
      wellPanel(
        h5("3. 创新思维 (20分)"),
        sliderInput("originality_approach", "方法原创性", 
                    min = 0, max = 10, value = 10),
        textInput("originality_approach_desc", "方法原创性描述", 
                  value = "评估解决方案/方法的独特性和创新性"),
        sliderInput("critical_thinking", "批判性思维", 
                    min = 0, max = 10, value = 10),
        textInput("critical_thinking_desc", "批判性思维描述", 
                  value = "评估分析的深度和批判性思考")
      ),
      
      # 文档规范标准
      wellPanel(
        h5("4. 文档规范 (10分)"),
        sliderInput("formatting_consistency", "格式一致性", 
                    min = 0, max = 5, value = 5),
        textInput("formatting_consistency_desc", "格式一致性描述", 
                  value = "评估文件格式的规范性和一致性"),
        sliderInput("presentation_clarity", "呈现清晰度", 
                    min = 0, max = 5, value = 5),
        textInput("presentation_clarity_desc", "呈现清晰度描述", 
                  value = "评估内容组织的逻辑性和可视化效果")
      ),
      
      sliderInput("strictness", "评分严格度", 
                  min = 1, max = 5, value = 3),
      actionButton("grade", "开始评分", class = "btn-primary"),
      hr(),
      h4("API设置"),
      selectInput("api", "选择AI模型", 
                  choices = c("DeepSeek", "Kimi", "GPT-4")),
      passwordInput("api_key", "API密钥"),
      hr(),
      h4("支持的文件格式"),
      tags$ul(
        tags$li("文本类: .txt, .csv, .json, .xml, .yaml, .yml, .md, .log, .ini, .cfg, .conf"),
        tags$li("文档类: .pdf, .doc, .docx, .ppt, .pptx, .xls, .xlsx, .odt, .rtf"),
        tags$li("图像类: .jpg, .jpeg, .png, .gif, .bmp, .tiff, .svg, .webp"),
        tags$li("代码文件: .R, .rmd, .qmd, .py, .js, .html, .css, .java, .c, .cpp, .php, .sh, .sql")
      ),
      
      # 数字人助手的CSS样式 - 小女孩 (修复了所有转义字符问题)
      tags$style(HTML('
        /* 小女孩数字人样式 */
        .digital-human-container {
          display: flex;
          justify-content: center;
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 10px;
          margin-bottom: 15px;
          box-shadow: 0 2px 5px rgba(0,0,0,0.1);
        }
        
        .digital-human {
          position: relative;
          width: 150px;
          height: 225px;
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        
        /* 头部样式 - 小女孩 */
        .head {
          position: relative;
          width: 50px;
          height: 50px;
          background: linear-gradient(to bottom, #ffe0bd, #ffd1a7);
          border-radius: 50%;
          margin-bottom: 8px;
          box-shadow: 0 3px 8px rgba(0, 0, 0, 0.1);
          z-index: 10;
          animation: headMove 8s ease-in-out infinite;
        }
        
        /* 头发样式 */
        .hair {
          position: absolute;
          top: -10px;
          left: -5px;
          width: 60px;
          height: 30px;
          background: #8b4513;
          border-radius: 50% 50% 0 0;
          overflow: hidden;
        }
        
        .hair::before {
          content: "";
          position: absolute;
          top: 15px;
          left: 15px;
          width: 30px;
          height: 40px;
          background: #8b4513;
          border-radius: 50%;
        }
        
        /* 面部特征 */
       .eye {
          position: absolute;
          width: 6px;
          height: 8px;
          background-color: #333;
          border-radius: 50%;
          top: 20px;
          animation: blink 4s infinite;
        }
        
        .eye.left {
          left: 15px;
        }
        
        .eye.right {
          right: 15px;
        }
        
        .mouth {
          position: absolute;
          width: 12px;
          height: 4px;
          bottom: 15px;
          left: 50%;
          transform: translateX(-50%);
          background-color: #e75480;
          border-radius: 0 0 6px 6px;
          transition: all 0.3s;
          animation: smile 6s infinite alternate;
        }
        
        .blush {
          position: absolute;
          width: 8px;
          height: 4px;
          background-color: #ffb6c1;
          border-radius: 50%;
          top: 28px;
          opacity: 0.6;
        }
        
        .blush.left {
          left: 8px;
        }
        
        .blush.right {
          right: 8px;
        }
        
        /* 身体样式 - 连衣裙 */
        .body {
          position: relative;
          width: 50px;
          height: 70px;
          background: linear-gradient(to bottom, #ff66b2, #ff3385);
          border-radius: 5px 5px 20px 20px;
          box-shadow: 0 3px 8px rgba(0, 0, 0, 0.1);
          animation: breath 5s ease-in-out infinite;
          z-index: 5;
        }
        
        .dress-detail {
          position: absolute;
          width: 100%;
          height: 15px;
          background-color: #ffccdd;
          top: 15px;
          border-radius: 5px;
        }
        
        /* 手臂样式 */
        .arm {
          position: absolute;
          width: 10px;
          height: 40px;
          background: linear-gradient(to bottom, #ffe0bd, #ffd1a7);
          border-radius: 5px;
          top: 5px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .arm.left {
          left: -8px;
          animation: waveLeft 4s ease-in-out infinite;
        }
        
        .arm.right {
          right: -8px;
          animation: waveRight 4s ease-in-out infinite;
        }
        
        /* 手部 */
        .hand {
          position: absolute;
          width: 10px;
          height: 10px;
          background: linear-gradient(to bottom, #ffe0bd, #ffd1a7);
          border-radius: 50%;
          bottom: -5px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        /* 腿部样式 */
        .legs {
          display: flex;
          width: 35px;
          justify-content: space-between;
          margin-top: -5px;
          z-index: 2;
        }
        
        .leg {
          width: 8px;
          height: 40px;
          background: linear-gradient(to bottom, #ffe0bd, #ffd1a7);
          border-radius: 0 0 4px 4px;
          box-shadow: 0 3px 8px rgba(0, 0, 0, 0.1);
        }
        
        .leg.left {
          animation: stepLeft 6s ease-in-out infinite;
        }
        
        .leg.right {
          animation: stepRight 6s ease-in-out infinite;
        }
        
        /* 脚部 */
        .foot {
          position: relative;
          width: 12px;
          height: 5px;
          background-color: #444;
          border-radius: 3px;
          top: 35px;
          left: 50%;
          transform: translateX(-50%);
        }
        
        /* 蝴蝶结 */
        .bow {
          position: absolute;
          width: 20px;
          height: 12px;
          background-color: #ffcc00;
          border-radius: 50%;
          top: -6px;
          left: 50%;
          transform: translateX(-50%);
          z-index: 15;
        }
        
        .bow::before, .bow::after {
          content: "";
          position: absolute;
          width: 12px;
          height: 12px;
          background-color: #ffcc00;
          border-radius: 50%;
          top: 5px;
        }
        
        .bow::before {
          left: -8px;
        }
        
        .bow::after {
          right: -8px;
        }
        
        /* 动画关键帧定义 */
        @keyframes headMove {
          0%, 100% { transform: rotate(-3deg); }
          25% { transform: rotate(3deg); }
          50% { transform: rotate(-3deg); }
          75% { transform: rotate(3deg); }
        }
        
        @keyframes breath {
          0%, 100% { transform: scaleY(1); }
          50% { transform: scaleY(1.03); }
        }
        
        @keyframes waveLeft {
          0%, 100% { transform: rotate(10deg) translate(-5px, 0); }
          25% { transform: rotate(-5deg) translate(-5px, -8px); }
          50% { transform: rotate(10deg) translate(-5px, 0); }
          75% { transform: rotate(-5deg) translate(-5px, -8px); }
        }
        
        @keyframes waveRight {
          0%, 100% { transform: rotate(-10deg) translate(5px, 0); }
          25% { transform: rotate(5deg) translate(5px, -8px); }
          50% { transform: rotate(-10deg) translate(5px, 0); }
          75% { transform: rotate(5deg) translate(5px, -8px); }
        }
        
        @keyframes stepLeft {
          0%, 100% { transform: rotate(0deg); }
          25% { transform: rotate(8deg); }
          50% { transform: rotate(0deg); }
          75% { transform: rotate(-5deg); }
        }
        
        @keyframes stepRight {
          0%, 100% { transform: rotate(0deg); }
          25% { transform: rotate(-5deg); }
          50% { transform: rotate(0deg); }
          75% { transform: rotate(8deg); }
        }
        
        @keyframes blink {
          0%, 45%, 55%, 100% { height: 8px; }
          48%, 52% { height: 1px; }
        }
        
        @keyframes smile {
          0%, 30% { height: 4px; border-radius: 0 0 6px 6px; }
          70%, 100% { height: 6px; border-radius: 0 0 12px 12px; }
        }
      '))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("评分结果", 
                 conditionalPanel(
                   condition = "input.mode == 'single'",
                   h3("单个作业评分结果"),
                   downloadButton("download_single_report", "下载评分报告"),
                   tableOutput("single_grade_table"),
                   h4("详细评分细则"),
                   tableOutput("single_detailed_grades"),
                   h4("详细反馈"),
                   verbatimTextOutput("single_feedback")
                 ),
                 conditionalPanel(
                   condition = "input.mode == 'batch'",
                   h3("批量评分结果"),
                   downloadButton("download_results", "下载全部结果"),
                   DT::dataTableOutput("batch_grade_table")
                 )),
        tabPanel("作业内容", 
                 conditionalPanel(
                   condition = "input.mode == 'single'",
                   verbatimTextOutput("single_content")
                 ),
                 conditionalPanel(
                   condition = "input.mode == 'batch'",
                   uiOutput("batch_file_selector"),
                   verbatimTextOutput("batch_content")
                 )),
        tabPanel("分析报告", 
                 plotOutput("grade_dist"),
                 h4("总体分析"),
                 verbatimTextOutput("batch_analysis"),
                 h4("详细反馈摘要"),
                 uiOutput("detailedFeedback"))
      )
    )
  )
)

# 定义服务器逻辑
server <- function(input, output, session) {
  
  # 生成评分标准描述 - 使用自定义描述
  rubric_text <- reactive({
    paste(
      "请根据以下通用作业评分标准进行评分（支持多种文件格式）:\n\n",
      "1. 内容质量 (40分)\n",
      "  - 内容完整性: ", input$content_completeness, "/15 (", input$content_completeness_desc, ")\n",
      "  - 内容正确性: ", input$correctness, "/15 (", input$correctness_desc, ")\n",
      "  - 分析深度: ", input$depth_analysis, "/10 (", input$depth_analysis_desc, ")\n\n",
      "2. 技术实现 (30分)\n",
      "  - 方法适当性: ", input$method_appropriateness, "/10 (", input$method_appropriateness_desc, ")\n",
      "  - 技术执行: ", input$technical_execution, "/10 (", input$technical_execution_desc, ")\n",
      "  - 问题解决: ", input$problem_solving, "/10 (", input$problem_solving_desc, ")\n\n",
      "3. 创新思维 (20分)\n",
      "  - 方法原创性: ", input$originality_approach, "/10 (", input$originality_approach_desc, ")\n",
      "  - 批判性思维: ", input$critical_thinking, "/10 (", input$critical_thinking_desc, ")\n\n",
      "4. 文档规范 (10分)\n",
      "  - 格式一致性: ", input$formatting_consistency, "/5 (", input$formatting_consistency_desc, ")\n",
      "  - 呈现清晰度: ", input$presentation_clarity, "/5 (", input$presentation_clarity_desc, ")\n\n",
      "评分严格度: ", input$strictness, "/5 (1=最宽松，5=最严格)",
      "\n\n注意：此标准适用于所有文件格式（R/Python脚本、PDF、Word、PPT、Excel等）"
    )
  })
  
  # 当前模式下的文件内容
  current_files <- reactive({
    if(input$mode == "single") {
      req(input$single_file)
      list(
        name = input$single_file$name,
        path = input$single_file$datapath,
        ext = tolower(tools::file_ext(input$single_file$name))
      )
    } else {
      req(input$batch_files)
      data.frame(
        name = input$batch_files$name,
        path = input$batch_files$datapath,
        ext = tolower(tools::file_ext(input$batch_files$name))
      )
    }
  })
  
  # 读取单个文件内容 (支持多种格式)
  read_single_file <- function(file_info) {
    tryCatch({
      ext <- file_info$ext
      
      # 文本类文件
      if(ext %in% c("txt", "csv", "json", "xml", "yaml", "yml", "md", "log", "ini", "cfg", "conf", "r", "rmd", "qmd", "py", "js", "html", "css", "java", "c", "cpp", "php", "sh", "sql")) {
        text <- readLines(file_info$path, warn = FALSE, encoding = "UTF-8")
        return(paste(text, collapse = "\n"))
      }
      
      # PDF文件
      if(ext == "pdf") {
        text <- pdf_text(file_info$path)
        return(paste(text, collapse = "\n\n"))
      }
      
      # Word文档 (docx)
      if(ext == "docx") {
        doc <- read_docx(file_info$path)
        content <- docx_summary(doc)$text
        return(paste(content, collapse = "\n"))
      }
      
      # Excel文件
      if(ext %in% c("xls", "xlsx")) {
        sheets <- excel_sheets(file_info$path)
        all_content <- map_chr(sheets, function(sheet) {
          data <- read_excel(file_info$path, sheet = sheet)
          paste(capture.output(print(data)), collapse = "\n")
        })
        return(paste(all_content, collapse = "\n\n"))
      }
      
      # 图像文件 (使用OCR)
      if(ext %in% c("jpg", "jpeg", "png", "gif", "bmp", "tiff", "webp")) {
        # 检查tesseract是否安装
        if(!requireNamespace("tesseract", quietly = TRUE)) {
          stop("图像OCR需要安装tesseract包: install.packages('tesseract')")
        }
        eng <- tesseract::tesseract("eng")
        text <- tesseract::ocr(file_info$path, engine = eng)
        return(text)
      }
      
      # PowerPoint文件
      if(ext %in% c("ppt", "pptx")) {
        ppt <- read_pptx(file_info$path)  
        content <- ppt_summary(ppt)$text
        return(paste(content, collapse = "\n"))
      }
      
      # 其他格式尝试通用读取
      tryCatch({
        text <- readLines(file_info$path, warn = FALSE, encoding = "UTF-8")
        paste(text, collapse = "\n")
      }, error = function(e) {
        stop(paste("不支持的文件格式:", ext))
      })
      
    }, error = function(e) {
      showNotification(paste("文件读取失败:", file_info$name, "-", e$message), type = "error")
      NULL
    })
  }
  
  # 辅助函数：读取PPT内容
  ppt_summary <- function(ppt) {
    slide_count <- length(ppt)
    all_text <- character(0)
    
    for(i in 1:slide_count) {
      slide_content <- ppt$slide$get_slide(i)$get()
      text_nodes <- xml2::xml_find_all(slide_content, ".//a:t")
      text_content <- sapply(text_nodes, xml2::xml_text)
      all_text <- c(all_text, text_content)
    }
    
    data.frame(text = all_text)
  }
  
  # 单个文件内容显示
  output$single_content <- renderText({
    if(input$mode != "single") return(NULL)
    file_info <- current_files()
    content <- read_single_file(file_info)
    if(is.null(content)) return("无法读取文件内容")
    if(nchar(content) > 10000) {
      paste0(substr(content, 1, 10000), "\n\n... (内容过长，已截断显示)")
    } else {
      content
    }
  })
  
  # 批量文件选择器
  output$batch_file_selector <- renderUI({
    if(input$mode != "batch") return(NULL)
    req(input$batch_files)
    selectInput("selected_batch_file", "选择查看的文件", 
                choices = input$batch_files$name)
  })
  
  # 批量文件内容显示
  output$batch_content <- renderText({
    if(input$mode != "batch") return(NULL)
    req(input$selected_batch_file)
    
    files <- current_files()
    selected_file <- files[files$name == input$selected_batch_file, ]
    content <- read_single_file(list(
      name = selected_file$name,
      path = selected_file$path,
      ext = selected_file$ext
    ))
    
    if(is.null(content)) return("无法读取文件内容")
    if(nchar(content) > 10000) {
      paste0(substr(content, 1, 10000), "\n\n... (内容过长，已截断显示)")
    } else {
      content
    }
  })
  
  # 调用AI API进行评分
  call_ai_api <- function(prompt) {
    # 根据选择的API进行不同的调用
    if(input$api == "DeepSeek") {
      url <- "https://api.deepseek.com/v1/chat/completions"
      headers <- c(
        "Authorization" = paste("Bearer", input$api_key),
        "Content-Type" = "application/json"
      )
      
      body <- list(
        model = "deepseek-chat",
        messages = list(list(role = "user", content = prompt)),
        temperature = input$strictness * 0.15
      )
      
    } else if(input$api == "Kimi") {
      url <- "https://api.moonshot.cn/v1/chat/completions"
      headers <- c(
        "Authorization" = paste("Bearer", input$api_key),
        "Content-Type" = "application/json"
      )
      
      body <- list(
        model = "moonshot-v1-8k",
        messages = list(list(role = "user", content = prompt)),
        temperature = input$strictness * 0.15
      )
    } else { # GPT-4
      url <- "https://api.openai.com/v1/chat/completions"
      headers <- c(
        "Authorization" = paste("Bearer", input$api_key),
        "Content-Type" = "application/json"
      )
      
      body <- list(
        model = "gpt-4",
        messages = list(list(role = "user", content = prompt)),
        temperature = input$strictness * 0.15
      )
    }
    
    # 发送请求
    response <- POST(
      url,
      add_headers(.headers = headers),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
    
    # 解析响应
    if(status_code(response) == 200) {
      content <- fromJSON(content(response, "text", encoding = "UTF-8"))
      return(content$choices$message$content)
    } else {
      stop(paste("API调用失败:", status_code(response)))
    }
  }
  
  # 单个文件评分函数
  grade_single_assignment <- function(file_content) {
    # 构造评分提示
    prompt <- paste(
      "你是一位经验丰富的多学科教授，请根据以下通用作业评分标准对学生的作业进行评分和反馈。",
      "作业可能包含多种格式：R/Python代码(.R, .py, .R极md, .qmd)、文档(PDF, .docx, .doc)、",
      "演示文稿(.pptx)、数据文件(.xlsx)、纯文本(.txt)或其他格式。",
      "请根据内容而非文件格式进行评分。",
      "\n\n评分标准:\n", rubric_text(),
      "\n\n作业内容:\n", substr(file_content, 1, 15000),  # 限制内容长度以避免API限制
      "\n\n请按照以下格式返回结果:",
      "1. 内容质量: [分数]/40",
      "  - 内容完整性: [分数]/15 [评价]",
      "  - 内容正确性: [分数]/15 [评价]",
      "  - 分析深度: [分数]/10 [评价]",
      "2. 技术实现: [分数]/30",
      "  - 方法适当性: [分数]/10 [评价]",
      "  - 技术执行: [分数]/10 [评价]",
      "  - 问题解决: [分数]/10 [评价]",
      "3. 创新思维: [分数]/20",
      "  - 方法原创性: [分数]/10 [评价]",
      "  - 批判性思维: [分数]/10 [评价]",
      "4. 文档规范: [分数]/10",
      "  - 格式一致性: [分数]/5 [评价]",
      "  - 呈现清晰度: [分数]/5 [评价]",
      "总分: [总分]/100",
      "\n综合评语: [总体评价和改进建议]",
      "\n详细反馈: [多段落的详细反馈，包括优点和需要改进的地方]"
    )
    
    # 调用API
    tryCatch({
      result <- call_ai_api(prompt)
      
      # 解析结果
      parse_grading_result(result)
    }, error = function(e) {
      showNotification(paste("评分失败:", e$message), type = "error")
      list(
        error = paste("评分失败:", e$message),
        raw_result = result
      )
    })
  }
  
  # 解析评分结果
  parse_grading_result <- function(result) {
    tryCatch({
      full_text <- paste(result, collapse = "\n")
      lines <- strsplit(result, "\n")[[1]]
      
      # 使用更灵活的匹配方式
      extract_section <- function(pattern) {
        idx <- which(grepl(pattern, lines, ignore.case = TRUE))
        if(length(idx) == 0) return(NULL)
        idx
      }
      
      # 提取各维度的位置
      content_quality_idx <- extract_section("1[.]?\\s*内容质量")
      technical_idx <- extract_section("2[.]?\\s*技术实现")
      creativity_idx <- extract_section("3[.]?\\s*创新思维")
      documentation_idx <- extract_section("4[.]?\\s*文档规范")
      
      # 内容质量
      content_quality <- if(!is.null(content_quality_idx)) {
        list(
          total = extract_score(lines[content_quality_idx]),
          completeness = extract_detail_score(lines[content_quality_idx + 1]),
          correctness = extract_detail_score(lines[content_quality_idx + 2]),
          depth_analysis = extract_detail_score(lines[content_quality_idx + 3])
        )
      } else {
        list(
          total = list(score = NA, comment = "未找到内容质量评分"),
          completeness = list(score = NA, comment = ""),
          correctness = list(score = NA, comment = ""),
          depth_analysis = list(score = NA, comment = "")
        )
      }
      
      # 技术实现
      technical <- if(!is.null(technical_idx)) {
        list(
          total = extract_score(lines[technical_idx]),
          method_appropriateness = extract_detail_score(lines[technical_idx + 1]),
          technical_execution = extract_detail_score(lines[technical_idx + 2]),
          problem_solving = extract_detail_score(lines[technical_idx + 3])
        )
      } else {
        list(
          total = list(score = NA, comment = "未找到技术实现评分"),
          method_appropriateness = list(score = NA, comment = ""),
          technical_execution = list(score = NA, comment = ""),
          problem_solving = list(score = NA, comment = "")
        )
      }
      
      # 创新思维
      creativity <- if(!is.null(creativity_idx)) {
        list(
          total = extract_score(lines[creativity_idx]),
          originality_approach = extract_detail_score(lines[creativity_idx + 1]),
          critical_thinking = extract_detail_score(lines[creativity_idx + 2])
        )
      } else {
        list(
          total = list(score = NA, comment = "未找到创新思维评分"),
          originality_approach = list(score = NA, comment = ""),
          critical_thinking = list(score = NA, comment = "")
        )
      }
      
      # 文档规范
      documentation <- if(!is.null(documentation_idx)) {
        list(
          total = extract_score(lines[documentation_idx]),
          formatting_consistency = extract_detail_score(lines[documentation_idx + 1]),
          presentation_clarity = extract_detail_score(lines[documentation_idx + 2])
        )
      } else {
        list(
          total = list(score = NA, comment = "未找到文档规范评分"),
          formatting_consistency = list(score = NA, comment = ""),
          presentation_clarity = list(score = NA, comment = "")
        )
      }
      
      # 计算加权总分(确保不超过100分)
      content_score <- if(is.numeric(content_quality$total$score)) content_quality$total$score else 0
      tech_score <- if(is.numeric(technical$total$score)) technical$total$score else 0
      creat_score <- if(is.numeric(creativity$total$score)) creativity$total$score else 0
      doc_score <- if(is.numeric(documentation$total$score)) documentation$total$score else 0
      
      total_score <- min(100, content_score + tech_score + creat_score + doc_score)
      
      # 提取评语和反馈
      general_comment_idx <- which(grepl("综合评语", lines))
      detailed_feedback_idx <- which(grepl("详细反馈", lines))
      
      general_comment <- if(length(general_comment_idx) > 0) {
        start_idx <- general_comment_idx + 1
        end_idx <- if(length(detailed_feedback_idx) > 0) detailed_feedback_idx - 1 else length(lines)
        paste(lines[start_idx:min(end_idx, length(lines))], collapse = "\n")
      } else ""
      
      detailed_feedback <- if(length(detailed_feedback_idx) > 0) {
        start_idx <- detailed_feedback_idx + 1
        paste(lines[start_idx:length(lines)], collapse = "\n")
      } else ""
      
      list(
        total_score = total_score,
        content_quality = content_quality,
        technical = technical,
        creativity = creativity,
        documentation = documentation,
        general_comment = general_comment,
        detailed_feedback = detailed_feedback,
        raw_result = full_text
      )
    }, error = function(e) {
      # 解析失败时返回一个包含错误信息的结果
      list(
        error = paste("解析评分结果失败:", e$message),
        raw_result = paste(result, collapse = "\n")
      )
    })
  }
  
  # 提取分数（更健壮的版本）
  extract_score <- function(line) {
    tryCatch({
      if(is.null(line) || is.na(line) || nchar(line) == 0) {
        return(list(score = NA, comment = "空行"))
      }
      
      # 使用正则表达式提取分数
      matches <- regmatches(line, gregexpr("[0-9.]+/[0-9.]+", line))[[1]]
      if(length(matches) == 0) {
        return(list(score = NA, comment = "未找到分数"))
      }
      
      # 取第一个匹配的分数
      score_match <- matches[1]
      score_value <- as.numeric(strsplit(score_match, "/")[[1]][1])
      
      # 提取评价
      comment_part <- gsub(score_match, "", line)
      comment_part <- gsub("^[^:]+:\\s*", "", comment_part)  # 移除分数前的标签
      
      list(
        score = score_value,
        comment = trimws(comment_part)
      )
    }, error = function(e) {
      list(
        score = NA,
        comment = paste("分数提取失败:", e$message)
      )
    })
  }
  
  # 提取详细分数（更健壮的版本）
  extract_detail_score <- function(line) {
    tryCatch({
      if(is.null(line) || is.na(line) || nchar(line) == 0) {
        return(list(score = NA, comment = "空行"))
      }
      
      # 使用正则表达式提取分数
      matches <- regmatches(line, gregexpr("[0-9.]+/[0-9.]+", line))[[1]]
      if(length(matches) == 0) {
        return(list(score = NA, comment = "未找到分数"))
      }
      
      # 取第一个匹配的分数
      score_match <- matches[1]
      score_value <- as.numeric(strsplit(score_match, "/")[[1]][1])
      
      # 提取评价
      comment_part <- gsub(score_match, "", line)
      comment_part <- gsub("^[^:]+:\\s*", "", comment_part)  # 移除分数前的标签
      
      list(
        score = score_value,
        comment = trimws(comment_part)
      )
    }, error = function(e) {
      list(
        score = NA,
        comment = paste("分数提取失败:", e$message)
      )
    })
  }
  
  # 单个文件评分结果
  single_grading_result <- eventReactive(input$grade, {
    if(input$mode != "single") return(NULL)
    file_info <- current_files()
    content <- read_single_file(file_info)
    if(is.null(content)) return(NULL)
    grade_single_assignment(content)
  })
  
  # 显示单个文件评分表格
  output$single_grade_table <- renderTable({
    result <- single_grading_result()
    if(is.null(result) || !is.null(result$error)) return(NULL)
    
    # 确保所有分数有效，否则使用NA
    total_score_val <- if(!is.null(result$total_score)) result$total_score else NA
    content_val <- if(!is.null(result$content_quality$total$score)) result$content_quality$total$score else NA
    technical_val <- if(!is.null(result$technical$total$score)) result$technical$total$score else NA
    creativity_val <- if(!is.null(result$creativity$total$score)) result$creativity$total$score else NA
    documentation_val <- if(!is.null(result$documentation$total$score)) result$documentation$total$score else NA
    
    data.frame(
      评分维度 = c("内容质量", "技术实现", "创新思维", "文档规范", "总分"),
      得分 = c(
        content_val,
        technical_val,
        creativity_val,
        documentation_val,
        total_score_val
      ),
      满分 = c(40, 30, 20, 10, 100),
      百分比 = c(
        if(!is.na(content_val)) paste0(round(content_val / 40 * 100, 1), "%") else "NA",
        if(!is.na(technical_val)) paste0(round(technical_val / 30 * 100, 1), "%") else "NA",
        if(!is.na(creativity_val)) paste0(round(creativity_val / 20 * 100, 1), "%") else "NA",
        if(!is.na(documentation_val)) paste0(round(documentation_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(total_score_val)) paste0(total_score_val, "%") else "NA"
      ),
      评价 = c(
        if(!is.null(result$content_quality$total$comment)) result$content_quality$total$comment else "",
        if(!is.null(result$technical$total$comment)) result$technical$total$comment else "",
        if(!is.null(result$creativity$total极$comment)) result$creativity$total$comment else "",
        if(!is.null(result$documentation$total$comment)) result$documentation$total$comment else "",
        ""
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # 显示详细评分细则
  output$single_detailed_grades <- renderTable({
    result <- single_grading_result()
    if(is.null(result) || !is.null(result$error)) return(NULL)
    
    # 确保所有分数有效，否则使用NA
    completeness_val <- if(!is.null(result$content_quality$completeness$score)) result$content_quality$completeness$score else NA
    correctness_val <- if(!is.null(result$content_quality$correctness$score)) result$content_quality$correctness$score else NA
    depth_analysis_val <- if(!is.null(result$content_quality$depth_analysis$score)) result$content_quality$depth_analysis$score else NA
    method_val <- if(!is.null(result$technical$method_appropriateness$score)) result$technical$method_appropriateness$score else NA
    execution_val <- if(!is.null(result$technical$technical_execution$score)) result$technical$technical_execution$score else NA
    solving_val <- if(!is.null(result$technical$problem_solving$score)) result$technical$problem_solving$score else NA
    originality_val <- if(!is.null(result$creativity$originality_approach$score)) result$creativity$originality_approach$score else NA
    critical_val <- if(!is.null(result$creativity$critical_thinking$score)) result$creativity$critical_thinking$score else NA
    formatting_val <- if(!is.null(result$documentation$formatting_consistency$score)) result$documentation$formatting_consistency$score else NA
    presentation_val <- if(!is.null(result$documentation$presentation_clarity$score)) result$documentation$presentation_clarity$score else NA
    
    data.frame(
      评分细则 = c(
        "1.1 内容完整性",
        "1.2 内容正确性",
        "1.3 分析深度",
        "2.1 方法适当性",
        "2.2 技术执行",
        "2.3 问题解决",
        "3.1 方法原创性",
        "3.2 批判性思维",
        "4.1 格式一致性",
        "4.2 呈现清晰度"
      ),
      得分 = c(
        completeness_val,
        correctness_val,
        depth_analysis_val,
        method_val,
        execution_val,
        solving_val,
        originality_val,
        critical_val,
        formatting_val,
        presentation_val
      ),
      满分 = c(15, 15, 10, 10, 10, 10, 10, 10, 5, 5),
      百分比 = c(
        if(!is.na(completeness_val)) paste0(round(completeness_val / 15 * 100, 1), "%") else "NA",
        if(!is.na(correctness_val)) paste0(round(correctness_val / 15 * 100, 1), "%") else "NA",
        if(!is.na(depth_analysis_val)) paste0(round(depth_analysis_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(method_val)) paste0(round(method_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(execution_val)) paste0(round(execution_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(solving_val)) paste0(round(solving_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(originality_val)) paste0(round(originality_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(critical_val)) paste0(round(critical_val / 10 * 100, 1), "%") else "NA",
        if(!is.na(formatting_val)) paste0(round(formatting_val / 5 * 100, 1), "%") else "NA",
        if(!is.na(presentation_val)) paste0(round(presentation_val / 5 * 100, 1), "%") else "NA"
      ),
      评价 = c(
        if(!is.null(result$content_quality$completeness$comment)) result$content_quality$completeness$comment else "",
        if(!is.null(result$content_quality$correctness$comment)) result$content_quality$correctness$comment else "",
        if(!is.null(result$content_quality$depth_analysis$comment)) result$content_quality$depth_analysis$comment else "",
        if(!is.null(result$technical$method_appropriateness$comment)) result$technical$method_appropriateness$comment else "",
        if(!is.null(result$technical$technical_execution$comment)) result$technical$technical_execution$comment else "",
        if(!is.null(result$technical$problem_solving$comment)) result$technical$problem_solving$comment else "",
        if(!is.null(result$creativity$originality_approach$comment)) result$creativity$originality_approach$comment else "",
        if(!is.null(result$creativity$critical_thinking$comment)) result$creativity$critical_thinking$comment else "",
        if(!is.null(result$documentation$formatting_consistency$comment)) result$documentation$formatting_consistency$comment else "",
        if(!is.null(result$documentation$presentation_clarity$comment)) result$documentation$presentation_clarity$comment else ""
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")
  
  # 显示单个文件详细反馈
  output$single_feedback <- renderText({
    result <- single_grading_result()
    if(is.null(result)) return("")
    
    if(!is.null(result$error)) {
      return(paste("评分失败:", result$error, "\n\n原始结果:\n", result$raw_result))
    }
    
    paste(
      "综合评语:\n", result$general_comment, "\n\n",
      "详细反馈:\n", result$detailed_feedback
    )
  })
  
  # 下载单个文件评分报告
  output$download_single_report <- downloadHandler(
    filename = function() {
      if(input$mode == "single") {
        file_info <- current_files()
        paste0(tools::file_path_sans_ext(file_info$name), "-评分报告-", Sys.Date(), ".txt")
      } else {
        paste0("评分报告-", Sys.Date(), ".txt")
      }
    },
    content = function(file) {
      result <- single_grading_result()
      if(is.null(result)) return(NULL)
      
      # 构建报告内容
      report_content <- ""
      
      if(!is.null(result$error)) {
        report_content <- paste(
          "评分失败报告\n",
          "================================\n",
          "文件名: ", if(input$mode == "single") current_files()$name else "多个文件", "\n",
          "错误信息: ", result$error, "\n\n",
          "原始结果:\n", result$raw_result
        )
      } else {
        # 添加评分总结
        report_content <- paste0(
          "作业评分报告\n",
          "================================\n",
          "文件名: ", current_files()$name, "\n",
          "评分时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n",
          "总分: ", result$total_score, "/100\n\n"
        )
        
        # 添加各维度评分
        report_content <- paste0(report_content,
                                 "1. 内容质量: ", result$content_quality$total$score, "/40\n",
                                 "  - 内容完整性: ", result$content_quality$completeness$score, "/15 - ", result$content_quality$completeness$comment, "\n",
                                 "  - 内容正确性: ", result$content_quality$correctness$score, "/15 - ", result$content_quality$correctness$comment, "\n",
                                 "  - 分析深度: ", result$content_quality$depth_analysis$score, "/10 - ", result$content_quality$depth_analysis$comment, "\n\n",
                                 
                                 "2. 技术实现: ", result$technical$total$score, "/30\n",
                                 "  - 方法适当性: ", result$technical$method_appropriateness$score, "/10 - ", result$technical$method_appropriateness$comment, "\n",
                                 "  - 技术执行: ", result$technical$technical_execution$score, "/10 - ", result$technical$technical_execution$comment, "\n",
                                 "  - 问题解决: ", result$technical$problem_solving$score, "/10 - ", result$technical$problem_solving$comment, "\n\n",
                                 
                                 "3. 创新思维: ", result$creativity$total$score, "/20\n",
                                 "  - 方法原创性: ", result$creativity$originality_approach$score, "/10 - ", result$creativity$originality_approach$comment, "\n",
                                 "  - 批判性思维: ", result$creativity$critical_thinking$score, "/10 - ", result$creativity$critical_thinking$comment, "\n\n",
                                 
                                 "4. 文档规范: ", result$documentation$total$score, "/10\n",
                                 "  - 格式一致性: ", result$documentation$formatting_consistency$score, "/5 - ", result$documentation$formatting_consistency$comment, "\n",
                                 "  - 呈现清晰度: ", result$documentation$presentation_clarity$score, "/5 - ", result$documentation$presentation_clarity$comment, "\n\n"
        )
        
        # 添加评语和反馈
        report_content <- paste0(report_content,
                                 "综合评语:\n", result$general_comment, "\n\n",
                                 "详细反馈:\n", result$detailed_feedback, "\n\n"
        )
        
        # 添加原始结果（可选）
        if(nchar(result$raw_result) < 5000) {
          report_content <- paste0(report_content,
                                   "原始评分结果:\n",
                                   "================================\n",
                                   result$raw_result
          )
        }
      }
      
      # 写入文件
      writeLines(report_content, file, useBytes = TRUE)
    }
  )
  
  # 批量评分结果
  batch_grading_results <- eventReactive(input$grade, {
    if(input$mode != "batch") return(NULL)
    req(input$batch_files)
    
    files <- current_files()
    
    withProgress(message = '正在批改作业', value = 0, {
      results <- map(1:nrow(files), function(i) {
        file_info <- list(
          name = files$name[i],
          path = files$path[i],
          ext = files$ext[i]
        )
        
        incProgress(1/nrow(files), detail = paste("正在处理", file_info$name))
        
        content <- read_single_file(file_info)
        if(is.null(content)) {
          return(list(
            filename = file_info$name,
            success = FALSE,
            error = "无法读取文件内容"
          ))
        }
        
        result <- tryCatch({
          grading <- grade_single_assignment(content)
          if(is.null(grading) || !is.null(grading$error)) {
            stop(grading$error)
          }
          
          list(
            filename = file_info$name,
            success = TRUE,
            total_score = if(!is.null(grading$total_score)) grading$total_score else NA,
            content_quality = if(!is.null(grading$content_quality$total$score)) grading$content_quality$total$score else NA,
            technical = if(!is.null(grading$technical$total$score)) grading$technical$total$score else NA,
            creativity = if(!is.null(grading$creativity$total$score)) grading$creativity$total$score else NA,
            documentation = if(!is.null(grading$documentation$total$score)) grading$documentation$total$score else NA,
            feedback = if(!is.null(grading$general_comment)) grading$general_comment else "",
            detailed_feedback = paste(
              "综合评语:\n", grading$general_comment, "\n\n",
              "详细反馈:\n", grading$detailed_feedback
            )
          )
        }, error = function(e) {
          list(
            filename = file_info$name,
            success = FALSE,
            error = e$message
          )
        })
        
        Sys.sleep(1)  # API速率限制
        result
      })
    })
    
    results
  })
  
  # 显示批量评分表格
  output$batch_grade_table <- DT::renderDataTable({
    results <- batch_grading_results()
    if(is.null(results)) return(NULL)
    
    data <- map_df(results, function(x) {
      if(x$success) {
        data.frame(
          文件名 = x$filename,
          总分 = x$total_score,
          内容质量 = x$content_quality,
          技术实现 = x$technical,
          创新思维 = x$creativity,
          文档规范 = x$documentation,
          反馈摘要 = substr(x$feedback, 1, 50)
        )
      } else {
        data.frame(
          文件名 = x$filename,
          总分 = NA,
          内容质量 = NA,
          技术实现 = NA,
          创新思维 = NA,
          文档规范 = NA,
          反馈摘要 = paste("失败:", x$error)
        )
      }
    })
    
    DT::datatable(
      data,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  # 下载批量评分结果
  output$download_results <- downloadHandler(
    filename = function() {
      paste("作业评分结果-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      results <- batch_grading_results()
      if(is.null(results)) return(NULL)
      
      data <- map_df(results, function(x) {
        if(x$success) {
          data.frame(
            文件名 = x$filename,
            总分 = x$total_score,
            内容质量 = x$content_quality,
            技术实现 = x$technical,
            创新思维 = x$creativity,
            文档规范 = x$documentation,
            详细反馈 = x$detailed_feedback
          )
        } else {
          data.frame(
            文件名 = x$filename,
            总分 = NA,
            内容质量 = NA,
            技术实现 = NA,
            创新思维 = NA,
            文档规范 = NA,
            详细反馈 = paste("评分失败:", x$error)
          )
        }
      })
      
      write.csv(data, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  # 生成评分分布图
  output$grade_dist <- renderPlot({
    if(input$mode == "single") {
      result <- single_grading_result()
      if(is.null(result) || !is.null(result$error)) return(NULL)
      
      # 确保所有分数有效，否则使用0
      completeness_val <- if(!is.na(result$content_quality$completeness$score)) result$content_quality$completeness$score else 0
      correctness_val <- if(!is.na(result$content_quality$correctness$score)) result$content_quality$correctness$score else 0
      depth_val <- if(!is.na(result$content_quality$depth_analysis$score)) result$content_quality$depth_analysis$score else 0
      method_val <- if(!is.na(result$technical$method_appropriateness$score)) result$technical$method_appropriateness$score else 0
      execution_val <- if(!is.na(result$technical$technical_execution$score)) result$technical$technical_execution$score else 0
      solving_val <- if(!is.na(result$technical$problem_solving$score)) result$technical$problem_solving$score else 0
      originality_val <- if(!is.na(result$creativity$originality_approach$score)) result$creativity$originality_approach$score else 0
      critical_val <- if(!is.na(result$creativity$critical_thinking$score)) result$creativity$critical_thinking$score else 0
      formatting_val <- if(!is.na(result$documentation$formatting_consistency$score)) result$documentation$formatting_consistency$score else 0
      presentation_val <- if(!is.na(result$documentation$presentation_clarity$score)) result$documentation$presentation_clarity$score else 0
      
      # 创建细化评分雷达图
      scores <- c(
        completeness_val / 15 * 100,
        correctness_val / 15 * 100,
        depth_val / 10 * 100,
        method_val / 10 * 100,
        execution_val / 10 * 100,
        solving_val / 10 * 100,
        originality_val / 10 * 100,
        critical_val / 10 * 100,
        formatting_val / 5 * 100,
        presentation_val / 5 * 100
      )
      
      categories <- c(
        "内容完整", "内容正确", "分析深度",
        "方法适当", "技术执行", "问题解决",
        "方法原创", "批判思维", 
        "格式一致", "呈现清晰"
      )
      
      # 创建雷达图数据
      data <- data.frame(
        Category = factor(categories, levels = categories),
        Score = scores
      )
      
      ggplot(data, aes(x = Category, y = Score, group = 1)) +
        geom_polygon(fill = "skyblue", alpha = 0.6) +
        geom_point(color = "blue", size = 2) +
        geom_line(color = "blue") +
        coord_polar() +
        ylim(0, 100) +
        theme_minimal() +
        labs(title = "细化评分雷达图", x = "", y = "得分百分比")
      
    } else {
      results <- batch_grading_results()
      if(is.null(results)) return(NULL)
      
      # 提取所有成功评分的总分
      total_scores <- map_dbl(results, function(x) {
        if(x$success) x$total_score else NA
      })
      total_scores <- na.omit(total_scores)
      
      if(length(total_scores) == 0) return(NULL)
      
      # 创建多个维度的箱线图
      data <- map_df(results, function(x) {
        if(x$success) {
          data.frame(
            文件名 = x$filename,
            内容质量 = x$content_quality,
            技术实现 = x$technical,
            创新思维 = x$creativity,
            文档规范 = x$documentation
          )
        } else NULL
      })
      
      if(nrow(data) == 0) return(NULL)
      
      data_long <- pivot_longer(data, -文件名, names_to = "维度", values_to = "分数")
      
      ggplot(data_long, aes(x = 维度, y = 分数, fill = 维度)) +
        geom_boxplot() +
        geom_jitter(width = 0.2, alpha = 0.5) +
        theme_minimal() +
        labs(title = "各维度评分分布", x = "评分维度", y = "得分") +
        scale_fill_brewer(palette = "Set2")
    }
  })
  
  # 生成批量分析报告
  output$batch_analysis <- renderText({
    if(input$mode != "batch") return("")
    results <- batch_grading_results()
    if(is.null(results)) return("")
    
    # 统计成功和失败的评分
    success_count <- sum(map_lgl(results, ~ .x$success))
    fail_count <- length(results) - success_count
    
    # 计算各维度平均分（忽略NA值）
    avg_scores <- list(
      total = mean(map_dbl(results, function(x) if(x$success) x$total_score else NA), na.rm = TRUE),
      content = mean(map_dbl(results, function(x) if(x$success) x$content_quality else NA), na.rm = TRUE),
      technical = mean(map_dbl(results, function(x) if(x$success) x$technical else NA), na.rm = TRUE),
      creativity = mean(map_dbl(results, function(x) if(x$success) x$creativity else NA), na.rm = TRUE),
      documentation = mean(map_dbl(results, function(x) if(x$success) x$documentation else NA), na.rm = TRUE)
    )
    
    # 识别常见问题
    common_issues <- c()
    if(avg_scores$content < 25) common_issues <- c(common_issues, "内容质量有待提高")
    if(avg_scores$technical < 20) common_issues <- c(common_issues, "技术实现能力不足")
    if(avg_scores$creativity < 12) common_issues <- c(common_issues, "创新思维不足")
    if(avg_scores$documentation < 6) common_issues <- c(common_issues, "文档规范问题较多")
    
    # 生成分析报告
    analysis <- paste(
      "作业批量评分结果分析报告\n",
      "================================\n",
      "基本统计:\n",
      "- 总评分数: ", length(results), "\n",
      "- 成功评分: ", success_count, "\n",
      "- 失败评分: ", fail_count, if(fail_count > 0) " (请检查文件格式和内容)" else "", "\n\n",
      "平均分数:\n",
      "- 总分: ", round(avg_scores$total, 1), "/100\n",
      "- 内容质量: ", round(avg_scores$content, 1), "/40\n",
      "- 技术实现: ", round(avg_scores$technical, 1), "/30\n",
      "- 创新思维: ", round(avg_scores$creativity, 1), "/20\n",
      "- 文档规范: ", round(avg_scores$documentation, 1), "/10\n\n",
      if(length(common_issues) > 0) {
        paste("常见问题:\n- ", paste(common_issues, collapse = "\n- "), "\n\n")
      } else "",
      "教学建议:\n",
      if(avg_scores$content < 30) "- 加强内容完整性和深度分析能力的培养\n" else "",
      if(avg_scores$technical < 22) "- 强化技术实现和问题解决能力的训练\n" else "",
      if(avg_scores$creativity < 14) "- 鼓励创新思维和批判性思考\n" else "",
      if(avg_scores$documentation < 7) "- 强调文档规范的重要性\n" else "",
      if(avg_scores$total > 85) "- 整体表现优秀，可考虑增加挑战性任务\n" else "",
      "\n详细反馈请查看各文件评分结果。"
    )
    
    analysis
  })
  
  # 每个文件的详细反馈摘要
  output$detailedFeedback <- renderUI({
    if(input$mode != "batch") return(NULL)
    results <- batch_grading_results()
    if(is.null(results)) return(NULL)
    
    # 创建空列表存储每个文件的反馈
    feedback_list <- list()
    
    # 遍历所有结果
    for (i in seq_along(results)) {
      result <- results[[i]]
      file_name <- result$filename
      
      if(result$success) {
        # 构建单个文件反馈
        feedback_list[[i]] <- div(
          h4(paste("文件反馈摘要:", file_name)),
          p(strong("总分:"), result$total_score, "/100"),
          p(strong("内容质量:"), result$content_quality, "/40"),
          p(strong("技术实现:"), result$technical, "/30"),
          p(strong("创新思维:"), result$creativity, "/20"),
          p(strong("文档规范:"), result$documentation, "/10"),
          tags$details(
            tags$summary("查看详细反馈"),
            pre(style = "white-space: pre-wrap; background-color: #f8f9fa; padding: 15px; border-radius: 5px;",
                result$detailed_feedback)
          ),
          hr(style = "border-top: 1px solid #ddd; margin: 20px 0;")
        )
      } else {
        # 处理失败情况
        feedback_list[[i]] <- div(
          h4(paste("文件反馈摘要:", file_name)),
          div(style = "color: #d9534f;",
              p(strong("评分失败:"), result$error)),
          hr(style = "border-top: 1px solid #ddd; margin: 20px 0;")
        )
      }
    }
    
    # 组合所有反馈
    do.call(tagList, feedback_list)
  })
}

# 运行应用
shinyApp(ui = ui, server = server)