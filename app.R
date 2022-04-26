" データ分析ツール"
library(shiny)
library(shinythemes)
library(DT)
library(shinycssloaders)
library(foreign)
library(shinyjs)

options(shiny.maxRequestSize = 1 * 1024 ^ 3)


### uiを定義

## 「データの読込み」タブのUIモジュール
" UI Module for fileread."

filereadTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("① データセットの読込み", sidebarLayout(
    sidebarPanel(
      fileInput(ns("file_SPSS"), "SPSSファイルを読込む",
                accept = "application/x-spss-sav")
    ),
    mainPanel(
      # navbarの移動はglobalスコープ
      # h3(actionLink("link_to_analytics", "このデータセットを分析")),
      # textOutput("notification"),
      
      # 読み込みデータのサマリー可視化
      h4(textOutput(ns(
        "spssSummaryTitle"
      )),
      htmlOutput(ns(
        "spssSummaryLink"
      ), align="right")
      ),
      withSpinner(tableOutput(ns("spssSumarry"))),
    )
  ))
}

## 「② 変数分析」タブのUIモジュール
" UI Module for analytics of variables."

analyticsTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("② 変数分析",
           fluidRow(
             column(
               3,
               h2("1. 変数選択", align="center"),
               p(hr(),"読込んだデータセット：",textOutput(ns("spssName")),hr()),
               selectInput(ns("selected_variable"),
                           "分析対象の変数を選択",
                           choices = NULL),
               p(textOutput(ns("typeOfVariable")),hr()),
               p(id = "CheckBox_variable",
                 uiOutput(ns("checkGroupUi"))),
               p(id = "slider_filter_ui_wrapper",
                 uiOutput(ns(
                   "slider_filter_ui"
                 )))
             ),
             column(3,
                    h2("2. 記述統計", align="center"),
                    hr(),
                    uiOutput(ns(
                      "analytics_button"
                    )), ),
             column(6,
                    h2("3. 演算結果", align="center"),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h4(withSpinner(uiOutput(ns(
                      "stats"
                    ))), align = "center"), )
           ))
}

## 「ご利用ガイド」タブのUIモジュール
" UI Module for guide."

guideTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("ご利用ガイド",
           h1("3/28のお打ち合わせ後、3/29に作成予定"))
}

## 「JDCatメタデータカタログ」タブのUIモジュール
" UI Module for metadata catalog."

metadataTabPanel <- function(id) {
  ns <- NS(id)
  
  tabPanel("JDCat メタデータカタログ",
           h1("3/29にhttps://jgss.daishodai.ac.jp/jgssdds/jgssdds_jdcat.html反映予定"))
}


## ui全体の統合
ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    HTML(
      "#shiny-notification-panel {
                position:fixed;
                top: 0;
              }
              .shiny-notification {
                opacity: 1;
              }
              "
    )
  )),
  navbarPage(
    strong(em("JGSS オンラインデータ分析ツール")),br(),
    id = "navbar",
    # グローバル空間
    # namespace: global
    
    # ① データセットの読込み
    # namespace: fileread
    filereadTabPanel(id = "fileread"),
    
    # ② 変数分析
    # namespace: analytics
    analyticsTabPanel(id = "analytics"),
    
    # ご利用ガイド
    # namespace: guide
    guideTabPanel(id = "guide"),
    
    # JDCat メタデータカタログ
    # namespace: metadata
    metadataTabPanel(id = "metadata"),

    # Footer
    footer= h5(hr(),"Copyright(C) 1999-2022, Japanese General Social Surveys. All Rights Reserved.", align = "center")
    
  )
))

### serverを定義

## 「データの読込み」タブのサーバーモジュール
" Module for fileread functions."

filereader <- function(id) {
  " File reader returning dataframe list of spss and numeric version of spss."
  moduleServer(id,
               function(input, output, session) {
                 # SPSSファイルの読込み
                 dfSpss <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = T,
                     use.value.labels = T
                   )
                   colNames <- names(df)
                   variableLabels <-
                     as.character(attr(df, "variable.labels"))
                   for (i in 1:length(variableLabels)) {
                     if (variableLabels[i] == " ") {
                       variableLabels[i] <- paste(colNames[i], "()")
                     } else {
                       variableLabels[i] <- paste(variableLabels[i], "(", colNames[i], ")")
                     }
                   }
                   names(df) <- variableLabels
                   df
                 })
                 
                 dfSpssValue <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   df <- read.spss(
                     input$file_SPSS$datapath,
                     reencode = "UTF-8",
                     to.data.frame = T,
                     use.value.labels = F
                   )
                   colNames <- names(df)
                   variableLabels <-
                     as.character(attr(df, "variable.labels"))
                   for (i in 1:length(variableLabels)) {
                     if (variableLabels[i] == " ") {
                       variableLabels[i] <- paste(colNames[i], "()")
                     } else {
                       variableLabels[i] <- paste(variableLabels[i], "(", colNames[i], ")")
                     }
                   }
                   names(df) <- variableLabels
                   df
                 })
                 
                 output$spssSummaryTitle <- renderText({
                   req(dfSpss())
                   paste("読込んだ変数一覧（合計", length(names(dfSpss())), "種類）")
                 })
                 
                 output$spssSummaryLink <- renderUI({
                   req(dfSpss())
                   actionLink("link_to_analytics", "このデータセットを分析する")
                 })
                 
                 output$spssSumarry <- renderTable({
                   data.frame(
                     "変数" = names(dfSpss()),
                     "回答数" = sapply(dfSpss(), function(x)
                       sum(!is.na(x))),
                     "欠損数" = sapply(dfSpss(), function(x)
                       sum(is.na(x)))
                   )
                 }, rownames = FALSE, colnames = TRUE)
                 
                 # 不正な拡張子の通知
                 observeEvent(input$file_SPSS, {
                   req(!endsWith(input$file_SPSS$datapath, '.sav'))
                   
                   showNotification(".sav以外のファイル拡張子です。", type = "error")
                 })
                 
                 # ファイル名の取得
                 spssName <- eventReactive(input$file_SPSS, {
                   req(endsWith(input$file_SPSS$name, '.sav'))
                   input$file_SPSS$name
                 })
                 
                 return(list(
                   spss = dfSpss,
                   spssValue = dfSpssValue,
                   spssName = spssName
                 ))
                 
               })
}

## 「② 変数分析」タブのサーバーモジュール
" Module for analytics functions."

fileNameGenerator <- function(id, spssName) {
  " Generator of file name text."
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   output$spssName <- renderText(spssName())
                 })
               })
}

selectorGenerator <- function(id, dfSpss) {
  " Generator of selector options."
  moduleServer(id,
               function(input, output, session) {
                 # 変数を選択するためのプルダウン項目制御
                 observe({
                   updateSelectInput(session,
                                     "selected_variable",
                                     choices = names(dfSpss()))
                 })
               })
}

typeOfVariableGenerator <- function(id, df) {
  " Generator of type of variable text based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req(column %in% names(df()))
                   
                   # 変数の型の種類のテキストを生成
                   output$typeOfVariable <- renderText({
                     x <- df()[, column]
                     if (is.factor(x)) {
                       "データ型：カテゴリー値"
                     } else{
                       choiceList = list()
                       "データ型：数値"
                     }
                   })
                 })
               })
}

checkboxGenerator <- function(id, df, dfValue) {
  " Generator of checkbox based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # チェックボックス生成
                 observe({
                   output$checkGroupUi <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # データ列名を取得
                     column <- input$selected_variable
                     
                     # データフレームに変数が含まれるか確認
                     req (column %in% names(df()))
                     
                     # データの型に応じてチェックボックスを作成
                     x <- df()[, column]
                     xLabels <-
                       attr(dfValue()[, column], "value.labels")
                     
                     req(is.factor(x))
                     tagList(
                       p(style = "display:inline-block", actionButton(ns(
                         "check_all"
                       ), "全選択")),
                       p(style = "display:inline-block", actionButton(ns(
                         "uncheck_all"
                       ), "全解除")),
                       p(id = "CheckBox_variable",
                         checkboxGroupInput(
                           ns("checkGroup"),
                           "Checkbox group"
                         ))
                     )
                   })
                 })
                 
                 # チェックボックス更新
                 observe({
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                       selected = choiceList,
                       
                     )
                   } else{
                     choiceList = list()
                     toggle("CheckBox_variable", condition = FALSE)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = NA,
                       choices = choiceList,
                       selected = choiceList
                     )
                   }
                 })
                 
                 # チェックボックスの全選択
                 observe({
                   req(input$check_all)
                   
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                       selected = choiceList,
                       
                     )
                   }
                 })
                 
                 # チェックボックスの全解除
                 observe({
                   req(input$uncheck_all)
                   
                   # データ列名を取得
                   column <- input$selected_variable
                   
                   # データフレームに変数が含まれるか確認
                   req (column %in% names(df()))
                   
                   # データの型に応じてチェックボックスを作成
                   x <- df()[, column]
                   xLabels <-
                     attr(dfValue()[, column], "value.labels")
                   
                   if (is.factor(x)) {
                     choiceList = levels(x)
                     values <-
                       as.integer(lapply(choiceList, function(c) {
                         if (is.na(xLabels[c])) {
                           c
                         } else{
                           xLabels[c]
                         }
                       }))
                     choiceList <- choiceList[order(values)]
                     choiceNameList <-
                       paste(values[order(values)], "：", choiceList)
                     updateCheckboxGroupInput(
                       session,
                       "checkGroup",
                       label = "",
                       choiceNames = choiceNameList,
                       choiceValues = choiceList,
                     )
                   }
                 })
               })
}

sliderGenerator <- function(id, df) {
  " Generator of checkbox based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # スライダ生成
                 output$slider_filter_ui <- renderUI({
                   # 名前空間を定義
                   ns = NS(id)
                   
                   # 計算に必要なデータを取得
                   column <- input$selected_variable
                   x <- df()[, column]
                   
                   req(!is.factor(x))
                   p(sliderInput(
                     ns("slider_filter"),
                     h5("数値範囲の絞込み"),
                     min = min(x),
                     max = max(x),
                     value = c(min(x), max(x))
                   ))
                 })
               })
}

analyticsButtonGenerator <- function(id, df) {
  " Generator of blank space on selector change."
  moduleServer(id,
               function(input, output, session) {
                 # selectorの変更に応じた記述統計ボタンの変更
                 observe({
                   req(nrow(df()) > 0)
                   
                   output$analytics_button <- renderUI({
                     # 名前空間を定義
                     ns = NS(id)
                     
                     # 計算に必要なデータを取得
                     column <- input$selected_variable
                     x <- df()[, column]
                     
                     # データの型に応じて計算
                     if (!is.factor(x)) {
                       # 非ラベル型
                       tagList(
                         p(strong("■ 変数の概観")),
                         p("ヒストグラムのbin数を指定"),
                         p(
                           sliderInput(
                             ns("slider_input_data"),
                             "",
                             min = 1,
                             max = 20,
                             value = 10
                           )
                         ),
                         p(actionButton(
                           ns("trigger_histogram"), "ヒストグラムを出力", width = "180px"), align = "center"),
                         hr(),
                         p(strong("■ 変数の代表値")),
                         p(actionButton(ns(
                           "trigger_average"
                         ), "平均値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_median"
                         ), "中央値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_max"
                         ), "最大値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_min"
                         ), "最小値", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_var"), "不偏分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_var"), "標本分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_std"), "不偏標準偏差", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_std"), "標本標準偏差", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_ste"
                         ), "標準誤差", width = "180px"), align = "center"),
                       )
                     } else {
                       # ラベル型
                       tagList(
                         p(strong("■ 変数の概観")),
                         p(actionButton(
                           ns("trigger_summary"), "カテゴリー値別に出力", width = "180px"), align = "center"),
                         hr(),
                         p(strong("■ 変数の代表値")),
                         p(actionButton(ns(
                           "trigger_average"
                         ), "平均値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_median"
                         ), "中央値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_max"
                         ), "最大値", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_min"
                         ), "最小値", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_var"), "不偏分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_var"), "標本分散", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_unbiased_std"), "不偏標準偏差", width = "180px"), align = "center"),
                         p(actionButton(
                           ns("trigger_sample_std"), "標本標準偏差", width = "180px"), align = "center"),
                         p(actionButton(ns(
                           "trigger_ste"
                         ), "標準誤差", width = "180px"), align = "center"),
                       )
                     }
                   })
                 })
               })
}

summaryGenerator <- function(id, df, dfValue) {
  " Generator of summary table on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた概観テーブル生成
                 observe({
                   req(input$trigger_summary)
                   req(nrow(df()) > 0)
                   
                   output$summary <- renderTable({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     xLabels <-
                       isolate(attr(dfValue()[, column], "value.labels"))
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # チェックボックス値を取得
                       checked <- input$checkGroup
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 集計テーブル作成
                       Categories <- levels(x)
                       Values <-
                         as.integer(lapply(Categories, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         }))
                       Frequency <-
                         as.numeric(table(x, useNA = "no"))
                       Proportioin <-
                         paste(round(as.numeric(prop.table(
                           Frequency
                         )), 3) * 100, "%")
                       data <-
                         data.frame(Values, Categories, Frequency, Proportioin)
                       
                       # 表示行を絞り込み
                       data <-
                         data[which(Categories %in% checked),]
                       
                       # ソート
                       data[order(data$Values),]
                     } else {
                       "連続値データです。"
                     }
                   })
                 })
               })
}

averageGenerator <- function(id, df, dfValue) {
  " Generator of average value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた平均値生成
                 observe({
                   req(input$trigger_average)
                   req(nrow(df()) > 0)
                   
                   output$average <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の平均を四捨五入し、一番近いラベル番号の要素を出力
                       if (length(x) > 0) {
                         stat <- round(mean(as.integer(x)))
                         stat <-
                           which(abs(as.numeric(xLabels) - stat) == min(abs(as.numeric(
                             xLabels
                           ) -
                             stat)))
                         paste(xLabels[stat], "：", names(xLabels)[stat])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(mean(x), 3)
                     }
                   })
                 })
               })
}

medianGenerator <- function(id, df, dfValue) {
  " Generator of median value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた中央値生成
                 observe({
                   req(input$trigger_median)
                   req(nrow(df()) > 0)
                   
                   output$median <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の中央値の要素を出力
                       if (length(x) > 0) {
                         stat <- median(as.integer(x))
                         paste(stat, "：", names(xLabels)[which(xLabels == stat)])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       median(x)
                     }
                   })
                 })
               })
}

maxGenerator <- function(id, df, dfValue) {
  " Generator of max value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた最大値生成
                 observe({
                   req(input$trigger_max)
                   req(nrow(df()) > 0)
                   
                   output$max <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       if (length(x) > 0) {
                         # 順序の最大値の要素を出力
                         stat <- max(as.integer(x))
                         paste(stat, "：", names(xLabels)[which(xLabels == stat)])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       max(x)
                     }
                   })
                 })
               })
}

minGenerator <- function(id, df, dfValue) {
  " Generator of min value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた最小値生成
                 observe({
                   req(input$trigger_min)
                   req(nrow(df()) > 0)
                   
                   output$min <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の最小値の要素を出力
                       if (length(x)) {
                         stat <- min(as.integer(x))
                         paste(stat, "：", names(xLabels)[which(xLabels == stat)])
                       } else{
                         paste("NA")
                       }
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       min(x)
                     }
                   })
                 })
               })
}

unbiasedVarGenerator <- function(id, df, dfValue) {
  " Generator of unbiased var value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた分散生成
                 observe({
                   req(input$trigger_unbiased_var)
                   req(nrow(df()) > 0)
                   
                   output$unbiased_var <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の不偏分散を出力
                       round(var(as.integer(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(var(x), 3)
                     }
                   })
                 })
               })
}

sampleVarGenerator <- function(id, df, dfValue) {
  " Generator of sample var value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた分散生成
                 observe({
                   req(input$trigger_sample_var)
                   req(nrow(df()) > 0)
                   
                   output$sample_var <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標本分散を出力
                       round(var(as.integer(x)) * (length(x) - 1) / length(x), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(var(x) * (length(x) - 1) / length(x), 3)
                     }
                   })
                 })
               })
}

unbiasedStdGenerator <- function(id, df, dfValue) {
  " Generator of unbiased std value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準偏差生成
                 observe({
                   req(input$trigger_unbiased_std)
                   req(nrow(df()) > 0)
                   
                   output$unbiased_std <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の不偏標準偏差を出力
                       round(sd(as.integer(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(sd(x), 3)
                     }
                   })
                 })
               })
}

sampleStdGenerator <- function(id, df, dfValue) {
  " Generator of sample std value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準偏差生成
                 observe({
                   req(input$trigger_sample_std)
                   req(nrow(df()) > 0)
                   
                   output$sample_std <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標本標準偏差を出力
                       variance <-
                         var(as.integer(x)) * (length(x) - 1) / length(x)
                       round(sqrt(variance), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       variance <-
                         var(x) * (length(x) - 1) / length(x)
                       round(sqrt(variance), 3)
                     }
                   })
                 })
               })
}

steGenerator <- function(id, df, dfValue) {
  " Generator of ste value text on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じた標準誤差生成
                 observe({
                   req(input$trigger_ste)
                   req(nrow(df()) > 0)
                   
                   output$ste <- renderText({
                     # 計算に必要なデータを取得
                     column <- isolate(input$selected_variable)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じて計算
                     if (length(x) == 0) {
                       "データがありません。"
                     } else if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       # 順序の標準誤差を出力
                       round(sd(as.integer(x)) / sqrt(length(x)), 3)
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       round(sd(x) / sqrt(length(x)), 3)
                     }
                   })
                 })
               })
}

histgramGenerator <- function(id, df, dfValue) {
  " Generator of histgram based on selector input."
  moduleServer(id,
               function(input, output, session) {
                 # action buttonに応じたヒストグラム生成
                 observe({
                   req(input$trigger_histogram)
                   req(nrow(df()) > 0)
                   
                   output$histgram <- renderPlot({
                     # プロットに必要なデータを取得
                     column <- isolate(input$selected_variable)
                     bin_number <- isolate(input$slider_input_data)
                     x <- isolate(df()[, column])
                     x <- na.omit(x)
                     
                     # データの型に応じてプロット
                     par(family = "HiraMaruProN-W4")
                     if (is.factor(x)) {
                       # valueデータを取得
                       x <- isolate(dfValue()[, column])
                       x <- na.omit(x)
                       xLabels <-
                         attr(isolate(dfValue()[, column]), "value.labels")
                       
                       # チェックボックス値を取得し、ラベル番号に変換
                       checked <- input$checkGroup
                       checked <-
                         lapply(checked, function(c) {
                           if (is.na(xLabels[c])) {
                             c
                           } else{
                             xLabels[c]
                           }
                         })
                       
                       # データを絞り込み
                       x <- x[which(x %in% checked)]
                       
                       par(mar = c(10, 4, 3, 3))
                       x <-
                         sapply(x, function(c)
                           gsub("(.{10})", "\\1\n", c)) # 10文字毎に折り返し
                       barplot(
                         table(x),
                         col = 'darkgray',
                         border = 'white',
                         main = column,
                         xlab = NA,
                         ylab = "Frequency",
                         las = 2
                       )
                     } else {
                       # スライダー値を取得
                       if (length(input$slider_filter) == 2) {
                         minLim <- input$slider_filter[1]
                         maxLim <- input$slider_filter[2]
                       } else{
                         minLim <- input$slider_filter[1]
                         maxLim <- minLim
                       }
                       
                       # データを絞り込み
                       x <- x[which(x >= minLim)]
                       x <- x[which(x <= maxLim)]
                       
                       if (is.na(var(x))) {
                         bins <- c(min(x))
                       } else{
                         bins <- seq(min(x), max(x), length.out = bin_number + 1)
                       }
                       hist(
                         x,
                         breaks = bins,
                         col = 'darkgray',
                         border = 'white',
                         main = column,
                         xlab = NA,
                         ylab = "Frequency"
                       )
                     }
                   }, height = 600, width = 400)
                 })
               })
}

statsSelector <- function(id) {
  " Select a type of stats to visualize."
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 satatsType <- reactiveVal("histgram")
                 
                 # selectorを変更
                 observe({
                   req(input$selected_variable)
                   satatsType("blank")
                 })
                 
                 # 概観ボタンを押下
                 observe({
                   req(input$trigger_summary)
                   satatsType("summary")
                 })
                 
                 # 平均値ボタンを押下
                 observe({
                   req(input$trigger_average)
                   satatsType("average")
                 })
                 
                 # 中央値ボタンを押下
                 observe({
                   req(input$trigger_median)
                   satatsType("median")
                 })
                 
                 # 最大値ボタンを押下
                 observe({
                   req(input$trigger_max)
                   satatsType("max")
                 })
                 
                 # 最小値ボタンを押下
                 observe({
                   req(input$trigger_min)
                   satatsType("min")
                 })
                 
                 # 不偏分散ボタンを押下
                 observe({
                   req(input$trigger_unbiased_var)
                   satatsType("unbiased_var")
                 })
                 
                 # 標本分散ボタンを押下
                 observe({
                   req(input$trigger_sample_var)
                   satatsType("sample_var")
                 })
                 
                 # 不偏標準偏差ボタンを押下
                 observe({
                   req(input$trigger_unbiased_std)
                   satatsType("unbiased_std")
                 })
                 
                 # 標本標準偏差ボタンを押下
                 observe({
                   req(input$trigger_sample_std)
                   satatsType("sample_std")
                 })
                 
                 # 標準誤差ボタンを押下
                 observe({
                   req(input$trigger_ste)
                   satatsType("ste")
                 })
                 
                 # ヒストグラムボタンを押下
                 observe({
                   req(input$trigger_histogram)
                   satatsType("histgram")
                 })
                 
                 # 押下した記述統計を出力
                 output$stats <- renderUI({
                   switch(
                     satatsType(),
                     'blank' = textOutput(ns('blank')),
                     'summary' = tableOutput(ns('summary')),
                     'average' = textOutput(ns('average')),
                     'median' = textOutput(ns('median')),
                     'max' = textOutput(ns('max')),
                     'min' = textOutput(ns('min')),
                     'unbiased_var' = textOutput(ns('unbiased_var')),
                     'sample_var' = textOutput(ns('sample_var')),
                     'unbiased_std' = textOutput(ns('unbiased_std')),
                     'sample_std' = textOutput(ns('sample_std')),
                     'ste' = textOutput(ns('ste')),
                     'histgram' = plotOutput(ns('histgram'))
                   )
                 })
               })
}

## 「ご利用ガイド」タブのサーバーモジュール
" Module for guide functions."

## 「JDCatメタデータカタログ」タブのサーバーモジュール
" Module for metadata functions."


## server全体の統合
server <- shinyServer(function(input, output, session) {
  ## navbar全体の制御
  
  # タブ移動
  observeEvent(input$link_to_analytics, {
    showTab("navbar", target = "② 変数分析", select = TRUE)
  })
  
  
  ## 「データの読込み」navbar
  
  # savの読み込みオブザーバー
  dfList <- filereader(id = "fileread")
  
  
  ## 「② 変数分析」navbar
  
  # ファイル名生成
  fileNameGenerator(id = "analytics",
                    spssName = dfList$spssName)
  
  # プルダウン生成
  selectorGenerator(id = "analytics",
                    dfSpss = dfList$spss)
  
  # 変数の型のテキストを生成
  typeOfVariableGenerator(id = "analytics", df = dfList$spss)
  
  # チェックボックス生成
  checkboxGenerator(id = "analytics",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)
  
  # チェックボックス生成
  sliderGenerator(id = "analytics", df = dfList$spss)
  
  # アクションボタンを生成
  analyticsButtonGenerator(id = "analytics", df = dfList$spss)
  
  # 概観を生成
  summaryGenerator(id = "analytics",
                   df = dfList$spss,
                   dfValue = dfList$spssValue)
  
  # 平均値生成
  averageGenerator(id = "analytics",
                   df = dfList$spss,
                   dfValue = dfList$spssValue)
  
  # 中央値生成
  medianGenerator(id = "analytics",
                  df = dfList$spss,
                  dfValue = dfList$spssValue)
  
  # 最大値生成
  maxGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # 最小値生成
  minGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # 不偏分散生成
  unbiasedVarGenerator(id = "analytics",
                       df = dfList$spss,
                       dfValue = dfList$spssValue)
  
  # 標本分散生成
  sampleVarGenerator(id = "analytics",
                     df = dfList$spss,
                     dfValue = dfList$spssValue)
  
  # 不偏標準偏差生成
  unbiasedStdGenerator(id = "analytics",
                       df = dfList$spss,
                       dfValue = dfList$spssValue)
  
  # 標本標準偏差生成
  sampleStdGenerator(id = "analytics",
                     df = dfList$spss,
                     dfValue = dfList$spssValue)
  
  # 標準誤差生成
  steGenerator(id = "analytics",
               df = dfList$spss,
               dfValue = dfList$spssValue)
  
  # ヒストグラム生成
  histgramGenerator(id = "analytics",
                    df = dfList$spss,
                    dfValue = dfList$spssValue)
  
  # 表示する記述統計を選択
  statsSelector(id = "analytics")
  
  
  ## 「ご利用ガイド」navbar
  
  
  ## 「JDCatメタデータカタログ」navbar

})


### アプリを実行

shinyApp(ui = ui, server = server)
