
#quanteda文本分析dashboard呈现
#加载包
library(shinydashboard)
library(quanteda)
#library(quanteda.dictionaries)
#library(ggwordcloud)
library(readtext)
library(text2vec)


#文件路径
data_path <- "D:/R_tmp/shinydashboard/quanteda-dashboard/"
#多文件读取，并指定encoding
data_raw <- readtext(paste0(data_path,"data/*.txt"),encoding = "UTF-8"
                      ,docvarsfrom = "filenames", dvsep = "_",
                      docvarnames = c("docname","isCopy")
                     )
#创建文集语料库
data_corpus <- corpus(data_raw)
#tokens
tokens_data <- tokens(data_corpus,remove_punct=T)
#情绪词典，没有中文词典。。。。
#dfm矩阵
dfm_data <- dfm(tokens_data,remove_punct=T,
                remove=stopwords("zh", source = "misc"))



ui <- dashboardPage(
  
  dashboardHeader(title = "quanteda文本分析用dashboard呈现",
                  titleWidth = 350),
  
#  dashboardSidebar----
  dashboardSidebar(#width = 350 #侧边栏宽度
            
     sidebarMenu(
              menuItem("文件信息",tabName = "rawData_tab",icon =icon("table"))
              ,
              menuItem("文档相似性分析",tabName = "docSimilarity_tab",icon =icon("image"))
              ,
              menuItem("关键词对比",tabName = "kwic_tab",icon =icon("table"))
              ,
              menuItem("关键词散布图",tabName = "xray_tab",icon =icon("image"))
              ,
              menuItem("词云",tabName = "wordcloud_tab",icon =icon("image"))
              ,
              menuItem("可视化语义网络分析",tabName = "network_tab",icon =icon("image"))
              ,
              menuItem("关键度分析",tabName = "keyness_tab",icon =icon("image"))
              
           
     )
            
          
  ),
  
  
#  dashboardBody----
  dashboardBody(
    
    tabItems(
      #数据选择、数据总体情况、token展示tab-----
      tabItem(tabName = "rawData_tab",
              fluidRow(
                column( width=6,
                #选择文件列表box
                box(title = "文件夹", status = "primary", solidHeader = TRUE,
                    collapsible = TRUE, width = NULL,height=150,
                    h4(paste0("文件路径：",data_path) ),
                    h3("Tips:文件夹下(子文件夹)所有文件都会作为数据源")
                ),
                #文件总体信息展示box
                box(title = "文件总体信息", status = "success", solidHeader = TRUE,
                    collapsible = TRUE, width = NULL,
                    tableOutput("summary")
                )
                
                ),#end column
                column(width=6,
                       #选择数量排名前N位的关键字
                       box(title="选择数量排名前N位的关键字", status = "primary", solidHeader = TRUE,
                           collapsible = TRUE, width = NULL,height=150,
                           sliderInput("fren_n",label = " ",min = 5,
                                       max = 50,
                                       value = 12)
                       ),
                       #选择文件的关键字信息
                       box(title = "所选择文件的关键字词频信息", status = "success", solidHeader = TRUE,
                           collapsible = TRUE, width = NULL,
                           tableOutput("tokens_box")
                       )
                )#end column
              )
        
      ),#end tabitem
      #文档相似性分析tab------
      tabItem(tabName = "docSimilarity_tab",
              fluidRow(
                column(width=12,
                       box(title = "文档相似性分析",status="success",
                           solidHeader=T,width=NULL,
                           plotOutput("docSimilarity",height = "600px")
                       )
                       
                )#end column
              ) #end fluridRow
        
      ),#end tabitem
      #关键字上下文分析tab------
      tabItem(tabName = "kwic_tab",
        fluidRow(
          column(width=12,
          box(title = "输入要查看的关键字",status="primary",
              solidHeader=T,width=6,height="150px",
              textInput("keyness_input",
                          label =" " )
          ),
          box(title = "选择关键字的上下文字数",status="primary",
              solidHeader=T,width=6,height="150px",
              sliderInput("keyness_n",label = " ",
                          min=1,max=10,value=3)
          )
          ),
          column(width=12,
          box(title = "关键字上下文分析",status="success",
              solidHeader=T,width=12,
              dataTableOutput("keyness_dataTable")
          )
          )
        )#end fluidRow
      )#end tabitem
      ,
      #词云tab----
      tabItem(tabName = "wordcloud_tab",
        fluidRow(
          
            box(title = "词云",status="success",
                solidHeader=T,width=NULL,
              sliderInput("wordcloud_n",label = "词云关键字出现的最少次数",
                          min=10,max=100,step=10,value=20,animate=T),
              plotOutput("wordcloud")
            )
        )#
      ),#end tabItem
      #关键字散布图----
      tabItem(tabName = "xray_tab",
              fluidRow(
              
                box(title = "关键字散布图",status="success",
                    solidHeader=T,width=NULL,height = "500px",
                    plotOutput("xray")
                )
              )#
      ),#end tabItem
      #可视化语义网络分析----
      tabItem(tabName = "network_tab",
              fluidRow(
                
                box(title = "可视化语义网络分析",status="success",
                    solidHeader=T,width=NULL,height = "500px",
                    sliderInput("network_n",label = "最频繁出现的前N个词",
                                min=10,max=100,step=10,value=20,animate=T),
                    plotOutput("network")
                )
              )#
      )#end tabItem
      ,tabItem(tabName = "keyness_tab",
               fluidRow(
                 
                 box(title = "原版和副本文件的关键度分析",status="success",
                     solidHeader=T,width=NULL,height = "700px",

                     plotOutput("keyness")
                 )
               )#
      )#end tabItem
    )#end tabitems
  )#end dashboardBody
)#end ui

# server----
server <- function(input, output) {
  #输出文件summary table
  output$summary <- renderTable({
    summary(data_corpus)
  })
  #输出前n个feature 特征
  output$tokens_box <- renderTable({
     textstat_frequency(dfm_data,n=input$fren_n)
  })
  #输出文档相似性系统聚类图
  output$docSimilarity <- renderPlot({
    tstat_dist <- as.dist(textstat_dist(dfm_data))
    clust <- hclust(tstat_dist)
    plot(clust,xlab = "文档距离",ylab = NULL)
  })
  #输出上下文关键字结果
  output$keyness_dataTable <- renderDataTable({
    keyness <- input$keyness_input #获取输入的keyness
    kwic(tokens_data,pattern=keyness,window=input$keyness_n)
  })
  #输出词云图片
  output$wordcloud <- renderPlot({
        textplot_wordcloud(dfm_data,min_count=input$wordcloud_n,
                           random_color=T,min_size=1.5,max_size=8,
                           color=c("red","darkblue","black"))
  })
  #输出关键字散布图
  output$xray <- renderPlot({
    textplot_xray(kwic(data_corpus,pattern= input$keyness_input))
  })
  #可视化语义网络分析
  output$network <- renderPlot({
    fcm <- fcm(dfm_data)
    #dim(fcm)
    feat <- names(topfeatures(fcm, input$network_n))
    fcm_select <- fcm_select(fcm,pattern = feat)
    #线条尺寸
    size <- log(colSums(dfm_select(dfm_data, feat)))
    set.seed(144)
    textplot_network(fcm_select, min_freq = 0.8, 
                     vertex_size = size / max(size) * 7)
    
  })
  #关键度分析
  output$keyness <- renderPlot({
    tstat_key <- textstat_keyness(dfm_data,
                                  target = docvars(dfm_data,"isCopy")=="副本")
    # head(tstat_key,50)
    # summary(tstat_key)
    # attr(tstat_key,"documnets") <- c("原版","副本")
    textplot_keyness(tstat_key,margin = 0.1,labelsize = 4,
                     min_count = 9,n=10,color = c("blue", "red"))
    })
}
  

shinyApp(ui, server)


































