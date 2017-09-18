library(rvest)
library(tidytext)
library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(shinydashboard)

url<-"https://www.amazon.com/product-reviews/"
url2<-"/ref=cm_cr_getr_d_paging_btm_2?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber="
Page_Number<-1:200
List<-NULL
List1<-NULL
data(stop_words)
senti_bing<-get_sentiments("bing")
senti_afinn<-get_sentiments("afinn")
senti_nrc<-get_sentiments("nrc")
senti_loughran<-get_sentiments("loughran")
######################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Amazon Reviews"),
  dashboardSidebar(
    textInput("Product", "Enter 2 ASIN values for comparison.","1501175564"),
    textInput("Product1","","1476751447"),
    br(),br(),br(),
    radioButtons("lexicon", "Lexicon Type",
                 choices = c("nrc", "bing", "afinn", "loughran"),
                 selected = "bing"),
    submitButton("Go!")
    
  ),
  dashboardBody(
    h3(textOutput("product")),
    fluidRow(tabBox(tabPanel("Sentiment Analysis", plotOutput("lexiconPlot")),
                    tabPanel("Unique Words", plotOutput("uniquePlot")), width = 12)
             
    )
  )
)

server <- function(input, output, session) {
  Product_Code<-renderText({input$Product})
  Product_Code1<-renderText({input$Product1})
  
  product_reviews<-reactive({for (i in Page_Number) {
    link<-read_html(paste0(url, Product_Code(), url2, i ))
    text<-cbind(html_text(html_nodes(link, ".review-text")))
    List<-rbind(text,List)}
    
    Reviews<-as.vector(List)
    getwords<-anti_join(unnest_tokens(data_frame(text=Reviews),word,text), stop_words)
  })
  
  product_reviews1<-reactive({for (i in Page_Number) {
    link1<-read_html(paste0(url, Product_Code1(), url2, i ))
    text1<-cbind(html_text(html_nodes(link1, ".review-text")))
    List1<-rbind(text1,List1)}
    
    Reviews1<-as.vector(List1)
    getwords1<-anti_join(unnest_tokens(data_frame(text=Reviews1),word,text), stop_words)  })
  

  
  
  
  bingfinal<-reactive({
    bing<-inner_join(product_reviews(), senti_bing)
  bing<-count(bing, index = row_number() %/% 50, sentiment)
  bing<-spread(bing, sentiment, n, fill = 0 )
  bing<-mutate(bing, sentiment=positive-negative)
  bing<-mutate(bing, method = "BING")
  bing<-mutate(bing,name = Product_Code())
  })
  
  afinnfinal<-reactive({
  afinn<-inner_join(product_reviews(), senti_afinn)
  afinn<-group_by(afinn, index = row_number() %/% 50)   
  afinn<-summarise(afinn, sentiment = sum(score)) 
  afinn<-mutate(afinn, method = "AFINN")
  afinn<-mutate(afinn, name = Product_Code())
  })
  
  nrcfinal<-reactive({
  nrc<-inner_join(product_reviews(), senti_nrc)
  nrc<-filter(nrc,sentiment %in% c("positive", "negative"))
  nrc<-mutate(nrc, method = "NRC")
  nrc<-count(nrc, method, index = row_number() %/% 50, sentiment)
  nrc<-spread(nrc, sentiment, n, fill = 0)
  nrc<-mutate(nrc, sentiment = positive - negative)
  nrc<-mutate(nrc, name = Product_Code())
  })
  
  loughranfinal<-reactive({
  loughran<-inner_join(product_reviews(), senti_loughran)
  loughran<-filter(loughran,sentiment %in% c("positive", "negative"))
  loughran<-mutate(loughran, method = "loughran")
  loughran<-count(loughran, method, index = row_number() %/% 50, sentiment)
  loughran<-spread(loughran, sentiment, n, fill = 0)
  loughran<-mutate(loughran,sentiment = positive - negative)
  loughran<-mutate(loughran, name = Product_Code())
  })
  
  bingfinal1<-reactive({
  bing1<-inner_join(product_reviews1(), senti_bing)
  bing1<-count(bing1, index = row_number() %/% 50, sentiment)
  bing1<-spread(bing1, sentiment, n, fill = 0 )
  bing1<-mutate(bing1, sentiment=positive-negative)
  bing1<-mutate(bing1, method = "BING")
  bing1<-mutate(bing1,name = Product_Code1())
  })
  
  afinnfinal1<-reactive({
  afinn1<-inner_join(product_reviews1(), senti_afinn)
  afinn1<-group_by(afinn1, index = row_number() %/% 50)   
  afinn1<-summarise(afinn1, sentiment = sum(score)) 
  afinn1<-mutate(afinn1, method = "AFINN")
  afinnfinal1<-mutate(afinn1, name = Product_Code1())
  })
  
  nrcfinal1<-reactive({
    nrc1<-inner_join(product_reviews1(), senti_nrc)
    nrc1<-filter(nrc1, sentiment %in% c("positive", "negative"))
    nrc1<-mutate(nrc1, method = "NRC")
    nrc1<-count(nrc1, method, index = row_number() %/% 50, sentiment)
    nrc1<-spread(nrc1, sentiment, n, fill = 0)
    nrc1<-mutate(nrc1, sentiment = positive - negative)
    nrc1<-mutate(nrc1, name = Product_Code1())
  })
  
  loughranfinal1<-reactive({
    loughran1<-inner_join(product_reviews1(), senti_loughran)
    loughran1<-filter(loughran1,sentiment %in% c("positive", "negative"))
    loughran1<-mutate(loughran1, method = "loughran")
    loughran1<-count(loughran1, method, index = row_number() %/% 50, sentiment)
    loughran1<-spread(loughran1, sentiment, n, fill = 0)
    loughran1<-mutate(loughran1,sentiment = positive - negative)
    loughran1<-mutate(loughran1, name = Product_Code1())
  })
  
  plot<-reactive({
    if (input$lexicon=="bing")
      { return(bingfinal())}
    if (input$lexicon=="afinn")
    { return(afinnfinal())}
    if (input$lexicon=="nrc")
    { return(nrcfinal())}
    if (input$lexicon=="loughran")
    { return(loughranfinal())}
  })
  
  plot1<-reactive({
    if (input$lexicon=="bing")
    { return(bingfinal1())}
    if (input$lexicon=="afinn")
    { return(afinnfinal1())}
    if (input$lexicon=="nrc")
    { return(nrcfinal1())}
    if (input$lexicon=="loughran")
    { return(loughranfinal1())}
  })
  
  output$lexiconPlot<-renderPlot({


      ggplot(bind_rows(plot(), plot1()), aes(index, sentiment, fill = name) )+
      geom_col(show.legend = FALSE)+
      facet_wrap(~name, ncol = 2, scales = "free_y")
  })
  
  output$uniquePlot<-renderPlot({
    
    tidy_Reviews<-mutate(product_reviews(), name = Product_Code())
    tidy_Reviews1<-mutate(product_reviews1(), name= Product_Code1())
    tidy_Reviews_Final<-rbind(tidy_Reviews, tidy_Reviews1)
    review_words<-ungroup(count(tidy_Reviews_Final, name, word, sort = TRUE)) 
    total_review_words <- summarize(group_by(review_words, name), total=sum(n)) 
    
    final <- left_join(total_review_words, review_words)
    final <- bind_tf_idf(final, word, name, n)
    final <- select(final, -total)
    final <-arrange(final, desc(tf_idf))
    
    plot_review <- mutate(arrange(final, desc(tf_idf)), word = factor(word, levels = rev(unique(word))))
    
    plot_review<-ungroup(top_n(group_by(plot_review),15))
    
    ggplot(plot_review, aes(word, tf_idf, fill = name)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(~name, ncol = 2, scales = "free") +
      coord_flip()
  })
}



shinyApp(ui, server)
######################################################################################
