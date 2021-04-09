#SERVER.R
# Server is a function used to render the objects created in the User Interface function of the shiny Application
# It takes the input and output as an argument
#shinyApp(ui=ui,server=server)

server <- function(input, output,session) {
  
  # update_material_text_box(
  # session,
  # input_id = "example_text_box",
  # value = "#saudi"
 # )
 material_spinner_show(session, output_id = "wholeApp")
  df <- reactive({
    #df <- GetTweetsBySearch("COIVD19",n=4000)
    #save(df,file="df.RData")
    load("df.RData")
    df=df
    #df <- GetTweetsBySearch("#saudi",n=1000)
    #df <- PreprocessTweets(df)
    #save(df,file="df.RData")
  })
 #  # update_material_dropdown(
 #  #   session,
 #  #   input_id = "rtrep_dropdown",
 #  #   value = "Retweet"
 #  # )
 #  
  output$network_proxy_nodes <- renderVisNetwork({
   material_spinner_show(session, "network_proxy_nodes")
    #
   # df <- GetTweetsBySearch(input$example_text_box,n=input$example_slider)
    #df <- GetTweetsBySearch("#saudi",n=1000)
    # load("df.RData")
    # df <- PreprocessTweets(df)
    load("df.RData")
    df=df
 #    #load("dfKashogjji.Rda")
 #    #df=dfKashogjji
     #df=df()
 material_spinner_hide(session,"network_proxy_nodes")
     countTweets <- GetTweetCountTable(df, "from_user")
    countRetweets <- GetTweetCountTable(df, "retweet_from")
    countReplies <- GetTweetCountTable(df, "reply_to")
    counts <- merge(countTweets, countRetweets, by = "user", all.x = TRUE)
    counts <- merge(counts, countReplies, by = "user", all.x = TRUE)
    colnames(counts) <- c("user", "tweets", "replied_to", "retweeted_by")
    counts[is.na(counts)] <- 0
    require(reshape2)
    # melt data
    counts.melt <- melt(counts, id.vars = c("user"))

 #    # to make it mor clear plot we plot the first 15 
     counts.melt <- melt(counts[order(counts$tweets,decreasing = TRUE)[1:15],], id.vars = c("user"))
 #    #
 #   # if (input$rtrep_dropdown=="Retweet"){
     rt.df <- CreateSNADataFrame(df, from = "from_user", to = "retweet_from", linkNames = "rt")
       g <- graph.data.frame(rt.df, directed = TRUE)
      
     mat <- get.adjacency(g)
 #      
    user.names=mat@Dimnames[[1]]
      #profile=c()
      #for(i in 1:length(user.names)){
       # profile[i]=df$profileImageURL[which(df$screen_name==user.names[i])[1]]
     # }
      material_spinner_hide(session,"network_proxy_nodes")
      #matchdata=data.frame(user.names=user.names,profile=as.character(profile))

      nodes <- data.frame(id =user.names)
     # nodesimage <- data.frame(id = matchdata$user.names, shape = c("circularImage"),image =paste0(matchdata$profile, ""))

      material_spinner_hide(session,"network_proxy_nodes")
     edges <- data.frame(from = df$from_user,
                        to = df$retweet_from)
    edgesnona=edges[-which(is.na(edges$to)),]

 #    ########
 #    
material_spinner_hide(session,"network_proxy_nodes")
 #    
    visNetwork(nodes, edgesnona) %>%
      visIgraphLayout() %>%
      visNodes( font="14px Cairo #141D38",size = 16,borderWidth=3,color = list(background="#EAEAEA",border="#243F8F",highligh=list(background="#A3C7E8",border="#243F8F"),hover=list(background="#A3C7E8",border="#243F8F"))) %>%
       visInteraction(hover=TRUE)%>%
       visEdges(shadow = TRUE,
               arrows =list(to = list(enabled = TRUE, scaleFactor =1)),
               color = list(color = "lightblue", highlight = "red"))%>%
      visOptions(highlightNearest = list(enabled = T, hover = T),
                nodesIdSelection = T)%>% 
      visLayout(randomSeed = 12) %>% 
      visInteraction(navigationButtons = TRUE)
  })
 # # output$title <- renderText(input$example_text_box)
  output$title <- renderText({
 #  # paste("SNA for Hashtag: ",input$example_text_box)
    "Corona Virus COVID-19"
    })
 # 
 output$dis <- renderPlot({
 require(ggplot2)
 require(reshape2)
   df=df()
   material_spinner_hide(session,"dis")
 countTweets <- GetTweetCountTable(df, "from_user")
 countRetweets <- GetTweetCountTable(df, "retweet_from")
 countReplies <- GetTweetCountTable(df, "reply_to")
 material_spinner_hide(session,"dis")
 # quickly check distribution of tweets per user
 p=qplot(countTweets$count, binwidth = 0.5, xlab = "Number of Tweets",fill=I("#8282bf"))
 p + theme_minimal()
 })

 output$dis2 <- renderPlot({
   require(ggplot2)
   require(reshape2)
   df=df()
   material_spinner_hide(session,"dis2")
   countTweets <- GetTweetCountTable(df, "from_user")
   countRetweets <- GetTweetCountTable(df, "retweet_from")
   countReplies <- GetTweetCountTable(df, "reply_to")
   counts <- merge(countTweets, countRetweets, by = "user", all.x = TRUE)
   counts <- merge(counts, countReplies, by = "user", all.x = TRUE)
   colnames(counts) <- c("user", "tweets", "replied_to", "retweeted_by")
   counts[is.na(counts)] <- 0

   # melt data
   counts.melt <- melt(counts, id.vars = c("user"))

   # to make it mor clear plot we plot the first 15
   counts.melt <- melt(counts[order(counts$tweets,decreasing = TRUE)[1:15],], id.vars = c("user"))

  material_spinner_hide(session,"dis2")
  # quickly check distribution of tweets per user
  p= ggplot(counts.melt, aes(x = user, y = value, color = variable)) + geom_point() +
    coord_flip() + ylab("Counts") +
    xlab("Users")
  #p+scale_color_manual(values=c("#8282bf","#8d79b4","#7683ae"))
  p + theme_minimal()
})

# output$dis3 <- renderPlot({
#   material_spinner_show(session, "dis3")
#   require(ggplot2)
#   require(reshape2)
#   df=df()
#   material_spinner_hide(session,"dis3")
#   countTweets <- GetTweetCountTable(df, "from_user")
#   countRetweets <- GetTweetCountTable(df, "retweet_from")
#   countReplies <- GetTweetCountTable(df, "reply_to")
#   counts <- merge(countTweets, countRetweets, by = "user", all.x = TRUE)
#   counts <- merge(counts, countReplies, by = "user", all.x = TRUE)
#   colnames(counts) <- c("user", "tweets", "replied_to", "retweeted_by")
#   counts[is.na(counts)] <- 0
# 
#   # melt data
#   counts.melt <- melt(counts, id.vars = c("user"))
# 
#   # to make it mor clear plot we plot the first 15
#   counts.melt <- melt(counts[order(counts$tweets,decreasing = TRUE)[1:15],], id.vars = c("user"))
# 
#   material_spinner_hide(session,"dis3")
# 
#   counts$ratio <- counts$retweeted_by/counts$tweets
# 
#   # plot ratio for users who have at least one rt
#   p=ggplot(counts[counts$retweeted_by > 0, ], aes(x = reorder(user, ratio), y = ratio)) +
#     geom_point() + coord_flip() + xlab("Users") +
#     ylab("Retweets/Tweets ratio")
#      p + theme_minimal()
# })

output$dis4 <- renderPlot({
  df=df()
  material_spinner_hide(session,"dis4")

  countLinks <- GetTweetCountTable(df, "links")
  names(countLinks)[1] <- "url"

  # check top links
  #head(countLinks[with(countLinks, order(-count)), ])
  material_spinner_hide(session,"dis4")
  # plot to see distribution of links
  p= ggplot(countLinks[countLinks$count > 1, ], aes(reorder(url, count), count)) +
    geom_point() + coord_flip() + xlab("URL") + ylab("Number of messages containing the URL")

  p + theme_minimal()

})

output$dis4 <- DT::renderDataTable({
  df=df()
  material_spinner_hide(session,"dis4")
  countLinks <- GetTweetCountTable(df, "links")
  names(countLinks)[1] <- "url"
  #df1=countLinks%>%
  Action=c()
  for(i in 1:dim(countLinks)[1]){
 Action[i] = HTML(paste('<a class="go-map" href=', countLinks$url[i]," ", "target='_blank'> Link <i class='material-icons'>open_in_new</i></a>", sep=""))
  }
 #action <- DT::dataTableAjax(session, df1)
  #options = list(ajax = list(url =action )
  material_spinner_hide(session,"dis4")
 countLinks$Link=Action
 DT::datatable( countLinks, escape = FALSE,rownames = FALSE)

})

# output$dis5 <-renderPlot({
#   material_spinner_show(session, "dis5")
#   df=df()
#   material_spinner_hide(session,"dis5")
#   corpus <- ConstructCorpus(df$text, removeTags = TRUE, removeUsers = TRUE)
#   td.mat <- TermDocumentMatrix(corpus, control = list(minWordLength = 3))
#   freq.terms <- findFreqTerms(td.mat, lowfreq = 5)
#   term.freq <- rowSums(as.matrix(td.mat))
#   term.freq <- subset(term.freq, term.freq >= 5)
#   df2 <- data.frame(term = names(term.freq), freq = term.freq)
#   material_spinner_hide(session,"dis5")
#   wordcloud::wordcloud(df2$term,df2$freq,random.order=FALSE, rot.per=0.35,
#                        colors=brewer.pal(8, "Dark2"))
# })

output$dis6 <- renderPlot({
  df=df()
  material_spinner_hide(session,"dis6")

  scores <- ScoreSentiment(df$text, .progress = "text")
  material_spinner_hide(session,"dis6")
  p= ggplot(scores, aes(x = score)) + geom_histogram(binwidth = 1,fill=I("#8282bf")) + xlab("Sentiment score") +
    ylab("Frequency")
  p + theme_minimal()

})
#material_spinner_hide(session, output_id = "wholeApp")
#plot of chunk counttables
 # observe({
 #  visNetworkProxy("network_proxy_nodes") %>%
 #visFocus(id = input$Focus, scale = 4)
 # })

 # observe({
 #   nodes_selection <- input$example_dropdown
 #   visNetworkProxy("network_proxy_select") %>%
 #     visSelectNodes(id = nodes_selection)

 #observe({
  # visNetworkProxy("network_proxy_nodes") %>%
   #  visFocus(id = input$example_dropdown, scale = 0.5,animation=list(duration = 500, easingFunction = "easeInOutQuad")) %>%
  # visSelectNodes(id = input$example_dropdown)

 #})

 ################################################## News paper

 # update_material_text_box(
 #   session,
 #   input_id = "search1",
 #   value = "corona"
 # )
 
 searches <- reactive({
   
   searches = str_replace_all(input$search1, ' ', '%20')
   
 }) 
 
 update_material_dropdown(
   session,
   input_id = "showtype",
   value = "CONTENT"
 )
 #  
 output_tabs<- reactive({
   
   output_tabs=input$showtype
   
 }) 
 
 update_material_text_box(
   session,
   input_id = "domain",
   value = ""
 )
 
 domains <- reactive({
   
   domains = input$domain
   
 }) 
 
 update_material_date_picker(
   session,
   input_id = "sdate"
 )

 Sdate <- reactive({

   Sdate = "sdate"

 })
 
 update_material_date_picker(
   session,
   input_id = "edate",
   value = ""
 )
 
 Edate <- reactive({
   
   Edate = input$edate
   
 }) 
 
 update_material_dropdown(
   session,
   input_id = "sort1",
   value = sort_options[1]
 )
 #  
 sort<- reactive({
   
   sort=input$sort1
   
 }) 
 # update_material_dropdown(
 #   session,
 #   input_id = "Sourcelanguage",
 #   value = ""
 # )
 # #  
 # Country<- reactive({
 #   
 #   Country=input$Sourcelanguage
 #   
 # }) 
 #        str_replace(link_url, '&times', '&amp;times'), "</a>")})
 output$frame <- renderUI({
   search="corona"
   domain=domains()
   if(domain != ''){
     domains = str_split(domain, ',')[[1]]
     search = paste0(search, '%20', format_search_str(domains, 'domainis'))
   }    
   # 
   #country=Country()
   # if(length(country) > 0){
   # search = paste0(search, '%20', format_search_str(country, 'sourcecountry'))
   # }
   
   #source_lang="English"
   #if(length(source_lang) > 0){
   # search = paste0(search, '%20', format_search_str(source_lang, 'sourcelang'))
   #}
   ##########
   
   output_tab=output_tabs()
   ###### for geo 
   geo_mode=geo_modes[1]
   geo_format=""
   if(output_tab == 'GEO24'){ # GEO MODE
     content_mode=content_modes[2]
     url = paste0(geo_root, search, '&mode=', geo_mode)
     if(geo_format != '') url = paste0(url, '&format=', geo_format) # output format
   }
   
   if(output_tab == 'TV'){ # TV MODE
     tvmode=c("clipgallery","timelinevol")
     url = paste0(tv_root, search,'%20market:%22National%22', '&mode=',tvmode[1] )
     #if(geo_format != '') url = paste0(url, '&format=', geo_format) # output format
   }
   #### for Content and timespan
   shinyjs::enable("daterange")
   ############
   if(output_tab=='CONTENT'|output_tab=='TIMELINE'){
     # timespan = ''
     # if(timespan == ''){       # period defined by date range
     #   end_date = ifelse(input$daterange[2] == as.character(today()),
     #                     format(Sys.time(), '%Y%m%d%H%M%S'), 
     #                     format(as.Date(input$daterange[2]), paste0('%Y%m%d', '235959')))
     #   start_date = format(as.Date(input$daterange[1]), paste0('%Y%m%d', '000000'))
     # }else{                          # period defined in past hours/days/weeks/months
     #   timespan = paste0('&timespan=', timespan)
     # }
     
     url = paste0(root, search)
     #search_lang="English"
     #if(search_lang != '') url = paste0(url, '&searchlang=', search_lang)
     
     #content
     #output_tab='CONTENT'
     content_mode=content_modes[2]
     data_sort=sort()
     if(output_tab == 'CONTENT'){
       url = paste0(url, '&mode=', content_mode)
       if(data_sort != '')  url = paste0(url, '&sort=', data_sort) # sort argument
     }
     
     #timline 
     #output_tab == 'TIMELINE'
     timeline_mode= timeline_modes[1]
     smooth=3
     if(output_tab == 'TIMELINE'){
       url = paste0(url, '&mode=', timeline_mode, '&timelinesmooth=', smooth)
     }
     max_records=200
     url = paste0(url, '&maxrecords=', max_records)
     data_format="HTML"
     if(data_format != '') url = paste0(url, '&format=', data_format)
     #if(timespan != ''){
     # default hours
     # url = paste0(url, timespan)
     # } else{ 
     # if(!all(is.na(input$daterange))){
     #if(Sdate()==""&&Edate()==""){
       start_date=format(as.Date("01 Jan,2020",format = "%d %B, %Y"), paste0('%Y%m%d', '000000'))
       end_date = format(as.Date(Sys.time(),format = '%Y%m%d%H%M%S'), paste0('%Y%m%d', '235959'))
  
       url = paste0(url, '&startdatetime=',  start_date, '&enddatetime=', end_date)
    # }
     # }
   }
   #######   # append args to URL in browser
   url_args = paste0('?', str_extract(url, '[a-z]+.[a-z]+[?]query=.*'))
   #js$pageURL(url_args)
   link_url = ifelse(content_mode %in% c('ArtList', 'ArtGallery'), url, url) # translate
   HTML(paste0("<a id='gdelt_url' href='", link_url), "' target='_blank'>", 
        str_replace(link_url, '&times', '&amp;times'), "</a>")
   #output$gdelt_url = renderUI({ HTML(paste0("<a id='gdelt_url' href='", link_url), "' target='_blank'>", 
   
   w =  650
   h = 776   
   tags$iframe(src = url, width = w, height = h)
 })

 ############## tv 
 
 # update_material_text_box(
 #   session,
 #   input_id = "search2",
 #   value = "saudi"
 # )
 
 # searches2 <- reactive({
 #   
 #   searches2 = str_replace_all(input$search2, ' ', '%20')
 #   
 # }) 
 
 update_material_dropdown(
   session,
   input_id = "tvmodes",
   value = tv_modes[1]
 )
 #  
 tvmodess<- reactive({
   
   tvmodess=input$tvmodes
   
 }) 
 
 update_material_date_picker(
   session,
   input_id = "sdate2"
 )
 
 Sdate2 <- reactive({
   
   Sdate2 = input$sdate2
   
 }) 
 
 update_material_date_picker(
   session,
   input_id = "edate2",
   value = ""
 )
 
 Edate2 <- reactive({
   
   Edate2 = input$edate2
   
 }) 
 
 update_material_dropdown(
   session,
   input_id = "sort2",
   value = sort_options_tv[1]
 )
 #  
 sort2<- reactive({
   
   sort2=input$sort2
   
 }) 
 
 update_material_slider(
   session,
   input_id = "smooth2",
   value = 10
 )
 #  
 smooth2<- reactive({
   
   smooth2=input$smooth2
   
 }) 
 
 output$frame2 <- renderUI({
   #search2=searches2()
     tvmode=tvmodess()
     url = paste0(tv_root, "corona",'%20market:%22National%22', '&mode=',tvmode )
    
   #### for Content and timespan
   shinyjs::enable("daterange")
   ############
     data_sort=sort2()
       if(data_sort != '')  url = paste0(url, '&sort=', data_sort) # sort argument
     
     max_records=50
     url = paste0(url, '&maxrecords=', max_records)
     data_format="HTML"
     if(data_format != '') url = paste0(url, '&format=', data_format)
    # if(Sdate2()!=""&&Edate2()!=""){
     start_date2=format(as.Date("01 Jan,2020",format = "%d %B, %Y"), paste0('%Y%m%d', '000000'))
     end_date2 = format(as.Date(Sys.time(),format = '%Y%m%d%H%M%S'), paste0('%Y%m%d', '235959'))
     
         url = paste0(url, '&STARTDATETIME=', start_date2, '&ENDDATETIME=', end_date2)
     #}
     smoth22=smooth2()
     url = paste0(url, '&timelinesmooth=',smoth22)
   #######   # append args to URL in browser
   url_args = paste0('?', str_extract(url, '[a-z]+.[a-z]+[?]query=.*'))
   #js$pageURL(url_args)
   link_url = url# translate
   HTML(paste0("<a id='gdelt_url' href='", link_url), "' target='_blank'>", 
        str_replace(link_url, '&times', '&amp;times'), "</a>")
   #output$gdelt_url = renderUI({ HTML(paste0("<a id='gdelt_url' href='", link_url), "' target='_blank'>", 
   
   w =  650
   h = 776   
   tags$iframe(src = url, width = w, height = h)
 })
 ###################################################
 # datAnal 
 ##############
 dataPays<- reactive({
   if(!is.null(input$choices)){
     if(input$choices == "Cases"){
       return( dataCases)}
       
     if(input$choices == "Deaths"){
       return( dataDeaths)}
     
     if(input$choices == "Recovered"){
       return( dataRecovered)}
     
   }
 })
 
 maxTotal<- reactive( max(dataPays()%>%select(-Pop)%>%select_if(is.numeric), na.rm = T)
 )
 maxTotalPrevalence<- reactive(max(dataPays()%>%select(-Pop)%>%select_if(is.numeric)%>%mutate_all(function(x) x/dataPays()$Pop*100000), na.rm = T)
 )
 
 Top5<-reactive( dataPays()$Pays[order(dataPays()[,dim(dataPays())[2]]%>%unlist(),decreasing = T)][1:5]
 )
 
 output$figg <- renderPlot({
   
   ######
   levels(dataraw$Country)[levels(dataraw$Country)=="United States of America"]="USA"
   levels(dataraw$Country)[levels(dataraw$Country)=="United Kingdom"]="UK"
   datarawor=dataraw[order(dataraw$Cases,decreasing = TRUE),]
   datarawl=datarawor[1:10,]
   datarawl=datarawl %>%
     mutate(Country = fct_reorder(Country, desc(Cases)))
   angle = 1:10 * 360/10
   angle11=c((angle[1:5]-90),(angle[6:10]+90))
   #dd = dd[1:20, ]
   #dd$country = factor(dd$country, levels=dd$country)
   
   #dd$angle = 1:20 * 360/20
   
   p <-ggplot(datarawl, aes(Country, Cases, fill=Cases)) + 
     geom_col(width=1, color='grey90') + 
     geom_col(aes(y=I(5)), width=1, fill='grey90', alpha = .2) +       
     geom_col(aes(y=I(3)), width=1, fill='grey90', alpha = .2) +    
     geom_col(aes(y=I(2)), width=1, fill = "white") +
     scale_y_log10() + 
     scale_fill_gradientn(colors=c("darkgreen", "green", "orange", "firebrick","red"), trans="log") + 
     geom_text(aes(label=paste(Country, Cases, sep="\n"), 
                   y = Cases *.1, angle=angle11),
               size=3, color = "white", fontface="bold", vjust=1)  + 
     # geom_text(aes(label=paste0(cum_confirm, " cases ", country), 
     #               y = max(cum_confirm) * 2, angle=angle+90),
     #           size=3, vjust=0) + 
     coord_polar(direction=-1) + 
     theme_void() + 
     theme(legend.position="none") 
   ####
   print(p)
 })
 
 output$map <- renderLeaflet({
   leaflet(data = countries) %>%
     addTiles(
       urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
       attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
     ) %>%
     
     setView(0, 30, zoom = 2)
   
   
 })
 
 pal <- reactive(colorNumeric(c("#f8f4ec" ,rev(inferno(400))), domain = c(0,log(arrondi(maxTotal())))))
 
 pal2 <- reactive(colorNumeric(c("#f8f4ec" ,rev(inferno(400))), domain = c(0,log(arrondi(maxTotalPrevalence())))))
 
 observe({
    if(input$choices == "Cases"){casesDeath="Cases"}
     if(input$choices == "Deaths"){casesDeath="Deaths"}
   if(input$choices == "Recovered"){casesDeath="Recovered"}
       #,"Recovered")
   if (!is.null(input$day1)) {
     indicator<-format.Date(input$day1, "%m/%d/%y")
     
   }else{
     indicator = format.Date(max(jourDate), "%m/%d/%y")
   }
   
   
   if (!is.null(input$day2)) {
     indicator2<-format.Date(input$day2-c(1,0), "%m/%d/%y")
     
   }else{
     indicator2 =format.Date(c(min(jourDate)-1,max(jourDate)), "%m/%d/%y")
   }
   
   if(is.null(input$variable)){
     
   }else{
     variable<- input$variable
     
     if(variable =="Total cases/population"){
       # nCases
       countries2 <- merge(countries,
                           dataPays(),
                           by.x = "NAME",
                           by.y = "Pays",
                           sort = FALSE)
       country_popup <- paste0("<strong>Country: </strong>",
                               countries2$NAME,
                               "<br><strong>",
                               "Total cases/population :",
                               
                               
                               " </strong>",
                               round(countries2[[indicator]]/countries2$Pop*100000,2)," /100 000")
       
       
       leafletProxy("map", data = countries2)%>%
         addPolygons(fillColor = pal2()(log((countries2[[indicator]]/countries2$Pop*100000)+1)),
                     layerId = ~NAME,
                     fillOpacity = 1,
                     color = "#BDBDC3",
                     weight = 1,
                     popup = country_popup)
       
     }else if(variable =="Total cases"){
       countries2 <- merge(countries,
                           dataPays(),
                           by.x = "NAME",
                           by.y = "Pays",
                           sort = FALSE)
       country_popup <- paste0("<strong>Country: </strong>",
                               countries2$NAME,
                               "<br><strong>",
                               "Total ",casesDeath," :",
                               
                               
                               " </strong>",
                               round(countries2[[indicator]],2))
       
       
       leafletProxy("map", data = countries2)%>%
         addPolygons(fillColor = pal()(log((countries2[[indicator]])+1)),
                     fillOpacity = 1,
                     layerId = ~NAME,
                     color = "#BDBDC3",
                     weight = 1,
                     popup = country_popup)
       
       
     }else if(variable =="New cases over period"){
       
       dataPaysSel<-dataPays()%>%select(Pays, Pop)
       if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
         
         dataPaysSel$ncases<-dataPays()[,indicator2[2]]
       }else{
         dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
         
       }
       
       # nCases
       countries2 <- merge(countries,
                           dataPaysSel,
                           by.x = "NAME",
                           by.y = "Pays",
                           sort = FALSE)
       country_popup <- paste0("<strong>Country: </strong>",
                               countries2$NAME,
                               "<br><strong>",
                               "New ",casesDeath," over period :",
                               
                               
                               " </strong>",
                               countries2$ncases)
       
       leafletProxy("map", data = countries2)%>%
         addPolygons(fillColor = pal()(log(countries2$ncases+1)),
                     fillOpacity = 1,
                     color = "#BDBDC3",
                     layerId = ~NAME,
                     weight = 1,
                     popup = country_popup)
     }else{
       
       dataPaysSel<-dataPays()%>%select(Pays, Pop)
       if(indicator2[1] == format.Date(min(jourDate)-1, "%m/%d/%y")){
         
         dataPaysSel$ncases<-dataPays()[,indicator2[2]]
       }else{
         dataPaysSel$ncases<-dataPays()[,indicator2[2]]-dataPays()[,indicator2[1]]
         
       }
       
       # nCases
       countries2 <- merge(countries,
                           dataPaysSel,
                           by.x = "NAME",
                           by.y = "Pays",
                           sort = FALSE)
       country_popup <- paste0("<strong>Country: </strong>",
                               countries2$NAME,
                               "<br><strong>",
                               "New ",casesDeath," over period / population :",
                               
                               
                               " </strong>",
                               round(countries2$ncases/countries2$Pop*100000,2)," /100 000")
       
       leafletProxy("map", data = countries2)%>%
         addPolygons(fillColor = pal2()(log(countries2$ncases/countries2$Pop*10000+1)),
                     fillOpacity = 1,
                     color = "#8283BB",
                     layerId = ~NAME,
                     weight = 1,
                     popup = country_popup)
       
       
       
     }
     
     
     
   }
   
 } )
 
 observe({
   
   
   if(is.null(input$variable)){
     
   }else{
     variable<- input$variable
     
     proxy <- leafletProxy("map", data = countries)
     
     
     # Remove any existing legend, and only if the legend is
     # enabled, create a new one.
     proxy %>% clearControls()
     if (input$legend) {
       if(variable %in% c("Total cases/population","New cases over period/population")){
         proxy %>% addLegend(position = "bottomright",
                             pal = pal2(),opacity = 1,
                             bins = log(10^(seq(0,log10(arrondi(maxTotalPrevalence())),0.5))),
                             value = log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                             data =log(1:10^(log10(arrondi(maxTotalPrevalence())))),
                             labFormat = labelFormat(transform = function(x) round(exp(x)) ,suffix = " /100 000")
                             
         )
         
       }else{
         
         
         
         proxy %>% addLegend(position = "bottomright",
                             pal = pal(),opacity = 1,
                             bins = log(10^(0:log10(arrondi(maxTotal())))),
                             value = log(1:10^(log10(arrondi(maxTotal())))),
                             data = log(10^(0:log10(arrondi(maxTotal())))),
                             labFormat = labelFormat(transform =  exp )
                             
         )
       }
     }
   }
 })
 #### raw data
 output$disraw <- DT::renderDataTable({
   datarawor=dataraw[order(dataraw$Cases,decreasing = TRUE),]
   DT::datatable( datarawor, escape = FALSE,rownames = FALSE)
   
 })
 ###
 
 
   output$Slider<-renderUI({
     
     if(is.null(input$variable)){
       
     }else{
       if(input$variable %in% c("Total cases", "Total cases/population")){
         sliderInput("day1", "Day", min(jourDate), max(jourDate),
                     value =  c(max(jourDate)),animate = T, step = 1
                     
                     #min(jourDate),
         )}else{
           sliderInput("day2", "Day", min(jourDate), max(jourDate),
                       value =  c(max(jourDate)-7,max(jourDate)),animate = T, step = 1
                       
                       #min(jourDate),
           )
           
         }
     }
   })
   
   output$selection <- renderUI({
     if(input$choices =="Cases"){
       radioButtons("variable", choices =  c("New cases over period",
                                             "New cases over period/population","Total cases", 'Total cases/population' ),
                    label = "Indicator")
     }else{
       radioButtons("variable", choices =  list("Number over period"="New cases over period",
                                                "Number over period/population"="New cases over period/population",
                                                "Total Number"="Total cases",
                                                'Total Number/population'='Total cases/population' ),
                    label = "Indicator")
       
       
     }
     
   })
   output$plotEvol<-renderUI({
     if (input$plotEvolT) {
       tagList(absolutePanel(
         id = "name",
         class = "panel panel-credits",
         top = 10,
         right  = 10,
         plotlyOutput(outputId = "evol",width = "600px")
         ,
         actionButton("reset", "Clear graph")
       ))
     }
   })
   output$evol <-renderPlotly({
     df_evo<- dataPays()%>%filter(Pays%in% Top5())%>%pivot_longer(cols = -c(Pays,Pop),
                                                                  values_to = "Cases",names_to = "Date")%>%
       mutate(Date= lubridate::parse_date_time(Date, orders = c("mdy")))
     
     plot_ly(df_evo,x = ~Date, y = ~Cases, color = ~Pays, type = "scatter",mode = "lines")%>%
       layout(yaxis = list(type = "log"))
     
     
     
   })
   
   
############## GRP
   get_data <- reactive({
     all_data <- all_data %>%
       mutate(days = as.numeric((date - data_start))) %>%
       filter(value > input$cases_limit || type == "deceased")

     start_dataset <- all_data %>%
       group_by(`Country/Region`) %>%
       summarise(minvalue = min(value),
                 onset = min(days))

     all_data %>% dplyr::left_join(start_dataset) %>%
       mutate(matched_days = days - onset) %>%
       mutate(lvalue = log(value + 1))
   })
   # 
   get_max_day_value <- reactive({
     get_data() %>% pull(matched_days) %>% max()
   })
   # 
   # 
   update_material_dropdown(
     session,
     input_id = "country_selector",
     value = "Saudi Arabia"
   )
   
   country_selectorr<- reactive({
     
     country_selectorr=input$country_selector
     
   }) 
   output$distPlot <- renderPlot({
     country_selector=country_selectorr()
     #=input$country_selector
     #req(input$country_selector)
     sdate <- input$start_date[1]
     edate <- input$start_date[2]
     scaleparam <- "fixed"
     if(input$scalesfree) scaleparam <- "free_y"
     p <- get_data() %>%
       filter(value > 0) %>%
       filter(`Country/Region` %in% country_selector) %>%
       filter(matched_days %in% sdate:edate) %>%
       ggplot()+
       aes(x = matched_days, y = value,
           group = interaction(`Country/Region`, type),
           color = `Country/Region`,
           shape = `Country/Region`,
           label = value) +
       geom_line(size = 1) +
       geom_point(size = 3)+
       facet_wrap(~type, scales = scaleparam, ncol = 1) +
       labs(x = paste("Days after", input$cases_limit, "confirmed cases were reached.")) +
       labs(y = "Count") +
       NULL
     if(input$logscale) {
       p <- p + scale_y_log10() + labs(y = "Count (log-scale)")
     }
     if (input$labelshow){
       p <- p + geom_label()
     }
     p+
       hrbrthemes::theme_ipsum_rc(base_size = 18) +
       ggtitle("Comparison of case trajectories by country") +
       theme(legend.position="bottom") +
       theme(plot.caption = element_text(family = NULL)) +
       theme(plot.title = element_text(size = 24),
             axis.title.x = element_text(size = 16),
             axis.title.y = element_text(size = 16),
             strip.text.x = element_text(size = 18)
       ) +
       scale_x_continuous(expand=c(0, 2)) +
       coord_cartesian(clip = 'off')
     #+
     #geom_dl(aes(label = `Country/Region`), method = list(dl.trans(x = x - 0.3, y = y + 0.4), dl.combine("last.points"), cex = 0.8)) -> p
     add_logoplot(p)



   })

   output$modeltable <- renderDT({
     get_data() %>%
       filter(type == "confirmed") %>%
       ungroup() %>%
       select(`Country/Region`, days, lvalue) %>%
       nest(data = c(days,lvalue)) %>%
       mutate(
         fit = map(data, ~lm(lvalue ~ days, data=.x)),
         tidied = map(fit, broom::tidy)
       ) %>%
       unnest(tidied) %>%
       filter(term == "days") %>%
       mutate(estimate = exp(estimate)) %>%
       select(`Country/Region`, estimate, std.error, statistic, p.value) %>%
       mutate(`Growth Rate` = paste0(round((estimate-1) * 100, 2), "%")) %>%
       mutate(`p Value` = scales::pvalue(p.value,
                                         accuracy = 0.001, # Number to round to
                                         decimal.mark = ".", # The character to be used to indicate the numeric decimal point
                                         add_p = TRUE)) %>%
       select(`Country/Region`, `Growth Rate`, `p Value`)


   })
   # 
   
   
 ###########
 update_material_text_box(
   session,
   input_id = "count",
   value = "Saudi Arabia"
 )
 
 contInput <- reactive({
   
   (input$count)
   
 }) 
 
 update_material_text_box(
   session,
   input_id = "cit",
   value = "Riyadh"
 )
 
 cityInput <- reactive({
   
   (input$cit)
   
 }) 
 
 update_material_dropdown(
   session,
   input_id = "where",
   value = "Demostic"
 )
 #  
 whereInput<- reactive({
   
   (input$where)
   
 }) 
 
 
 output$selected_var <- DT::renderDataTable({
   #search2=searches2()
   #tvmode=tvmodess()
   #data_sort=sort2()
   options(geonamesUsername="dr_khaled")
   
   GNsearchK <- function(city,country) {
     res <- GNsearch(name=city, country=country)
     if(dim(res)[1]==0){res <- GNsearch(name=city)}
     return(res[1, ])
   }
   res=reactive ({GNsearchK(cityInput(),contInput())})  
   geocit=subset(res(), select=c("lng", "lat", "adminName1"))
   woe=closestTrendLocations(geocit$lat,geocit$lng)
   
   trend <- getTrends(woe$woeid, exclude=NULL)
   trendsWorld <- getTrends(1, exclude=NULL)
   
   contWorld <- inner_join(trend, trendsWorld, by = "name")
   contWorld <- subset(contWorld, select = c(name))
   colnames(contWorld) <-paste("اللتي دخلت الهاشتاق العالمي")
   
   # in the contry itself
   contself <- inner_join(trend, trend, by = "name")
   contself <- subset(contself, select = c(name))
   colnames(contself) <- paste(cityInput())
   
   if (whereInput()=="Demostic"){resul= contself}
   if (whereInput()!="Demostic"){resul= contWorld}
   #resul= contself
   #names(contself)="الهاشتاق في نفس الدولة / المدينة المختارة"
   #resul=do.call("merge", c(lapply(list(contself, contWorld), data.frame, row.names=NULL), by = 0, all = TRUE))[-1]
   DT::datatable(resul, escape = FALSE,rownames = FALSE)
   
   
 })
 ###################################################
 # 10 day forcast 
 #### Reactive expressions for forecast page ####
 yAfCast <-reactive({ # subset country for forecast page
   tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
 })
 
 projfCast <- reactive({ # projection for forecast
   yA <- yAfCast()
   projSimple(yA, dates, inWindow = input$fitWinSlider)
 })
 
 ##### Raw stats #####  
 output$rawStats <- renderTable({
   yA <- yAfCast()
   yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
   yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
   #yR <- tsSub(tsR,tsR$Country.Region %in% input$countryFinder)
   nn <-length(yI)
   if (is.na(yA[nn])) nn <- nn-1
   out <- as.integer(c(yI[nn], yD[nn]))
   dim(out) <-c(1,2)
   colnames(out) <- c("Total", "Deaths")
   format(out, big.mark = ",")
 }, rownames = FALSE)
 
 ##### Raw plot #####  
 output$rawPlot <- renderPlotly({
   yA <- yAfCast()
   lDat <- projfCast()
   yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
   yTxt <- "Confirmed active cases"
   # plot(yA~dates, 
   #      xlim = c(min(dates), max(lDat$x)),
   #      ylim = c(0, yMax),
   #      pch = 19, 
   #      bty = "u", 
   #      xlab = "Date", 
   #      ylab = yTxt,
   #      main = input$countryFinder)
   # axis(side = 4)
   # lines(lDat$y[, "fit"]~lDat$x,col=3)
   # lines(lDat$y[, "lwr"]~lDat$x, lty = 2,col=2)
   # lines(lDat$y[, "upr"]~lDat$x, lty = 2,col=2)
   # legend("topleft", legend=c("Observed", "fitted","lower","upper"),
   #        col=c(1,3,2,2), lty=c(NA,1,2,2),pch=c(1,NA,NA,NA), cex=1,box.lty=0)
   # 
   ####
 
   gplot_data=data.frame(Confirme_Active_Cases=yA,Date=dates)
   gplot_data2=data.frame(Fitted=lDat$y[, "fit"],Lower=lDat$y[, "lwr"],Upper=lDat$y[, "upr"],Date=lDat$x)
   
   require(data.table)
   zz <- melt(list(p1=gplot_data,p2=gplot_data2), id.vars="Date")
   colors <- c("Opserved" = "black", "Fitted" = "green", "Lower" = "red","Upper"="red")
   name=c("Date","Confirme Active Cases")
   p <- zz %>%
     ggplot( aes(Date,value)) +
     geom_point(data=zz[zz$L1=="p1", ]) +
     geom_line(data=zz[zz$variable=="Fitted", ],col=3)+
     geom_line(data=zz[zz$variable=="Lower", ],col=2,linetype = "dashed")+
     geom_line(data=zz[zz$variable=="Upper", ],col=2,linetype = "dashed")+
     labs(y = "Confirme Active Cases",
          x = "Date",
          color = "Legend") +
     labs(title = input$countryFinder)  +
   #+
   #   scale_colour_manual(name="Legend", values = c("Fitted" = "black", "Lower" = "red", "Upper" = "red")) +
   #   scale_linetype_manual(name="Legend", values = c("Fitted" = "dashed", "Lower" = "dotted", "Upper" = "dotted"))+
   # theme_gray(base_size = 18) + 
   #   theme(axis.text = element_text(color = "black"),
   #         legend.key.height  = grid::unit(0.1, "npc")) 
   theme(panel.background = element_rect(fill = "white"))
   
   ggplotly(p)
 })
 
 ##### Log plot #####    
 output$logPlot <- renderPlot({
   yA <- yAfCast()
   lDat <- projfCast()
   yMax <- max(c(lDat$y[,"fit"], yA), na.rm = TRUE)
   yTxt <- "Confirmed active cases (log scale)"
   
   # gplot_data=data.frame(Confirme_Active_Cases=yA+0.1,Date=dates)
   # gplot_data2=data.frame(Fitted=lDat$y[, "fit"],Lower=lDat$y[, "lwr"],Upper=lDat$y[, "upr"],Date=lDat$x)
   # 
   # require(data.table)
   # zz <- melt(list(p1=gplot_data,p2=gplot_data2), id.vars="Date")
   # colors <- c("Opserved" = "black", "Fitted" = "green", "Lower" = "red","Upper"="red")
   # name=c("Date","Confirme Active Cases")
   # p <- zz %>%
   #   ggplot( aes(Date,value)) +
   #   geom_point(data=zz[zz$L1=="p1", ]) +
   #   geom_line(data=zz[zz$variable=="Fitted", ],col=3)+
   #   geom_line(data=zz[zz$variable=="Lower", ],col=2,linetype = "dashed")+
   #   geom_line(data=zz[zz$variable=="Upper", ],col=2,linetype = "dashed")+
   #   labs(x = "Confirme Active Cases",
   #        y = "Date",
   #        color = "Legend") +
   #   labs(title = input$countryFinder)  
   # #+
   # #   scale_colour_manual(name="Legend", values = c("Fitted" = "black", "Lower" = "red", "Upper" = "red")) +
   # #   scale_linetype_manual(name="Legend", values = c("Fitted" = "dashed", "Lower" = "dotted", "Upper" = "dotted"))+
   # # theme_gray(base_size = 18) + 
   # #   theme(axis.text = element_text(color = "black"),
   # #         legend.key.height  = grid::unit(0.1, "npc")) 
   # 
   # 
   # ggplotly(p)
   plot((yA+0.1)~dates,
        xlim = c(min(dates), max(lDat$x)),
        ylim = c(1, yMax),
        log = "y",
        pch = 19,
        bty = "u",
        xlab = "Date",
        ylab = yTxt,
        main = input$countryFinder)
   #axis(side=4)
   lines(lDat$y[, "fit"]~lDat$x,col=3)
   lines(lDat$y[, "lwr"]~lDat$x, lty = 2,col=2)
   lines(lDat$y[, "upr"]~lDat$x, lty = 2,col=2)
   legend("topleft", legend=c("Observed", "fitted","lower","upper"),
          col=c(1,3,2,2), lty=c(NA,1,2,2),pch=c(1,NA,NA,NA), cex=1,box.lty=0)
 })
 
 ##### Detection rate #####    
 output$detRate <- renderText({
   yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
   yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
   dR<-round(detRate(yI, yD), 4)
   if (is.na(dR)) "Insufficient data for estimation" else dR
 })
 
 ##### Prediction table confirmed #####    
 # output$tablePredConf <- renderTable({
 #   #yA <- yAfCast()
 #   # lDat <- projfCast()
 #   # nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
 #   # nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
 #   # dim(nowThen) <- c(1, 2)
 #   #colnames(nowThen)<-c("Now", "In 10 days (min-max)")
 #   #nowThen
 #   yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
 #   lDat <- projSimple(yA, dates)
 #   #nowThen <- format(as.integer(c(tail(yA[!is.na(yA)], 1), tail(lDat$y[,"lwr"],1), tail(lDat$y[,"upr"],1))), big.mark = ",")
 #   #nowThen <- c(nowThen[1], paste(nowThen[2], "-", nowThen[3]))
 #   #dim(nowThen) <- c(1, 2)
 #  # colnames(nowThen)<-c("Now", "In 10 days (min-max)")
 #  # nowThen
 #   tableall=data.frame(Date=lDat$x,tru=c((yA[(length(yA)-9):length(yA)]),rep("not yet",10)),estim= round(lDat$y[,1],0),lower=round(lDat$y[,2],0),upper=round(lDat$y[,3],0))
 #   colnames(tableall)<-c("Date", "Opserved","Estimate","Min","Max")
 #   tableall
 # }, rownames = FALSE)
 
 output$tablePredConf <- DT::renderDataTable({
   yA <- tsSub(tsA,tsA$Country.Region %in% input$countryFinder)
   lDat <- projSimple(yA, dates)
   #yA <- yAfCast()
   #lDat <- projfCast()
   tableall=data.frame(Date=lDat$x,tru=c((yA[(length(yA)-9):length(yA)]),rep("not yet",10)),estim= round(lDat$y[,1],0),lower=round(lDat$y[,2],0),upper=round(lDat$y[,3],0))
   colnames(tableall)<-c("Date", "Opserved","Estimate","Min","Max")
   DT::datatable( tableall, escape = FALSE,rownames = FALSE,options = list(pageLength = 20 ,initComplete = JS(
     "function(settings, json) {",
     "$(this.api().table().header()).css({'background-color': '#8282bf', 'color': '#fff'});",
     "}")))
 })
 
 ##### Prediction table true #####    
 output$tablePredTrue <- renderText({
   yA <- yAfCast()
   lDat <- projfCast()
   #yD <- tsSub(tsD,tsD$Country.Region %in% input$countryFinder)
   #yI <- tsSub(tsI,tsI$Country.Region %in% input$countryFinder)
   #dRate <- detRate(yI, yD)
   #lDat <- projfCast()
   now <- tail(yA[!is.na(yA)], 1)
   nowTrue <- format(round(now/dRate, 0), big.mark = ",")
   #nowThenTrue <- c(round(nowThenTrue[1],0), paste(round(nowThenTrue[2],0), "-", round(nowThenTrue[3],0)))
   #dim(nowThenTrue) <- c(1, 2)
   #colnames(nowThenTrue)<-c("Now", "In 10 days (min-max)")
   nowTrue
 })
 
 ##### Reactive expressions for growth page #####    
 growthSub <- reactive({
   subset(tsACountry, tsACountry$Country %in% input$country_selector)
 })
 ##### Curve-flattenning #####    
 output$cfi <- renderPlot({
   pDat <- growthSub()#subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
   pMat<-as.matrix(log(pDat[,-1]))
   row.names(pMat)<-pDat$Country
   cfiDat<-apply(pMat, MARGIN = 1, FUN = "cfi")
   cfiDat[!is.finite(cfiDat)]<-0
   clrs<-hcl.colors(length(input$country_selector))
   dateSub<-3:length(dates) # date subset
   plot(cfiDat[,1]~dates[dateSub], 
        type = "n", 
        ylim = range(c(-1.2,1.2)*sd(cfiDat)),
        bty = "l",
        xlab = "Date",
        ylab = "Curve-flatenning index")
   abline(a = 0, b = 0, lty = 2, lwd = 2)
   for (cc in 1:ncol(cfiDat)){
     cfiSmooth<-loess(cfiDat[,cc]~as.numeric(dates[dateSub]))
     lines(cfiSmooth$fitted~dates[dateSub], col = clrs[cc], lwd=3)
   }
   legend("topleft", 
          legend = pDat$Country, 
          lty = 1, 
          col = clrs,
          bty = "n")
 })
 ##### Growth rate #####    
 output$growthRate <- renderPlot({
   pDat <- growthSub()#subset(tsACountry, tsACountry$Country %in% input$countryGrowthRate)
   gRate <- as.matrix(growthRate(pDat))
   clrs<-hcl.colors(length(input$country_selector))
   dates10 <- dates[(length(pDat)-10+1):length(pDat)]
   counts <- table(gRate)
   barplot(gRate,
           main="Growth rate",
           xlab="Date", 
           ylab="Growth rate (% per day)",
           beside=TRUE,
           col = clrs,
           legend = pDat$Country,
           args.legend = list(bty = "n", x = "topright"))
 })
 
 ##### Doubling time ##### 
 output$doubTime <- renderText({
   pDat <- tsSub(tsACountry, tsACountry$Country %in% input$countryFinder)
   dTime <- round(doubTime(pDat, dates, inWindow = input$fitWinSlider), 1)
 })
 
 ##### Doubling time plot #####    
 output$doubTimePlot <- renderPlot({
   pDat <- subset(tsACountry, tsACountry$Country %in% input$country_selector)
   dTime <- as.matrix(doubTime(pDat, dates, inWindow = input$fitWinSlider))
   dTime[!is.finite(dTime)]<-NA
   clrs<-hcl.colors(length(input$country_selector))
   dates10 <- dates[(length(pDat)-10+1):length(pDat)]
   counts <- table(dTime)
   barplot(dTime,
           main="Doubling time",
           xlab="Date", 
           ylab="Doubling time (days)",
           beside=TRUE,
           col = clrs,
           legend = input$country_selector,
           args.legend = list(bty = "n", x = "topleft"))
 })
 
 
 ####### Stocastuc model 
 
 output$tablePredstocastc <- DT::renderDataTable({
   
   read_confirmed_cases <- function() {
     read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                      "confirmed.rds")
   }
   # Death
   read_death_cases <- function() {
     read_cached_file("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                      "deaths.rds")
   }
   #
   confirmed <- read_confirmed_cases() %>%
     gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
     mutate(date = paste0(date,"20")) %>%
     mutate(date = mdy(date)) %>%
     mutate(type = "confirmed")

   deaths <- read_death_cases() %>%
     gather(date, value, -`Province/State`, -`Country/Region`, -Lat, -Long) %>%
     mutate(date = paste0(date,"20")) %>%
     mutate(date = mdy(date)) %>%
     mutate(type = "deceased")

   all_data <- bind_rows(confirmed, deaths) %>%
     group_by(`Country/Region`, date, type) %>%
     summarise(value = sum(value))

   countries1 <- all_data %>% arrange(desc(value)) %>%
     pull(`Country/Region`) %>% unique()

   data_start <- all_data %>% pull(date) %>% min()
   data_end <- all_data %>% pull(date) %>% max()
   #cases_day_saudi= all_data[all_data$`Country/Region` == "Saudi Arabia",]
   
  
   
   cases_day_saudi1= reactive({
     
     all_data[all_data$`Country/Region` == input$countryFinder2,]
   })
   
   cases_day_saudi=cases_day_saudi1()
   
  cases_day_saudi_confirmed=cases_day_saudi[cases_day_saudi$type=="confirmed",]
   cases_day_saudi_deceased=cases_day_saudi[cases_day_saudi$type=="deceased",]
  
   # Now from cumulative to the actual
   cases_day_saudi_confirmed1=cases_day_saudi_confirmed[,4]
   cases_day_saudi_confirmed1[] <- lapply(cases_day_saudi_confirmed1, function(x) diff(c(0, x)))
   cases_day_saudi_confirmed$dayvalue=cases_day_saudi_confirmed1
   
   cases_day_saudi_deceased1=cases_day_saudi_deceased[,4]
   cases_day_saudi_deceased1[] <- lapply(cases_day_saudi_deceased1, function(x) diff(c(0, x)))
  cases_day_saudi_deceased$dayvalue= cases_day_saudi_deceased1
   
  SAcase=cases_day_saudi_confirmed[c(2,5)]
   
   
   #if (input$countryFinder=="Saudi Arabia"){
   #Cases=c(2,4,4,5,1,24,41,1,15,15,38,67,36,70,48,119,51,205,133,112,92,99,96,156,110,157,167,154,140,206,138)
  #Cases=c(2,4,4,5,1,24,41,1,15,15,38,67,36,70,48,119,51,205,133,112,92,99,96,156)
  Cases=(SAcase$dayvalue$value)
   #}else{Cases=SAcase$dayvalue$value}
   
   Cases=log(Cases)
   Cases[is.infinite(Cases)]=1
   logmodel <- SSModel(Cases ~ SSMtrend(1, Q = 0.01), H = 0.01)
   out <- KFS(logmodel)
   
   library(TTR)
   sma10=data.frame(SMA(exp(Cases),n=10))
   col_headings<-c("sma10")
   names(sma10)<-col_headings
  
   
   ## calculates an exponentially-weighted mean
   EMA10=data.frame(EMA(exp(Cases),n=10))
   col_headings<-c("EMA10")
   names(EMA10)<-col_headings
  
   
   #WMA
   WMA10=data.frame(WMA(exp(Cases), wts = 1:length(Cases)))
   col_headings<-c("WMA10")
   names(WMA10)<-col_headings
  
   
   #ZLEMA
   #ZLEMA(x, n = 10, ratio = NULL, ...)
   ZLEMA10=data.frame(ZLEMA(exp(Cases),n=10))
   col_headings<-c("ZLEMA10")
   names(ZLEMA10)<-col_headings
  
   
   #HMA
   HMA10=data.frame(HMA(exp(Cases),n=10))
   col_headings<-c("HMA10")
   names(HMA10)<-col_headings

   require(smooth)
   require(Mcomp)
   ourModel=sma(exp(Cases), h=5, silent=FALSE)
   Moving2=ourModel$forecast[1]
   ourModel2=ssarima(exp(Cases), h=5, silent=FALSE)
   arima=ourModel2$forecast[1]
   ourModel3=gum(exp(Cases), h=5, silent=FALSE)
   GLM=ourModel3$forecast[1]
  ourModel4=es(exp(Cases), h=5, silent=FALSE)
   ExpSmooth=ourModel4$forecast[1]
   ourModel5=ces(exp(Cases), h=5, silent=FALSE)
   ComplExpSmooth=ourModel5$forecast[1]
   
  ourModel6=smoothCombine(exp(Cases), h=5, silent=FALSE)
   smoothCombine=ourModel6$forecast[1]
   
   
    #Moving2=134
   #arima=135
   #GLM=135
   #ExpSmooth=179
   #ComplExpSmooth=147
   #smoothCombine=179
   #ourModel=sma(exp(Cases), h=18, silent=FALSE)
   #summary(ourModel)
   #forecast(ourModel)
   #plot(forecast(ourModel))
  # if(input$countryFinder=="Saudi Arabia"){
    DDD=SAcase$date[(length(SAcase$date)-length(exp(Cases))):length(SAcase$date)]
     DD=c(DDD[-1],(DDD[length(DDD)]+1))
     overall_estimate=mean( round(exp(out$a)[length(out$a)],0), round(exp(out$att)[length(out$att)],0), round(exp(out$alpha)[length(out$alpha)],0),round(sma10$sma10[length(sma10$sma10)],0),round(EMA10$EMA10[length(EMA10$EMA10)],0),round(WMA10$WMA10[length(WMA10$WMA10)],0),round(ZLEMA10$ZLEMA10[length(ZLEMA10$ZLEMA10)],0),round(HMA10$HMA10[length(HMA10$HMA10)],0),round(Moving2,0),arima,
                            GLM,
                            ExpSmooth,
                            ComplExpSmooth,
                            smoothCombine)
      min=min( round(exp(out$a)[length(out$a)],0), round(exp(out$att)[length(out$att)],0), round(exp(out$alpha)[length(out$alpha)],0),round(sma10$sma10[length(sma10$sma10)],0),round(EMA10$EMA10[length(EMA10$EMA10)],0),round(WMA10$WMA10[length(WMA10$WMA10)],0),round(ZLEMA10$ZLEMA10[length(ZLEMA10$ZLEMA10)],0),round(HMA10$HMA10[length(HMA10$HMA10)],0),round(Moving2,0),arima,
               GLM,
               ExpSmooth,
               ComplExpSmooth,
               smoothCombine)
      max=max( round(exp(out$a)[length(out$a)],0), round(exp(out$att)[length(out$att)],0), round(exp(out$alpha)[length(out$alpha)],0),round(sma10$sma10[length(sma10$sma10)],0),round(EMA10$EMA10[length(EMA10$EMA10)],0),round(WMA10$WMA10[length(WMA10$WMA10)],0),round(ZLEMA10$ZLEMA10[length(ZLEMA10$ZLEMA10)],0),round(HMA10$HMA10[length(HMA10$HMA10)],0),round(Moving2,0),arima,
               GLM,
               ExpSmooth,
               ComplExpSmooth,
               smoothCombine)
      
      df<-data.frame(D=DD[length(DD)]+1, round(exp(out$a)[length(out$a)],0), round(exp(out$att)[length(out$att)],0), round(exp(out$alpha)[length(out$alpha)],0),round(sma10$sma10[length(sma10$sma10)],0),round(EMA10$EMA10[length(EMA10$EMA10)],0),round(WMA10$WMA10[length(WMA10$WMA10)],0),round(ZLEMA10$ZLEMA10[length(ZLEMA10$ZLEMA10)],0),round(HMA10$HMA10[length(HMA10$HMA10)],0),round(Moving2,0),round(arima,0),
                     round(GLM,0),
                     round(ExpSmooth,0),
                     round(ComplExpSmooth,0),
                     round(smoothCombine,0),round(min,0),round(overall_estimate,0),round(max,0))
  # }else{ df<-data.frame(SAcase$date[(length(SAcase$date)-length(exp(Cases))):length(SAcase$date)],c(exp(Cases),"Not Yet"), exp(out$a))}
  #  
   dft=t(df)
   
   rownames(dft)=NULL
   
  model<-c("Date","Estimate (a)","Estimate (att)","Estimate (alpha)","Estimate (sma10)","Estimate (EMA10)","Estimate (WMA10)","Estimate (ZLEMA10)","Estimate (HMA10)","Estimate (Moving Avarge optim)","Estimate (Arima)",
                   "Estimate (GLM)",
                   "Estimate (ExpSmooth)",
                   "Estimate (ComplExpSmooth)",
                   "Estimate (SmoothCombine)","Minimum Estimate","Overall Estimate","Maximum Estimate")
  
  DF=data.frame(Model=model,Value=dft[,1])
  #dft$ID=1:18
  #colnames(dft)=c("ID","Value")
  #write.csv(dft,file="dft.csv")
  my_colors = ifelse(DF$Model=='Overall Estimate','red','white')
  my_colors[c(16,18)]="Yellow"
  my_colors[1]="gray"
  DT::datatable( DF, escape = FALSE,rownames = TRUE,
                 
                 options=list(
                   pageLength=100, 
                   dom='ltp', 
                   initComplete = JS("
                                     function(settings, json) {
                                     $(this.api().table().body()).css({
                                     'background-color': 'red',
                                     'outline-color': 'red',
                                     'margin':'100px',
                                     'color': 'black',
                                     'text-align': 'center',
                                     'font-family': 'Courier New',
                                     'border-radius': '25px'
                                     });
                                     $(this.api().table().header()).css({
                                     'background-color': '#8282bf',
                                     'color': '#fff',
                                     'outline-color': 'red',
                                     'margin':'100px',
                                     'text-align': 'center',
                                     'font-family': 'Courier New',
                                     'border-radius': '25px'
                                     });
                                     }
                                     ")
                   )
     #             options = list(pageLength = 30 ,initComplete = JS(
     # "function(settings, json) {",
     # "$(this.api().table().header()).css({'background-color': '#8282bf', 'color': '#fff'});",
     # "}"))
     
     )%>%
     
     formatStyle('Model', target = 'row', 
                 backgroundColor = styleEqual(DF$Model,my_colors))
   
   #my_vals = unique(df$)
  # my_colors = ifelse(my_vals=='myID','orange','White')
   
   #datatable(df) %>%
     
 })
   
}