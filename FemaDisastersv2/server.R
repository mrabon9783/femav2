#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(sqldf)
library(maps)
library(DT)

options(shiny.maxRequestSize = 10000*1024^2)

relateFematoFilter<-function(femadata,hTerm,sregion){
  startTerm <- as.Date(Sys.Date()-as.integer(hTerm), format = "%Y-%m-%d")
  termlist<-femadata$declarationDate > startTerm
  femadata<-femadata[termlist,]
  data(state.fips)
  
  femadata$statefips<-state.fips$fips[match(femadata$state,state.fips$abb)]
  nplacecode<- ifelse(substring(femadata$placeCode,1,2)==99,paste(femadata$statefips,substring(femadata$placeCode,3),sep=""),femadata$placeCode)
  
  #femadata$placeCode<-nplacecode
  femadata$usregion<-state.fips$region[match(femadata$state,state.fips$abb)]
  #femadata$dlink<-apply(femadata[4],1,function(x){paste("https://www.fema.gov/disaster/",x,collapse="")})
  femadata$dlink<-apply(femadata[4],1,function(x){paste("<a href='https://www.fema.gov/disaster/",gsub(" ","",x),"' target = '_blank'>","https://www.fema.gov/disaster/",gsub(" ","",x),"</a>",collapse="")})
  #femadata$dlink<-gsub("%20","",femadata$dlink)
  femadata$dlink<-gsub("/ ","/",femadata$dlink)
  if(sregion=="Northeast"){
    femadata<-femadata[femadata$usregion==1,]
  }
  if(sregion=="Midwest"){
    femadata<-femadata[femadata$usregion==2,]

  }
  if(sregion=="Southeast"){
    femadata<-femadata[femadata$usregion==3,]

  }
  if(sregion=="West"){
    femadata<-femadata[femadata$usregion==4,]
  }
  femadata
}

relateFematoSpatial<-function(rawfemadata,historyTerm){
  data(county.fips)
  cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                      county.fips$polyname)]
  cntydf<- map_data("county")
  cntydf<-mutate(cntydf,poly = paste(region,subregion,sep=","))
  cntydf$fips<-county.fips$fips[match(cntydf$poly,
                                      county.fips$polyname)]
  startTerm <- as.Date(Sys.Date()-as.integer(historyTerm), format = "%Y-%m-%d")
  
  termlist<-rawfemadata$declarationDate > startTerm
  rawfemadata$cut<-cut(rawfemadata$DaysSinceDeclaration, breaks = 60*(0:((historyTerm/365)*7)))
  rawfemadata<-rawfemadata[termlist,]
  data(state.fips)
  
  rawfemadata$statefips<-state.fips$fips[match(rawfemadata$state,state.fips$abb)]
  nplacecode<- ifelse(substring(rawfemadata$placeCode,1,2)==99,paste(rawfemadata$statefips,substring(rawfemadata$placeCode,3),sep=""),rawfemadata$placeCode)
  
  rawfemadata$placeCode<-nplacecode
  cntydf$col<- rawfemadata$cut[match(cntydf$fips,rawfemadata$placeCode)]
  sfipdf<-state.fips
  sfipdf$polyname<-apply(sfipdf[6],1,function(x){unlist(strsplit(x,":",fixed = TRUE))[1]})
  cntydf$usregion<-sfipdf$region[match(cntydf$region,sfipdf$polyname)]
  cntydf
}

getfemadata<- function(){
  dsfile<-file.path(getwd(),paste("fema",as.Date(Sys.Date(), format("%Y%m%d")),".csv", sep = ""))
  if(!file.exists(dsfile)){
    a<-read.csv("https://www.fema.gov/api/open/v1/DisasterDeclarationsSummaries.csv")
    f<-sqldf("select max(declarationDate) as declarationDate, state, placeCode  from a where a.disasterNumber between 4000 and 4999 group by state, placeCode")
    a<-sqldf("select a.declarationDate, a.state, a.placeCode, a.disasterNumber, a.title  from a inner join f on  f.declarationDate = a.declarationDate and f.state = a.state and f.placeCode = a.placeCode ")
    write.csv(a,dsfile, row.names = FALSE)
  }
  a<-read.csv(dsfile)
  a$declarationDate <- apply(a[1],1,function(x){unlist(strsplit(x,"T",fixed = TRUE))[1]})
  a$declarationDate <- as.character(a$declarationDate)
  a$declarationDate <- as.Date(a$declarationDate,format = "%Y-%m-%d")
  a$DaysSinceDeclaration <- as.integer(Sys.Date()-a$declarationDate)
  a
}


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  session$onSessionEnded(function() {
    stopApp()
  })
  output$fematable <- renderTable({
    ftdf<-getfemadata()
    ftdf$declarationDate <- as.character(ftdf$declarationDate)
    ftdf
  })
  output$femausplot <- renderPlot({
    cntydf<- relateFematoSpatial(getfemadata(),as.integer(input$sPeriod)*365)
    lbl<-c("0-60 Days Past","60-120 Days Past","120-180 Days Past","180-240 Days Past","240-300 Days Past","300-360 Days Past","360-420 Days Past","420-480 Days Past","480-540 Days Past","540-600 Days Past","600-660 Days Past","660-720 Days Past","720-780 Days Past")
    stdf<-map_data("state")
    sfipdf<-state.fips
    sfipdf$polyname<-apply(sfipdf[6],1,function(x){unlist(strsplit(x,":",fixed = TRUE))[1]})
    stdf$usregion<-sfipdf$region[match(stdf$region,sfipdf$polyname)]
    if(input$sRegion=="Northeast"){
      cntydf<-cntydf[cntydf$usregion==1,]
      stdf<-stdf[stdf$usregion==1,]
    }
    if(input$sRegion=="Midwest"){
      cntydf<-cntydf[cntydf$usregion==2,]
      stdf<-stdf[stdf$usregion==2,]
    }
    if(input$sRegion=="Southeast"){
      cntydf<-cntydf[cntydf$usregion==3,]
      stdf<-stdf[stdf$usregion==3,]
    }
    if(input$sRegion=="West"){
      cntydf<-cntydf[cntydf$usregion==4,]
      stdf<-stdf[stdf$usregion==4,]
    }
    p1<- ggplot()
    p1<- p1 + geom_polygon(data = stdf, aes(x=long, y=lat, group = group), colour = "black", size = 1)
    p1<- p1 + geom_polygon(data = cntydf, aes(x=long, y=lat,group = group),colour = "black",fill = "grey",size = .25, alpha = .5)
    p1<- p1 + geom_polygon(data = cntydf, aes(x=long, y=lat, fill = factor(col),group = group),size = .35, alpha = .68)
    p1<- p1 + scale_fill_manual(values = c("#990000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fff7ec",
                                            "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec", "#fff7ec"),name = "Days Since Presidential \nMajor Disaster Declaration",labels = lbl )
    p1<- p1 + theme(panel.grid = element_blank(),panel.background = element_blank(), axis.ticks = element_blank(), axis.text = element_blank(), axis.title  = element_blank())
    p1
    
  })
  output$femausdata<-renderTable({
    df<-getfemadata()
    
    a<-relateFematoFilter(df,as.integer(input$sPeriod)*365,input$sRegion)
    a$declarationDate<-as.character(a$declarationDate)
    a
  })
  output$femausdatatest<- DT::renderDataTable({
    df<-getfemadata()
    
    a<-relateFematoFilter(df,as.integer(input$sPeriod)*365,input$sRegion)
    a$declarationDate<-as.character(a$declarationDate)
    a<-sqldf("select declarationDate as DeclarationDate, disasterNumber as DisasterNumber, state as State, placeCode as StateCountyFips, dlink as FemaDisasterURL, title as FemaDisasterTitle from a  order by disasterNumber desc, state asc")
    DT::datatable(a,escape = FALSE, rownames = FALSE)
  })
  output$sBarText <- renderText("Proof of Concept")
})
