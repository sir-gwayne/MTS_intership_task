#---
#title: "MTS intership test case"
#author: "Kaurova Polina"
#date: "2022-12-09"
#output: html_document
#---
library(jsonlite)
function_get_data<-function(){
  # Функция загрузки и фильтрации датасета. В датасете есть данные с битыми координатами, их просто отфильтровывыем и не включаем в итоговый датасет
  url <-"https://gist.githubusercontent.com/BogdanPetrov/7e5175bcab8c0b4987f50742e2dd7599/raw/a7f0af770060fb82cd0e30b53efac535ab704721/kfc_stores"
  kfc1 <-fromJSON(url)
  kfc<- kfc1[["searchResults"]][["storePublic"]]
  
  latitude<-NULL
  longtitude<-NULL
  
  for (i in 1:length(kfc$contacts$coordinates$geometry$coordinates)){
    latitude<-c(latitude,kfc$contacts$coordinates$geometry$coordinates[[i]][1])
    longtitude<-c(longtitude,kfc$contacts$coordinates$geometry$coordinates[[i]][2])
  }
  
  
  df_kfc <-data.frame(cbind(kfc$localStoreId, 
                            kfc$title$ru,
                            latitude, longtitude,
                            kfc$openingHours$regular$startTimeLocal, 
                            kfc$openingHours$regular$endTimeLocal,
                            kfc$contacts$streetAddress$ru,
                            kfc$timeZone))
  
  colnames(df_kfc) <- c("id", "Name", "Latitude", "Longtitude", "Opening_hours", "CLosing_hours", "Time_zone")
  
  
  df_kfc$Latitude <- as.numeric(df_kfc$Latitude)
  
  df_kfc$Longtitude <- as.numeric(df_kfc$Longtitude)
  
  
  
  
  df_kfc <- df_kfc[df_kfc$Latitude<=90 & df_kfc$Latitude>=-90,]
  df_kfc <- df_kfc[df_kfc$Longtitude<=180 & df_kfc$Longtitude>=-180,]
  
  df_kfc<- df_kfc[complete.cases(df_kfc), ]
  return(df_kfc)
  
}

function_search<-function(lat, lng){
  # Функция ищет kfc ресторан по координатам
  return(df_kfc[df_kfc$Latitude == lat & df_kfc$Longtitude==lng,][1,])
}
function_neighbour<-function(lat, lng,data){
  # Функция находит ближайший ресторан к заданой географической точке. 
  # Используется простой расчет длинны вектора по точкам. 
  # Существуют более точные и оптимальные алгоритмы поиска, однако для данного датасета такой вариант поиска кажется достаточным
  distance<-NULL
  for (i in 1:length(data$Latitude)){
    distance<-c(distance,sqrt((lat-data[i,"Latitude"])^2 + (lng-data[i,"Longtitude"])^2))
  }
  new<-data.frame(data[,"Latitude"], data[,"Longtitude"], distance, data[,"CLosing_hours"] )
  return(new[which.min(new$distance),])
  
}

get_time_from_seconds <-function(input_seconds){
  hours <-floor(input_seconds/3600)
  minutes <- floor((input_seconds - hours*3600)/60)
  seconds <- floor(input_seconds - hours*3600 - minutes*60)
  return(paste0(hours,":",minutes,":",seconds))
}

get_route_info <- function(lat1,lng1,lat2,lng2){
  # Отправляет запрос в OSRM для построения маршрута
  url <- str_interp("http://router.project-osrm.org/route/v1/driving/${lng1},${lat1};${lng2},${lat2}?")
  route_info <-fromJSON(url)
  if (route_info$code=="Ok"){
    distance <- signif(route_info$routes$legs[[1]]$distance/1000,3)
    eta <-get_time_from_seconds(route_info$routes$legs[[1]]$duration)
    polyline_str <- route_info$routes[1]$geometry
    coords <- decode(polyline_str)
    return(c(coords,distance,eta))
  }else{
    return(NULL)
  }
}

library(shiny)
library(leaflet)
library(htmlwidgets)
library(googlePolylines)
library(stringr)

kfc_icon <- makeIcon(
  iconUrl = "https://www.kfc.ru/images/favicon_32x32.png",
  iconWidth = 32, iconHeight = 32,
  iconAnchorX = 32, iconAnchorY = 32
  
)

ui <- fluidPage(
  htmltools::includeMarkdown("Main.Rmd"),
  leafletOutput("map"),
  br(),
  verbatimTextOutput("my_position"),
  verbatimTextOutput("kfc_position"),
  actionButton("finder", "Проложить маршрут"),
  actionButton("nearest", "KFC рядом")
)

df_kfc <- function_get_data()
curr_point<-reactiveValues(points = vector())
curr_kfc_point<-reactiveValues(points = vector())
initial_position = c(37.6192474365234, 55.7586596110349)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet()  %>%
      addTiles() %>% 
      setView(initial_position[1],initial_position[2], zoom = 6) %>% 
      addMarkers(initial_position[1],initial_position[2],options = markerOptions(draggable = TRUE))%>%
      addMarkers( df_kfc$Longtitude,df_kfc$Latitude,clusterOptions = markerClusterOptions(), icon = kfc_icon)
      
    
  }) 
  
  output$my_position <- renderText({
    if(is.null(curr_point$points)) {
      "Не можем определить вашу позицию"
      curr_point$points <- rev(initial_position)
    } else {
      paste0("Ваша позиция Lat: ", curr_point$points[1], 
             " Lng: ", curr_point$points[2])
    }
  })
  
  output$kfc_position <- renderText({
    if(is.null(curr_kfc_point$points)) {
      "Пожалуйста, выберите Ресторан KFC на карте, или нажмите кнопку KFC рядом"
    } else {
      kfc_info = function_search( curr_kfc_point$points[1], curr_kfc_point$points[2])
      paste0("Выбраный ресторан: ",kfc_info[[2]] ,"\nПозиция Lat: ", curr_kfc_point$points[1], 
             " Lng: ", curr_kfc_point$points[2],
             "\nАдрес: ",kfc_info[[7]],
             '\nВремя работы: ',kfc_info[[5]],' - ',kfc_info[[6]])
    }
  })
  
  observe({
    p <- input$map_marker_click  # typo was on this line
    curr_kfc_point$points<-c(p$lat,p$lng)
  })
  
  observe({
    p <-input$map_marker_dragend
    curr_point$points<-c(p$lat,p$lng)
  })
  
  observeEvent(input$finder, {
    result <-get_route_info(curr_point$points[1],curr_point$points[2],curr_kfc_point$points[1],curr_kfc_point$points[2])
    coords = result[1]
    distance = result[2]
    eta = result[3]
    leafletProxy('map')%>%removeShape(layerId="route")%>%removeShape(layerId="route_popup")
    if (is.null(coords)){
      showModal(modalDialog(
        title = "Невозможно проложить маршрут",
        "Для данных двух точек, невозможно проложить маршрут, пожалуйста выберите другие точки на карте",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      leafletProxy('map')%>%addPolylines(lng = coords[[1]][[2]], lat = coords[[1]][[1]],layerId = "route",color="green")%>%
      addPopups(lng=curr_point$points[2], lat=curr_point$points[1], paste0("Растояние: ",distance,"км <br>Время в пути: ", eta," часов <br>Время без учета пробок") ,layerId = "route_popup",options = popupOptions(closeButton = TRUE))
    }
  })
  observeEvent(input$nearest, {
    nearest_kfc <- function_neighbour(curr_point$points[1],curr_point$points[2],df_kfc)
    curr_kfc_point$points<-c(nearest_kfc[[1]],nearest_kfc[[2]])
    result <-get_route_info(curr_point$points[1],curr_point$points[2],curr_kfc_point$points[1],curr_kfc_point$points[2])
    coords = result[1]
    distance = result[2]
    eta = result[3]
    leafletProxy('map')%>%removeShape(layerId="route")%>%removeShape(layerId="route_popup")
    if (is.null(coords)){
      showModal(modalDialog(
        title = "Невозможно проложить маршрут",
        "Для данных двух точек, невозможно проложить маршрут, пожалуйста выберите другие точки на карте",
        easyClose = TRUE,
        footer = NULL
      ))
    }else{
      leafletProxy('map')%>%addPolylines(lng = coords[[1]][[2]], lat = coords[[1]][[1]],layerId = "route",color="green")%>%
      addPopups(lng=curr_point$points[2], lat=curr_point$points[1], paste0("Растояние: ",distance,"км <br>Время в пути: ", eta, " часов <br>Время без учета пробок") ,layerId = "route_popup",options = popupOptions(closeButton = TRUE)) 
    }
  })
  
}

shinyApp(ui, server)
