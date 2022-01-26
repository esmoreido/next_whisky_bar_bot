library(telegram.bot)
library(dplyr)
library(sp)
library(sf)
library(rgdal)
library(leaflet)
library(rgeos)

# функция для определения номера зоны UTM, в которой находится пользователь
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

# если на новой машине запускается - добавить токен бота в .Renviron
# file.edit(path.expand(file.path("~", ".Renviron")))

# экземпляр бота
updater <- Updater(bot_token(bot_name = 'next_wine_bar_bot'))

# приветствие
init_msg <- function(bot, update) {

  # Имя пользователя с которым надо поздароваться
  user_name <- update$message$from$first_name

  # Отправка приветственного сообщения
  bot$sendMessage(update$message$chat_id,
                  text = paste0("Привет, ", user_name, "! \U000270C"),
                  parse_mode = "Markdown")
  bot$sendMessage(update$message$chat_id,
                  text = "Отправь свою геолокацию \U0001F4CD и я подскажу 5 отличных заведений поблизости \U0001F377",
                  parse_mode = "Markdown")
}

# обработчик приветствия
init_handler <- CommandHandler('start', init_msg)

# собственно функция поиска
bar_search <- function(bot, update){
  # загружаем точки
  load('bars_wgs.RData')
  # получаем от пользователя координаты
  lat <- as.numeric(update$message$location[1])
  lon <- as.numeric(update$message$location[2])
  # debug
  # lat <- 55.739604
  # lon <- 37.578525
  
  userCRS <- paste0("+proj=utm +zone=", long2UTM(lon)," +datum=WGS84 +units=km +no_defs ")
  
  # делаем точку из координат
  userloc <- readWKT(paste0("POINT (", lon, ' ', lat, ')'), p4s = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  userloc <- spTransform(userloc, CRS(userCRS))
  
  bars_proj <- spTransform(bars_wgs, CRS(userCRS)) 
  proj4string(bars_proj)
  bars_proj$dist <- t(gDistance(spTransform(bars_wgs, CRS(userCRS)), userloc, byid = T))
  bars_proj <- as.data.frame(bars_proj)
  
  bars_proj$prox <- cut(bars_proj$dist, breaks = c(0, 0.5, 1, 2, 3, 5, +Inf), 
                        labels = c('500 м', '1 км', '2 км', '3 км', '5 км', '>5 км'))
  markerCol <- colorFactor(palette = 'Set1', bars_proj$prox)
  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = bars_proj, lng = ~lon, lat = ~lat, radius = ~sqrt(dist)*10, fillOpacity = 200,
                     color=~markerCol(prox), stroke = F, fillColor = ~markerCol(prox), popup = ~name) %>%
    addMarkers(userloc, lng = lon, lat = lat) %>%
    setView(userloc, lng = lon, lat = lat, zoom = 14)

  
  
  
  bars_near <- slice_min(bars_proj, order_by = dist, n = 5)
  
  bot$sendMessage(update$message$chat_id,
                  text = paste0('Вот 5 отличных баров на расстоянии от ', 
                                round(min(bars_near$dist), 1), ' до ', 
                                round(max(bars_near$dist), 1), ' км: '),
                  parse_mode = "Markdown")
  
  for(i in 1:nrow(bars_near)){
    bot$sendMessage(update$message$chat_id,
                    text = paste0(bars_near[i, 'name'], ', ', round(bars_near[i, 'dist'], 1), ' км по прямой'),
                    parse_mode = "Markdown")
    bot$sendLocation(
      update$message$chat_id,
      latitude = bars_near[i, 'lat'],
      longitude = bars_near[i, 'lon']
    )
  }
}
location_handler <- MessageHandler(bar_search, filters = MessageFilters$location)

unknown <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Извините, я так не умею \U0001F612")
}

unknown_message_handler <- MessageHandler(unknown, MessageFilters$command)

unknown_text <- function(bot, update){
  bot$sendMessage(chat_id = update$message$chat_id,
                  text = "Извините, я вас не понял \U0001F612")
}

unknown_text_handler <- MessageHandler(unknown_text, MessageFilters$all)

updater <- updater + init_handler + location_handler + unknown_message_handler

# запускаем бота
updater$start_polling()
