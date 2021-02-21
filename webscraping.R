library("readxl") ## lees el csv
library(xml2) ## para hacer web scraping
library(rvest) ## para hacer web scraping
library(stringr) ## sirve para manipular caracteres
library(lubridate) ## manipular fechas



base=read_excel("D:/Edgar/Mish/cityexpress/Hoteles y habitaciones.xlsx") #Carga la base del excel
base_fechas=read_excel("D:/Edgar/Mish/cityexpress/fechas_reservacion.xlsx") #Carga la base del excel

####################################
########### expedia.com ############
####################################


base1=base[which(base$Buscador=="expedia"),]  #Filtra solo los registros de expedia
base11<-as.vector(base1$pagina1)   #crea un vector con las ligas de cada hotel
base11

geturl1<- function(base11){
  
  fecini<-'2019-12-28'   #Fecha inicial
  fecfin<-'2019-12-29'   #Fecha Final

url<-paste0(base11,'chkin=',day(fecini),'%2F',month(fecini),'%2F',year(fecini),'&chkout=',day(fecfin),'%2F',month(fecfin),'%2F',year(fecfin))    #Concatena las ligas y anade las fechas de reservacion
print(url)

pagina_web<-read_html(url) # Carga la pagina en la variable Pagina Web
pagina_web
#### Extrae el precio de la pagina  ###
precio_nodo
    precio_nodo<-html_form(pagina_web)
    precio_texto<-tryCatch(precio_nodo[[2]][["fields"]][["prices"]][["value"]],error = function(e) "AGOTADO")
    precio_texto<-if (precio_texto=="AGOTADO") "AGOTADO" else str_split(precio_texto,"=")[[1]][3]

#### Extrae las caracteristicas de la reservacion  ###
    
    hab<-'span.uitk-cell.all-cell-3-4.all-cell-shrink.amenities-list__item-title'
    hab_nodo<-html_nodes(pagina_web,hab)
    hab_nodo2<-html_text(hab_nodo)
    hab_nodo2<-unique(hab_nodo2)
    hab_nodo2

    
    i=1
    tryCatch(repeat{
      i<- i+1
      if(hab_nodo2[1]==hab_nodo2[i]) {
        break
      }
    },error = function(e) i==length(hab_nodo2))
    
    car1<-if(1<i) hab_nodo2[1] else "NA"
    car2<-if(2<i) hab_nodo2[2] else "NA"
    car3<-if(3<i) hab_nodo2[3] else "NA"
    car4<-if(4<i) hab_nodo2[4] else "NA"
    car5<-if(5<i) hab_nodo2[5] else "NA"
    car6<-if(6<i) hab_nodo2[6] else "NA"
    car7<-if(7<i) hab_nodo2[7] else "NA"
    car8<-if(8<i) hab_nodo2[8] else "NA"
    car9<-if(9<i) hab_nodo2[9] else "NA"
    car10<-if(10<i) hab_nodo2[10] else "NA"

#### Extrae la calificacion del hotel  ###
  
    calif<-'div.uitk-type-900'
    calif_nodo<-html_nodes(pagina_web,calif)
    calif_nodo2<-html_text(calif_nodo)
    calif_nodo2

#### Extrae el nombre del hotel  ###
    
    hotel<-'h1'
    hotel_nodo<-html_nodes(pagina_web,hotel)
    hotel_text<-html_text(hotel_nodo)
    hotel_text

    
#### Crea un vector con todas las variables  ###

    
    base_fin<-c(as.character.Date(base11,today()),fecini,fecfin,trimws(hotel_text),calif_nodo2,precio_texto,car1,car2,car3,car4,car5,car6,car7,car8,car9,car10)
    base_fin

}

resultado_datos<-sapply(base11,geturl1) 
#resultado_datos<-sapply(fecini,fecfin,geturl2) # aplica la funcion geturl1 al vector que creamos base11
class(resultado_datos)
resultado_datos<-as.data.frame(t(resultado_datos))  #Transforma la matriz en un data frame
#resultado_datos<-do.call('cbind',resultado_datos)
#resultado_datos<-unlist(resultado_datos)


####################################
########### booking.com ############
####################################

base_fechas1<-as.vector(paste0(base_fechas$Fec_ini,"|",base_fechas$Fec_fin))

geturl3<- function(base_fechas1){
  print(base_fechas1)
  
  base2=base[which(base$Buscador=="booking"),]  #Filtra solo los registros de expedia
  base12<-as.vector(paste0(base2$pagina1,"|",base2$id1,"|",base_fechas1))
  
  resultado_datos<-sapply(base12,geturl2)
  #class(resultado_datos)
  #resultado_datos<-as.data.frame(t(resultado_datos))  #Transforma la matriz en un data frame
  resultado_datos
}

geturl2<- function(base12){
  
  datos<-strsplit(base12, split="|",fixed=TRUE)
  liga<-datos[[1]][1]
  id1<-datos[[1]][2]
  fecini<-datos[[1]][3]
  fecfin<-datos[[1]][4]

  #fecini<-'2020-07-28'   #Fecha inicial
  #fecfin<-'2020-08-02'   #Fecha Final

url<-paste0(liga,'checkin=',fecini,';','checkout=',fecfin)
#url<-paste0(base12,'checkin=',fecini,';','checkout=',fecfin)

print(url)

pagina_web<-read_html(url)
nombre<-'#hp_hotel_name'
nombre_nodo<-html_node(pagina_web,nombre)
nombre_texto<-html_text(nombre_nodo)
nombre_texto<-gsub("\nHotel\n","",nombre_texto)
nombre_texto<-gsub("\n","",nombre_texto)
nombre_texto

precio<-'div.bui-price-display__value.prco-ltr-center-align-helper.prco-font16-helper'
precio_nodo<-html_node(pagina_web,precio)
precio_texto<-html_text(precio_nodo)
precio_texto<-gsub("\nMXN","",precio_texto)
precio_texto<-gsub("\n","",precio_texto)
precio_texto

impuesto<-paste0('div.prd-taxes-and-fees-under-price')
impuesto_nodo<-html_node(pagina_web,impuesto)
impuesto_texto<-html_text(impuesto_nodo)
impuesto_texto<-gsub("MXN","",impuesto_texto)
impuesto_texto<-gsub(" de impuestos y cargos  ","",impuesto_texto)
impuesto_texto<-gsub(" ","",impuesto_texto)
impuesto_texto<-strsplit(impuesto_texto, split="+",fixed=TRUE)
impuesto_texto<-unlist(impuesto_texto)
impuesto_texto<-impuesto_texto[2]
impuesto_texto

habitacion<-paste0('#room_type_id_',id1,' > span')
habitacion_nodo<-html_node(pagina_web,habitacion)
habitacion_texto<-html_text(habitacion_nodo)
habitacion_texto<-gsub("\n","",habitacion_texto)
habitacion_texto

calif<-'div.bui-review-score__badge'
calif_nodo<-html_node(pagina_web,calif)
calif_texto<-html_text(calif_nodo)
calif_texto

caracteristicas<-'li.hprt-green-condition.rt_clean_up_options'
caracteristicas_nodo<-html_node(pagina_web,caracteristicas)
caracteristicas_texto<-html_text(caracteristicas_nodo)
caracteristicas_texto<-gsub("\n","",caracteristicas_texto)
caracteristicas_texto

comentarios<-'div.bui-review-score__text'
comentarios_nodo<-html_node(pagina_web,comentarios)
comentarios_texto<-html_text(comentarios_nodo)
comentarios_texto

califdet<-'#review_list_score > div.v2_review-scores__wrapper.ugc-sub-scores > div'
califdet_nodo<-html_nodes(pagina_web,califdet)
califdet_texto<-html_text(califdet_nodo)
califdet_texto<-gsub("\n\n","|",califdet_texto)
califdet_texto<-gsub("\n","",califdet_texto)
califdet_texto<-gsub("  ","",califdet_texto)
califdet_texto<-strsplit(califdet_texto, split="|",fixed=TRUE)
califdet_texto<-unlist(califdet_texto)

personal<-gsub("Personal","",califdet_texto[2])
instalyserv<-gsub("Instalaciones y servicios","",califdet_texto[3])
limpieza<-gsub("Limpieza","",califdet_texto[4])
confort<-gsub("Confort","",califdet_texto[5])
rel_cal_prec<-gsub("Relación calidad - precio","",califdet_texto[6])
ubicacion<-gsub("Ubicación","",califdet_texto[7])
wifigratis<-gsub("WiFi gratis","",califdet_texto[8])

base_fin2<-c(as.character.Date(today()),fecini,fecfin,nombre_texto,precio_texto,impuesto_texto,habitacion_texto,calif_texto,caracteristicas_texto,comentarios_texto,personal,instalyserv,limpieza,confort,rel_cal_prec,ubicacion,wifigratis)
base_fin2<-paste0(as.character.Date(today()),"|",fecini,"|",fecfin,"|",nombre_texto,"|",precio_texto,"|",impuesto_texto,"|",habitacion_texto,"|",calif_texto,"|",caracteristicas_texto,"|",comentarios_texto,"|",personal,"|",instalyserv,"|",limpieza,"|",confort,"|",rel_cal_prec,"|",ubicacion,"|",wifigratis)
base_fin2

}


resultado_datos<-sapply(base_fechas1,geturl3)

resultado_datos_fin<-data.frame(resultado_datos[,1])
colnames(resultado_datos_fin)<-"v1"


for(i in 2:ncol(resultado_datos)) {
  
  b<-data.frame(resultado_datos[,i])
  colnames(b)<-"v1"
  resultado_datos_fin<-rbind(resultado_datos_fin,b)
}

resultado_datos_fin<-as.vector(resultado_datos_fin$v1)
resultado_datos_fin

geturlfin<- function(resultado_datos_fin){
  a<-strsplit(resultado_datos_fin, split="|",fixed=TRUE)
}

resultado_datos2<-sapply(resultado_datos_fin,geturlfin)

y1 <- c(resultado_datos2[[1]][1],resultado_datos2[[1]][2],resultado_datos2[[1]][3],resultado_datos2[[1]][4],resultado_datos2[[1]][5],resultado_datos2[[1]][6],resultado_datos2[[1]][7],resultado_datos2[[1]][8],resultado_datos2[[1]][9],resultado_datos2[[1]][10],resultado_datos2[[1]][11],resultado_datos2[[1]][12],resultado_datos2[[1]][13],resultado_datos2[[1]][14],resultado_datos2[[1]][15],resultado_datos2[[1]][16],resultado_datos2[[1]][17])

for(i in 1:length(resultado_datos2)) {
  
  y2 <- c(resultado_datos2[[i]][1],resultado_datos2[[i]][2],resultado_datos2[[i]][3],resultado_datos2[[i]][4],resultado_datos2[[i]][5],resultado_datos2[[i]][6],resultado_datos2[[i]][7],resultado_datos2[[i]][8],resultado_datos2[[i]][9],resultado_datos2[[i]][10],resultado_datos2[[i]][11],resultado_datos2[[i]][12],resultado_datos2[[i]][13],resultado_datos2[[i]][14],resultado_datos2[[i]][15],resultado_datos2[[i]][16],resultado_datos2[[i]][17])
  y1<-rbind(y1,y2)
}

y1<-as.data.frame(y1)
col<-c('ejecucion','fec_reserv_ini','fec_reserv_fin','hotel','precio','impuestos','habitacion','calificacion','desayuno','comentarios','Personal','Instalaciones y servicios','Limpieza','Confort','Relación calidad - precio','Ubicación','WiFi gratis')
colnames(y1)<-col

