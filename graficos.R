
#removing warnings
options(warn = -1)

# Script para gerar os graficos
require(grid)
require(hrbrthemes)
require(viridis)
require(ggplot2)
library(tidyverse)
library(dplyr)
# library(geobr) # dados do mapa brasil
library(sf)
library(maptools)
# library(leaflet)


makeG0 <- function(inGraph0, crop, trt1, item, tpp, stg, yr){
  # inGraph0 <- myDFF
  # crop="Corn"
  # trt1="Corn Stunt"
  # item="Nota"
  # tpp="Brazil"
  # stg=6
  
  if(tpp != "Brazil"){
    inGraph00 <- inGraph0 %>% dplyr::filter(TPP == tpp)%>% dplyr::filter(Year == yr)
  }else{
    inGraph00 <- inGraph0
  }
  inGraph000 <- inGraph00 %>% filter(Stage == stg, Crop == crop, Trait == trt1, Year == yr)
  inGraph000 %>%
    ggplot( aes(x=local, y=get(item), fill=local)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    # theme_ipsum() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position="none",
      plot.title = element_text(size=11)
    ) +
    ggtitle("") +
    xlab("") +
    ylab(item)
  
}

makeH0 <- function(inGrapH0, crop, trt1, termo, tpp, stg){
  if(tpp != "Brazil"){
    inGrapH00 <- inGrapH0 %>% dplyr::filter(TPP == tpp)
  }else{
    inGrapH00 <- inGrapH0
  }
  inGrapH000 <- inGrapH00 %>% filter(Stage == stg, Crop == crop, Trait == trt1)
  H0 <- inGrapH000 %>%
    ggplot( aes(x=get(termo))) +
    geom_histogram( binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    # ggtitle("") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    ) + xlab('Score')
  
  return(H0)
}



makeG1 <- function(inGraph1){
  g.mid<-ggplot(inGraph1,aes(x=1,y=genotipo))+geom_text(aes(label=genotipo))+
    geom_segment(aes(x=0.94,xend=0.96,yend=genotipo))+
    geom_segment(aes(x=1.04,xend=1.065,yend=genotipo))+
    ggtitle("")+
    ylab(NULL)+
    scale_x_continuous(expand=c(0,0),limits=c(0.94,1.065))+
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          plot.margin = unit(c(1,-1,1,-1), "mm"))
  
  g1 <- ggplot(data = inGraph1, aes(x = genotipo, y = blupNota)) +
    geom_bar(stat = "identity") + ggtitle("Corn Stunt Score") +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,-1,1,0), "mm")) +
    scale_y_reverse() + coord_flip()
  
  g2 <- ggplot(data = inGraph1, aes(x = genotipo, y = blupYield)) +xlab(NULL)+
    geom_bar(stat = "identity") + ggtitle("Yield") +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          plot.margin = unit(c(1,0,1,-1), "mm")) +
    coord_flip()
  library(gridExtra)
  gg1 <- ggplot_gtable(ggplot_build(g1))
  gg2 <- ggplot_gtable(ggplot_build(g2))
  gg.mid <- ggplot_gtable(ggplot_build(g.mid))
  
  grid.arrange(gg1,gg.mid,gg2,ncol=3,widths=c(4/9,2/9,4/9))
}

makeG2 <- function(inGraph2){
  ggplot(inGraph2, aes(x = reorder(genotipo,-Indice, sum))) +
    xlab("Genotipos")+
    geom_col(aes( y = Indice, fill="redfill")) +
    geom_text(aes(y = Indice, label = round(Indice,2)), fontface = "bold", vjust = 1.4, color = "black", size = 3) +
    geom_line(aes(y = blupNota * 1.5, group = 1, color = 'blackline')) +
    geom_text(aes(y = blupNota * 1.5, label = round(blupNota, 0)), vjust = 1.4, color = "black", size = 3) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.5)) +
    scale_fill_manual('', labels = 'Indice', values = "#70D997") +
    scale_color_manual('', labels = 'Nota', values = 'black') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}

makeG3 <- function(inGraph3){
  
  inGraph3$Tnota <- 10-inGraph3$blupNota
  
  bp <-  reshape2::melt(inGraph3, id.vars=c("genotipo"))
  
  bpf <- bp %>% filter(variable != "Acuracia",
                       variable != "nLocais",
                       variable != "Indice",
                       variable != "blupNota")
  
  ggplot(bpf, aes(x=reorder(genotipo,-value, sum),y=value, fill = variable)) +
    geom_bar(stat = "identity")+
    scale_fill_viridis(discrete = T) +
    ggtitle("Agrupando Yield e Inverso da Nota CS") +
    xlab("Genotipos")+
    # theme_ipsum()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

makeG4 <- function(inGraph4){
  ggplot(inGraph4, aes(x=reorder(genotipo,blupNota, sum),y=blupNota, fill = genotipo)) +
    geom_bar(stat = "identity")+
    scale_fill_viridis(discrete = T) +
    # ggtitle("Agrupando Yield e Inverso da Nota CS") +
    xlab("Genotipos")+
    # theme_ipsum()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}

makeG5 <- function(ctl, hb1, inGraph5){
  # ctl <- controles
  # hb1 <- "NS90PRO2"
  
  if(hb1 %in% ctl){
    hb1 <- ""
  }
  
  
  ctlHB <- append(ctl,hb1)
  inGraph5$genotipo <- as.character(inGraph5$genotipo)
  
  cd <- inGraph5[1,]
  for( i in 1: length(ctlHB)){
    cddf <-filter(inGraph5,genotipo == ctlHB[i])
    cd <- rbind(cd,cddf)
  }
  cd <- cd[-1,]
  
  
  
  cdf <-  reshape2::melt(cd, id.vars=c("genotipo"))
  
  cdf <- cdf %>% filter(variable == "blupNota")
  cdf$value <- as.numeric(cdf$value)
  
  cdf <- cdf %>%
    mutate(cond = case_when(
      value < 3 ~ 'gree',
      value == 3 ~ '#FFFEBA',
      value == 4 ~ '#FFFEBA',
      value == 5 ~ '#DFA995',
      value == 6 ~ '#DFA995',
      value > 6 ~ 'red',
      TRUE ~ 'yellow'   #anything that does not meet the criteria above
    ))
  
  cdf <- cdf %>% filter(variable == "blupNota")
  
  ggplot(cdf %>% arrange(variable, desc(value)) %>%
           mutate(genotipo=factor(genotipo, levels=genotipo)),
         aes(x=genotipo,y=value, fill = cond)) +
    geom_bar(stat="identity", show.legend=FALSE) +
    geom_text(aes(label=round(value,2), y=0.5*value), colour="black", size=3) +
    facet_grid(. ~ variable, scales="free_x", space="free_x") +
    scale_y_continuous(limits=c(-0.005, 1.05*max(cdf$value)), expand=c(0,0)) +
    theme_classic() +
    scale_fill_identity() +
    theme(panel.spacing=unit(0,"pt"),
          panel.border=element_rect(colour="grey50", fill=NA))
  
}

makeG6 <- function(inGraph6){
  ggplot(inGraph6, aes(x = genotipo)) +
    geom_col(aes( y = blupYield, fill="redfill")) +
    geom_text(aes(y =blupYield, label = blupYield), fontface = "bold", vjust = 1.4, color = "black", size = 3) +
    geom_line(aes(y = blupNota * 1.5, group = 1, color = 'blackline')) +
    geom_text(aes(y = blupNota * 1.5, label = round(blupNota, 0)), vjust = 1.4, color = "black", size = 3) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~ . / 1.5)) +
    scale_fill_manual('', labels = 'Yield', values = "darkgreen") +
    scale_color_manual('', labels = 'Nota', values = 'black') +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}

makeG7 <- function(inGraph7){
  ggplot(inGraph7,aes(x="Nota",y="yield")) +
    geom_point() +
    scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
    geom_smooth(method="lm")
  
}


makeG8 <- function(controls, candis, accHB, inStg){
  
  # controls <- controles
  # candis <- "NS90PRO2"
  # accHB <- 83.10
  # inStg <- 6
  
  
  # Filtrando controles
  card1 <- filter(preFINAL, genotipo %in% controls, Stage == inStg) %>% arrange(Indice)
  card1$correct <- card1$Indice + (-1)*min(card1$Indice)
  card1$Percent <- card1$correct/max(card1$correct)*100
  
  # Filtrando candidato
  cand1 <- filter(preFINAL, genotipo %in% candis, Stage == inStg)
  cand1$correct <- cand1$Indice + (-1)*min(card1$Indice)
  cand1$Percent <- cand1$correct/max(card1$correct)*100
  cH <- cand1$Percent
  
  cardT1 <- rbind(card1,cand1)
  cardT <- cardT1 %>% select(genotipo, blupNota, blupYield, Indice, nLocais, Acuracia, correct, Percent)
  cardT <<- cardT %>% distinct(genotipo, .keep_all = T)
  
  # brks <- card1$Percent
  # nomesG <- card1$genotipo
  # nomeH <- cand1$genotipo
  
  # Padrao
  gg.gauge <- function(pos,breaks=card1$Percent) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    ggplot()+
      ggtitle(paste0("Reliability: ",accHB))+
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="#EB5C5F")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="#FA9594")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="#FED3D9")+
      geom_polygon(data=get.poly(breaks[4],breaks[5]),aes(x,y),fill="#F2E599")+
      geom_polygon(data=get.poly(breaks[5],breaks[6]),aes(x,y),fill="#C4D1B2")+
      geom_polygon(data=get.poly(breaks[6],breaks[7]),aes(x,y),fill="#94B39A")+
      geom_polygon(data=get.poly(breaks[7],breaks[8]),aes(x,y),fill="#658C72")+
      geom_polygon(data=get.poly(pos-1,pos+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=3, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=card1$genotipo))+
      annotate("text",x=0,y=0,label=cand1$genotipo,vjust=0,size=4,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
  }
  # card1$Percent <- as.numeric(card1$Percent)
  gg.gauge(cH,breaks=card1$Percent)
  
}

controles <- c("DEFENDERVIP3","FEROZVIP3","SUPREMOVIP3","FORMULAVIP2","SYN488VIP3",
  "AG9025PRO3","DKB230PRO3","SYN422VIP3","STATUSVIP3","NS90PRO2")

makeG9 <- function(stg, crp, trait, contro, candi){
  stg=6
  crp="Corn"
  trait = "HELMR"
  contro = controles
  candi = "NK555VIP3"
  # 
  nomesList <- c(candi,contro)
  
  if(trait == "Ear Rot"){
    inG <- dataRot
    colnames(inG) <- c("Nota","genotipo","Year")
    labName <- "Percentage (%)"
  }else{
    # inG <- myDFF %>% filter(Stage == stg, Crop == crp, Trait == trait, genotipo==candi )
    inG <- myDF %>% filter(genotipo%in%nomesList, stage==stg, )
    colnames(inG)[2] <- "Nota"
    labName <- "Nota"
  }
  
  avg9 <- data.frame(tapply(inG$Nota,inG$genotipo,mean))
  avg9$genotipo <- row.names(avg9)
  colnames(avg9) <- c("Nota","genotipo")
  if(candi %in% contro){
    candi <- ""
  }
  allG <- c(contro,candi)
  avgF <- filter(avg9, genotipo %in% allG)
  avgF$Nota <- round(avgF$Nota, 0)
  
  # plot
  avgF <- avgF %>%
    mutate(cond = case_when(
      Nota < 3 ~ 'green',
      Nota == 3 ~ '#FFFEBA',
      Nota == 4 ~ '#FFFEBA',
      Nota == 5 ~ '#DFA995',
      Nota == 6 ~ '#DFA995', 
      Nota > 6 ~ 'red',
      TRUE ~ 'yellow'   #anything that does not meet the criteria above
    ))
  ggplot(data=avgF, aes(x=reorder(genotipo,-Nota, sum),y=Nota, fill=cond)) +
    ylab(labName)+
    xlab("Genotype")+
    geom_bar(stat="identity", show.legend=FALSE)+
    geom_text(aes(label=Nota), vjust=-0.3, size=3.5)+
    theme_classic() +
    scale_fill_identity()
}


makeG10 <- function(myGM, crp, trait, contro, candi){
  # myGM = "6L-7C"
  # crp = "Soy"
  # trait = "ANTRCR"
  # candi = "DM68I69IPRO"
  # contro = "NS7780IPRO"
  gmCandi <- soyFinal %>% filter(genotipo %in% candi, Trait==trait, GM == myGM) %>% select(genotipo, GM, Nota)
  
  avg9 <- data.frame(genotipo=candi, GM=myGM, Nota= mean(gmCandi$Nota))
  
  
  allControl <- unique(checks$Cultivar)
  
  selControl <- soyFinal %>% filter(genotipo %in% allControl,
                                    Trait==trait, GM==myGM) %>% select(genotipo, GM, Nota)
  
  
  meanControl <- data.frame(tapply(selControl$Nota,selControl$genotipo,mean))
  meanControl$genotipo <- rownames(meanControl)
  meanControl$GM <- myGM
  colnames(meanControl)[1] <- "Nota"
  meanControl <- meanControl %>% select(genotipo, GM, Nota)
  
  
  avg9 <- rbind(avg9, meanControl)
  avg9 <- avg9 %>% distinct(genotipo, .keep_all = T)
  
  # plot
  avg9 <- avg9 %>%
    mutate(cond = case_when(
      Nota < 3 ~ 'green',
      Nota == 3 ~ '#FFFEBA',
      Nota == 4 ~ '#FFFEBA',
      Nota == 5 ~ '#DFA995',
      Nota == 6 ~ '#DFA995', 
      Nota > 6 ~ 'red',
      TRUE ~ 'yellow'   #anything that does not meet the criteria above
    ))
  
  avg9$Nota <- round(avg9$Nota,0)
  ggplot(data=avg9, aes(x=reorder(genotipo,-Nota), y=Nota, fill=cond)) +
    ylab("Score")+
    xlab(paste0("Genotype GM ", myGM))+
    geom_bar(stat="identity", show.legend=FALSE)+
    coord_flip() +
    geom_text(aes(label=Nota), vjust=-0.3, size=3.5)+
    theme_classic() +
    # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_fill_identity()
}

# myMap <- function(coord_pontos){
#   data.frame(st_coordinates(coord_pontos),
#              com_rede = coord_pontos$com_rede,
#              UF = coord_pontos$name_state) %>%
#     leaflet() %>%
#     addTiles() %>%
#     addCircleMarkers(~ X, ~ Y,
#                      label = ~ as.character(paste0(UF, ": ", com_rede, "%")),
#                      labelOptions = labelOptions(textsize = "13px"),
#                      radius = ~ com_rede/10,
#                      fillOpacity = 0.5)
# }



