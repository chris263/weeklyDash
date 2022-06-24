
#removing warnings
options(warn = -1)

# Script para gerar os graficos
library(ggplot2)


makeG0 <- function(inGraph0, cand1, tpp1, stg1){
  # inGraph0 <- myGenLoc
  # stg1 = 6
  # tpp1 <- "TPP09"
  # cand1 <- "DKB255PRO3"
  
  inGraph00 <- inGraph0 %>% filter(genotipo %in% cand1, TPP == tpp1, stage==stg1)
  
  ggplot(inGraph00, aes(x=classe, y=blueN, fill=classe)) + 
    ggtitle(tpp1) +
    geom_boxplot() +
    theme(legend.position="none")
  
}

makeG01 <- function(inData, inChecks, candi){
  # inData <- myJoint
  # inChecks <- myChecks
  # candi <- "B2612PWU"
  
  inChecks <- inChecks %>% filter(TPP == "TPP11", Trait == "COSTR", valor== 1) %>% select(Genotipo, Esperado)
  inChecks <- distinct(inChecks,Genotipo, .keep_all = T)
  
  inData$blupNota <- round(as.numeric(inData$blupNota),0)

  
  inData <- inData %>% filter(genotipo %in% candi, type == 'trial') %>% select(genotipo, blupNota)
  colnames(inData) <- c("genotipo","Nota")
  colnames(inChecks) <- c("genotipo","Nota")
  
  avgF <- rbind(inChecks,inData)
  
  avgF <- distinct(avgF,genotipo, .keep_all = T)
  
  
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
    ggtitle("Resultado Conjunto - Alta Pressao:") +
    ylab("Notas")+
    xlab("Genotype")+
    geom_bar(stat="identity", show.legend=FALSE)+
    geom_text(aes(label=Nota), vjust=-0.3, size=3.5)+
    theme_classic() +
    scale_fill_identity() +
    coord_flip()
}

makeG02 <- function(inData, candi1, candi2){
  # inData <- myJoint
  # candi1 <- "DKB255PRO3"
  # candi2 <- "AS1820PRO3"
  
  candis <- c(candi1,candi2)
 
  inData$blupNota <- round(as.numeric(inData$blupNota),0)
  inData <- inData %>% filter(genotipo %in% candis, type=="trial") %>% select(genotipo, blupNota)
  colnames(inData) <- c("genotipo","Nota")
  avgF <- distinct(inData,genotipo, .keep_all = T)
 
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
    ylab("Notas")+
    xlab("Genotype")+
    geom_bar(stat="identity", show.legend=FALSE)+
    geom_text(aes(label=Nota), vjust=-0.3, size=3.5)+
    theme_classic() +
    scale_fill_identity() +
    coord_flip()
}




