library(tidyverse)

dados <- readxl::read_excel("CEP.xlsx")
dados$total <- dados$REJEITADAS+dados$LIBERADAS
dados <- dados |> 
  filter(CLIENTE == 'PAMPA')

# graifico U

u <- dados$REJEITADAS/dados$total
UM <- mean(u)
me <- 3*sqrt((UM/dados$total))
LSC <- UM + me
LIC <- UM - me

g_controle2(vari = u,lsc = LSC,lic = LIC,lm = UM,laby = "")



# Função :
g_controle2<-function(vari,lsc,lic,lm,labx="Amostras",laby="Valor",cal="mean"){
  n<-1:length(vari)
  lic1 = (lm + (1/3*(lic-lm)))
  lic2 = (lm + (2/3*(lic-lm)))
  lsc1 = (lm + (1/3*(lsc-lm)))
  lsc2 = (lm + (2/3*(lsc-lm)))
  lic1[lic1 < 0] <- 0
  lic2[lic2 < 0] <- 0
  lic[lic < 0] <- 0
  lsc1[lsc1 > 1] <- 1
  lsc2[lsc2 > 1] <- 1
  lsc[lsc > 1] <- 1
  graf<-data.frame(vari,n,lsc,lic,lm,lic1,lic2,lsc1,lsc2)
  graf1<-graf |> 
    mutate(fora=case_when(
      vari >= lsc | vari<= lic ~ vari
    ))
  if(cal=="mean"){
    g<-ggplot(graf1) +
      geom_step(aes(x = n, y = lsc1),linetype="longdash",colour = "gray",size = 0.3,data = graf1)+
      geom_step(aes(x = n, y = lic1),linetype="longdash",colour = "gray",size = 0.3,data = graf1)+
      geom_step(aes(x = n, y = lsc2),linetype="longdash",colour = "gray",size = 0.3,data = graf1)+
      geom_step(aes(x = n, y = lic2),linetype="longdash",colour = "gray",size = 0.3,data = graf1)+
      geom_step(aes(x = n, y = lsc),size = 0.6,data = graf1,colour = "gray")+
      geom_step(aes(x = n, y = lic),size = 0.6,data = graf1,colour = "gray")+
      geom_line(aes(x = n, y = lm),size = 0.4,data = graf1,colour = "gray")+
      geom_line(aes(x = n, y = vari),size = 0.5, colour = "black")+
      geom_point(aes(x = n, y = vari),shape = "circle", size = 1.5L,colour = "black") +
      geom_point(aes(x = n, y = fora),shape = "circle", size = 1.5L,colour = "red") +
      labs(x = labx, y = laby) +
      scale_y_continuous(breaks = round(c(mean(lsc),mean(lic),mean(lm),mean(lic1),mean(lic2),mean(lsc1),mean(lsc2)),3),)+
      scale_x_continuous(breaks = seq(0,100,2))+
      theme_classic()
  }else{
    g<-ggplot(graf1) +
      geom_step(aes(x = n, y = lsc),colour = "gray",size = 0.6,data = graf1)+
      geom_step(aes(x = n, y = lic),colour = "gray",size = 0.6,data = graf1)+
      geom_line(aes(x = n, y = lm),colour = "gray",size = 0.4,data = graf1)+
      geom_line(aes(x = n, y = vari),size = 0.5, colour = "black")+
      geom_point(aes(x = n, y = vari),shape = "circle", size = 1.5L,colour = "black") +
      geom_point(aes(x = n, y = fora),shape = "circle", size = 1.5L,colour = "red") +
      labs(x = labx, y = laby) +
      scale_y_continuous(breaks = round(c(mean(lsc),mean(lic),mean(lm),mean(lic1),mean(lic2),mean(lsc1),mean(lsc2)),3),)+
      # annotate("label", x=c(0,0,0), y = c(lsc,lm,lic), label = c("LSC","LM","LIC"),hjust=c(0.2,0.2,0.2))+
      scale_x_continuous(breaks = seq(0,100,2))+
      theme_classic()
  }
  return(g)
}
