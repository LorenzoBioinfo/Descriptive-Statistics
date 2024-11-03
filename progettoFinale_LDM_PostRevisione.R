##########################################
# Progetto Finale Statistica Descrittiva #
# Lorenzo De Marco						           #
##########################################
library(ggplot2)
#install.packages("ggstats")
library(ggstats)

# 1)
data<-read.delim("./realestate_texas.csv",sep=",")


cat(unique(data$city),sep="\n")
min(data$year)
max(data$year)
# 2) Indica il tipo di variabili contenute nel dataset.

str(data)

#3) Calcola Indici di posizione, variabilità e forma per tutte le variabili 
# per le quali ha senso farlo, per le altre crea una distribuzione di frequenza. 
# Commenta tutto brevemente.


# Indici Posizione

findIndexPosition<-function(var){
  min_var<-min(var)
  max_var<-max(var)
  quartile_1<-quantile(var)[2]
  quartile_3<-quantile(var)[4]
  mean_var<-mean(var)
  median_var<-median(var)
  cat("Valore minimo: ",min_var)
  cat("\n")
  cat("Valore massimo: ",max_var)
  cat("\n")
  cat("Primo quartile: ",quartile_1)
  cat("\n")
  cat("Terzo quartile: ",quartile_3)
  cat("\n")
  cat("Media è: ",  mean_var)
  cat("\n")
  cat("Mediana è: ",  median_var)
}
## Sales
findIndexPosition(data$sales)
## Volume
findIndexPosition(data$volume)
## Listing
findIndexPosition(data$listings)
## Month inventor
findIndexPosition(data$months_inventory)
## Median price
findIndexPosition(data$median_price)



# Indici Variabilità

findIndexVariability<-function(col,name){
 
  range<-max(col)-min(col)
  interquartile<-IQR(col)
  varianza<-var(col)
  sd<-sd(col)
  
  cv<-sd(col)/mean(col)*100
 
  ggplot(data,aes(x=factor(0),y=col))+geom_boxplot()+theme_classic()+ylab(name)+
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
  
  cat("Range: ",range)
  cat("\n")
  cat("Range interquatile:",interquartile)
  cat("\n")
  cat("Varianza: ",varianza)
  cat("\n")
  cat("Deviazione standard :",sd)
  cat("\n")
  cat("Coefficiente di variazione :",cv)
  cat("\n")
}

## Sales
findIndexVariability(data$sales,"sales")
## Volume
findIndexVariability(data$volume,"volume")
## Listing
findIndexVariability(data$listings,"listings")
## Month inventor
findIndexVariability(data$months_inventory,"Month inventory")
## Median price
findIndexVariability(data$median_price,"Median price")


############################
install.packages("moments")
library(moments)

findIndexForm<-function(col){
  asimmetria=skewness(col)
  kurtosis=kurtosis(col)-3
  cat("Asimmetria :",asimmetria)
  cat("\n")
  cat("Curtosi :",kurtosis)
  cat("\n")
  
}
## Sales
findIndexForm(data$sales)
## Volume
findIndexForm(data$volume)
## Listing
findIndexForm(data$listings)
## Month inventor
findIndexForm(data$months_inventory)
## Median price
findIndexForm(data$median_price)

####### Frequenza
freq_abs<-table(data$freq_abs)
freq_rel<-table(data$freq_abs)/length(data$freq_abs)



# DataViz 
library(pheatmap)

data$sales
ggplot(data,aes(sales))+geom_histogram(color="black", fill="white")+theme_classic()+xlab("Sales")+
  geom_vline(aes(xintercept=mean(sales)),
               color="blue", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(sales)),
             color="indianred", linetype="dashed", size=1) 


ggsave("Hist_sales.jpeg")


ggplot(data, aes(x=sales)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +theme_classic()

ggsave("Hist_sales_dist.jpeg")


ggplot(data, aes(x=volume)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="violet") +theme_classic()+
  geom_vline(aes(xintercept=mean(volume)),
             color="orange", linetype="dashed", size=1)+
  geom_vline(aes(xintercept=median(volume)),
             color="black", linetype="dashed", size=1)



tab<-table(data$city)/240*100
dis<-t(tab)
pheatmap::pheatmap(t(tab),cellwidth = 35,cellheight = 35,scale="none",display_numbers ="25%" ,
                   color = c("white"),angle_col = 45,cluster_cols = F,cluster_rows = F,legend = F,breaks = 1)






#5) Dividi una delle variabili quantitative in classi, 
#scegli tu quale e come, costruisci la distribuzione di frequenze, 
#il grafico a barre corrispondente e infine calcola l’indice di Gini.


quantile(data$listings)

data$Group<-0
data[data$listings<=1026.5,]$Group<-1
data[data$listings>1026.5 & data$listings<=2056.0,]$Group<-2
data[data$listings>2056.0,]$Group<-3


table(data$Group)
table(data$Group)/length(data$Group)


gini.index<-function(x){
  ni<-table(x)
  frel<-ni/length(x)
  f2=frel**2
  j=length(table(x))
  gini=1-sum(f2)
  gini.norm=gini/((j-1)/j)
  
  return(gini.norm)
}

gini.index(data$Group)

data$Group<-as.factor(data$Group)
library(gridExtra)
p<-ggplot(data)+geom_boxplot(aes(factor(Group),median_price,col=Group))+theme_classic()+xlab("Group")


p2<-ggplot(data)+geom_boxplot(aes(factor(Group),sales,col=Group))+theme_classic()+xlab("Group")


p3<-ggplot(data)+geom_boxplot(aes(factor(Group),volume,col=Group))+theme_classic()+xlab("Group")


p4<-ggplot(data)+geom_boxplot(aes(factor(Group),months_inventory,col=Group))+theme_classic()+xlab("Group")

grid.arrange(p,p2,p3,p4,ncol=2)



#6 )Indovina l’indice di gini per la variabile city
gini.index(data$city)



#7) Qual è la probabilità che presa una riga a caso di questo dataset essa riporti la città “Beaumont”? 
#E la probabilità che riporti il mese di Luglio? E la probabilità che riporti il mese di dicembre 2012?


nrow(data[data$city=="Beaumont",])/nrow(data)

nrow(data[data$month==7,])/nrow(data)*100


nrow(data[data$month==12& data$year==2012,])/nrow(data)*100



#8) Esiste una colonna col prezzo mediano, creane una che indica invece il prezzo medio,
# utilizzando le altre variabili che hai a disposizione.

data$mean_price<-data$volume/data$sales



#9) Prova a creare un’altra colonna che dia un’idea di “efficacia” degli annunci di vendita.
#Riesci a fare qualche considerazione?

data$score<-data$sales/data$listings

data[data$score==max(data$score),]
data[data$score==min(data$score),]


#10) Prova a creare dei summary(), o 
# semplicemente media e deviazione standard, di alcune variabili a tua scelta, 
#condizionatamente alla città, agli anni e ai mesi. 
#uoi utilizzare il linguaggio R di base oppure essere un vero Pro con il pacchetto dplyr. 
#Ti lascio un suggerimento in pseudocodice, oltre al cheatsheet nel materiale:

library(dplyr)
data %>%
  
  group_by(city) %>%
  
  summarise(mean=mean(sales),
            
            devst=sd(sales))


######################### Grafici con ggplot

#1) Utilizza i boxplot per confrontare la distribuzione del prezzo mediano delle case tra le varie città.
#Commenta il risultato

library(ggbeeswarm)
data %>%
  
  group_by(city) %>%
  
  ggplot()+geom_boxplot(aes(city,median_price,color=city))+geom_quasirandom(aes(city,median_price,color=city))+theme_classic()+ylab("Median price")+
  ggtitle("Prezzo mediano per città")+xlab("Cities")

ggsave("graph_1.jpeg")



#2) Utilizza i boxplot o qualche variante per confrontare la distribuzione del
#valore totale delle vendite tra le varie città ma anche tra i vari anni
#Qualche considerazione da fare?

data %>%
  
  group_by(city,year) %>%
  ggplot()+
  geom_boxplot(aes(x=factor(year),sales))+facet_wrap(~city)+theme_minimal()+xlab("Year")+
  ylab("Sales")

ggsave("graph_2.jpeg")



#3) Usa un grafico a barre sovrapposte per ogni anno, 
#per confrontare il totale delle vendite nei vari mesi, 
#sempre considerando le città. Prova a commentare ciò che viene fuori.
#Già che ci sei prova anche il grafico a barre normalizzato. 
#Consiglio: Stai attento alla differenza tra geom_bar() e geom_col().
#PRO LEVEL: cerca un modo intelligente per inserire ANCHE la variabile
#Year allo stesso blocco di codice,
#senza però creare accrocchi nel grafico.


data %>%
  
  group_by(city) %>%
  ggplot(aes(x =month, y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Vendite mensili per città (sovrapposte)",
       x = "Mese",
       y = "Vendite") +scale_x_continuous(breaks = 1:12, labels = month.abb[1:12])+
  scale_fill_brewer(palette = "Set3") +theme_classic()

ggsave("graph_3.jpeg")


data %>%
  
  group_by(city) %>%
  ggplot(aes(x =month, y = sales, fill = city)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Vendite mensili per città (sovrapposte)",
       x = "Mese",
       y = "Vendite") +scale_x_continuous(breaks = 1:12, labels = month.abb[1:12])+
  scale_fill_brewer(palette = "Set3") +theme_classic()+facet_wrap(~ year, ncol = 1)

ggsave("graph_3_pro.jpeg")


#4) Crea un line chart di una variabile a tua scelta per fare confronti 
#commentati fra città e periodi storici.
#Ti avviso che probabilmente all’inizio ti verranno fuori 
#linee storte e poco chiare, ma non demordere. 
#Consigli: Prova inserendo una variabile per volta. 
#Prova a usare variabili esterne al dataset, 
#tipo vettori creati da te appositamente.
data %>%
  
  group_by(city,month)%>%
  summarise(median=median(sales))%>%
  ggplot( aes(x = month, y = median, color = city, group = city)) +
    geom_point() +  # Aggiunge i punti
    geom_line() +
    labs(title = "Andamento delle vendite totali nel tempo",
         x = "Mese",
         y = "Vendite totali") +
    scale_color_brewer(palette = "Set1") +theme_bw()+
    scale_x_continuous(breaks = 1:12, labels = month.abb[1:12])

ggsave("graph_4.jpeg")











