#6 novembre 2017: programma che mette insieme i breakpoints di ACMANT con i breakpoints di Climatol
#I breakpoints sono quelli trovati a livello mensile.

#Il programma usa la funzione reformatACMANT() per leggere i breakpoints di ACMANT e trasporli in un dataframe.
#Il programma inoltre si aspetta un corrispettivo file di breakpoints con i risultati di climatol. Questo file
#viene identificato dal fatto di iniziare con il prefisso "climatol_". Se uno dei due file non viene trovato il programma termina.

#Output: un dataframe "breakpoints_comuni_climatol_acmant.csv"
#Due grafici: uno che illustra il numero di breakpoints per ciascuna serie, l'altro che mostra il numero di anni (in valore assoluto)
#che intercorrono tra un breakpoint individuato da ACMANT e uno individuato da Climatol.
rm(list=objects())
library("tidyverse")
library("cairoDevice")
library("RColorBrewer")
source("reformat_acmant_breakpoints.R")

#tolleranza, gap, tra il breakpoint trovato da climatol e acmant
GAP_ANNI<-2

#i breakpoint vicini agli estremi della serie (anno di inizio o anno di fine) vengono scartati se distatno da anno inizio/fine meno di GAP_ANNI_ESTREMI
GAP_ANNI_ESTREMI<-3

#legge il file dei breakpoints di acmant e restituisce un df con i risultati per ciascuna serie (solo per le serie con 1 o più bp)
#Siccome il file di breakpèoint potrebbe già essere il risultato di reformatACMANT, prevediamo anche una lettura con read_delim
tryCatch({
  reformatACMANT("acmant.brk.txt") %>%
    mutate(Date=as.Date(Date)) %>%
    rename(acmant_date=Date)
},error=function(e){
  read_delim("acmant.brk.txt",delim=",",col_names = TRUE) %>%
    mutate(Date=as.Date(Date)) %>%
      rename(acmant_date=Date)    
})->acmantBrk

#cerca un file per Climatol.. se non lo trova il programma termina
list.files(pattern = "^climatol.+csv$")->fileClimatol
if(length(fileClimatol)!=1) stop(sprintf("Mi aspettavo un file per climatol ne ho trovati %d",length(fileClimatol)))
read_delim(fileClimatol,delim=",",col_names = TRUE) %>%
  mutate(metodo="Climatol") %>%
  mutate(Date=as.Date(Date)) %>%
  rename(climatol_date=Date)->climatolBrk

#filtra i breakpoint troppo vicini in Climatol:
#1) se due breakpoint cadono nello stesso anno, prendiamo per quell'anno solo il breakpoint con SNHT maggiore
climatolBrk %<>% mutate(anno=year(climatol_date)) 
climatolBrk %>% 
    group_by(Code,anno) %>%
      summarise(massimo=max(SNHT))->filtrati

left_join(filtrati,climatolBrk,by=c("Code"="Code","anno"="anno","massimo"="SNHT")) %>%
  rename(SNHT=massimo)->climatolBrk_filtro1

#2) Secondo filtro: due breakpoint troppo vicini equivalgono a un breakpoint
purrr::map(unique(climatolBrk_filtro1$Code),.f=function(stazione){

  climatolBrk_filtro1 %>% filter(Code==stazione)->subDati

  subDati[order(subDati$anno),]->subDati
  diff(subDati$anno)->diffanno
  if(any(diffanno==0)) stop("Non è possibile: ho già eliminato i breakpoint nello stesso anno!")
  #if(all(diffanno>2)) return(subDati)

  subDati->tmp
  
  while(any(diffanno<=2)){

      posizioni<-c()
      daeliminare<-c()
      
      print(unique(subDati$Code))
    
      for(ii in 1:length(diffanno)){

        if(diffanno[ii]>2){
          
          posizioni<-c(posizioni,c(ii+1,ii))
        }else{
  
          c(ii,ii+1)[which.max(c(subDati$SNHT[ii],subDati$SNHT[ii+1]))]->ris
          posizioni<-c(posizioni,ris)
          if(ris==ii){
            daeliminare<-c(daeliminare,ii+1)            
          }else{
            daeliminare<-c(daeliminare,ii)  
          }
         
        }  
        
      }#fine ciclo for
      

      setdiff(posizioni,daeliminare)->posizioni
      subDati[sort(unique(posizioni)),]->subDati
      diff(subDati$anno)->diffanno

  }
  

  subDati
  
    
}) %>% reduce(rbind) %>% as.data.frame() -> climatolBrk_filtro2 #fine map


full_join(climatolBrk_filtro2,acmantBrk,by=c("Code"="Code")) %>% mutate(gapYears=round(as.double((climatol_date-acmant_date)/365),2) )->listaBrk
  
#calcoliamo la differenza (in termini di anni) tra il breakpoint trovato da ACMANT  e quello trovato da CLIMATOL:
#il focus è sui breakpoint che distano in valore assoluto + o - GAP_ANNI
listaBrk %>% 
  filter(abs(gapYears)<=GAP_ANNI)->risultato1

  risultato1$firstYear<-as.Date(paste0(unlist(map(str_split(risultato1$period,"-"),1)),"-01-01"))
  risultato1$lastYear<-as.Date(paste0(unlist(map(str_split(risultato1$period,"-"),2)),"-12-31"))

#a questo punto abbiamo solo i breakpoint vicini tra acmant e climatol: abbiamo però bisogno di un altro filtro
#Ovvero: dobbiamo eliminare i breakpoint troppo vicini al punto di partenza della serie (firstYear o lastYear)

risultato1 %>% mutate(zz=min(abs(as.double((climatol_date-firstYear)/365)),abs(as.double((acmant_date-firstYear)/365)))  )

risultato1 %>% 
  mutate(tmp1=abs(as.double((climatol_date-firstYear)/365)),tmp2=abs(as.double((acmant_date-firstYear)/365)),minimo1=ifelse(tmp1<=tmp2,tmp1,tmp2)) %>%
  mutate(tmp3=abs(as.double((climatol_date-lastYear)/365)),tmp4=abs(as.double((acmant_date-lastYear)/365)),minimo2=ifelse(tmp3<=tmp4,tmp3,tmp4)) %>%
  mutate(gap_from_extremes=ifelse(minimo1<=minimo2,minimo1,minimo2)) %>%
  filter(gap_from_extremes>GAP_ANNI_ESTREMI) %>%
  select(-matches("^tmp[0-9]"),-matches("^minimo[0-9]"))->risultato2

write_delim(risultato2,"breakpoints_comuni_climatol_acmant.csv",delim=";",col_names=TRUE)

rm(risultato1)

risultato2 %<>%mutate(regione=str_replace(Code,"_.+",""),Code2=str_replace(Code,"^[^_]+_",""))
cairo_pdf("conteggio_breakpoints.pdf",width=12,height=8,pointsize = 5,onefile = TRUE)
purrr::walk(unique(risultato2$regione),.f=function(rr){
  
  risultato2 %>%
  filter(regione==rr) %>%  
  ggplot(aes(x=Code2))+
  geom_bar(aes(fill=regione))+
  facet_wrap(~regione,scales = "free_x")+
    scale_fill_discrete(guide="none")+
  theme_bw()+
  theme(text=element_text(family="Lato"),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill="#FFFFFF"))->grafico
  print(grafico)
  
})
dev.off()

#grafico per gapYears
brewer.pal(n=9,name="YlOrRd")[4:9]->colori
colori[as.integer(cut(abs(risultato2$gapYears),breaks = seq(-0.5,GAP_ANNI,0.5)))]->risultato2$colori
cairo_pdf("gapYears_breakpoints_climatol_acmant.pdf",width=12,height=8,pointsize = 5)
par(family="Lato")
order(abs(risultato2$gapYears))->ordine
dotchart(x=abs(risultato2$gapYears[ordine]),labels = risultato2$Code[ordine],lcolor="gainsboro",bg = risultato2$colori[ordine],color=NULL)
dev.off()
