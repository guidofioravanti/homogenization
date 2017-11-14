#6 Novembre 2017
#Il programma legge l'output di ACMANT (il file che contiene la lista dei breakpoints) e produce un nuovo file
#di breakpoints riformattato, con un formato simile e più facilmente confrontabile con quello prodotto da Climatol.
library("tidyverse")
library("stringr")
library("magrittr")

#cerca il file dei breakpoints di ACMANT: deve essere solo 1!
#La funzione accetta un nome file, se non viene passato allora viene cercato un file che
#termina con "brk.txt"

#La funzione restituisce un dataframe con le informazioni sui breakpoints trovati
reformatACMANT<-function(fileBrk=NULL){

    if(is.null(fileBrk)){
      
      list.files(pattern=".+brk.txt")->fileBrk

      if(length(fileBrk)!=1){
        if(!length(fileBrk)) stop("Non ho trovato nessun file brk.txt di ACMANT")
        stop("Ho trovato più file brk.txt di ACAMANT")
      }#fine if
    
    }#fine if su is.null(fileBrk)  
    
    read_lines(fileBrk)-> righe
    righe[!righe %in% ""]->righe
  
    #quali righe contengono i nomi
    grep(" +[A-Z][a-z]+_[0-9]+",righe)->posRigheNomi
    righe[posRigheNomi]->stazioni
    
    #eliminiamo le righe che corrispondono a serie per cui non è stato possibile fare l'omogeneizzazione ( - 1 )
    #Necessario eliminarle perchè altrimenti fallisce il codice che segue.
    if(length(grep(" -1 ",stazioni))) stazioni[-grep(" -1 ",stazioni)]->stazioni
    
    #elimina la sequenza di spazi e produce una tabella (tabellaBrk) in cui sono riportati il nome della serie
    #il numero di breakpoints e il periodo in cui si è omogeneizzato. Sulla base di queste informazioni dobbiamo poi
    #cercare gli effettivi breakpoints.
    str_split(stazioni," +") %>% purrr::map(.,.f=~(.[which(nchar(.)!=0)])) %>% reduce(rbind) %>% as.data.frame()->tabellaBrk
    
    names(tabellaBrk)<-c("id","period","homo_period","nCP","nO","Code")
    #nO: numero di outliers--> non ci interessa
    #nCP: numero di breakpoints per la serie 
    #id: numero sequenziale della serie
    #homo_period: periodo in cui si è omogeneizzato
    #period: periodo della serie:
    #Code: nome della serie (usiamo Code in conformita con Climatol)
    tabellaBrk %<>% mutate(nCP=as.integer(as.character(nCP)),nO=as.integer(as.character(nO)),Code=as.character(Code),period=as.character(period),homo_period=as.character(homo_period))

    #per ogni stazione in tabellaBrk cerchiamo i corrispettivi breakpoints
    purrr::pmap(.l=list(tabellaBrk$Code,tabellaBrk$nCP,tabellaBrk$homo_period,tabellaBrk$period),.f=function(Code,nCP,homo_period,period){
      
      grep(paste0(" +",Code," +$") ,righe)->posizione
      if(length(posizione)!=1) browser()
      
      #quanti breakpoints?
      if(nCP==0) return(NULL)
    
      firstCP<-posizione+1
      lastCP<-posizione+nCP
      
      purrr::map(righe[firstCP:lastCP],.f=function(rr){
        unlist(str_split(rr," +"))->rr
        rr[!rr %in% ""]->rr
        data.frame(Date=paste(rr[2],rr[3],"01",sep="-"),annualChange=rr[4],seasonalChange=rr[5])
      }) %>% reduce(rbind) %>% as.data.frame()->breakpoints
      
      breakpoints$Code<-Code
      breakpoints$nCP<-nCP
      breakpoints$homo_period<-homo_period
      breakpoints$period<-period      
      
      breakpoints
      
    }) %>% compact %>% 
      reduce(rbind) %>% 
      as.data.frame() %>% 
      mutate(metodo="ACMANT")->dfBreakPoints

      dfBreakPoints
      
}