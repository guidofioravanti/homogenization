#!/bin/bash
#Esegue ACMANT3 (ACMANT3monthsinus.exe) nella versione per la temperatura media
#e per la temperatura massima
#elimina i file prodotti da ACMANT3

rm -rf *j.txt
rm -rf *t.txt

#elimina il file delle correlazioni
rm -rf *r1.dat

#elimina il file con i breakpoints
rm -rf *brk.txt


parametro=${1}

if [[ ${parametro} != "tmax" ]] && [[ ${parametro} != "tmin" ]];then
	echo "Parametro ${parametro} non riconosciuto: usa tmax(tempratura massima)/tmin(temperatura minim)"
	exit 1
fi

if [ ${parametro} == "tmax" ];then
	echo "Eseguo ACMANT3monthsinus.exe --> Temperatura massima"
	wine ACMANT3monthsinus.exe
else
	echo "Eseguo ACMANT3monthirreg.exe --> Temperatura minima"
	wine ACMANT3monthirreg.exe
fi
