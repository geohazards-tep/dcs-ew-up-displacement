;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDL 8.2 code
; writen by F. Casu 04/2016
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro main

; Define path
pathGen = ''
read,pathGen
pathOut=pathGen+'/result'
spawn,'mkdir '+pathOut,/sh

; Define input files
lista_ela=pathGen+'/lista_ela'
lista_v=pathGen+'/lista_v'
lista_m=pathGen+'/lista_m'

latAgg = 0.
read,latAgg

lonAgg = 0.
read,lonAgg

; Run the combination tool
combine,pathOut,lista_ela,lista_v,lista_m

end
