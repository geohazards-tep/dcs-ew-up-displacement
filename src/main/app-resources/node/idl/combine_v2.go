;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDL 8.2 code
; writen by F. Casu 04/2016
;
; To be run from the parent directory 
; at command line with:
; @tools/combine_v2.go
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; compile source code
.r tools/combine_v2.pro

; Define path
pathgen='.'
pathAsc   = './data/asc_T408'
pathDesc  = './data/desc_T472'

; Define input files
;T408
mask_cohA = 'mask_GEO_1165x1085.dat'
x_cohA    = 1165l
y_cohA    = 1085l
mask_velA = 'vel_GEO_1165x1085.dat'

;T472
mask_cohD = 'mask_GEO_1412x1336.dat'
x_cohD    = 1412l
y_cohD    = 1336l
mask_velD = 'vel_GEO_1412x1336.dat'


; Run the combination tool
combine,pathgen,d_zz,d_ew,COH_COM,pathAsc=pathAsc,pathDesc=pathDesc,mask_cohA=mask_cohA,x_cohA=x_cohA,y_cohA=y_cohA,mask_velA=mask_velA,mask_cohD=mask_cohD,x_cohD=x_cohD,y_cohD=y_cohD,mask_velD=mask_velD