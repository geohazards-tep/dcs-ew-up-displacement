pro combine,pathout,lista_ela,lista_v,lista_m,latAgg=latAgg,lonAgg=lonAgg


;    device,decomposed=0
;    loadct,44,file='prova_colortable.tbl'
    spawn,'cat '+lista_ela,lista_elab,/sh
    n=N_elements(lista_elab)
  
    spawn,'cat '+lista_v,lista_vel,/sh
    n1=N_elements(lista_vel)

    spawn,'cat '+lista_m,lista_mask,/sh
    n2=N_elements(lista_mask)

;stop 

    if (n ne n1) or (n ne n2) or (n1 ne n2) then STOP

    dimXVett = lonarr(n)
    dimYVett = lonarr(n)
    lonSt = dblarr(n)
    latSt = dblarr(n)
    latEn = dblarr(n)
    lonEn = dblarr(n)

    Vettore_delle_Orbite = strarr(n)
    Vettore_dei_Sensori =  strarr(n)
;*************************************************************************
    obj=OBJ_NEW('IDL_savefile',filename=lista_elab(0)+'/ParDem.sav')    
    obj->restore,'PASSOE'  
    obj->restore,'PASSON'   
    obj_destroy,obj 
    passoLon = PASSOE
    passoLat = PASSON
 ;***********************************************************************

;;  vett_min_Max=dblarr(4,n)
;;  Limits_geo=dblarr(8,n)
  orbitVett = strarr(n)
  valOrbVett = fltarr(n)

  for i=0l,n-1l do begin
     obj=OBJ_NEW('IDL_savefile',filename=lista_elab(i)+'/ParDem.sav')    
     obj->restore,'DIMXP'  
     obj->restore,'DIMYP'
     obj->restore,'ESTARTNP'  
     obj->restore,'NSTARTNP'     
     obj_destroy,obj 
     
     dimXVett(i)=DIMXP
     dimYVett(i)=DIMYP
     lonSt(i)=ESTARTNP
     latSt(i)=NSTARTNP
     latEn(i)=NSTARTNP+(DIMYP*passoLat)
     lonEn(i)=ESTARTNP+(DIMXP*passoLon)

     ;;vettSD_RU(0,i)=latSt(i)
     ;;vettSD_RU(1,i)=latEn(i)
     ;;vettSD_RU(2,i)=lonSt(i)
     ;;vettSD_RU(3,i)=lonEn(i)

;  endfor




; for l=0l,n-1l do begin
;    obj=OBJ_NEW('IDL_savefile',filename=lista_elab(l)+'/limits.sav')    
;    obj->restore,'LIMITSG'  
;    obj_destroy,obj 

;;     limitsg=latSt(i),lonSt(i),LatEn(i)

    ;; Limits_geo(*,l)=limitsg
    
    ;; vett_min_Max(0,l)=min([limitsg(0),limitsg(2),limitsg(4),limitsg(6)])
    ;; vett_min_Max(1,l)=max([limitsg(0),limitsg(2),limitsg(4),limitsg(6)])
     ;; vett_min_Max(2,l)=min([limitsg(1),limitsg(3),limitsg(5),limitsg(7)])
     ;; vett_min_Max(3,l)=max([limitsg(1),limitsg(3),limitsg(5),limitsg(7)])

     orb = strsplit(lista_elab(i),'/',/extract)
     print,orb
     orb = orb(n_elements(orb)-1)
     print,orb
     orb = strmid(orb,0,1)
     print,orb
     if orb eq 'a' then begin
        orbb = 'ASC'
        valo = 2.
     endif else begin
        orbb = 'DESC'
        valo = 1.
     endelse

    orbitVett(i) = orbb
    valOrbVett(i) = valo

 endfor




; maxLat = max(latEn)
; minLat = min(latSt)
; maxLon = max(lonEn)
; minLon = min(lonSt)


 maxLat = min(latEn)
 minLat = max(latSt)
 maxLon = min(lonEn)
 minLon = max(lonSt)



;stop

 rangeLat = (minLat)+(lindgen(long64((maxLat-minLat)/passoLat)+1)*passoLat)
 rangeLon = (minLon)+(lindgen(long64((maxLon-minLon)/passoLon)+1)*passoLon)

 
; DimrangeLat = long64(((maxLat-minLat)/passoLat)+1l)
; DimrangeLon = long64(((maxLon-minLon)/passoLon)+1l)
 DimrangeLat = long64(((maxLat-minLat)/passoLat))
 DimrangeLon = long64(((maxLon-minLon)/passoLon))


 MatriceVel=fltarr(DimrangeLon,DimrangeLat,n)
 MatriceVel(*,*,*)=!VALUES.F_NAN


 MaskComm=fltarr(DimrangeLon,DimrangeLat,n)


 MatriceCosE=fltarr(DimrangeLon,DimrangeLat,n)
 ;;MatriceCosN=fltarr(DimrangeLon,DimrangeLat,n)
 MatriceCosU=fltarr(DimrangeLon,DimrangeLat,n)

 MatriceAscDesc=fltarr(DimrangeLon,DimrangeLat,n)


;******************capiamo seè orbita ascendente o discendente***********************

 for yyy=0l,n-1l do begin
    
    lista_curr=lista_elab(yyy)
    
    ;;contoLivello=long(lista_curr)
    
    ;; Limits_geo_curr=Limits_geo(*,yyy)
    
    ;;    if Limits_geo_curr(0) gt Limits_geo_curr(4) then begin
    ;;       orbit='DESC'
    ;;       valOrb=1.
    ;;    endif
       
    ;;    if Limits_geo_curr(0) lt Limits_geo_curr(4) then begin
    ;;       orbit='ASC' 
    ;;       valOrb=2.
    ;;    endif

       orbit = orbitVett(yyy)
       valOrb = valOrbVett(yyy)
       
       print,'Orbita: '+orbit, valOrb
       Vettore_delle_Orbite(yyy)=orbit
;**************Inizializziamo le matrici che ci servono******************
       mask_curr_orig=fltarr(dimXVett(yyy),dimYVett(yyy))
       vel_curr_orig=fltarr(dimXVett(yyy),dimYVett(yyy))
       CosE_curr_orig=fltarr(dimXVett(yyy),dimYVett(yyy))
       CosU_curr_orig=fltarr(dimXVett(yyy),dimYVett(yyy))
 ;;      CosN_curr=fltarr(dimXVett(yyy),dimYVett(yyy))
;************************************************************************
       print,''
;       print,lista_curr
       print,''
;       currMast=leggiParam(lista_elab(yyy)+'/'+'paramFile.txt','MASTER')
;       interf=leggiParam(lista_elab(yyy)+'/'+'paramFile.txt','INTERF')
;       result=leggiParam(lista_elab(yyy)+'/'+'paramFile.txt','RESULT')
;       log=leggiParam(lista_elab(yyy)+'/'+'paramFile.txt','LOG')
;       slc=leggiParam(lista_elab(yyy)+'/'+'paramFile.txt','SLC')
      
;       obj=OBJ_NEW('IDL_savefile',filename=lista_elab(yyy)+'/'+log+'/lambda.sav')    
;       obj->restore,'LAMBDACM_VETT'  
;       obj_destroy,obj 

;       spawn,'ls '+lista_elab(yyy)+'/'+slc+'/'+currMast+'/FocPar.sav_???',namefoc
;       obj=OBJ_NEW('IDL_savefile',filename=namefoc)    
;       obj->restore,'MISSIONID'  
;       obj_destroy,obj 
;       Vettore_dei_Sensori(yyy)=MISSIONID
       Vettore_dei_Sensori(yyy)='ENV'

;       lambda_curr=LAMBDACM_VETT(0)
       lambda_curr=5.56d0 

;*************Apriamo le matrici geocodificate che ci servono************
       openr,1,lista_vel(yyy) & readu,1,vel_curr_orig & close,1
       openr,1,lista_mask(yyy) & readu,1,mask_curr_orig & close,1

       spawn,'ls '+lista_elab(yyy)+'/'+'CosU_'+strcompress(dimXVett(yyy),/REMOVE_ALL)+'x'+strcompress(dimYVett(yyy),/REMOVE_ALL)+'.dat',CosenoU,/sh
       openr,1,CosenoU & readu,1,CosU_curr_orig & close,1

       spawn,'ls '+lista_elab(yyy)+'/'+'CosE_'+strcompress(dimXVett(yyy),/REMOVE_ALL)+'x'+strcompress(dimYVett(yyy),/REMOVE_ALL)+'.dat',CosenoE,/sh
       openr,1,CosenoE & readu,1,CosE_curr_orig & close,1

;************************************************************************
;stop
cost=lambda_curr/(4*!pi)
cost=1.
vel_curr_orig = vel_curr_orig*cost

;****************tagliamo le matrici sull'area comune********************
pixLat_st_curr=long64((abs(minLat-latSt(yyy)))/passoLat)
pixLon_st_curr=long64((abs(minLon-lonSt(yyy)))/passoLon)

mask_curr=mask_curr_orig(pixLon_st_curr:pixLon_st_curr+DimrangeLon-1l,pixLat_st_curr:pixLat_st_curr+DimrangeLat-1l)
vel_curr=vel_curr_orig(pixLon_st_curr:pixLon_st_curr+DimrangeLon-1l,pixLat_st_curr:pixLat_st_curr+DimrangeLat-1l)
CosU_curr=CosU_curr_orig(pixLon_st_curr:pixLon_st_curr+DimrangeLon-1l,pixLat_st_curr:pixLat_st_curr+DimrangeLat-1l)
CosE_curr=CosE_curr_orig(pixLon_st_curr:pixLon_st_curr+DimrangeLon-1l,pixLat_st_curr:pixLat_st_curr+DimrangeLat-1l)
;stop
;************************************************************************

       indGood = where(mask_curr gt .5,/L64,contagood)


       for ss=0l,contagood-1l do begin
          ;;xx=indGood(ss) mod dimXVett(yyy)
          ;;yy=indGood(ss)/dimXVett(yyy)
          ;;xxg= lonSt(yyy)+(xx*passoLon)
          ;;yyg= latSt(yyy)+(yy*passoLat)
          xx=indGood(ss) mod DimrangeLon
          yy=indGood(ss)/DimrangeLon
          xxg=xx
          yyg=yy
  

          ;;posx=where(abs(rangeLon-xxg) eq min(abs(rangeLon-xxg)))
          ;;posy=where(abs(rangeLat-yyg) eq min(abs(rangeLat-yyg)))
          posx=xx
          posy=yy

          MatriceVel(posx,posy,yyy)=vel_curr(xx,yy)
          MaskComm(posx,posy,yyy)=1.
          MatriceCosE(posx,posy,yyy)=CosE_curr(xx,yy)
          MatriceCosU(posx,posy,yyy)=CosU_curr(xx,yy)
          MatriceAscDesc(posx,posy,yyy)=valOrb

       endfor

; stop

    endfor

;stop


 mask_Area_Com = total(MaskComm,3)
 mask_Comb=float(mask_Area_Com eq max(mask_Area_Com))
 mask_AscDesc=fltarr(DimrangeLon,DimrangeLat)
 
 indGoodFin_tmp = where(mask_Comb gt 0.,/L64,contagoodFin_tmp)

 for sss=0l,contagoodFin_tmp-1l do begin
   px = indGoodFin_tmp(sss) mod DimrangeLon
   py = indGoodFin_tmp(sss) / DimrangeLon
   vettoreOrb= MatriceAscDesc(px,py,*)
   ;;print,vettoreOrb
   vettoreOrbUniq=vettoreOrb[uniq(vettoreOrb,sort(vettoreOrb))]
   ;maschera_orbite(px,py)=total(vettoreOrbUniq)
   if total(vettoreOrbUniq) ge 3 then mask_AscDesc(px,py)=1.
endfor

;stop

 mask_Comb2=mask_Comb*mask_AscDesc

 indGoodFin = where(mask_Comb2 gt 0.,/L64,contagoodFin)

if contagoodFin gt 0. then begin

   if keyword_set(latAgg) and keyword_set(lonAgg) then begin
  
    
      poslon_agg=lonAgg
      poslat_agg=latAgg
      posy_agg=long64((abs(latAgg-minLat))/passoLat)
      posx_agg=long64((abs(lonAgg-minLon))/passoLon)
      
      verifica = mask_Comb2(posx_agg,posy_agg)

;stop
      if verifica ne 1. then begin
         boxino=10.
         agg_new=where(mask_Comb2(posx_agg-boxino:posx_agg+boxino,posy_agg-boxino:posy_agg+boxino) eq 1.,count)
         if count gt 0. then begin
         posx_agg=(agg_new(0) mod ((boxino*2.)+1))+posx_agg-boxino
         posy_agg=(agg_new(0) / ((boxino*2.)+1))+posy_agg-boxino   
         ;stop
      endif else STOP
      endif 

;stop

   endif else begin
      print,''
      print,'calcolo automatico del punto di aggancio'
      print,''
      ind_x_Agg=where(mask_Area_Com(indGoodFin) eq max(mask_area_com(indGoodFin)),contagoodAgg)
      vettorone_agg=fltarr(contagoodAgg)

      for rr=0l,contagoodAgg-1l do begin
         ppx = indGoodFin(ind_x_Agg(rr)) mod DimrangeLon 
         ppy = indGoodFin(ind_x_Agg(rr)) / DimrangeLon 
         vel_x_agg = MatriceVel(ppx,ppy,*) 
         ind_tmp = where(finite(vel_x_agg))
         vel_x_agg = abs(vel_x_agg(ind_tmp)) 
         vettorone_agg(rr)=mean(vel_x_agg) 
      endfor
      
      aggancio=(where(vettorone_agg eq min(vettorone_agg)))(0)
      posx_agg = indGoodFin(ind_x_Agg(aggancio)) mod DimrangeLon
      posy_agg = indGoodFin(ind_x_Agg(aggancio)) / DimrangeLon

      poslon_agg=minLon+(passoLon*(posx_agg-1l))
      poslat_agg=minLat+(passoLat*(posy_agg-1l))



   endelse

   save,filename=pathout+'/AggancioComune.sav',poslat_agg,poslon_agg
   save,filename=pathout+'/AreaComune_Geo.sav',minLon,maxlon,minLat,maxLat,passoLon,PassoLat,DimrangeLon,DimrangeLat

   agg_vel = MatriceVel(posx_agg,posy_agg,*)



   Vel_E=fltarr(DimrangeLon,DimrangeLat)
   Vel_U=fltarr(DimrangeLon,DimrangeLat)


   for ii=0l,contagoodFin-1l do begin
    px = indGoodFin(ii) mod DimrangeLon
    py = indGoodFin(ii) / DimrangeLon
;stop  
    vett_vel = MatriceVel(px,py,*)
    index = where(finite(vett_vel))
    vett_vel = vett_vel(index)
    vett_vel_agg = vett_vel-agg_vel(index)
    
    check1=where(~finite(vett_vel))
    check2=where(~finite(vett_vel_agg))

    if (check1 ne -1) or (check2 ne -1) then STOP

    B=dblarr(2,N_elements(index))
    B(0,*)=reform(MatriceCosE(px,py,index))
    B(1,*)=reform(MatriceCosU(px,py,index))

    
    ;;;;;;calcolo della pseudo inversa e inversione del sistema per il calcolo di East e Up

    svdc,B,w,u,vi

;;   if N_elements(index) gt 2 then  nw=3 else nw=2
    nw=2

    diag=fltarr(nw,nw)
    for hh=0,nw-1l do diag(hh,hh)=1/w(hh)
    area=where(w lt .0001)
    if (area(0) ne -1) then begin	
       diag(area,area)=0.
    endif
    ;*******************************
    ; Creating pseudo-invers matrix
    ;*******************************
    pseudoIn = vi##diag##(transpose(u))
   ; Comp_N_E_U = pseudoIn##transpose(vett_vel) 
    Comp_N_E_U = pseudoIn##(vett_vel_agg)


    Vel_E(px,py) = Comp_N_E_U(0)
    Vel_U(px,py) = Comp_N_E_U(1)


 endfor



endif else print,'NON CI SONO PUNTI COMUNI !!!!'

;stop

openw,1,pathout+'/Componente_East_'+strcompress(long(DimrangeLon),/REMOVE_ALL)+'x'+strcompress(long(DimrangeLat),/REMOVE_ALL)+'.dat' & writeu,1,Vel_E & close,1

openw,1,pathout+'/Componente_Up_'+strcompress(long(DimrangeLon),/REMOVE_ALL)+'x'+strcompress(long(DimrangeLat),/REMOVE_ALL)+'.dat' & writeu,1,Vel_U & close,1

openw,1,pathout+'/MaskAreaComm_'+strcompress(long(DimrangeLon),/REMOVE_ALL)+'x'+strcompress(long(DimrangeLat),/REMOVE_ALL)+'.dat' & writeu,1,mask_Comb2 & close,1

end
