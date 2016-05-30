;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDL 8.2 code
; writen by F. Casu 04/2016
;
; Computes the EW and Up component of velocity from 1 asc
; and 1 desc LOS files 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

pro main,pathgen,d_zz,d_ew,COH_COM,pathAsc=pathAsc,pathDesc=pathDesc,mask_cohA=mask_cohA,x_cohA=x_cohA,y_cohA=y_cohA,mask_velA=mask_velA,mask_cohD=mask_cohD,x_cohD=x_cohD,y_cohD=y_cohD,mask_velD=mask_velD,pac_x = pac_x, pac_y=pac_y

if n_params() eq 0 then begin
  print,' *******************************************************************************************'
  print,' Usage'
  print,''
  print,' Input:'
  print,' pathgen: working directory'
  print,''
  print,' Output:'
  print,' d_zz: vertical component of displacement'
  print,' d_ew: East-West component of displacement'
  print,' COH_COM: common coherence mask'
  print,''
  print,' Paramters:'
  print,' pathAsc, pathDesc: location of asc and desc input files'
  print,' mask_cohA, mask_cohD: coherence mask file name (asc/desc)'
  print,' x_cohA, x_cohD: X size of input files (asc/desc)'
  print,' y_cohA, y_cohD: Y size of input files (asc/desc)'
  print,' mask_velA, mask_velD: input velocity file name (asc/desc)'
  print,' pac_x, pac_y: reference point coordinates (in samples)'
  print,' *******************************************************************************************'
  return
endif
pathGen = ''
read,pathGen

  spawn,'mkdir '+pathgen+'/result'

  if ~arg_present(pathAsc) then begin
     pathasc=''
     print,'Insert the full path for Ascending data'
     read,'    ',pathasc
  endif

  if ~arg_present(pathDesc) then begin
     pathdesc=''
     print,'Insert the full path for Descending data'
     read,'    ',pathdesc
  endif

  restore,pathasc+'/'+'ParDem.sav'

  DIMXN_A=DIMXp
  DIMYN_A=DIMYp
  Wasc=ESTARTnp
  Sasc=NSTARTnp

  restore,pathdesc+'/'+'ParDem.sav'

  DIMXN_D=DIMXp
  DIMYN_D=DIMYp
  Wdisc=ESTARTnp
  Sdisc=NSTARTnp



;'*********************************************************'
;' patch boundaries calculation  (N,S,E,W)'
;'*********************************************************'

  ;PASSO*: DEM step size

  LRX_A=((DIMXN_A-1)*PASSOE)+Wasc

  Nasc=((DIMYN_A-1)*PASSON)+Sasc

  Easc=LRX_A

  LRX_D=((DIMXN_D-1)*PASSOE)+Wdisc

  Ndisc=((DIMYN_D-1)*PASSON)+Sdisc

  Edisc=LRX_D


;*********************************************
; common area coordinates calculation
;*********************************************

  Wpc=max([Wasc,Wdisc])
  Spc=max([Sasc,Sdisc])
  Epc=min([Easc,Edisc])
  Npc=min([Nasc,Ndisc])


;*******************************************************
; SHIFT calculation (samples) ASC / DESC
;*******************************************************

  SHIFT_AX=ROUND((Wpc-Wasc)/PASSOE)
  SHIFT_AY=ROUND((Spc-Sasc)/PASSON)


  SHIFT_DX=ROUND((Wpc-Wdisc)/PASSOE)
  SHIFT_DY=ROUND((Spc-Sdisc)/PASSON)

;**************************************************************
; ASC / DESC size
;**************************************************************

  CAMP_AX=DIMXN_A-SHIFT_AX
  CAMP_AY=DIMYN_A-SHIFT_AY

  CAMP_DX=DIMXN_D-SHIFT_DX
  CAMP_DY=DIMYN_D-SHIFT_DY


;***********************************************************
; Common area
;***********************************************************

  DIMX_CUT=fix((Epc-Wpc)/PASSOE)+1
  DIMY_CUT=fix((Npc-Spc)/PASSON)+1

  MASK_ASC_CUT=fltarr(DIMX_CUT,DIMY_CUT)
  MASK_DISC_CUT=fltarr(DIMX_CUT,DIMY_CUT)
  VEL_ASC_CUT=fltarr(DIMX_CUT,DIMY_CUT)
  VEL_DISC_CUT=fltarr(DIMX_CUT,DIMY_CUT)

;***********************************************************
; Saving common parameters
;***********************************************************

  comsav='CommonAreaParam.sav'

  print,'Saving...' 
  save,filename=pathgen+'/result/'+comsav,Spc,Wpc,DIMX_CUT,DIMY_CUT,PASSOE,PASSON,SHIFT_AX,SHIFT_AY,SHIFT_DX,SHIFT_DY
  
;***********************************************************  

  print,' ***********************************'
  print,' *         Ascending Input         *'
  print,' ***********************************'

;------------------------------------------
; Asc Mask reading
;------------------------------------------

  if ~arg_present(mask_cohA) then begin
     mask_cohA=''
     print,'Insert the Ascending Coherence Mask'
     read,'    ',mask_cohA
  endif

  if ~arg_present(x_cohA) then begin
     print,'Insert matrix size'
     read,'X size: ',x_cohA
     read,'Y size: ',y_cohA
  endif

  openr,1,pathasc+'/'+mask_cohA
  coh_A=fltarr(x_cohA,y_cohA)
  readu,1,coh_A
  close,1

  coh_a(where(coh_a ge .5))=1.
  coh_a(where(coh_a lt .5))=0.

;------------------------------------------
; Asc Mask cutting
;------------------------------------------

  coh_cutA=coh_A(SHIFT_AX : SHIFT_AX+DIMX_CUT-1,SHIFT_AY : SHIFT_AY+DIMY_CUT-1)

;------------------------------------------
; Asc Velocity reading
;------------------------------------------

  if ~arg_present(mask_velA) then begin
     mask_velA=''
     print,''
     print,'Insert the Ascending Velocity'
     read,'    ',mask_velA
  endif

  openr,1,pathasc+'/'+mask_velA
  vel_A=fltarr(x_cohA,y_cohA)
  readu,1,vel_A
  close,1

;------------------------------------------
; Asc Velocity cutting
;------------------------------------------

  vel_cutA=vel_A(SHIFT_AX : SHIFT_AX+DIMX_CUT-1,SHIFT_AY : SHIFT_AY+DIMY_CUT-1)

;------------------------------------------
; Asc cos reading & cutting
;------------------------------------------

  print,''
  print,'Reading Ascending cos...'

  openr,1,pathasc+'/'+'CosE_'+strcompress(x_cohA,/remove_all)+'x'+strcompress(y_cohA,/remove_all)+'.dat'
  loseA = fltarr(x_cohA,y_cohA)
  readu,1,loseA
  close,1
  loseA = loseA(SHIFT_AX : SHIFT_AX+DIMX_CUT-1,SHIFT_AY : SHIFT_AY+DIMY_CUT-1)

  ;; openr,1,pathasc+'/'+'CosN_'+strcompress(x_cohA,/remove_all)+'x'+strcompress(y_cohA,/remove_all)+'.dat'
  ;; losnA = fltarr(x_cohA,y_cohA)
  ;; readu,1,losnA
  ;; close,1
  ;; losnA = losnA(SHIFT_AX : SHIFT_AX+DIMX_CUT-1,SHIFT_AY : SHIFT_AY+DIMY_CUT-1)

  openr,1,pathasc+'/'+'CosU_'+strcompress(x_cohA,/remove_all)+'x'+strcompress(y_cohA,/remove_all)+'.dat'
  losuA = fltarr(x_cohA,y_cohA)
  readu,1,losuA
  close,1
  losuA = losuA(SHIFT_AX : SHIFT_AX+DIMX_CUT-1,SHIFT_AY : SHIFT_AY+DIMY_CUT-1)

; ----------------------------------------------------------

  print,' ***********************************'
  print,' *        Descending Input         *' 
  print,' ***********************************'

;------------------------------------------
; Desc Mask reading
;------------------------------------------

  print,''
  print,'Reading Descending Mask...'

  if ~arg_present(mask_cohD) then begin

     mask_cohD=''

     print,''
     print,'Insert the Descending Coherence Mask'
     read,'    ',mask_cohD
  endif

  if ~arg_present(x_cohD) then begin
     print,'Insert matrix size'
     read,'X size: ',x_cohD
     read,'Y size: ',y_cohD
  endif

  openr,1,pathdesc+'/'+mask_cohD
  coh_D=fltarr(x_cohD,y_cohD)
  readu,1,coh_D
  close,1

  coh_D(where(coh_D ge .5))=1.
  coh_D(where(coh_D lt .5))=0.

;-------------------------------------------
; Desc Mask cutting
;-------------------------------------------

  coh_cutD=coh_D(SHIFT_DX : SHIFT_DX+DIMX_CUT-1,SHIFT_DY : SHIFT_DY+DIMY_CUT-1)

;------------------------------------------
; Desc Velocity reading
;------------------------------------------

  print,''
  print,'Reading Descending Velocity...'

  if ~arg_present(mask_velD) then begin
     mask_velD=''
     print,''
     print,'Insert the Descending Velocity'
     read,'    ',mask_velD
  endif

  openr,1,pathdesc+'/'+mask_velD
  vel_D=fltarr(x_cohD,y_cohD)
  readu,1,vel_D
  close,1

;-------------------------------------------
; Desc Velocity cutting
;-------------------------------------------

  vel_cutD=vel_D(SHIFT_DX : SHIFT_DX+DIMX_CUT-1,SHIFT_DY : SHIFT_DY+DIMY_CUT-1)

;------------------------------------------
; Desc cos reading & cutting
;------------------------------------------

  print,''
  print,'Reading Descending cos...'

  openr,1,pathdesc+'/'+'CosE_'+strcompress(x_cohD,/remove_all)+'x'+strcompress(y_cohD,/remove_all)+'.dat'
  loseD = fltarr(x_cohD,y_cohD)
  readu,1,loseD
  close,1
  loseD = loseD(SHIFT_DX : SHIFT_DX+DIMX_CUT-1,SHIFT_DY : SHIFT_DY+DIMY_CUT-1)

  ;; openr,1,pathdesc+'/'+'CosN_'+strcompress(x_cohD,/remove_all)+'x'+strcompress(y_cohD,/remove_all)+'.dat'
  ;; losnD = fltarr(x_cohD,y_cohD)
  ;; readu,1,losnD
  ;; close,1
  ;; losnD = losnD(SHIFT_DX : SHIFT_DX+DIMX_CUT-1,SHIFT_DY : SHIFT_DY+DIMY_CUT-1)

  openr,1,pathdesc+'/'+'CosU_'+strcompress(x_cohD,/remove_all)+'x'+strcompress(y_cohD,/remove_all)+'.dat'
  losuD = fltarr(x_cohD,y_cohD)
  readu,1,losuD
  close,1
  losuD = losuD(SHIFT_DX : SHIFT_DX+DIMX_CUT-1,SHIFT_DY : SHIFT_DY+DIMY_CUT-1)

; ----------------------------------------------------------
  print,' **********************************'
  print,' *         Common matrix          *'
  print,' **********************************'

  COH_AD=COH_CUTD+2*COH_CUTA

  COH_COM=fltarr(DIMX_CUT,DIMY_CUT)

  COH_COM(where(COH_AD eq 3))=1

; ----------------------------------------------------------
  print,'****************************************************'
  print,'Reference point'
  print,'****************************************************'

  if ~arg_present(pac_x) then begin
     
     ;window,1,xs=dimx_cut,ys=dimy_cut,title='Select Reference Point'
     ;tvscl,coh_com
     ;rdpix,coh_com
     pac_x=''
     pac_y=''
     read,'X coordinate: ' ,pac_x
     read,'Y coordinate: ' ,pac_y

  endif
  
  appD=vel_cutD(pac_x,pac_y)
  appA=vel_cutA(pac_x,pac_y)

  vel_cutD(*,*)=vel_cutD(*,*)-appD
  vel_cutA(*,*)=vel_cutA(*,*)-appA

; ----------------------------------------------------------
  print,'*******************************************'
  print,' Displacement Component Computation'
  print,'*******************************************'

  d_ZZ = fltarr(dimx_cut,dimy_cut)
  d_ew = fltarr(dimx_cut,dimy_cut)

  for i=0l,DIMY_CUT-1l do begin

     pointGood = where(COH_COM(*,i) ne 0,countGood)
     if countGood ne 0 then begin

        for j=0l,n_elements(pointGood)-1l do begin    
           
           del_t = fltarr(2,2)
           del_t(*,0) = [loseA(pointGood(j),i),losuA(pointGood(j),i)]
           del_t(*,1) = [loseD(pointGood(j),i),losuD(pointGood(j),i)]
           igdel_tm = invert(transpose(del_t)##del_t)##transpose(del_t)

           kt = [vel_cutA(pointGood(j),i),vel_cutD(pointGood(j),i)]

           res = igdel_tm##kt 
           
           d_zz(pointGood(j),i) = res(1)
           d_ew(pointGood(j),i) = res(0)
        endfor
     endif
     
  endfor

; ----------------------------------------------------------
  print,'*******************************************'
  print,' Saving results'
  print,'*******************************************'

openw,1,pathgen+'/result/'+'Up_GEO_'+strcompress(dimx_cut,/remove_all)+'x'+strcompress(dimy_cut,/remove_all)+'dat'
writeu,1,d_zz & close,1

openw,1,pathgen+'/result/'+'EW_GEO_'+strcompress(dimx_cut,/remove_all)+'x'+strcompress(dimy_cut,/remove_all)+'dat'
writeu,1,d_ew & close,1

openw,1,pathgen+'/result/'+'MaskCom_GEO_'+strcompress(dimx_cut,/remove_all)+'x'+strcompress(dimy_cut,/remove_all)+'dat'
writeu,1,COH_COM & close,1

openw,1,pathgen+'/result/'+'VelCutDesc_'+strcompress(dimx_cut,/remove_all)+'x'+strcompress(dimy_cut,/remove_all)+'dat'
writeu,1,vel_cutD & close,1

openw,1,pathgen+'/result/'+'VelCutAsc_'+strcompress(dimx_cut,/remove_all)+'x'+strcompress(dimy_cut,/remove_all)+'dat'
writeu,1,vel_cutA & close,1


end
