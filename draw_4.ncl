load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"
begin
;;;;;;;readfile
  fils1 = systemfunc("ls ./origin/1level/wrfchemi_d01_2016-11-01_00:00:00.nc")
  ;fils1 = systemfunc("ls ./004/wrfchemi_d01_2016-11-01_00:00:00.nc")
  a1    = addfiles(fils1,"r")
  fils2 = systemfunc("ls ./000/wrfchemi_d01_2016-11-01_00:00:00.nc")
  a2    = addfiles(fils2,"r")
  fils3 = systemfunc("ls ./001/wrfchemi_d01_2016-11-01_00:00:00.nc")
  a3    = addfiles(fils3,"r")
  fils4 = systemfunc("ls ./002/wrfchemi_d01_2016-11-01_00:00:00.nc")
  a4    = addfiles(fils4,"r")
  wrffile=systemfunc("ls /public/daitie/daitietest/lh/1WRFV3/WRFV3/1drun_ass/wrfout_d01_2016-11-01_07:00:00")
  awrf    = addfiles(wrffile,"r")
 ;;;;;get_variable
  r1 = wrf_user_getvar(a1,"E_ECI",-1)
  r2 = wrf_user_getvar(a2,"E_ECI",-1)
  r3 = wrf_user_getvar(a3,"E_ECI",-1)
  r4 = wrf_user_getvar(a4,"E_ECI",-1)
;;;;;;;;
  wks = gsn_open_wks("png","./4_emissions")
  res                      = True
  res@gsnDraw              = False
  res@gsnFrame             = False
  res@cnFillOn             = True
  res@cnFillMode           = "RasterFill"
  res@gsnAddCyclic         = False
  res@cnLinesOn            = False
  res@cnLevelSpacingF      = 2.           ; NCL chose 4.0
  res@lbOrientation        = "Vertical"
  res@lbLabelBarOn         = False;close the color bar
  res@cnLineLabelsOn       = False;remove the numbers on the picture.used together with the one above
  ;res@tiMainString         = "correlation coefficients"
  res@gsnMaximize     = False
;---Necessary for contours to be overlaid correctly on WRF projection
  res@tfDoNDCOverlay       = True
  res@gsnMaximize     = False
;  res@gsnRightString = ""        ;
  res@gsnLeftString = ""
  res@gsnRightString = ""
  res@cnFillPalette = "BlAqGrYeOrReVi200"
  res@mpDataSetName="/public/daitie/daitietest/lh/NCL-Chinamap-master/database/Earth..4"
  res@mpDataBaseVersion          = "MediumRes" ; or "Ncarg4_1"
  res@mpAreaMaskingOn            = True
  res@mpProvincialLineColor               = "black"
  res@mpProvincialLineThicknessF          =1
  res@mpNationalLineColor                 = "black"
  res@mpNationalLineThicknessF            =1
  res@mpGeophysicalLineColor              = "black"
  res@mpGeophysicalLineThicknessF         =1
  res@mpMaskAreaSpecifiers       = (/"China"/)
  res@mpOutlineSpecifiers        = (/"China","China:Provinces"/)
  ;res@mpMinLatF=34.875
  ;res@mpMaxLatF=44.875
  ;res@mpMinLonF=111.375
  ;res@mpMaxLonF=121.375
;---Set map resources based on projection on WRF output file
  ;res = wrf_map_resources(aNov[0],res)
  mnmxint=(/0.0025,0.04,0.0025/)
  res@cnLevelSelectionMode = "ManualLevels"
  res@cnMinLevelValF       = mnmxint(0)
  res@cnMaxLevelValF       = mnmxint(1)
  res@cnLevelSpacingF      = mnmxint(2)
  ;plot = gsn_csm_contour_map(wks,r(0,0,:,:),res)
  
  res@cnLevelSelectionMode     = "ManualLevels"
  res@gsnMajorLonSpacing=2
  res@gsnMinorLonSpacing=1
  res@gsnMajorLatSpacing=2
  res@gsnMinorLatSpacing=1
  res = wrf_map_resources(awrf[0],res)
  
  plot=new(4,graphic)
  res@gsnLeftString   = "(a)origin"
  plot(0)=gsn_csm_contour_map(wks, r1(0,0,:,:), res)
  res@gsnLeftString   = "(b)000"
  plot(1)=gsn_csm_contour_map(wks, r2(0,0,:,:), res)
  res@gsnLeftString   = "(c)001"
  plot(2)=gsn_csm_contour_map(wks, r3(0,0,:,:), res)
  res@gsnLeftString   = "(d)002"
  plot(3)=gsn_csm_contour_map(wks, r4(0,0,:,:), res)
  panres=True
  panres@gsnPanelRowSpec = True
  panres@gsnMaximize     =False
  panres@gsnFrame=False
  panres@gsnPanelLeft =0.1
  panres@gsnPanelTop  =0.85
  panres@gsnPanelXWhiteSpacePercent   = 2
  panres@gsnPanelYWhiteSpacePercent   = 2
  panres@pmLabelBarWidthF = 0.6
  panres@gsnPanelLabelBar=True
  panres@lbLabelFontHeightF  = 0.015
  ;panres@tiMainString         = "Correlation Coefficients"\

  txtres=True
  txtres@txFontHeightF =0.03
  gsn_text_ndc(wks,"Emission Petrubation (E_ECI)", 0.55, 0.9, txtres)
  gsn_panel(wks,plot,(/2,2/),panres)
  frame(wks)
end

