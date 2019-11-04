load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/wrf/WRFUserARW.ncl"

begin
;;;;domain_id
domain_id=1
;;;;;;;readfile
  fils = systemfunc("ls ../001/../11/wrfout_d0"+domain_id+"*")
  a    = addfiles(fils,"r")
 ;;;;;get_variable
  PM2_5_DRY_Nov = wrf_user_getvar(a,"PM2_5_DRY",-1)
;;;;;;;;plot_average_pm25
  PM2_5_DRY_avg=dim_avg_n_Wrap(PM2_5_DRY,0)
  wks = gsn_open_wks("png","./avg_PM2_5_DRY_d0"+domain_id+"")
  res                      = True
  res@cnFillOn             = True
  res@cnFillMode           = "RasterFill"  
  res@cnLinesOn            = False
  res@cnLevelSpacingF      = 2.           ; NCL chose 4.0
  res@lbOrientation        = "Vertical"
  res@tiMainString         = "PM2_5_DRY"
;---Necessary for contours to be overlaid correctly on WRF projection
  res@tfDoNDCOverlay       = True
;  res@gsnRightString = ""        ;副标题右侧。引号内设空格意味着不输出。
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
;---Set map resources based on projection on WRF output file
  res = wrf_map_resources(a[0],res)
  ;mnmxint=(/0.1,2,0.1/)
  ;res@cnLevelSelectionMode = "ManualLevels"
  ;res@cnMinLevelValF       = mnmxint(0)
  ;res@cnMaxLevelValF       = mnmxint(1)
  ;res@cnLevelSpacingF      = mnmxint(2)
  plot = gsn_csm_contour_map(wks,PM2_5_DRY_avg(0,:,:),res) 
end
