PRO localtime_new
  COMPILE_OPT IDL2
  ENVI,/RESTORE_BASE_SAVE_FILES
  ENVI_BATCH_INIT

  file_name=file_search('D:\M3\south pole\band_depth_proj\cold','*_band_depth.img',count=num,/test_regular)
  file_name1=file_search('D:\M3\south pole\snri\cold','*_nan_SNRI',count=num1,/test_regular)
  file_name2=file_search('D:\M3\south pole\data\obs_geo\cold','*_geo.img',count=num2,/test_regular)
  file_name3=file_search('D:\M3\south pole\data\reff\cold\geo','*_geo.img',count=num3,/test_regular)
  result=fltarr(num)
  inc=fltarr(num)
  band77=dblarr(num)
  band78=dblarr(num)
  band82=dblarr(num)
  band83=dblarr(num)

  for i=0,num-1 do begin
    print,i
    ENVI_OPEN_FILE, file_name[i],r_fid=fid
    ENVI_FILE_QUERY, fid,dims=dims,ns=ns,nl=nl,data_type=data_type,INTERLEAVE=INTERLEAVE
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    data_file=ENVI_GET_DATA(fid=fid,dims=dims,pos=0)

    ENVI_OPEN_FILE, file_name1[i],r_fid=fid
    ENVI_FILE_QUERY, fid,dims=dims,ns=ns,nl=nl,data_type=data_type,INTERLEAVE=INTERLEAVE
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid)
    data_file1=ENVI_GET_DATA(fid=fid,dims=dims,pos=0)
    
    ENVI_OPEN_FILE, file_name2[i],r_fid=fid1
    ENVI_FILE_QUERY, fid1,dims=dims,nb=nb,bnames=bnames,ns=ns,nl=nl,data_type=data_type,INTERLEAVE=INTERLEAVE
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid1)
    data_file2=ENVI_GET_DATA(fid=fid1,dims=dims,pos=1)
    data_file3=ENVI_GET_DATA(fid=fid1,dims=dims,pos=3)
    
    ENVI_OPEN_FILE, file_name3[i],r_fid=fid2
    ENVI_FILE_QUERY, fid2,dims=dims,nb=nb,bnames=bnames,ns=ns,nl=nl,data_type=data_type,INTERLEAVE=INTERLEAVE
    MAP_INFO=ENVI_GET_MAP_INFO(fid=fid2)
    b77=ENVI_GET_DATA(fid=fid2,dims=dims,pos=76)
    b78=ENVI_GET_DATA(fid=fid2,dims=dims,pos=77)
    b82=ENVI_GET_DATA(fid=fid2,dims=dims,pos=81)
    b83=ENVI_GET_DATA(fid=fid2,dims=dims,pos=82)

    envi_open_file, 'D:\M3\south pole\mosaic\2.proj',r_fid=p1fid
    map_info1 = envi_get_map_info(fid=p1fid)
    i_proj=map_info1.proj
    envi_open_file, 'D:\M3\south pole\mosaic\1.proj',r_fid=p2fid
    map_info2 = envi_get_map_info(fid=p2fid)
    o_proj=map_info2.proj
    
    count=0
    
    for j=0, 40 do begin
      envi_convert_projection_coordinates, 237, -float(j+8890)/100, i_proj, xmap, ymap, o_proj

      envi_convert_file_coordinates, fid, xf, yf, xmap, ymap
      xf=fix(xf) & yf=fix(yf)

      if (xf LE ns && yf LE nl && data_file[xf,yf] gt 0 && data_file[xf,yf] le 0.2 && data_file1[xf,yf] gt 0 && data_file1[xf,yf] LE 0.025) then begin
        count=count+1
        band77[i]=band77[i]+b77[xf,yf]
        band78[i]=band78[i]+b78[xf,yf]
        band82[i]=band82[i]+b82[xf,yf]
        band83[i]=band83[i]+b83[xf,yf]
        inc[i]=inc[i]+data_file2[xf,yf]
        ;inc[i]=inc[i]+(data_file2[xf,yf]+data_file2[xf+1,yf]+data_file2[xf+2,yf]+data_file2[xf,yf+1]+data_file2[xf+1,yf+1]+data_file2[xf+2,yf+1]+data_file2[xf,yf+2]+data_file2[xf+1,yf+2]+data_file2[xf+2,yf+2])/9
        result[i]=result[i]+data_file[xf,yf]
        ;result[i]=result[i]+(data_file[xf,yf]+data_file[xf+1,yf]+data_file[xf+2,yf]+data_file[xf,yf+1]+data_file[xf+1,yf+1]+data_file[xf+2,yf+1]+data_file[xf,yf+2]+data_file[xf+1,yf+2]+data_file[xf+2,yf+2])/9
      endif
  
    endfor
    
    result[i]=result[i]/count
    inc[i]=inc[i]/count
    band77[i]=band77[i]/count
    band78[i]=band78[i]/count
    band82[i]=band82[i]/count
    band83[i]=band83[i]/count

  ENDFOR

  a=result
  aa=inc
  aaa=band77
  a1=band78
  a2=band82
  a3=band83
  b = WHERE(FINITE(a, /NAN))
  b1 = WHERE(FINITE(a1, /NAN))
  b2 = WHERE(FINITE(a2, /NAN))
  b3 = WHERE(FINITE(a3, /NAN))
  bb=WHERE(FINITE(aa, /NAN))
  bbb=WHERE(FINITE(aaa, /NAN))
  a[b] = 0
  a1[b1]=0
  a2[b2]=0
  a3[b3]=0
  aa[bb]=0
  aaa[bbb]=0
  result = a
  inc=aa
  band77=aaa
  band78=a1
  band82=a2
  band83=a3
  
;  filename='D:\M3\single_pixel\lon222_lat889-893_banddepth1.txt'
;  openw,lun,filename,/get_lun
;  for i=0, num-1 do begin
;    printf,lun,result[i]
;  endfor
;  free_lun,lun
  
  filename1='D:\M3\single_pixel\lon237_lat889-893_band77.txt'
  openw,lun,filename1,/get_lun
  for i=0, num-1 do begin
    printf,lun,band77[i]
  endfor
  free_lun,lun
  
  filename1='D:\M3\single_pixel\lon237_lat889-893_band78.txt'
  openw,lun,filename1,/get_lun
  for i=0, num-1 do begin
    printf,lun,band78[i]
  endfor
  free_lun,lun
  
  filename1='D:\M3\single_pixel\lon237_lat889-893_band82.txt'
  openw,lun,filename1,/get_lun
  for i=0, num-1 do begin
    printf,lun,band82[i]
  endfor
  free_lun,lun
  
  filename1='D:\M3\single_pixel\lon237_lat889-893_band83.txt'
  openw,lun,filename1,/get_lun
  for i=0, num-1 do begin
    printf,lun,band83[i]
  endfor
  free_lun,lun
;  
;  filename1='D:\M3\single_pixel\lon222_lat889-893_inc1.txt'
;  openw,lun,filename1,/get_lun
;  for i=0, num-1 do begin
;    printf,lun,inc[i]
;  endfor
;  free_lun,lun

END