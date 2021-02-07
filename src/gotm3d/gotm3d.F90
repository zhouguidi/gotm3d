#include"cppdefs.h"
module gotm3d

  use time, ONLY: read_time_string, calendar_date, sec2hms
  use ncio, ONLY: ncread_dimshape, ncread_lonlatlev, ncread_surface, &
                  ncread_subsurface, ncread_timelen, ncread_missing, ncread_time
  use gotm, ONLY: gotm1d

  implicit none

  private
  public :: init_gotm3d, time_loop_3d, clean_up_3d

  integer, parameter         :: namlst=10

  character(len=19)  :: dt_start, dt_stop
  integer            :: jul_st, sec_st, jul_ed, sec_ed
  integer            :: year_st, month_st, day_st
  integer            :: year_ed, month_ed, day_ed
  integer            :: dt_intv
  integer            :: time_unit

  integer, parameter :: one_file = 0
  integer, parameter :: per_year = 1
  integer, parameter :: per_month = 2

  integer, parameter :: unit_nmlt = 100
  integer, parameter :: unit_yaml = 101
  integer, parameter :: unit_temp = 102
  integer, parameter :: unit_salt = 103
  integer, parameter :: unit_elev = 104
  integer, parameter :: unit_heat = 105
  integer, parameter :: unit_mome = 106
  integer, parameter :: unit_fres = 107

  integer :: nlon, nlat, nlev, npnt
  REALTYPE, dimension(:), allocatable :: lon, lat, lev
  integer, dimension(:,:), allocatable :: botlev
  REALTYPE, dimension(:,:), allocatable :: depth

  REALTYPE :: missing

  character(len=1024) :: sst_file, sss_file, ssh_file, temp_file, salt_file
  character(len=1024) :: qnet_file, qsw_file, taux_file, tauy_file, fsw_file
  character(len=100) :: sst_name, sss_name, ssh_name, temp_name, salt_name
  character(len=100) :: qnet_name, qsw_name, taux_name, tauy_name, fsw_name

  character(len=1024), public :: gotm3d_nmlt_file = 'gotm3d.nml'

  REALTYPE, parameter :: pi = 3.141592653
  REALTYPE, parameter :: R = 6371000
  REALTYPE, parameter :: deg2rad = pi / 180
  REALTYPE, parameter :: deg2m = deg2rad * R

contains

  subroutine init_gotm3d()
    character(len=1024) :: fn
    REALTYPE, dimension(:,:,:), allocatable :: t3
    REALTYPE, dimension(:,:,:,:), allocatable :: t4
    integer :: ilon, ilat, ntime
    logical :: file_exists
    integer :: ierr
    character(len=1024) :: cmsg
    namelist /gotm3d_config/ dt_start, dt_stop, dt_intv, sst_file, sst_name, &
                             ssh_file, ssh_name, sss_file, sss_name, qnet_file, &
                             qnet_name, qsw_file, qsw_name, taux_file, taux_name, &
                             tauy_file, tauy_name, fsw_file, fsw_name, temp_file, &
                             temp_name, salt_file, salt_name

    dt_start = '2017-01-01 00:00:00'
    dt_stop = '2017-12-31 00:00:00'
    dt_intv = 3600
    sst_file = 'sst.nc'
    sst_name = 'sst'
    ssh_file = 'ssh.nc'
    ssh_name = 'ssh'
    sss_file = 'sss.nc'
    sss_name = 'sss'
    qnet_file = 'qnet.nc'
    qnet_name = 'qnet'
    qsw_file = 'qsw.nc'
    qsw_name = 'qsw'
    taux_file = 'taux.nc'
    taux_name = 'taux'
    tauy_file = 'tauy.nc'
    tauy_name = 'tauy'
    fsw_file = 'fsw.nc'
    fsw_name = 'fsw'
    temp_file = 'temp.nc'
    temp_name = 'temp'
    salt_file = 'salt.nc'
    salt_name = 'salt'

    LEVEL1 'init_gotm3d'

    open(unit=unit_nmlt, file=trim(gotm3d_nmlt_file), status='old', iostat=ierr, iomsg=cmsg)
    if (ierr /= 0) then
      print*,trim(cmsg)
      stop 3
    endif
    read(unit=unit_nmlt, nml=gotm3d_config)
    close(unit=unit_nmlt)

    call read_time_string(dt_start,jul_st,sec_st)
    call read_time_string(dt_stop,jul_ed,sec_ed)
    call calendar_date(jul_st, year_st, month_st, day_st)
    call calendar_date(jul_ed, year_ed, month_ed, day_ed)

    ! determine forcing file type (regarding time), and get an example file of the first year
    if (index(temp_file, '%year%') /= 0) then
      if (index(temp_file, '%month%') /= 0) then
        time_unit = per_month
        fn = substitute_file_year_month(temp_file, year_st, month_st)
      else
        time_unit = per_year
        fn = substitute_file_year(temp_file, year_st)
      endif
    else
      if (index(temp_file, '%month%') /= 0) then
        FATAL 'temp file pattern contains %month% but does not contain %year%'
        stop 2
      else
        time_unit = one_file
        fn = temp_file
      endif
    endif

    ! read dimension info
    call ncread_dimshape(trim(fn), nlon, nlat, nlev)
    if (nlon == -1) then
      FATAL 'temp file does not have a recognizable longitude dimension'
      stop 2
    endif
    if (nlat == -1) then
      FATAL 'temp file does not have a recognizable latitude dimension'
      stop 2
    endif
    if (nlev == -1) then
      FATAL 'temp file does not have a recognizable level dimension'
      stop 2
    endif
    allocate(lon(nlon))
    allocate(lat(nlat))
    allocate(lev(nlev))
    call ncread_lonlatlev(trim(fn), nlon, nlat, nlev, lon, lat, lev)

    ! get missing value, bottom level, and depth
    allocate(t3(nlon, nlat, 1))
    allocate(t4(nlon, nlat, nlev, 1))
    ntime = ncread_timelen(trim(fn))
    t4 = ncread_subsurface(trim(fn), trim(temp_name), nlon, nlat, nlev, ntime, 1, "first")
    missing = ncread_missing(trim(fn), trim(temp_name))
    allocate(botlev(nlon, nlat))
    allocate(depth(nlon, nlat))
    t3 = findloc(t4, missing, dim=3)
    botlev(:,:) = t3(:,:,1)
    do ilon = 1, nlon
      do ilat = 1, nlat
        if (botlev(ilon,ilat) == 0) then
          depth(ilon, ilat) = lev(nlev)
        elseif (botlev(ilon,ilat) == 1) then
          depth(ilon, ilat) = 0
        else
          depth(ilon,ilat) = lev(botlev(ilon,ilat))
        endif
      enddo
    enddo
    npnt = count(depth == 0)

    deallocate(t4)
  end subroutine init_gotm3d

  subroutine time_loop_3d()
    IF (time_unit == one_file) THEN
      ! TODO
    ELSEIF (time_unit == per_year) THEN
      call time_loop_3d_year
    ELSE
      call time_loop_3d_month
    ENDIF
  end subroutine time_loop_3d

  subroutine time_loop_3d_year
    integer :: year, pyear, nyear, ilon, ilat, ipnt
    logical :: restart
    character(len=1024) :: fn
    integer :: ntime
    integer, dimension(:), allocatable :: time_jul, time_sec
    REALTYPE, dimension(:,:,:), allocatable :: ssh
    REALTYPE, dimension(:,:,:,:), allocatable :: temp, salt
    REALTYPE, dimension(:,:,:), allocatable :: dsshdx, dsshdy
    REALTYPE, dimension(:,:,:,:), allocatable :: dtempdx, dtempdy, dsaltdx, dsaltdy
    REALTYPE, dimension(:,:,:), allocatable :: qnet, qsw, taux, tauy, fsw

    do year = year_st, year_ed
      pyear = year - 1
      nyear = year + 1

      fn = substitute_file_year(trim(sst_file), year)
      ntime = ncread_timelen(trim(fn))
      allocate(time_jul(ntime+2))
      allocate(time_sec(ntime+2))
      allocate(ssh(nlon, nlat, ntime+2))
      allocate(qnet(nlon, nlat, ntime+2))
      allocate(qsw(nlon, nlat, ntime+2))
      allocate(taux(nlon, nlat, ntime+2))
      allocate(tauy(nlon, nlat, ntime+2))
      allocate(fsw(nlon, nlat, ntime+2))
      allocate(temp(nlon, nlat, nlev+1, ntime+2))
      allocate(salt(nlon, nlat, nlev+1, ntime+2))

      call read_time_year_2d(trim(sst_file), year, ntime, time_jul, time_sec)

      ssh = read_data_year_2d(trim(ssh_file), trim(ssh_name), year, ntime)
      qnet = read_data_year_2d(trim(qnet_file), trim(qnet_name), year, ntime)
      qsw = read_data_year_2d(trim(qsw_file), trim(qsw_name), year, ntime)
      taux = read_data_year_2d(trim(taux_file), trim(taux_name), year, ntime)
      tauy = read_data_year_2d(trim(tauy_file), trim(tauy_name), year, ntime)
      fsw = read_data_year_2d(trim(fsw_file), trim(fsw_name), year, ntime)
      temp(:,:,1,:) = read_data_year_2d(trim(sst_file), trim(sst_name), year, ntime)
      temp(:,:,2:,:) = read_data_year_3d(trim(temp_file), trim(temp_name), year, ntime)
      salt(:,:,1,:) = read_data_year_2d(trim(sss_file), trim(sss_name), year, ntime)
      salt(:,:,2:,:) = read_data_year_3d(trim(salt_file), trim(salt_name), year, ntime)

      allocate(dsshdx(nlon, nlat, ntime+2))
      allocate(dsshdy(nlon, nlat, ntime+2))
      allocate(dtempdx(nlon, nlat, nlev+1, ntime+2))
      allocate(dtempdy(nlon, nlat, nlev+1, ntime+2))
      allocate(dsaltdx(nlon, nlat, nlev+1, ntime+2))
      allocate(dsaltdy(nlon, nlat, nlev+1, ntime+2))

      call gradient_2d(nlon, nlat, ntime+2, lon, lat, ssh, dsshdx, dsshdy)
      call gradient_3d(nlon, nlat, nlev+1, ntime+2, lon, lat, temp, dtempdx, dtempdy)
      call gradient_3d(nlon, nlat, nlev+1, ntime+2, lon, lat, salt, dsaltdx, dsaltdy)

      restart = year /= year_st

      ipnt = 0
      do ilon = 1, nlon
        do ilat = 1, nlat
          if (depth(ilon, ilat) /= 0) then
            ipnt = ipnt + 1
            call prepare_1d_data(nlev+1,ntime,time_jul,time_sec,(/0.0_8,lev/),ssh(ilon,ilat,:),&
                                 qnet(ilon,ilat,:),qsw(ilon,ilat,:),taux(ilon,ilat,:),tauy(ilon,ilat,:),fsw(ilon,ilat,:),&
                                 temp(ilon,ilat,:,:),salt(ilon,ilat,:,:), &
                                 dsshdx(ilon,ilat,:),dsshdy(ilon,ilat,:), &
                                 dtempdx(ilon,ilat,:,:),dtempdy(ilon,ilat,:,:), &
                                 dsaltdx(ilon,ilat,:,:),dsaltdy(ilon,ilat,:,:))
            call prepare_1d_yaml(ilon,ilat,ipnt,restart)
            call gotm1d()
          endif
        enddo
      enddo
      call collect_result()

      deallocate(time_jul, time_sec)
      deallocate(ssh)
      deallocate(qnet, qsw, taux, tauy, fsw)
      deallocate(temp, salt)
      deallocate(dsshdx, dsshdy)
      deallocate(dtempdx, dtempdy, dsaltdx, dsaltdy)
    enddo
  end subroutine time_loop_3d_year

  subroutine gradient_2d(nlon, nlat, ntime, lon, lat, data, ddx, ddy)
    integer, intent(in) :: nlon, nlat, ntime
    REALTYPE, dimension(nlon), intent(in) :: lon
    REALTYPE, dimension(nlat), intent(in) :: lat
    REALTYPE, dimension(nlon, nlat, ntime), intent(in) :: data
    REALTYPE, dimension(nlon, nlat, ntime), intent(out) :: ddx, ddy

    integer :: ilon, ilat, itime
    REALTYPE :: backward, forward
    logical :: prevmissing, nextmissing
    REALTYPE, dimension(nlat) :: deg2m_x

    do itime = 1, ntime
      do ilon = 1, nlon
        do ilat = 1, nlat
          if (data(ilon, ilat, itime) == missing) then
            ddx(ilon, ilat, itime) = missing
            ddy(ilon, ilat, itime) = missing
            continue
          endif

          if (ilon == 1) then
            prevmissing = .true.
            nextmissing = data(ilon+1, ilat, itime) == missing
            forward = (data(ilon+1, ilat, itime) - data(ilon, ilat, itime)) / (lon(ilon+1) - lon(ilon))
          elseif (ilon == nlon) then
            prevmissing = data(ilon-1, ilat, itime) == missing
            nextmissing = .true.
            backward = (data(ilon, ilat, itime) - data(ilon-1, ilat, itime)) / (lon(ilon) - lon(ilon-1))
          else
            prevmissing = data(ilon-1, ilat, itime) == missing
            nextmissing = data(ilon+1, ilat, itime) == missing
            forward = (data(ilon+1, ilat, itime) - data(ilon, ilat, itime)) / (lon(ilon+1) - lon(ilon))
            backward = (data(ilon, ilat, itime) - data(ilon-1, ilat, itime)) / (lon(ilon) - lon(ilon-1))
          endif
          if (prevmissing) then
            if (nextmissing) then
              ddx(ilon, ilat, itime) = missing
            else
              ddx(ilon, ilat, itime) = forward
            endif
          else
            if (nextmissing) then
              ddx(ilon, ilat, itime) = backward
            else
              ddx(ilon, ilat, itime) = (forward + backward) / 2
            endif
          endif

          if (ilat == 1) then
            prevmissing = .true.
            nextmissing = data(ilon, ilat+1, itime) == missing
            forward = (data(ilon, ilat+1, itime) - data(ilon, ilat, itime)) / (lat(ilat+1) - lat(ilat))
          elseif (ilat == nlat) then
            prevmissing = data(ilon, ilat-1, itime) == missing
            nextmissing = .true.
            backward = (data(ilon, ilat, itime) - data(ilon, ilat-1, itime)) / (lat(ilat) - lat(ilat-1))
          else
            prevmissing = data(ilon, ilat-1, itime) == missing
            nextmissing = data(ilon, ilat+1, itime) == missing
            forward = (data(ilon, ilat+1, itime) - data(ilon, ilat, itime)) / (lat(ilat+1) - lat(ilat))
            backward = (data(ilon, ilat, itime) - data(ilon, ilat-1, itime)) / (lat(ilat) - lat(ilat-1))
          endif
          if (prevmissing) then
            if (nextmissing) then
              ddy(ilon, ilat, itime) = missing
            else
              ddy(ilon, ilat, itime) = forward
            endif
          else
            if (nextmissing) then
              ddy(ilon, ilat, itime) = backward
            else
              ddy(ilon, ilat, itime) = (forward + backward) / 2
            endif
          endif
        enddo
      enddo
    enddo

    deg2m_x = deg2m * cos(lat * deg2rad);
    ddx = ddx / spread(spread(deg2m_x, 1, nlon), 3, ntime)
    ddy = ddy / deg2m
  end subroutine gradient_2d

  subroutine gradient_3d(nlon, nlat, nlev, ntime, lon, lat, data, ddx, ddy)
    integer, intent(in) :: nlon, nlat, nlev, ntime
    REALTYPE, dimension(nlon), intent(in) :: lon
    REALTYPE, dimension(nlat), intent(in) :: lat
    REALTYPE, dimension(nlon, nlat, nlev, ntime), intent(in) :: data
    REALTYPE, dimension(nlon, nlat, nlev, ntime), intent(out) :: ddx, ddy

    integer :: ilev
    REALTYPE, dimension(nlon, nlat, ntime) :: ddx2, ddy2

    do ilev = 1, nlev
      call gradient_2d(nlon, nlat, ntime, lon, lat, data, ddx2, ddy2)
      ddx(:, :, ilev, :) = ddx2
      ddy(:, :, ilev, :) = ddy2
    enddo
  end subroutine gradient_3d

  subroutine read_time_year_2d(fn, year, ntime, jul, sec)
    character(len=*), intent(in) :: fn
    integer, intent(in) :: year, ntime
    integer, dimension(ntime+2), intent(out) :: jul, sec

    character(len=1024) :: fn1
    integer :: ntime_p, ntime_n

    fn1 = substitute_file_year(trim(fn), year)
    call ncread_time(trim(fn1), ntime, ntime, jul(2:ntime+1), sec(2:ntime+1))
    fn1 = substitute_file_year(trim(fn), year-1)
    ntime_p = ncread_timelen(trim(fn1))
    call ncread_time(trim(fn1), ntime_p, 1, jul(1), sec(1), "last")
    fn1 = substitute_file_year(trim(fn), year+1)
    ntime_n = ncread_timelen(trim(fn1))
    call ncread_time(trim(fn1), ntime_n, 1, jul(ntime+2), sec(ntime+2))
  end subroutine read_time_year_2d

  function read_data_year_2d(fn, varn, year, ntime) result(data)
    character(len=*), intent(in) :: fn, varn
    integer, intent(in) :: year, ntime
    REALTYPE, dimension(nlon, nlat, ntime+2) :: data

    character(len=1024) :: fn1
    integer :: ntime_p, ntime_n

    fn1 = substitute_file_year(trim(fn), year)
    data(:, :, 2:ntime+1) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime, ntime)
    fn1 = substitute_file_year(trim(fn), year-1)
    ntime_p = ncread_timelen(trim(fn1))
    data(:, :, 1:1) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime_p, 1, "last")
    fn1 = substitute_file_year(trim(fn), year+1)
    ntime_n = ncread_timelen(trim(fn1))
    data(:, :, ntime+2:ntime+2) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime_n, 1)
  end function read_data_year_2d

  function read_data_year_3d(fn, varn, year, ntime) result(data)
    character(len=*), intent(in) :: fn, varn
    integer, intent(in) :: year, ntime
    REALTYPE, dimension(nlon, nlat, nlev, ntime+2) :: data

    character(len=1024) :: fn1
    integer :: ntime_p, ntime_n

    fn1 = substitute_file_year(trim(fn), year)
    data(:, :, :, 2:ntime+1) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime, ntime)
    fn1 = substitute_file_year(trim(fn), year-1)
    ntime_p = ncread_timelen(trim(fn1))
    data(:, :, :, 1:1) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime_p, 1, 'last')
    fn1 = substitute_file_year(trim(fn), year+1)
    ntime_n = ncread_timelen(trim(fn1))
    data(:, :, :, ntime+2:ntime+2) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime_n, 1)
  end function read_data_year_3d

  function substitute_file_year(file, year) result(newfile)
    character(len=*), intent(in) :: file
    integer,intent(in) :: year
    character(len=1024) :: newfile

    integer :: pos
    character(len=4) :: stryear

    write(stryear, fmt='(I0)')year
    pos = index(file, '%year%')
    if (pos == 0) then
      pos = index(file, '%YEAR%')
      if (pos == 0) then
        pos = index(file, '%Year%')
      endif
    endif

    if (pos /= 0) then
      newfile = file(1:pos-1) // stryear // file(pos+6:)
    else
      newfile = file
    endif
  end function substitute_file_year
  
  function substitute_file_year_month(file, year, month) result(newfile)
    character(len=*), intent(in) :: file
    integer,intent(in) :: year, month
    character(len=1024) :: newfile

    integer :: pos
    character(len=4) :: strmonth

    newfile = substitute_file_year(file, year)

    write(strmonth, fmt='(I0.2)')month
    pos = index(file, '%month%')
    if (pos == 0) then
      pos = index(file, '%MONTH%')
      if (pos == 0) then
        pos = index(file, '%Month%')
        if (pos == 0) then
          pos = index(file, '%mon%')
          if (pos == 0) then
            pos = index(file, '%MON%')
            if (pos == 0) then
              pos = index(file, '%Mon%')
            endif
          endif
        endif
      endif
    endif

    if (pos /= 0) then
      newfile = file(1:pos-1) // strmonth // file(pos+6:)
    else
      newfile = file
    endif
  end function substitute_file_year_month

  subroutine prepare_1d_data(nlev,ntime,jul,sec,lev,ssh,&
                             qnet,qsw,taux,tauy,fsw,&
                             temp,salt,&
                             dhdx,dhdy,&
                             dtdx,dtdy,&
                             dsdx,dsdy)
    integer, intent(in) :: nlev, ntime
    integer, dimension(ntime), intent(in) :: jul, sec
    REALTYPE, dimension(nlev), intent(in) :: lev
    REALTYPE, dimension(ntime), intent(in) :: ssh
    REALTYPE, dimension(ntime), intent(in) :: qnet,qsw,taux,tauy,fsw
    REALTYPE, dimension(nlev, ntime), intent(in) :: temp,salt
    REALTYPE, dimension(ntime), intent(in) :: dhdx,dhdy
    REALTYPE, dimension(nlev, ntime), intent(in) :: dtdx,dtdy,dsdx,dsdy

    integer, dimension(ntime) :: yyyy, mm, dd, hh, min, ss
    character(len=*), parameter :: fmt_short = 'F7.4', fmt_mid = 'F10.4', fmt_long = 'ES14.6'
    
    call calendar_date(jul, yyyy, mm, dd)
    call sec2hms(sec, hh, min, ss)

    call write_surface_data(unit_elev,'elevation.dat',ntime,yyyy,mm,dd,hh,min,ss,ssh,fmt_short,dhdx,fmt_long,dhdy,fmt_long)
    call write_surface_data(unit_heat,'heatflux.dat',ntime,yyyy,mm,dd,hh,min,ss,qnet-qsw,fmt_long,qsw,fmt_long)
    call write_surface_data(unit_mome,'windstress.dat',ntime,yyyy,mm,dd,hh,min,ss,taux,fmt_long,tauy,fmt_long)
    call write_surface_data(unit_fres,'freshwater.dat',ntime,yyyy,mm,dd,hh,min,ss,fsw,fmt_long)
    call write_profile_data(unit_temp,'temperature.dat',ntime,nlev,yyyy,mm,dd,hh,min,ss,lev,fmt_mid,temp,fmt_mid,dtdx,fmt_long,dtdy,fmt_long)
    call write_profile_data(unit_salt,'salinity.dat',ntime,nlev,yyyy,mm,dd,hh,min,ss,lev,fmt_mid,salt,fmt_mid,dsdx,fmt_long,dsdy,fmt_long)
  end subroutine prepare_1d_data

  subroutine write_surface_data(fu,fn,ntime,yyyy,mm,dd,hh,min,ss,data1,fmt1,data2,fmt2,data3,fmt3)
    integer, intent(in) :: fu
    character(len=*), intent(in) :: fn
    integer, intent(in) :: ntime
    integer, dimension(ntime), intent(in) :: yyyy,mm,dd,hh,min,ss
    REALTYPE, dimension(ntime), intent(in) :: data1
    character(len=*), intent(in) :: fmt1
    REALTYPE, dimension(ntime), optional, intent(in) :: data2, data3
    character(len=*), optional, intent(in) :: fmt2, fmt3

    integer :: ierr, it
    character(len=1024) :: cmsg
    character(len=*), parameter :: cfmt_time = 'I4,A,I0.2,A,I0.2,1X,I0.2,A,I0.2,A,I0.2,2X'

    open(unit=fu, file=trim(fn), status='replace', iostat=ierr, iomsg=cmsg)
    if (ierr /= 0) then
      print*,trim(cmsg)
      stop 3
    endif

    do it = 1, ntime
      write(unit=fu, fmt='('//trim(cfmt_time)//')', advance='no')yyyy(it),'/',mm(it),'/',dd(it),hh(it),':',min(it),':',ss(it)
      write(unit=fu, fmt='('//trim(fmt1)//',2X)', advance='no')data1(it)
      if (present(data2)) then
        write(unit=fu, fmt='('//trim(fmt2)//',2X)', advance='no')data2(it)
      endif
      if (present(data3)) then
        write(unit=fu, fmt='('//trim(fmt3)//')', advance='no')data3(it)
      endif
      write(unit=fu, fmt='(A1)', advance='yes')' '
    enddo

    close(unit=fu)
  end subroutine write_surface_data

  subroutine write_profile_data(fu,fn,ntime,nlev,yyyy,mm,dd,hh,min,ss,lev,fmt_lev,data1,fmt1,data2,fmt2,data3,fmt3)
    integer, intent(in) :: fu
    character(len=*), intent(in) :: fn
    integer, intent(in) :: ntime, nlev
    integer, dimension(ntime), intent(in) :: yyyy,mm,dd,hh,min,ss
    REALTYPE, dimension(nlev), intent(in) :: lev
    REALTYPE, dimension(nlev, ntime), intent(in) :: data1
    character(len=*), intent(in) :: fmt_lev, fmt1
    REALTYPE, dimension(nlev,ntime), optional, intent(in) :: data2, data3
    character(len=*), optional, intent(in) :: fmt2, fmt3

    integer :: ierr, it, il, nc
    character(len=1024) :: cmsg
    character(len=*), parameter :: cfmt_time = 'I4,A,I0.2,A,I0.2,1X,I0.2,A,I0.2,A,I0.2,2X'

    open(unit=fu, file=trim(fn), status='replace', iostat=ierr, iomsg=cmsg)
    if (ierr /= 0) then
      print*,trim(cmsg)
      stop 3
    endif

    nc = 2
    if (present(data2)) nc = nc + 1
    if (present(data3)) nc = nc + 1

    do it = 1, ntime
      write(unit=fu, fmt='('//trim(cfmt_time)//')', advance='no')yyyy(it),'/',mm(it),'/',dd(it),hh(it),':',min(it),':',ss(it)
      write(unit=fu, fmt='(I3,X,I2)', advance='yes')nlev+1,nc

      do il = 1, nlev
        write(unit=fu, fmt='('//trim(fmt_lev)//',2X)', advance='no')-lev(il)
        write(unit=fu, fmt='('//trim(fmt1)//',2X)', advance='no')data1(il,it)
        if (present(data2)) then
          write(unit=fu, fmt='('//trim(fmt2)//',2X)', advance='no')data2(il,it)
        endif
        if (present(data3)) then
          write(unit=fu, fmt='('//trim(fmt3)//')', advance='no')data3(il,it)
        endif
        write(unit=fu, fmt='(A1)', advance='yes')' '
      enddo
    enddo

    close(unit=fu)
  end subroutine write_profile_data

  subroutine prepare_1d_yaml(ilon,ilat,ipnt,restart)
    integer, intent(in) :: ilon, ilat, ipnt
    logical, intent(in) :: restart

    integer :: ierr
    character(len=1024) :: cmsg

    open(unit=unit_yaml, file='gotm.yaml', status='replace', iostat=ierr, iomsg=cmsg)
    if (ierr /= 0) then
      print*,trim(cmsg)
      stop 3
    endif

    write(unit=unit_yaml, fmt='(A)')'title: GOTM simulation'
    write(unit=unit_yaml, fmt='(A)')'location:'
    write(unit=unit_yaml, fmt='(A,I0.2,A,I0.2,A,I0.2)')'  name: point_',ipnt,'_',ilon,'x',ilat
    write(unit=unit_yaml, fmt='(A,F10.6)')'  latitude: ',lat(ilat)
    write(unit=unit_yaml, fmt='(A,F11.6)')'  longitude: ',lon(ilon)
    write(unit=unit_yaml, fmt='(A,F12.6)')'  depth: ',depth(ilon,ilat)-0.01
    write(unit=unit_yaml, fmt='(A)')'time:'
    write(unit=unit_yaml, fmt='(A,I0.4,A,I0.2,A,I0.2,A)')'  start: ',year_st,'-',month_st,'-',day_st,' 00:00:00'
    write(unit=unit_yaml, fmt='(A,I0.4,A,I0.2,A,I0.2,A)')'  stop: ',year_ed,'-',month_ed,'-',day_ed,' 00:00:00'
    write(unit=unit_yaml, fmt='(A,I0)')'  dt: ',dt_intv
    write(unit=unit_yaml, fmt='(A)')'grid:'
    write(unit=unit_yaml, fmt='(A,I0)')'  nlev: ',min(floor(depth(ilon,ilat)), 250)
    write(unit=unit_yaml, fmt='(A)')'  method: analytical'
    write(unit=unit_yaml, fmt='(A)')'temperature:'
    write(unit=unit_yaml, fmt='(A)')'  method: file'
    write(unit=unit_yaml, fmt='(A)')'  file: temperature.dat'
    write(unit=unit_yaml, fmt='(A)')'  column: 1'
    write(unit=unit_yaml, fmt='(A)')'salinity:'
    write(unit=unit_yaml, fmt='(A)')'  method: file'
    write(unit=unit_yaml, fmt='(A)')'  file: salinity.dat'
    write(unit=unit_yaml, fmt='(A)')'  column: 1'
    write(unit=unit_yaml, fmt='(A)')'surface:'
    write(unit=unit_yaml, fmt='(A)')'  fluxes:'
    write(unit=unit_yaml, fmt='(A)')'    heat:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: heatflux.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 1'
    write(unit=unit_yaml, fmt='(A)')'    tx:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: windstress.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 1'
    write(unit=unit_yaml, fmt='(A)')'    ty:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: windstress.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 2'
    write(unit=unit_yaml, fmt='(A)')'  precip:'
    write(unit=unit_yaml, fmt='(A)')'    method: file'
    write(unit=unit_yaml, fmt='(A)')'    file: freshwater.dat'
    write(unit=unit_yaml, fmt='(A)')'    column: 1'
    write(unit=unit_yaml, fmt='(A)')'  swr:'
    write(unit=unit_yaml, fmt='(A)')'    method: file'
    write(unit=unit_yaml, fmt='(A)')'    file: heatflux.dat'
    write(unit=unit_yaml, fmt='(A)')'    column: 2'
    write(unit=unit_yaml, fmt='(A)')'mimic_3d:'
    write(unit=unit_yaml, fmt='(A)')'  ext_pressure:'
    write(unit=unit_yaml, fmt='(A)')'    type: elevation'
    write(unit=unit_yaml, fmt='(A)')'    dpdx:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: elevation.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 2'
    write(unit=unit_yaml, fmt='(A)')'    dpdy:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: elevation.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 3'
    write(unit=unit_yaml, fmt='(A)')'  int_press:'
    write(unit=unit_yaml, fmt='(A)')'    dtdx:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: temperature.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 2'
    write(unit=unit_yaml, fmt='(A)')'    dtdy:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: temperature.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 3'
    write(unit=unit_yaml, fmt='(A)')'    dsdx:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: salinity.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 2'
    write(unit=unit_yaml, fmt='(A)')'    dsdy:'
    write(unit=unit_yaml, fmt='(A)')'      method: file'
    write(unit=unit_yaml, fmt='(A)')'      file: salinity.dat'
    write(unit=unit_yaml, fmt='(A)')'      column: 3'
    write(unit=unit_yaml, fmt='(A)')'  zeta:'
    write(unit=unit_yaml, fmt='(A)')'    method: file'
    write(unit=unit_yaml, fmt='(A)')'    file: elevation.dat'
    write(unit=unit_yaml, fmt='(A)')'    column: 1'
    write(unit=unit_yaml, fmt='(A)')'restart:'
    if (restart) then
      write(unit=unit_yaml, fmt='(A)')'  load: true'
    else
      write(unit=unit_yaml, fmt='(A)')'  load: false'
    endif
    write(unit=unit_yaml, fmt='(A)')'output:'
    write(unit=unit_yaml, fmt='(A,I0.2,A,I0.2,A,I0.2,A)')'  point_',ipnt,'_',ilon,'x',ilat,':'
    write(unit=unit_yaml, fmt='(A)')'    format: netcdf'
    write(unit=unit_yaml, fmt='(A)')'    time_unit: dt'
    write(unit=unit_yaml, fmt='(A)')'    time_step: 1'
    write(unit=unit_yaml, fmt='(A)')'    time_method: point'
    write(unit=unit_yaml, fmt='(A)')'    variables:'
    write(unit=unit_yaml, fmt='(A)')'    - source: *'

    close(unit=unit_yaml)
  end subroutine prepare_1d_yaml

  subroutine collect_result()
  end subroutine collect_result

  subroutine time_loop_3d_month()
  end subroutine time_loop_3d_month

  subroutine clean_up_3d()
    deallocate(lon, lat, lev)
    deallocate(botlev, depth)
  end subroutine clean_up_3d

end module gotm3d
