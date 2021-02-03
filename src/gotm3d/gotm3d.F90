#include"cppdefs.h"
module gotm3d

  use time, ONLY: read_time_string, calendar_date
  use settings, ONLY: type_settings, type_gotm_settings

  implicit none

  private
  integer, parameter         :: namlst=10

  type(type_gotm_settings) :: settings_3d

  character(len=19)  :: start, stop
  integer            :: jul_st, sec_st, jul_ed, sec_ed
  integer            :: year_st, month_st, day_st
  integer            :: year_ed, month_ed, day_ed
  integer            :: dt
  integer            :: time_unit

  integer :: npnt

  integer, parameter :: one_file = 0
  integer, parameter :: per_year = 1
  integer, parameter :: per_month = 2

  integer :: nlon, nlat, nlev, npnt
  REALTYPE, dimension(:), allocatable :: lon, lat, lev
  integer, dimension(:,:), allocatable :: botlev
  REALTYPE, dimension(:,:), allocatable :: depth

  character(len=1024) :: sst_file, sss_file, ssh_file, temp_file, salt_file
  character(len=100) :: sst_name, sss_name, ssh_name, temp_name, salt_name

  character(len=1024), public :: gotm3d_yaml_file = 'gotm3d.yaml'

  REALTYPE, parameter :: pi = 3.141592653
  REALTYPE, parameter :: R = 6371000
  REALTYPE, parameter :: deg2rad = pi / 180
  REALTYPE, parameter :: deg2m = deg2rad * R

contains

  subroutine init_gotm3d()
    class (type_settings), pointer :: branch, twig
    
    character(len=1024) :: fn
    REALTYPE, dimension(:,:), allocatable :: t2
    REALTYPE, dimension(:,:,:), allocatable :: t3
    integer, ilon, ilat, ntime
    REALTYPE :: missing

    LEVEL1 'init_gotm3d'
    settings_3d%path = ''
    inquire(file=trim(gotm3d_yaml_file),exist=file_exists)
    if (file_exists) then
       LEVEL2 'Reading configuration from: ',trim(gotm3d_yaml_file)
       call settings_3d%load(trim(gotm3d_yaml_file), namlst)
    else
       FATAL 'Configuration file ' // trim(gotm3d_yaml_file) // ' not found.'
       stop 2
    end if

    ! time handling
    branch => settings_3d%get_child('time')
    call branch%get(start, 'start', 'start date and time', units='yyyy-mm-dd HH:MM:SS', &
                    default='2017-01-01 00:00:00')
    call branch%get(stop, 'stop', 'stop date and time', units='yyyy-mm-dd HH:MM:SS', &
                    default='2017-12-31 00:00:00')
    call branch%get(dt, 'dt', 'time step for integration', 's', &
                    minimum=0_timestepkind, default=3600_timestepkind)
    call read_time_string(start,jul_st,sec_st)
    call read_time_string(stop,jul_ed,sec_ed)

    call calendar_date(jul_st, year_st, month_st, day_st)
    call calendar_date(jul_ed, year_ed, month_ed, day_ed)

    ! forcing files
    branch => settings_3d%get_child('surface')
    twig => branch%get_child('sst')
    call twig%get(sst_file, 'file', 'SST forcing file name (can contain placeholders such as %year% and %month%)', default='sst.nc')
    call twig%get(sst_name, 'name', 'variable name for SST in the forcing file', default='sst')
    twig => branch%get_child('ssh')
    call twig%get(ssh_file, 'file', 'SSH forcing file name (can contain placeholders such as %year% and %month%)', default='ssh.nc')
    call twig%get(ssh_name, 'name', 'variable name for SSH in the forcing file', default='ssh')
    twig => branch%get_child('sss')
    call twig%get(sss_file, 'file', 'SSS forcing file name (can contain placeholders such as %year% and %month%)', default='sss.nc')
    call twig%get(sss_name, 'name', 'variable name for SSS in the forcing file', default='sss')
    branch => settings_3d%get_child('subsurface')
    twig => branch%get_child('temp')
    call twig%get(temp_file, 'file', 'subsurface temperature forcing file name (can contain placeholders such as %year% and %month%)', default='temp.nc')
    call twig%get(temp_name, 'name', 'variable name for subsurface temperature in the forcing file', default='temp')
    twig => branch%get_child('salt')
    call twig%get(salt_file, 'file', 'subsurface salinity forcing file name (can contain placeholders such as %year% and %month%)', default='salt.nc')
    call twig%get(salt_name, 'name', 'variable name for subsurface salinity in the forcing file', default='salt')

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
    allocate(t3(nlon, nlat, nlev))
    ntime = ncread_timelen(trim(fn))
    t3 = ncread_subsurface(trim(fn), trim(temp_name), nlon, nlat, nlev, ntime, 1, "first")
    missing = ncread_missing(trim(fn), trim(temp_name))
    allocate(botlev(nlon, nlat))
    allocate(depth(nlon, nlat))
    botlev = findloc(t3, missing, dim=3)
    where (botlev == 0)
      botlev = nlev
      depth = lev(nlev)
    elsewhere (botlev == 1)
      depth = 0
    else
      depth = lev(botlev)
    endwhere
    npnt = count(depth == 0)

    deallocate(t3)
  end subroutine init_gotm3d

  function pack_ocean_const(nlon, nlat, nlev, npnt, data, mask) result(oce)
    integer, intent(in) :: nlon, nlat, nlev, npnt
    REALTYPE, dimension(nlon, nlat, nlev), intent(in) :: data
    logical, dimension(nlon, nlat), intent(in) :: mask

    REALTYPE, dimension(npnt, nlev) :: oce

    integer :: i, j, k

    k = 0
    do i = 1, nlon
      do j = 1, nlat
        if (mask(i,j)) then
          k = k + 1
          oce(k, :) = data(i, j, :)
        endif
      enddo
    enddo
  end function pack_ocean_const

  function pack_ocean_series(nlon, nlat, nlev, npnt, ntime, data, mask) result(oce)
    integer, intent(in) :: nlon, nlat, nlev, npnt, ntime
    REALTYPE, dimension(nlon, nlat, nlev, ntime), intent(in) :: data
    logical, dimension(nlon, nlat), intent(in) :: mask

    REALTYPE, dimension(npnt, nlev, ntime) :: oce

    integer :: i, j, k

    k = 0
    do i = 1, nlon
      do j = 1, nlat
        if (mask(i,j)) then
          k = k + 1
          oce(k, :, :) = data(i, j, :, :)
        endif
      enddo
    enddo
  end function pack_ocean_series

  function unpack_ocean(nlon, nlat, nlev, npnt, oce, mask, missing) result(data)
    integer, intent(in) :: nlon, nlat, nlev, npnt
    REALTYPE, dimension(npnt, nlev), intent(in) :: oce
    logical, dimension(nlon, nlat), intent(in) :: mask
    REALTYPE, intent(in) :: missing

    REALTYPE, dimension(nlon, nlat, nlev) :: data

    integer :: i, j, k

    k = 0
    do i = 1, nlon
      do j = 1, nlat
        if (mask(i,j)) then
          k = k + 1
          data(i, j, :) = oce(k, :)
        else
          data(i, j, :) = missing
        endif
      enddo
    enddo
  end function unpack_ocean

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
    integer :: year, pyear, nyear
    logical :: restart
    character(len=1024) :: fn
    integer :: ntime
    REALTYPE, dimension(:,:,:), allocatable :: sst, ssh, sss
    REALTYPE, dimension(:,:,:,:), allocatable :: temp, salt
    REALTYPE, dimension(:,:,:), allocatable :: dsstdx, dsstdy, dsshdx, dsshdy, dsssdx, dsssdy
    REALTYPE, dimension(:,:,:,:), allocatable :: dtempdx, dtempdy, dsaltdx, dsaltdy

    DO year = year_st, year_ed
      pyear = year - 1
      nyear = year + 1

      fn = substitute_file_year(trim(sst_file), year)
      ntime = ncread_timelen(trim(fn))
      allocate(sst(nlon, nlat, 0:ntime+1))
      allocate(ssh(nlon, nlat, 0:ntime+1))
      allocate(sss(nlon, nlat, 0:ntime+1))
      allocate(temp(nlon, nlat, nlev, 0:ntime+1))
      allocate(salt(nlon, nlat, nlev, 0:ntime+1))

      sst = read_data_year_2d(trim(sst_file), trim(sst_name), year, ntime)
      ssh = read_data_year_2d(trim(ssh_file), trim(ssh_name), year, ntime)
      sss = read_data_year_2d(trim(sss_file), trim(sss_name), year, ntime)
      temp = read_data_year_3d(trim(temp_file), trim(temp_name), year, ntime)
      salt = read_data_year_3d(trim(salt_file), trim(salt_name), year, ntime)

      allocate(dsstdx(nlon, nlat, 0:ntime+1))
      allocate(dsstdy(nlon, nlat, 0:ntime+1))
      allocate(dsshdx(nlon, nlat, 0:ntime+1))
      allocate(dsshdy(nlon, nlat, 0:ntime+1))
      allocate(dsssdx(nlon, nlat, 0:ntime+1))
      allocate(dsssdy(nlon, nlat, 0:ntime+1))
      allocate(dtempdx(nlon, nlat, nlev, 0:ntime+1))
      allocate(dtempdy(nlon, nlat, nlev, 0:ntime+1))
      allocate(dsaltdx(nlon, nlat, nlev, 0:ntime+1))
      allocate(dsaltdy(nlon, nlat, nlev, 0:ntime+1))

      call gradient_2d(nlon, nlat, ntime+2, lon, lat, sst, dsstdx, dsstdy)
      call gradient_2d(nlon, nlat, ntime+2, lon, lat, ssh, dsshdx, dsshdy)
      call gradient_2d(nlon, nlat, ntime+2, lon, lat, sss, dsssdx, dsssdy)
      call gradient_3d(nlon, nlat, nlev, ntime+2, lon, lat, temp, dtempdx, dtempdy)
      call gradient_3d(nlon, nlat, nlev, ntime+2, lon, lat, salt, dsaltdx, dsaltdy)

      restart = year /= year_st

      ipnt = 0
      do ilon = 1, nlon
        do ilat = 1, nlat
          if (depth(ilon, ilat) /= 0) then
            ipnt = ipnt + 1
            call prepare_1d_data(ipnt)
            call prepare_1d_yaml(ipnt)
            call gotm1d()
          endif
        enddo
      enddo
      call collect_result()

      deallocate(sst)
      deallocate(ssh)
      deallocate(sss)
      deallocate(temp)
      deallocate(salt)
      deallocate(dsstdx)
      deallocate(dsstdy)
      deallocate(dsshdx)
      deallocate(dsshdy)
      deallocate(dsssdx)
      deallocate(dsssdy)
      deallocate(dtempdx)
      deallocate(dtempdy)
      deallocate(dsaltdx)
      deallocate(dsaltdy)
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

  function read_var_year_2d(fn, varn, year, ntime) result(data)
    character(len=*), intent(in) :: fn, varn
    integer, intent(in) :: year, ntime
    REALTYPE, dimension(nlon, nlat, 0:ntime+1) :: data

    character(len=1024) :: fn1
    integer :: ntime_p, ntime_n

    fn1 = substitute_file_year(trim(fn), year)
    data(:, 1:ntime) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime, ntime)
    fn1 = substitute_file_year(trim(fn), year-1)
    ntime_p = ncread_timelen(trim(fn1))
    data(:, 0) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime_p, 1, "last")
    fn1 = substitute_file_year(trim(fn), year+1)
    ntime_n = ncread_timelen(trim(fn1))
    data(:, ntime+1) = ncread_surface(trim(fn1), trim(varn), nlon, nlat, ntime_n, 1)
  end function read_var_year_2d

  function read_var_year_3d(fn, varn, year, ntime) result(data)
    character(len=*), intent(in) :: fn, varn
    integer, intent(in) :: year, ntime
    REALTYPE, dimension(nlon, nlat, nlev, 0:ntime+1) :: data

    character(len=1024) :: fn1
    integer :: ntime_p, ntime_n

    fn1 = substitute_file_year(trim(fn), year)
    data(:, :, 1:ntime) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime, ntime)
    fn1 = substitute_file_year(trim(fn), year-1)
    ntime_p = ncread_timelen(trim(fn1))
    data(:, :, 0) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime_p, 1, 'last')
    fn1 = substitute_file_year(trim(fn), year+1)
    ntime_n = ncread_timelen(trim(fn1))
    data(:, :, ntime+1) = ncread_subsurface(trim(fn1), trim(varn), nlon, nlat, nlev, ntime_n, 1)
  end function read_var_year_3d

  function substitute_file_year(file, year) result(newfile)
    character(len=*), intent(in) :: file
    integer,intent(in) :: year
    character(len=1024) :: newfile

    integer :: pos
    character(len=4) :: stryear

    write(stryear,*)year
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

  subroutine prepare_1d()
  end subroutine prepare_1d

  subroutine collect_result()
  end subroutine collect_result

end module gotm3d
