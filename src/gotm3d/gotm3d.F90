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

  integer :: nlon, nlat, nlev, ntime, npnt
  REALTYPE, dimension(:), allocatable :: lon, lat, lev, time
  logical, dimension(:,:), allocatable :: mask
  integer, dimension(:), allocatable :: botlev
  REALTYPE, dimension(:), allocatable :: depth
  REALTYPE, dimension(:), allocatable :: sst, ssh, sss
  REALTYPE, dimension(:,:), allocatable :: temp, salt

  character(len=1024), public :: gotm3d_yaml_file = 'gotm3d.yaml'

contains

  subroutine init_gotm3d()
    class (type_settings), pointer :: branch, twig
    
    character(len=1024) :: fn
    REALTYPE, dimension(:,:,:), allocatable :: ttt
    integer, ilon, ilat
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
    call ncread_dimshape(trim(fn), nlon, nlat, nlev, ntime)
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

    ! get mask, bottom level, and depth
    allocate(ttt(nlon, nlat, nlev))
    ttt = ncread_subsurface(trim(fn), trim(temp_name), nlon, nlat, nlev, ntime, 1, "first")
    allocate(mask(nlon, nlat))
    missing = ncread_missing(trim(fn), trim(temp_name))
    mask = ttt(:,:,1) /= missing
    npnt = count(mask)
    allocate(temp(npnt, nlev))
    temp = pack_ocean(nlon, nlat, nlev, npnt, ttt, mask)
    allocate(botlev(npnt))
    allocate(depth(npnt))
    botlev = findloc(temp, missing, dim=2)
    where (botlev == 0)
      botlev = nlev
      depth = lev(nlev)
    elsewhere
      depth = lev(botlev)
    endwhere

    deallocate(ttt)
    deallocate(temp)
  end subroutine init_gotm3d

  function pack_ocean(nlon, nlat, nlev, npnt, data, mask) result(oce)
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
  end function pack_ocean

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

  subroutine time_loop_3d_year()
    integer :: year, pyear, nyear
    logical :: restart
    character(len=1024) :: fn

    DO year = year_st, year_ed
      pyear = year - 1
      nyear = year + 1

      call read_year_data(year)
      restart = year /= year_st
      DO ipnt = 1, npnt
        call prepare_1d_data(ipnt)
        call prepare_1d_yaml(ipnt)
        call gotm1d()
      END DO
      call collect_result()
    END DO
  end subroutine time_loop_3d_year

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
