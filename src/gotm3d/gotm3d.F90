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

  integer :: npnt

  integer, parameter :: per_year = 0
  integer, parameter :: per_month = 1

  REALTYPE, dimension(:), allocatable :: sst, ssh, sss
  REALTYPE, dimension(:,:), allocatable :: temp, salt

  character(len=1024), public :: gotm3d_yaml_file = 'gotm3d.yaml'

contains

  subroutine init_gotm3d()
    class (type_settings), pointer :: branch, twig

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
  end subroutine init_gotm3d

  subroutine time_loop_3d()
    IF (time_unit == per_year) THEN
      call time_loop_3d_year
    ELSE
    ENDIF
  end subroutine time_loop_3d

  subroutine time_loop_3d_year()
    integer :: year
    logical :: restart

    DO year = year_st, year_ed
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

  subroutine prepare_1d()
  end subroutine prepare_1d

  subroutine collect_result()
  end subroutine collect_result

end module gotm3d
