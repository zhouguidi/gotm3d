#include"cppdefs.h"
module ncutils
  use netcdf

  implicit none

  public ncread_dimshape, ncread_lonlat, ncread_lonlatlev, ncread_surface, ncread_subsurface

  contains
  subroutine ncread_dimshape(fn, , nlat, nlev, ntime)
    character(len=*), intent(in) :: fn
    integer, intent(out) :: nlon, nlat, nlev, ntime

    integer :: ncid, dimid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_inq_dimid(ncid, 'lon', dimid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_dimid(ncid, 'Lon', dimid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_dimid(ncid, 'LON', dimid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_dimid(ncid, 'longitude', dimid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_dimid(ncid, 'Longitude', dimid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_dimid(ncid, 'LONGITUDE', dimid)
              if (ierr /= NF90_NOERR) then
                dimid = -1
              endif
            endif
          endif
        endif
      endif
    endif
    if (dimid == -1) then
      nlon = -1
    else
      ierr = nf90_inqure_dimension(ncid, dimid, len=nlon)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif

    ierr = nf90_inq_dimid(ncid, 'lat', dimid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_dimid(ncid, 'Lat', dimid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_dimid(ncid, 'LAT', dimid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_dimid(ncid, 'latitude', dimid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_dimid(ncid, 'Latitude', dimid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_dimid(ncid, 'LATITUDE', dimid)
              if (ierr /= NF90_NOERR) then
                dimid = -1
              endif
            endif
          endif
        endif
      endif
    endif
    if (dimid == -1) then
      nlat = -1
    else
      ierr = nf90_inqure_dimension(ncid, dimid, len=nlat)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif

    ierr = nf90_inq_dimid(ncid, 'lev', dimid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_dimid(ncid, 'Lev', dimid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_dimid(ncid, 'LEV', dimid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_dimid(ncid, 'level', dimid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_dimid(ncid, 'Level', dimid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_dimid(ncid, 'LEVEL', dimid)
              if (ierr /= NF90_NOERR) then
                ierr = nf90_inq_dimid(ncid, 'depth', dimid)
                if (ierr /= NF90_NOERR) then
                  ierr = nf90_inq_dimid(ncid, 'Depth', dimid)
                  if (ierr /= NF90_NOERR) then
                    ierr = nf90_inq_dimid(ncid, 'DEPTH', dimid)
                    if (ierr /= NF90_NOERR) then
                      ierr = nf90_inq_dimid(ncid, 'z', dimid)
                      if (ierr /= NF90_NOERR) then
                        dimid = -1
                      endif
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif
      endif
    endif
    if (dimid == -1) then
      nlev = -1
    else
      ierr = nf90_inqure_dimension(ncid, dimid, len=nlev)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif

    ierr = nf90_inq_dimid(ncid, 'time', dimid)
    if (ierr /= NF90_NOERR) then
      ntime = -1
    else
      ierr = nf90_inqure_dimension(ncid, dimid, len=ntime)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif

    ierr = nf90_close(ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)
  end subroutine ncread_dimshape

  subroutine ncread_lonlat(fn, nlon, lon, nlat, lat)
    character(len=*), intent(in) :: fn
    integer, intent(in) :: nlon, nlat
    REALTYPE, dimension(nlon), intent(out) :: lon
    REALTYPE, dimension(nlat), intent(out) :: lat

    integer :: ncid, varid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_inq_varid(ncid, 'lon', varid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_varid(ncid, 'Lon', varid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_varid(ncid, 'LON', varid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_varid(ncid, 'longitude', varid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_varid(ncid, 'Longitude', varid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_varid(ncid, 'LONGITUDE', varid)
              if (ierr /= NF90_NOERR) then
                varid = -1
              endif
            endif
          endif
        endif
      endif
    endif
    if (varid /= -1) then
      ierr = nf90_get_var(ncid, varid, lon)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif

    ierr = nf90_inq_varid(ncid, 'lat', varid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_varid(ncid, 'Lat', varid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_varid(ncid, 'LAT', varid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_varid(ncid, 'latitude', varid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_varid(ncid, 'Latitude', varid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_varid(ncid, 'LATITUDE', varid)
              if (ierr /= NF90_NOERR) then
                varid = -1
              endif
            endif
          endif
        endif
      endif
    endif
    if (varid /= -1) then
      ierr = nf90_get_var(ncid, varid, lat)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif
  end subroutine ncread_lonlat

  subroutine ncread_lonlatlev(fn, nlon, lon, nlat, lat, nlev, lev)
    character(len=*), intent(in) :: fn
    integer, intent(in) :: nlon, nlat, nlev
    REALTYPE, dimension(nlon), intent(out) :: lon
    REALTYPE, dimension(nlat), intent(out) :: lat
    REALTYPE, dimension(nlev), intent(out) :: lev

    integer :: ncid, varid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    call ncread_lonlat(fn, nlon, lon, nlat, lat)

    ierr = nf90_inq_varid(ncid, 'lev', varid)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_inq_varid(ncid, 'Lev', varid)
      if (ierr /= NF90_NOERR) then
        ierr = nf90_inq_varid(ncid, 'LEV', varid)
        if (ierr /= NF90_NOERR) then
          ierr = nf90_inq_varid(ncid, 'level', varid)
          if (ierr /= NF90_NOERR) then
            ierr = nf90_inq_varid(ncid, 'Level', varid)
            if (ierr /= NF90_NOERR) then
              ierr = nf90_inq_varid(ncid, 'LEVEL', varid)
              if (ierr /= NF90_NOERR) then
                ierr = nf90_inq_varid(ncid, 'depth', varid)
                if (ierr /= NF90_NOERR) then
                  ierr = nf90_inq_varid(ncid, 'Depth', varid)
                  if (ierr /= NF90_NOERR) then
                    ierr = nf90_inq_varid(ncid, 'DEPTH', varid)
                    if (ierr /= NF90_NOERR) then
                      ierr = nf90_inq_varid(ncid, 'z', varid)
                      if (ierr /= NF90_NOERR) then
                        varid = -1
                      endif
                    endif
                  endif
                endif
              endif
            endif
          endif
        endif
      endif
    endif
    if (varid /= -1) then
      ierr = nf90_get_var(ncid, varid, lev)
      if (ierr /= NF90_NOERR) call handle_err(ierr)
    endif
  end subroutine ncread_lonlatlev

  function ncread_surface(fn, varn, nlon, nlat, ntime_tot, ntime, loc) result(data)
    character(len=*), intent(in) :: fn
    character(len=*), intent(in) :: varn
    integer, intent(in) :: nlon, nlat, ntime_tot, ntime
    character(len=*), intent(in) :: loc

    REALTYPE, dimension(nlon, nlat, ntime) :: data

    integer :: ncid, varid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_inq_varid(ncid, trim(varn), varid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    if (loc == "first" ) then
      ierr = nf90_get_var(ncid, varid, data, start=(/1,1,1/), count=(/nlon,nlat,ntime/))
    elseif (loc == "last" ) then
      ierr = nf90_get_var(ncid, varid, data, start=(/1,1,ntime_tot-ntime+1/), count=(/nlon,nlat,ntime/))
    endif
    if (ierr /= NF90_NOERR) call handle_err(ierr)
  end function ncread_surface

  function ncread_subsurface(fn, varn, nlon, nlat, nlev, ntime_tot, ntime, loc) result(data)
    character(len=*), intent(in) :: fn
    character(len=*), intent(in) :: varn
    integer, intent(in) :: nlon, nlat, nlev, ntime_tot, ntime
    character(len=*), intent(in) :: loc

    REALTYPE, dimension(nlon, nlat, nlev, ntime) :: data

    integer :: ncid, varid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_inq_varid(ncid, trim(varn), varid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    if (loc == "first" ) then
      ierr = nf90_get_var(ncid, varid, data, start=(/1,1,1,1/), count=(/nlon,nlat,nlev,ntime/))
    elseif (loc == "last" ) then
      ierr = nf90_get_var(ncid, varid, data, start=(/1,1,1,ntime_tot-ntime+1/), count=(/nlon,nlat,nlev,ntime/))
    endif
    if (ierr /= NF90_NOERR) call handle_err(ierr)
  end function ncread_subsurface

  function ncread_missing(fn, varn) result(missing)
    character(len=*), intent(in) :: fn
    character(len=*), intent(in) :: varn
    REALTYPE, intent(out) :: missing

    integer :: ncid, varid, ierr

    ierr = nf90_open(trim(fn),NF90_NOWRITE,ncid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_inq_varid(ncid, trim(varn), varid)
    if (ierr /= NF90_NOERR) call handle_err(ierr)

    ierr = nf90_get_att(ncid, varid, "missing_value", missing)
    if (ierr /= NF90_NOERR) then
      ierr = nf90_get_att(ncid, varid, "_FillValue", missing)
      if (ierr /= NF90_NOERR) then
        call handle_err(ierr)
      endif
    endif
  end function ncread_missing

  subroutine handle_err(ierr,msg)
    integer, intent(in) :: ierr
    character(len=*), optional :: msg
    LEVEL2 'read_restart_data(): error'
    if (present(msg)) then
       LEVEL3 trim(nf90_strerror(ierr)),' - ',trim(msg)
    else
       LEVEL3 trim(nf90_strerror(ierr))
    end if
    stop
  end subroutine handle_err
end module ncutils
