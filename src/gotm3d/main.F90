#include<cppdefs.h>
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: GOTM --- the main program  \label{sec:main}
!
! !INTERFACE:
   program main
!
! !DESCRIPTION:
! This is the main program of GOTM. However, because GOTM has been programmed
! in a modular way, this routine is very short and merely calls internal
! routines of other modules. Its main purpose is to update the time and to
! call the internal routines {\tt init\_gotm()}, {\tt time\_loop()}, and
! {\tt clean\_up()}, which are defined in the module {\tt gotm} as discussed in
! \sect{sec:gotm}.
!
! !USES:
   use time, ONLY: simtime
   use gotm3d, ONLY: init_gotm3d, time_loop_3d, clean_up_3d, gotm3d_nmlt_file
                     
!
   IMPLICIT NONE
!
! !REVISION HISTORY:
!  Original author(s): Karsten Bolding & Hans Burchard
!
!EOP
!
! !LOCAL VARIABLES:

   character(LEN=8)          :: systemdate
   character(LEN=10)         :: systemtime
   real                      :: t1=-1,t2=-1

   integer :: n, ipnt, year
!
!-----------------------------------------------------------------------
!BOC
   call cmdline()

  !  monitor CPU time and report system time and date
#ifdef FORTRAN95
   call CPU_Time(t1)

   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE3D
   STDERR 'GOTM3D started on ', systemdate(1:4), '/', &
                                systemdate(5:6), '/', &
                                systemdate(7:8),      &
                        ' at ', systemtime(1:2), ':', &
                                systemtime(3:4), ':', &
                                systemtime(5:6)
   STDERR LINE3D
#else
   STDERR LINE3D
   STDERR 'GOTM3D'
   STDERR LINE3D
#endif

   call init_gotm3d()

   call time_loop_3d()

   call clean_up_3d()

  !  report system date and time at end of run
#ifdef FORTRAN95
   call Date_And_Time(DATE=systemdate,TIME=systemtime)

   STDERR LINE3D
   STDERR 'GOTM3D finished on ', systemdate(1:4), '/', &
                                 systemdate(5:6), '/', &
                                 systemdate(7:8),      &
                         ' at ', systemtime(1:2), ':', &
                                 systemtime(3:4), ':', &
                                 systemtime(5:6)
   STDERR LINE3D
#else
   STDERR LINE3D
   STDERR 'GOTM3D'
   STDERR LINE3D
#endif

  !  report CPU time used for run
#ifdef FORTRAN95
   call CPU_Time(t2)

   STDERR 'CPU time:                    ',t2-t1,' seconds'
   STDERR 'Simulated time/CPU time:     ',simtime/(t2-t1)
#endif

   call print_version_3d()

   contains

!EOC
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: Parse the command line
!
! !INTERFACE:
   subroutine cmdline
!
! !DESCRIPTION:
!
! !LOCAL VARIABLES:
   character(len=1024) :: arg
   integer :: n, i, ios
   logical :: file_exists
!EOP
!-----------------------------------------------------------------------
!BOC
   n = command_argument_count()
   i = 1
   do while (i <= n)
      call get_command_argument(i, arg)
      select case (arg)
      case ('-v', '--version')
         call print_version()
         stop
      case ('-c', '--compile')
         call print_version()
         call compilation_options()
         stop
      case ('-h', '--help')
         call print_help()
         stop
      case default
         if (arg(1:2) == '--') then
            FATAL 'Command line option '//trim(arg)//' not recognized. Use -h to see supported options'
            stop 2
         end if
         gotm3d_nmlt_file = arg
         inquire(file=trim(gotm3d_nmlt_file),exist=file_exists)
         if (.not. file_exists) then
            FATAL 'Custom configuration file '//trim(arg)//' does not exist.'
            stop 2
         end if
      end select
      i = i+1
   end do

   end subroutine  cmdline

   subroutine compilation_options()
#ifdef _FABM_
      LEVEL1 '_FABM_'
#endif
#ifdef SEAGRASS
      LEVEL1 'SEAGRASS'
#endif
#ifdef SPM
      LEVEL1 'SPM'
#endif
#ifdef SEDIMENT
      LEVEL1 'SEDIMENT'
#endif
      STDERR LINE
   end subroutine compilation_options

   subroutine print_help()
      print '(a)', 'Usage: gotm3d [OPTIONS]'
      print '(a)', ''
      print '(a)', 'Options:'
      print '(a)', ''
      print '(a)', '  -h, --help            print usage information and exit'
      print '(a)', '  -v, --version         print version information'
      print '(a)', '  -c, --compiler        print compilation options'
      print '(a)', '  <namelist file>       read configuration from file (default gotm3d.nml)'
      print '(a)', ''
   end subroutine print_help

end program

!-----------------------------------------------------------------------
! Copyright by the GOTM-team under the GNU Public License - www.gnu.org
!-----------------------------------------------------------------------
