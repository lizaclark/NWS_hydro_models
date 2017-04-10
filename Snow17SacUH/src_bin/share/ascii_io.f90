! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!      subroutines for reading param files: sac, snow17, unit hydrograph & pet
! ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

subroutine read_sac_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: uztwm,uzfwm,uzk,pctim,adimp,zperc,rexp, &
                           lztwm,lzfsm,lzfpm,lzsk,lzpk,pfree,riva,side,rserv, &
                           hru_area, hru_id, peadj, pxadj
  implicit none

  !input variables
  character(len=400),intent(in) :: param_file_name
  integer(I4B),intent(in) :: n_hrus

  !local variables
  character(len=400)             :: readline
  character(len=50)	         :: param
  integer(I4B)		         :: ios=0
  integer                        :: n_params_read  ! count number read
  integer :: pos

  ! open parameter file
  open(unit=50,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(hru_id(n_hrus))
  allocate(hru_area(n_hrus))
  allocate(uztwm(n_hrus))
  allocate(uzfwm(n_hrus))
  allocate(uzk(n_hrus))
  allocate(pctim(n_hrus))
  allocate(adimp(n_hrus))
  allocate(zperc(n_hrus))
  allocate(rexp(n_hrus))
  allocate(lztwm(n_hrus))
  allocate(lzfsm(n_hrus))
  allocate(lzfpm(n_hrus))
  allocate(lzsk(n_hrus))
  allocate(lzpk(n_hrus))
  allocate(pfree(n_hrus))
  allocate(riva(n_hrus))
  allocate(side(n_hrus))
  allocate(rserv(n_hrus))
  allocate(peadj(n_hrus))
  allocate(pxadj(n_hrus))

  print*, 'Reading Sac parameters'

  ! --- now loop through parameter file and assign parameters
  n_params_read = 0
  do while(ios == 0)
    read(unit=50,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = trim(readline(1:pos))
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          read(readline, *, iostat=ios) hru_id
          n_params_read = n_params_read + 1
        case ('hru_area')
          read(readline, *, iostat=ios) hru_area
          n_params_read = n_params_read + 1
        case ('uztwm')
          read(readline, *, iostat=ios) uztwm
          n_params_read = n_params_read + 1
        case ('uzfwm')
          read(readline, *, iostat=ios) uzfwm
          n_params_read = n_params_read + 1
        case('uzk')
          read(readline, *, iostat=ios) uzk
          n_params_read = n_params_read + 1
        case('pctim')
          read(readline, *, iostat=ios) pctim
          n_params_read = n_params_read + 1
        case('adimp')
          read(readline, *, iostat=ios) adimp
          n_params_read = n_params_read + 1
        case('zperc')
          read(readline, *, iostat=ios) zperc
          n_params_read = n_params_read + 1
        case('rexp')
          read(readline, *, iostat=ios) rexp
          n_params_read = n_params_read + 1
        case('lztwm')
          read(readline, *, iostat=ios) lztwm
          n_params_read = n_params_read + 1
        case('lzfsm')
          read(readline, *, iostat=ios) lzfsm
          n_params_read = n_params_read + 1
        case('lzfpm')
          read(readline, *, iostat=ios) lzfpm
          n_params_read = n_params_read + 1
        case('lzsk')
          read(readline, *, iostat=ios) lzsk
          n_params_read = n_params_read + 1
        case('lzpk')
          read(readline, *, iostat=ios) lzpk
          n_params_read = n_params_read + 1
        case('pfree')
          read(readline, *, iostat=ios) pfree
          n_params_read = n_params_read + 1
        case('riva')
          read(readline, *, iostat=ios) riva
          n_params_read = n_params_read + 1
        case('side')
          read(readline, *, iostat=ios) side
          n_params_read = n_params_read + 1
        case('rserv')
          read(readline, *, iostat=ios) rserv
          n_params_read = n_params_read + 1
        case('peadj')
          read(readline, *, iostat=ios) peadj  ! peadj & pxadj are used by NWS
                                               ! to modify
          n_params_read = n_params_read + 1    ! forcings before running models
        case('pxadj')                          ! included here as sac params
          read(readline, *, iostat=ios) pxadj  ! often used as calibration
                                               ! parameters in nwsrfs
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in soil file'
          ! something weird here...doesn't break it but somehow enters
          ! here after last real read
      end select

    end if

  end do
  close(unit=50)

  ! quick check on completeness
  if(n_params_read /= 20) then
    print *, 'Only ',n_params_read, ' SAC params read, but need 20. Quitting...'
    stop
  end if
  !print*, '  -------------------'

  return
end subroutine read_sac_params

! ================================================
subroutine read_snow17_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: scf,mfmax,mfmin,uadj,si,pxtemp,nmf,&
                        tipm,mbase,plwhc,daygm,adc,latitude, elev
  implicit none

  !input variables
  character(len=1024),intent(in) :: param_file_name
  integer(I4B),intent(in) :: n_hrus

  !local variables
  character(len=400)            :: readline
  character(len=50)		:: param
  integer(I4B)			:: ios=0
  integer :: pos
  integer                        :: n_params_read  ! count number read

  ! open parameter file
  open(unit=51,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(scf(n_hrus))
  allocate(mfmax(n_hrus))
  allocate(mfmin(n_hrus))
  allocate(uadj(n_hrus))
  allocate(si(n_hrus))
  allocate(pxtemp(n_hrus))
  allocate(nmf(n_hrus))
  allocate(tipm(n_hrus))
  allocate(mbase(n_hrus))
  allocate(plwhc(n_hrus))
  allocate(daygm(n_hrus))
  allocate(latitude(n_hrus))
  allocate(elev(n_hrus))

  print*, 'Reading Snow17 parameters'

  ! --- now loop through parameter file and assign parameters
  n_params_read = 0
  do while(ios .eq. 0)
    read(unit=51,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = trim(readline(1:pos))
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          ! do nothing, already stored it
          n_params_read = n_params_read + 1
        case ('latitude')
          read(readline, *, iostat=ios) latitude
          n_params_read = n_params_read + 1
        case ('elev')
          read(readline, *, iostat=ios) elev
          n_params_read = n_params_read + 1
        case ('mfmax')
          read(readline, *, iostat=ios) mfmax
          n_params_read = n_params_read + 1
        case ('mfmin')
          read(readline, *, iostat=ios) mfmin
          n_params_read = n_params_read + 1
        case ('scf')
          read(readline, *, iostat=ios) scf
          n_params_read = n_params_read + 1
        case ('uadj')
          read(readline, *, iostat=ios) uadj
          n_params_read = n_params_read + 1
        case ('si')
          read(readline, *, iostat=ios) si
          n_params_read = n_params_read + 1
        case ('pxtemp')
          read(readline, *, iostat=ios) pxtemp
          n_params_read = n_params_read + 1
        case ('nmf')
          read(readline, *, iostat=ios) nmf
          n_params_read = n_params_read + 1
        case ('tipm')
          read(readline, *, iostat=ios) tipm
          n_params_read = n_params_read + 1
        case ('mbase')
          read(readline, *, iostat=ios) mbase
          n_params_read = n_params_read + 1
        case ('plwhc')
          read(readline, *, iostat=ios) plwhc
          n_params_read = n_params_read + 1
        case ('daygm')
          read(readline, *, iostat=ios) daygm
          n_params_read = n_params_read + 1
        case ('adc1')
          read(readline, *, iostat=ios) adc(1)
          n_params_read = n_params_read + 1
        case ('adc2')
          read(readline, *, iostat=ios) adc(2)
          n_params_read = n_params_read + 1
        case ('adc3')
          read(readline, *, iostat=ios) adc(3)
          n_params_read = n_params_read + 1
        case ('adc4')
          read(readline, *, iostat=ios) adc(4)
          n_params_read = n_params_read + 1
        case ('adc5')
          read(readline, *, iostat=ios) adc(5)
          n_params_read = n_params_read + 1
        case ('adc6')
          read(readline, *, iostat=ios) adc(6)
          n_params_read = n_params_read + 1
        case ('adc7')
          read(readline, *, iostat=ios) adc(7)
          n_params_read = n_params_read + 1
        case ('adc8')
          read(readline, *, iostat=ios) adc(8)
          n_params_read = n_params_read + 1
        case ('adc9')
          read(readline, *, iostat=ios) adc(9)
          n_params_read = n_params_read + 1
        case ('adc10')
          read(readline, *, iostat=ios) adc(10)
          n_params_read = n_params_read + 1
        case ('adc11')
          read(readline, *, iostat=ios) adc(11)
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in snow file'
      end select

    end if

  end do
  close(unit=51)

  ! quick check on completeness
  if(n_params_read /= 25) then
    print *, 'Only ',n_params_read, ' SNOW17 params read, but need 25. Quitting...'
    stop
  end if
  !print*, '  -------------------'

  return
end subroutine read_snow17_params

! cccccccccccc
subroutine read_uh_params(param_file_name,n_hrus)
  use nrtype
  use def_namelists, only: unit_shape,unit_scale
  implicit none

  !input variables
  integer(I4B),intent(in) :: n_hrus
  character(len=1024),intent(in) :: param_file_name

  !local variables
  character(len=400)            :: readline
  character(len=50)		:: param
  integer(I4B)			:: ios=0
  integer :: pos
  integer                        :: n_params_read  ! count number read

  ! open parameter file
  open(unit=52,file=trim(param_file_name),status='old')

  ! allocate parameter variables
  allocate(unit_shape(n_hrus))
  allocate(unit_scale(n_hrus))

  print*, 'Reading UH parameters'

  ! --- now loop through parameter file and assign parameters
  n_params_read = 0
  do while(ios .eq. 0)
    read(unit=52,FMT='(A)',IOSTAT=ios) readline

    if(ios == 0) then   ! means 'readline' was from the file
      !print*, '  ',trim(readline)

      ! Find the first instance of whitespace in line read. Split label vs data.
      pos = scan(readline, '    ')
      param = readline(1:pos)
      readline = readline(pos+1:)  ! shorten readline to include only data

      ! assign line to correct parameter array & type
      ! (following http://jblevins.org/log/control-file)
      select case (param)
        case ('hru_id')
          ! do nothing, already stored it
          n_params_read = n_params_read + 1
        case ('unit_shape')
          read(readline, *, iostat=ios) unit_shape
          n_params_read = n_params_read + 1
        case ('unit_scale')
          read(readline, *, iostat=ios) unit_scale
          n_params_read = n_params_read + 1
        case default
          print *, 'Parameter ',param,' not recognized in UH file'
      end select

    end if

  end do
  close(unit=52)

  ! quick check on completeness
  if(n_params_read /= 3) then
    print *, 'Only ',n_params_read, ' UH params read, but need 3.  Quitting...'
    stop
  end if

  return
end subroutine read_uh_params
