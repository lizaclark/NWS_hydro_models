! A. Wood, Aug 2016:  These are all reconfigured to work with multi-HRU model setup

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

subroutine read_write_uh_state(curr_hru_id)
  ! E. Clark 2017 from read_uh_state in main NWS code. Read ascii uh_state
  ! file and write binary uh_state files

  use nrtype
  use def_namelists, only: uh_state_in_root, uh_state_out_root
  implicit none

  !input variable
  character(len=20), intent(in) :: curr_hru_id ! HRU extension for state fname

  !local variables
  integer(I4B)         :: ios=0
  character(len = 480) :: state_infile
  character(len = 480) :: state_outfile
  character(len = 10)  :: file_state_date_str
  real(sp), allocatable, dimension(:) :: prior_tci

  ! make state input filename
  state_infile = trim(uh_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading UH state file: ', trim(state_infile)

  ! make state input filename
  state_outfile = trim(uh_state_out_root) // trim(curr_hru_id)

  open(unit=96,FILE=trim(state_outfile),FORM='unformatted',status='replace')
  print*, 'Writing UH state file: ', trim(state_outfile)
  print*, ' '

  ! format for input is an unknown number of rows with uh_length+1 columns
  !   the first column is the datestring, with no other information
  !   note, only the data values 2:uh_length will get used, since the next
  !   routing will include the first new day of the simulation
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read (95,*,IOSTAT=ios) file_state_date_str, prior_tci(:)
    write(96) file_state_date_str, prior_tci(:)

  end do
  close(unit=95)
  close(unit=96)
  return
end subroutine read_write_uh_state

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

subroutine read_write_snow17_state(curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_in_root, snow_state_out_root
  implicit none

  ! input variables
  character(len = 20), intent(in)    :: curr_hru_id ! HRU extension for snow
                                                    ! state fname

  !local variables
  integer(I4B)                         :: ios=0
  character(len = 480)                 :: state_infile, state_outfile
  character(len = 10)                  :: file_state_date_str
  real(sp)                             :: tprev ! carry over variable
  real(sp), allocatable, dimension(:)  :: cs ! carry over array

  ! make state filename
  state_infile = trim(snow_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading snow state file: ', trim(state_infile)

  ! make state input filename
  state_outfile = trim(snow_state_out_root) // trim(curr_hru_id)

  open(unit=96,FILE=trim(state_outfile),FORM='unformatted',status='replace')
  print*, 'Writing snow state file: ', trim(state_outfile)

  ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
  !   the first column is the datestring
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,*,IOSTAT=ios) file_state_date_str, tprev, cs(:)
    write(96) file_state_date_str, tprev, cs(:)

  end do
  close(unit=95)
  close(unit=96)
  return

end subroutine read_write_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine read_write_sac_state(curr_hru_id)
  use nrtype
  use def_namelists, only: sac_state_in_root, sac_state_out_root
  implicit none

  ! input variables
  character(len=20), intent(in) :: curr_hru_id ! HRU extension for sac state fname

  ! local variables
  integer(I4B)         :: ios=0
  character(len = 480) :: state_infile
  character(len = 480) :: state_outfile
  character(len = 10)  :: file_state_date_str
  real(sp)             :: uztwc !state variable
  real(sp)             :: uzfwc !state array
  real(sp)             :: lztwc !state array
  real(sp)             :: lzfsc !state array
  real(sp)             :: lzfpc !state array
  real(sp)             :: adimc

  ! make state filename
  state_infile = trim(sac_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='formatted',status='old')
  print*, 'Reading sac state file: ', trim(state_infile)

  ! make state input filename
  state_outfile = trim(sac_state_out_root) // trim(curr_hru_id)
  open(unit=96,FILE=trim(state_outfile),FORM='unformatted',status='replace')
  print*, 'Writing sac state file: ', trim(state_outfile)

  ! format for input is an unknown number of rows with 6 data columns
  !   the first column is the datestring
  do while(ios .ge. 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,*,IOSTAT=ios) file_state_date_str, uztwc, uzfwc, lztwc, lzfsc, &
                          lzfpc, adimc
    write(96) file_state_date_str, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc

  end do
  close(unit=95)
  close(unit=96)
  return

end subroutine read_write_sac_state

! ccccccccccccccccccccccccccccccc

!subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
! AWW modified to read PET instead of dayl, vpd and swdown
! AWW modified to return basin area in sq km
subroutine read_write_areal_forcing(curr_hru_id)
  use nrtype
  use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  implicit none

  ! input variables
  character(len = 20), intent(in) :: curr_hru_id ! HRU extension for sac
                                                    ! state fname

  ! local variables
  integer(I4B)              :: ios=0
  integer(I4B)              :: yr,mnth,dy,hr
  character(len = 1024)     :: dum_str
  real(DP)                  :: pcp,tma,tmn,pm_pet

  character(len = 420) :: filename, outfilename

  ! make filename to read
  filename = trim(forcing_root) // trim(curr_hru_id)
  outfilename = trim(forcing_root) // trim(curr_hru_id) // trim('.bin')
  ! =========  code below  =============

  ! read met file
  open (UNIT=50,file=trim(filename),form='formatted',status='old')
  open (unit=51,file=trim(outfilename),form='unformatted',status='replace')
  ! skip header info
  read (UNIT=50,FMT='(80A)') dum_str   ! column labels
  write(51) dum_str
  ! read the data, keeping only forcings in simulation period
  do while(ios .ge. 0)
    ! forcing could have any format, nice!
    read (UNIT=50,FMT=*,IOSTAT=ios) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet
    write(51) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet
  end do

  close(unit=50)
  close(51)
  return
end subroutine read_write_areal_forcing
