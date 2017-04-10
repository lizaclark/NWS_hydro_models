! A. Wood, Aug 2016:  These are all reconfigured to work with multi-HRU model setup

subroutine write_snow17_state(year,month,day,hour,cs,tprev,sim_length,curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp),dimension(:,:),intent(in)	:: cs	    ! carry over array
  real(sp),dimension(:),intent(in)	:: tprev    ! carry over variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(snow_state_out_root) // trim(curr_hru_id)

  open(unit=95,FILE=trim(state_outfile),FORM='formatted',status='replace')
  print*, 'Writing snow state file: ', trim(state_outfile)

  do i = 1,sim_length
    ! print*, 'tprev = ',tprev(i)  AWW debugging

    write(95) year(i),month(i),day(i),hour(i),tprev(i), cs(:,i)
  enddo

  close(unit=95)

  return
end subroutine write_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine write_sac_state(year,month,day,hour,uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,sim_length,curr_hru_id)
  use nrtype
  use def_namelists, only: sac_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(dp),dimension(:),intent(in)	:: uztwc	!state variable
  real(dp),dimension(:),intent(in)	:: uzfwc	!state variable
  real(dp),dimension(:),intent(in)	:: lztwc        !state variable
  real(dp),dimension(:),intent(in)	:: lzfsc	!state variable
  real(dp),dimension(:),intent(in)	:: lzfpc	!state variable
  real(dp),dimension(:),intent(in)	:: adimc	!state variable
  integer(I4B),intent(in)               :: sim_length   ! length of simulation

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(sac_state_out_root) // trim(curr_hru_id)

  open(unit=95,FILE=trim(state_outfile),FORM='unformatted',status='replace')
  print*, 'Writing sac state file: ', trim(state_outfile)

  do i = 1,sim_length
    write(95) year(i),month(i),day(i),hour(i),uztwc(i),uzfwc(i),&
                       lztwc(i),lzfsc(i),lzfpc(i),adimc(i)
  enddo

  close(unit=95)

  return
end subroutine write_sac_state

! cccccccccccccccccccccccccccccccccccccccccc

subroutine write_uh_state(year,month,day,hour,expanded_tci,sim_length,uh_length,curr_hru_id)
  ! A.Wood, 2016 -- this routine writes out TCI (total channel input) for the
  !   UH_LENGTH period preceding the first timestep of the simulation
  !   When it is read in, it is concatenated with the simulation TCI to initialize
  !   the routing model
  use nrtype
  use def_namelists, only: uh_state_out_root
  implicit none

  !input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for state fname
  integer(I4B),dimension(:),intent(in)	:: year
  integer(I4B),dimension(:),intent(in)	:: month
  integer(I4B),dimension(:),intent(in)	:: day
  integer(I4B),dimension(:),intent(in)	:: hour
  real(sp), dimension(:), intent(in) 	:: expanded_tci
  integer(I4B), intent(in)		:: sim_length
  integer(I4B), intent(in)		:: uh_length

  !local variables
  integer(I4B)	:: i
  character(len = 480) :: state_outfile

  ! make state input filename
  state_outfile = trim(uh_state_out_root) // trim(curr_hru_id)

  open(unit=95,FILE=trim(state_outfile),FORM='unformatted',status='replace')
  print*, 'Writing UH state file: ', trim(state_outfile)
  print*, ' '

  ! for each tstep, write out (uh_length-1) values before the current tstep, + the current tstep
  ! reasoning: all state files are written for the END of timestep, so by grabbing a timestep's
  ! state, one is initializing a simulation starting the following timestep (eg, day)

  do i = 1,sim_length
    write(95) year(i),month(i),day(i),hour(i),expanded_tci(i:i+uh_length-1)
  enddo

  close(unit=95)

  return
end subroutine write_uh_state

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

subroutine read_uh_state(state_date_str, prior_tci, curr_hru_id)
  ! A.Wood, 2016 -- just read prior (uh_length) values ending at timestep-1 for tci
  !   includes ability to read a keyword in state file to identify desired state
  !   rather than match the date

  use nrtype
  use def_namelists, only: uh_state_in_root
  implicit none

  !input variable
  character(len=10), intent(in) :: state_date_str  ! AWW string to match date in input states
  character(len=20), intent(in) :: curr_hru_id	! HRU extension for state fname

  !output variables
  real(sp), dimension(:), intent(out) 	:: prior_tci

  !local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state input filename
  state_infile = trim(uh_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='unformatted',status='old')
  print*, 'Reading UH state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with uh_length+1 columns
  !   the first column is the datestring, with no other information
  !   note, only the data values 2:uh_length will get used, since the next
  !   routing will include the first new day of the simulation
  do while(ios == 0)

    ! read each row and check to see if the date matches the initial state date
    read (95,IOSTAT=ios) file_state_date_str, prior_tci(:)

    ! checks either for real date or special word identifying the state to use
    !   this functionality facilitates ESP forecast initialization
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial UH state on ', state_date_str
      print *, ' '
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  UH init state not found in UH initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_uh_state

! CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

subroutine read_snow17_state(state_date_str, cs,tprev,curr_hru_id)
  use nrtype
  use def_namelists, only: snow_state_in_root
  implicit none

  ! input variables
  character(len=10),intent(in)	:: state_date_str  ! AWW string to match date in input states
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for snow state fname

  ! output variables
  real(sp), intent(out) 		:: tprev	! carry over variable
  real(sp), dimension(:), intent(out)	:: cs		! carry over array

  !local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state filename
  state_infile = trim(snow_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='unformatted',status='old')
  print*, 'Reading snow state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with 20 data columns (1 tprev, 19 for cs)
  !   the first column is the datestring
  do while(ios == 0)

    ! read each row and check to see if the date matches the initial state date
    read(95, IOSTAT=ios) file_state_date_str, tprev, cs(:)

    ! checks either for real date or special word identifying the state to use
    !   this functionality facilitates ESP forecast initialization
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial snow state on ', state_date_str
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  snow init state not found in snow initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_snow17_state

! ccccccccccccccccccccccccccccccc

subroutine read_sac_state(state_date_str, uztwc,uzfwc,lztwc,lzfsc,lzfpc,adimc,curr_hru_id)
  use nrtype
  use def_namelists, only: sac_state_in_root
  implicit none

  ! input variables
  character(len=10), intent(in)	:: state_date_str  ! AWW string to match date in input states
  character(len=20), intent(in) :: curr_hru_id	! HRU extension for sac state fname
  real(sp), intent(out)	:: uztwc			!state variable
  real(sp), intent(out)	:: uzfwc			!state array
  real(sp), intent(out)	:: lztwc			!state array
  real(sp), intent(out)	:: lzfsc			!state array
  real(sp), intent(out)	:: lzfpc			!state array
  real(sp), intent(out)	:: adimc

  ! local variables
  integer(I4B)	       :: ios=0
  character(len = 480) :: state_infile
  character(len = 10)  :: file_state_date_str

  ! make state filename
  state_infile = trim(sac_state_in_root) // trim(curr_hru_id)
  open(unit=95,FILE=trim(state_infile),FORM='unformatted',status='old')
  print*, 'Reading sac state file: ', trim(state_infile)

  ! format for input is an unknown number of rows with 6 data columns
  !   the first column is the datestring
  do while(ios == 0)

    ! read each row and check to see if the date matches the initial state date
    read(95,IOSTAT=ios) file_state_date_str, uztwc, uzfwc, lztwc, lzfsc, lzfpc, adimc

    ! checks either for real date or special word identifying the state to use
    !   this functionality facilitates ESP forecast initialization
    if(file_state_date_str==state_date_str .or. file_state_date_str=='PseudoDate') then
      print *, '  -- found initial sac model state on ', state_date_str
      close(unit=95)
      return
    end if

  end do
  close(unit=95)

  ! if you reach here without returning, quit -- the initial state date was not found
  print*, 'ERROR:  sac init state not found in sac initial state file.  Looking for: ',state_date_str
  print*, '  -- last state read was: ', file_state_date_str
  print*, 'Stopping.  Check inputs!'
  stop

end subroutine read_sac_state

! ccccccccccccccccccccccccccccccc

!subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,vpd,dayl,swdown,precip)
! AWW modified to read PET instead of dayl, vpd and swdown
! AWW modified to return basin area in sq km
subroutine read_areal_forcing(year,month,day,hour,tmin,tmax,precip,pet,curr_hru_id)
  use nrtype
  use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day

  implicit none

  ! input variables
  character(len = 20), intent(in) 	:: curr_hru_id	! HRU extension for sac state fname

  ! output variables
  integer(I4B),dimension(:),intent(out)	:: year
  integer(I4B),dimension(:),intent(out)	:: month
  integer(I4B),dimension(:),intent(out)	:: day
  integer(I4B),dimension(:),intent(out)	:: hour
  real(dp),dimension(:),intent(out)	:: tmax   ! deg C
  real(dp),dimension(:),intent(out)	:: tmin    ! deg C
  real(dp),dimension(:),intent(out)	:: precip  ! mm/day
  real(dp),dimension(:),intent(out)	:: pet   ! mm/day


  ! local variables
  integer(I4B)				:: i,ios=0
  integer(I4B)				:: yr,mnth,dy,hr
  integer(I4B)				:: read_flag
  character(len = 1024)			:: dum_str
  real(DP)				:: pcp,tma,tmn,pm_pet

  character(len = 420) :: filename

  ! make filename to read
  filename = trim(forcing_root) // trim(curr_hru_id)

  ! =========  code below  =============
  i = 1
  read_flag = 0

  ! read met file
  open (UNIT=50,file=trim(filename),form='unformatted',status='old')

  ! skip header info
  read (UNIT=50) dum_str   ! column labels

  ! read the data, keeping only forcings in simulation period
  do while(ios == 0 .and. read_flag < 2)
    ! forcing could have any format, nice!
    read (UNIT=50,IOSTAT=ios) yr,mnth,dy,hr,pcp,tma,tmn,pm_pet

    if(yr .eq. start_year .and. mnth .eq. start_month .and. dy .eq. start_day) then
      read_flag = 1
    end if

    ! read and store data for simulation period
    if(read_flag .eq. 1) then
      year(i)	= yr
      month(i)	= mnth
      day(i)	= dy
      hour(i)	= hr
      precip(i)	= pcp
      tmax(i)	= tma
      tmin(i)	= tmn
      pet(i)	= pm_pet
      i = i + 1
    end if

    if(yr .eq. end_year .and. mnth .eq. end_month .and. dy .eq. end_day) then
      read_flag = 2
    end if

  end do

  close(unit=50)
  return
end subroutine read_areal_forcing
