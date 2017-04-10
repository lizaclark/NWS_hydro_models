! E. Clark Apr. 2017
! Read snow17 state in ascii and write in unformatted binary
program rewrite_states
  ! specify modules needed
  use nrtype
  use interfaces, only: read_write_sac_state, read_write_snow17_state, &
                        read_write_uh_state
  implicit none
  character(len=400) :: namelist_name = ''
  character(len = 20), dimension(:), allocatable :: hru_id   ! local hru id
  integer(I4B) :: n_hrus = 2
  integer(I4B) :: nh

  hru_id(1) = 'HHDW1IL'
  hru_id(2) = 'HHDW1U'
  call read_namelist(namelist_name)
  do nh=1,n_hrus
    call read_write_snow17_state(hru_id(nh))
    call read_write_sac_state(hru_id(nh))
    call read_write_uh_state(hru_id(nh))
  end do
end program
