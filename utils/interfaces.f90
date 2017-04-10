! AWW:  this code file just contains interfaces for functions & subs
! AWW:  used to be called 'gauge_calib'

module interfaces

  interface

    subroutine read_namelist(namelist_name)
      use nrtype
      !input variable
      character(len=2000),intent(in) :: namelist_name
    end subroutine read_namelist

    subroutine read_write_snow17_state(curr_hru_id)
      use nrtype
      use def_namelists, only: snow_state_in_root, snow_state_out_root
      ! input variables
      character(len = 20), intent(in) :: curr_hru_id ! HRU extension for
                                                     ! snow state filename
    end subroutine read_write_snow17_state

    subroutine read_write_sac_state(curr_hru_id)
      use nrtype
      use def_namelists, only: sac_state_in_root, sac_state_out_root
      !input variables
      character(len = 20), intent(in) :: curr_hru_id ! HRU extension for
                                                      ! state fname
    end subroutine read_write_sac_state

    subroutine read_write_uh_state(curr_hru_id)
      use nrtype
      use def_namelists, only: uh_state_in_root, uh_state_out_root
      implicit none
      !input variables
      character(len = 20), intent(in) :: curr_hru_id ! HRU extension for
                                                     ! state fname
    end subroutine read_write_uh_state

    subroutine read_write_areal_forcing(curr_hru_id)
      use nrtype
      use def_namelists, only: forcing_root, start_year,start_day,start_month, &
                        end_year,end_month,end_day
      !output variables
      character(len = 20), intent(in) :: curr_hru_id ! HRU extension for
                                                    ! state fname
    end subroutine read_write_areal_forcing

  end interface
end module interfaces
