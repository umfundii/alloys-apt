!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 

module sysdef
    implicit none
    integer*8 :: ntype
    integer*8, dimension(:), allocatable :: nt
    real*8, dimension(:,:), allocatable :: ee
    
    contains

    subroutine sysdef_input(filmol)
        implicit none
        character*80 :: filmol, str
        real*8 :: w
        integer*8 :: i, j, k, npars, it

        ! read interaction parameters
        open(1, file=filmol, status='old')
            read(1,*) str, ntype  
            allocate(nt(ntype), ee(ntype,ntype))
            nt = 0 ; ee = 0.0

            read(1,*) str, npars
            do k = 1, npars
                read(1,*) i, j, w
                ee(i, j) = w
                ee(j, i) = w
            end do

        close(1, status='keep')
        !print *,'ee'
        !print *, ee

        return
    end subroutine sysdef_input

end module sysdef
