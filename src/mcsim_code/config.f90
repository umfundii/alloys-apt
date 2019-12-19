!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 

module config
    use sysdef
    implicit none

    integer*8 :: tix, tiy, tiz, tjx, tjy, tjz
    integer*8 :: n, nx, ny, nz
    integer*8, dimension(:), allocatable :: ti, xi, yi, zi
    integer*8, dimension(:,:,:), allocatable :: ss
    integer*8, dimension(:), allocatable :: nbx, nby, nbz

    contains

    subroutine config_input(filcfg)
        implicit none
        character*80 :: filcfg
        integer*8 :: i, ii, jj, j, k

        ! read configuration
        open(1, file=filcfg, status='old')
            read(1,*) n
            allocate(ti(n), xi(n), yi(n), zi(n))
            ti = 0 ; xi = 0 ; yi = 0 ; zi = 0
 
            read(1,*) nx, ny, nz 
            allocate(ss(nx, ny, nz))
            ss = 0
 
            do i = 1, n
                read(1,*) ti(i), xi(i), yi(i), zi(i)
                ss(xi(i), yi(i), zi(i)) = i
                nt(ti(i)) = nt(ti(i)) + 1
            end do
        close(1, status='keep')

        allocate(nbx(12), nby(12), nbz(12))
        nbx = 0 ; nby = 0 ; nbz = 0

        nbz(1:4) = -1
        nbx(1) = 1
        nbx(2) = -1
        nby(3) = 1
        nby(4) = -1

        nbx(5) = 1
        nby(5) = 1
        nbx(6) = 1
        nby(6) = -1
        nbx(7) = -1 
        nby(7) = 1 
        nbx(8) = -1 
        nby(8) = -1 

        nbz(9:12) = 1
        nbx(9)  = 1
        nbx(10) = -1
        nby(11) = 1
        nby(12) = -1

        return
    end subroutine config_input

    subroutine config_output(filcfg)
        implicit none
        character*80 :: filcfg
        integer*8 :: i

        ! write configuration
        open(1, file=filcfg, status='unknown', position='append')
            write(1,'(I8)') n
            write(1,'(I3X1I3X1I3)') nx, ny, nz 
            do i = 1, n
                write(1,'(I2X1I3X1I3X1I3)') ti(i), xi(i), yi(i), zi(i)
            end do
        close(1, status='keep')

        return
    end subroutine config_output

end module config
