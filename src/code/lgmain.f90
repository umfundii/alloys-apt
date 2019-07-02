!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 


program lg
    use rndmod
    use sysdef
    use config
    use ener
    use move
    use measure
    
    implicit none

    character*80 :: str, filmol, filcfg, fnam
    integer*8 :: i, ix, iy, iz, it, itmax, ierr, try, acc
    integer*8 :: ic, idx, idy, idz, j, kk, tt
    real*8 :: dummy, b, Tr

    ! initialize random number generator 
    seed = 113314724325.0

    ! read simulation parameters
    read(*,*) str, Tr     ! absolute temperature
    read(*,*) str, itmax  ! reduced temperature
    read(*,*) str, filmol
    read(*,*) str, filcfg
    b = 1.0/(8.314472*Tr)*1000 ! beta set up for energies in kJ/mol

    call sysdef_input(filmol)
    call config_input(filcfg)
    !call ener_ini
    !call move_ini

    call ener_tot
    print *, 0, utot, 0, 0, 0

    call measure_ini
    call measure_do(0_8)
    call config_output('lg.xyz')

    do it = 1, itmax
        acc = 0
        try = 0
        do ic = 1, n
            try = try + 1

            i = int(rnd(dummy)*float(n)) + 1
            j = int(rnd(dummy)*float(n)) + 1
            if (ti(i) == ti(j)) then
                acc = acc + 1
            else
                call move_swap(i, j)
                call ener_dif(i, j)
                if (exp(-b*dui) > rnd(dummy)) then
                    !print *, 'acc', dui, utot
                    call move_accept(i, j)
                    acc = acc + 1
                end if
            end if

        end do
        if (modulo(it,100) == 0) then
            print *, it, utot, Tr, float(acc)/float(try), try, acc
            call measure_do(it)
            call config_output('lg.xyz')
            !if (modulo(it,1000) == 0) then
            !    write(fnam, '(i6)') it/1000
            !    fnam = adjustl(fnam)
            !    ierr = len_trim(fnam)
            !    fnam(ierr+1:ierr+4) = '.xyz'
            !    call config_output(fnam)
            !end if
        end if

    end do

    call measure_output

    stop
end program lg
