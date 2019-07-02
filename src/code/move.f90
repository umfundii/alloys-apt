!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 

module move
    use sysdef
    use config
    use ener
    implicit none

    contains
        
    subroutine move_ini
        implicit none


        return
    end subroutine move_ini

    subroutine move_swap(i, j)
        implicit none
        integer*8 :: i, j

        tix = xi(j)
        tiy = yi(j)
        tiz = zi(j)
        tjx = xi(i)
        tjy = yi(i)
        tjz = zi(i)

        return
    end subroutine move_swap

    subroutine move_accept(i, j)
        implicit none
        integer*8 :: i, j, k, l

        utot = utot + dui
    
        xi(i) = tix
        yi(i) = tiy
        zi(i) = tiz
        xi(j) = tjx
        yi(j) = tjy
        zi(j) = tjz

        ss(xi(i), yi(i), zi(i)) = i
        ss(xi(j), yi(j), zi(j)) = j

        return
    end subroutine move_accept
    
end module move
