!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 

module ener
    use sysdef
    use config

    real*8 :: dui, utot

    contains

subroutine ener_ini
    implicit none

    return
end subroutine ener_ini


subroutine ener_tot
    implicit none
    integer*8 :: i, kk, ixx, iyy, izz, tj

    utot = 0.0
    do i = 1, n
        do kk = 1, 12
            ixx = modulo(xi(i) + nbx(kk) - 1, nx) + 1
            iyy = modulo(yi(i) + nby(kk) - 1, ny) + 1
            izz = modulo(zi(i) + nbz(kk) - 1, nz) + 1
            tj = ti(ss(ixx,iyy,izz))
            utot = utot + ee(ti(i), tj)
        end do
    end do
    utot = 0.5*utot

    return
end subroutine ener_tot


subroutine ener_dif(i, j)
    implicit none
    integer*8 :: i, j, kk, ixx, iyy, izz, k

    ! trial for new i position
    dui = 0.0
    do kk = 1, 12
        ixx = modulo(tix + nbx(kk) - 1, nx) + 1
        iyy = modulo(tiy + nby(kk) - 1, ny) + 1
        izz = modulo(tiz + nbz(kk) - 1, nz) + 1
        k = ss(ixx,iyy,izz)
        if (k /= i) then
            dui = dui + ee(ti(i), ti(k)) - ee(ti(j), ti(k))
        end if
    end do

    ! trial for new j position
    do kk = 1, 12
        ixx = modulo(tjx + nbx(kk) - 1, nx) + 1
        iyy = modulo(tjy + nby(kk) - 1, ny) + 1
        izz = modulo(tjz + nbz(kk) - 1, nz) + 1
        k = ss(ixx,iyy,izz)
        if (k /= j) then
            dui = dui + ee(ti(j), ti(k)) - ee(ti(i), ti(k))
        end if
    end do

    return
end subroutine ener_dif

end module ener
