!
!  File name: lg.f90
!  Date:      2011/05/15 23:49
!  Author:    Lukas Vlcek
! 

module measure
    use sysdef
    use config
    use ener

    integer*8 :: icount
    real*8, dimension(:), allocatable :: hcrl, hcr, hcnt
    integer*8, dimension(:,:), allocatable :: hu
    real*8, dimension(:,:,:,:), allocatable :: hhh
    integer*8, dimension(:,:,:,:), allocatable :: hhi

    contains

subroutine measure_ini
    implicit none
    integer*8 :: ix, iy, iz, tt

    allocate(hu(1:ntype,1:ntype))
    hu = 0
    allocate(hhh(nz,2,6,6), hhi(nz,2,6,6))
    hhh = 0.0
    allocate(hcrl(10), hcr(10), hcnt(10))
    hcrl = 0.0 ; hcr = 0.0 ; hcnt = 0.0

    icount = 0

    open(3, file='lg.hst', status='unknown')
    write(3,*) '# ntype', ntype
    close(3, status='keep')

    return
end subroutine measure_ini


subroutine measure_do(it)
    implicit none
    integer*8 :: it, ix, iy, iz, ixx, iyy, izz, tt, tj, kk, i
    integer*8 :: ie, je, ije, ic, ia, ja, ija, ii, jj, j


    ! surface statistics
!    hhi = 0
!    do i = 1, n
!        ix = xi(i)
!        iy = yi(i)
!        iz = zi(i)
!        !if (mod(iz, 2) == 1) cycle
!        !if (iz /= nz) cycle
!    
!        ic = 0
!        if (ti(ss(ix,iy,iz)) == 2) ic = ic + 1
!    
!        ie = 0
!        if (ti(ss(modulo(ix+1-1,nx)+1,modulo(iy+1-1,ny)+1,iz)) == 2) ie = ie + 1
!        if (ti(ss(modulo(ix-1-1,nx)+1,modulo(iy-1-1,ny)+1,iz)) == 2) ie = ie + 1
!        je = 0
!        if (ti(ss(modulo(ix+1-1,nx)+1,modulo(iy-1-1,ny)+1,iz)) == 2) je = je + 1
!        if (ti(ss(modulo(ix-1-1,nx)+1,modulo(iy+1-1,ny)+1,iz)) == 2) je = je + 1
!        ije = ie + je
!        if (ije == 2 .and. ie /= 1) ije = 5
!        !print *, 'hu', ix, modulo(ix+1-1,nx)+1,modulo(ix-1-1,nx)+1
!    
!        ia = 0
!        if (ti(ss(modulo(ix+2-1,nx)+1,iy,iz)) == 2) ia = ia + 1
!        if (ti(ss(modulo(ix-2-1,nx)+1,iy,iz)) == 2) ia = ia + 1
!        ja = 0
!        if (ti(ss(ix,modulo(iy+2-1,ny)+1,iz)) == 2) ja = ja + 1
!        if (ti(ss(ix,modulo(iy-2-1,ny)+1,iz)) == 2) ja = ja + 1
!        ija = ia + ja
!        !if (ija == 2 .and. ia /= 1) ija = 5
!    
!        hhi(iz, ic+1, ije+1, ija+1) = hhi(iz, ic+1, ije+1, ija+1) + 1
!    end do

!    ! pair correlation function
!    hcrl = 0
!    hcnt = 0
!    do i = 1, n
!        if (ti(i) /= 2) cycle
!        ix = xi(i)
!        iy = yi(i)
!        iz = zi(i)
!        do j = 1, 10
!            if (ti(ss(modulo(ix+j-1,nx)+1,modulo(iy+j-1,ny)+1,iz)) == 2) then
!                hcrl(j) = hcrl(j) + 1.0
!            end if
!            if (ti(ss(modulo(ix+j-1,nx)+1,modulo(iy-j-1,ny)+1,iz)) == 2) then
!                hcrl(j) = hcrl(j) + 1.0
!            end if
!            if (ti(ss(modulo(ix-j-1,nx)+1,modulo(iy+j-1,ny)+1,iz)) == 2) then
!                hcrl(j) = hcrl(j) + 1.0
!            end if
!            if (ti(ss(modulo(ix-j-1,nx)+1,modulo(iy-j-1,ny)+1,iz)) == 2) then
!                hcrl(j) = hcrl(j) + 1.0
!            end if
!            hcnt(j) = hcnt(j) + 4.0
!        end do
!    end do

    ! pair interaction statistics
    hu = 0
    do i = 1, n
        do kk = 1, 12
            ixx = modulo(xi(i) + nbx(kk) - 1, nx) + 1
            iyy = modulo(yi(i) + nby(kk) - 1, ny) + 1
            izz = modulo(zi(i) + nbz(kk) - 1, nz) + 1
            tj = ti(ss(ixx,iyy,izz))
            if (tj > ti(i)) then
                hu(ti(i), tj) = hu(ti(i), tj) + 1
            else
                hu(tj, ti(i)) = hu(tj, ti(i)) + 1
            end if
        end do
    end do
    ! only unique pairs
    hu = hu/2

    open(3, file='lg.hst', access='append', status='old')

    write(3,*) '#', it
    write(3,*) '# hu', utot, sum(ee(:, :)*hu(:,:))
    do tt = 1, ntype
        do tj = tt, ntype
            write(3,*) tt, tj, hu(tt, tj)
        end do
    end do
!    write(3,*) '# hcrl'
!    hcrl = hcrl/hcnt
!    do j = 1, 10
!        write(3,*) hcrl(j)
!    end do
!    write(3,*) '# hsu'
!    do ic = 1, 2
!        do ije = 1, 6
!            write(3,*) (sum(hhi(iz,ic,ije,1:5)), iz = 1,  nz)
!        end do
!    end do

    close(3, status='keep')

    icount = icount + 1
!    hhh = hhh + real(hhi)
!    hcr = hcr + real(hcrl)

    return
end subroutine measure_do

subroutine measure_output
    implicit none
    integer*8 :: ix, iy, iz, tt, tj, ic, ije, ia, j

    do iz = 1, nz
        hhh(iz,:,:,:) = hhh(iz,:,:,:)/sum(hhh(iz,:,:,:))
    end do
    hcr = hcr/float(icount)

    open(3, file='lg.hst', access='append', status='old')

!    write(3,*) '# Average'
!    write(3,*) '# hcrl'
!    do j = 1, 10
!        write(3,*) hcr(j)
!    end do
!    write(3,*) '# hsu'
!    do ic = 1, 2
!        do ije = 1, 6
!            write(3,*) (sum(hhh(iz,ic,ije,1:5)), iz = 1,  nz)
!            !do ia = 1, 5
!            !    write(3,*) (hhh(iz, ic, ije, ia), iz = 1, nz)
!            !end do
!        end do
!    end do

    close(3, status='keep')

    return
end subroutine measure_output

end module measure
