#!/usr/bin/python
#
# File name:   mklcmo.py
# Date:        2016/05/01 20:53
# Author:      Lukas Vlcek
#
# Description: 
#

import numpy as np

if __name__ == "__main__":

    conc = np.array([0.1, 1, 1, 1, 1])
    conc /= np.sum(conc)

    #nx, ny, nz = (32, 32, 32)
    nx, ny, nz = (14, 14, 14)
    nat = nx*ny*nz//2

    nums = np.int_(np.round_(nat*conc, 0))
    dn = nat - np.sum(nums)
    idx = np.argmax(conc)
    nums[idx] += dn
    #print(np.sum(nums), nat)
    nums = [35, 344, 344, 321, 328]

    # create an array of atom types and shuffle it
    ti = np.array([tt+1 for tt, nt in enumerate(nums) for _ in range(nt)])
    np.random.shuffle(ti)

    #print(list(ti), len(ti), nat)

    xi, yi, zi = ([], [], [])
    for iz in range(nz):
        dz = np.mod(iz, 2)
        for iy in range(ny):
            for ix in range(nx):
                if np.mod(ix + iy, 2) == 0:
                    xi.append(ix+dz)
                    yi.append(iy)
                    zi.append(iz)

    assert ti.shape[0] == len(xi), f"Wrong number of atoms: {len(ti)} vs. {len(xi)}"

    print(nat)
    print(nx, ny, nz)
    for i in range(nat):
        print(ti[i], np.mod(xi[i], nx)+1, np.mod(yi[i],ny)+1, np.mod(zi[i],nz)+1)
