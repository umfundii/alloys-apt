import re
import numpy as np
from scipy.stats import binom


# functions to read data files with pairwise statistics

def read_pairwise(file_name, nn, ntype):
    """
    Parameters
    ----------
    file_name: str
        Input file with pairwise statistics
    nn: int
        number of nearest neighbors
    
    Returns
    -------
    stats: ndarray(5,5,nn)
        all pairwise statistics for 5 atom types
    """
    
    # output array
    stats = np.zeros((ntype, ntype, nn+1), dtype=int)
    
    # regex to extract particle types
    atom_info = re.compile(r'IonType\s*(\d+)\s*,\s*type\s*(\d+)\s+')
    
    with open(file_name, 'r') as fin:
        for line in iter(fin.readline, ''):
            m = atom_info.search(line)
            i, j = [int(c) for c in m.groups()]

            counts = re.findall('\d+', fin.readline())
            #print(i, j, len(counts))
            stats[i, j, :] = [int(c) for c in counts]

    return stats


def get_binomials(k, probs):

    ntype = len(probs)

    probs_null = np.zeros((ntype, ntype, k+1), dtype=float)

    # marginal distributions for selected atoms (equivalent to binomial distributions)
    for it in range(ntype):
        for jt in range(ntype):
            bn = binom(n=k, p=probs[jt])
            pmf_bn = probs[it]*bn.pmf(range(k+1))
            probs_null[it, jt, :] = pmf_bn
            #print(k, it, jt, pmf_bn)

    return probs_null


