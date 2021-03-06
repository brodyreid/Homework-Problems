# A recursive function that returns the value of the first 5 Chebyshev polynomials

import numpy as np
import matplotlib.pyplot as plt
def cheb_poly(n,x):
    if n == 0:
        return 1
    elif n == 1:
        return x
    else:
        return 2*x*cheb_poly(n-1,x) - cheb_poly(n-2,x)
				
for i in range(1,6):
    x = np.linspace(-1,1,50)
    plt.plot(x,cheb_poly(i,x))
