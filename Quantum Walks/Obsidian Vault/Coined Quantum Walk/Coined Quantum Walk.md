 
In this case, the walker's position position $n$ on the line is described by a vector $\ket{n}$ in a Hilbert space $H_p$ of infinite dimension and $n$ is an integer.

The evolution of the walk depends on the quantum "coin". If the result is "heads", then the next position is described by $\ket{n+1}$. Otherwise, if it "tails", the position is described by $\ket{n-1}$ .

Naturally, this coin is not a physical one, but we can, for instance, use an electron spin as our coin. If the spin is up, it acts as "heads", if it's down, as "tails".

The Hilbert space of the system will then be $H=H_c \otimes H_p$ , where the $H_c$ computation basis is $\{\ket{0}, \ket{1}\}$. We can define the "coin" as a unitary matrix $C$ with dimension 2 that acts on $H_c$ . $C$ is called the ***coin operator*** .

The shift from $\ket{n}$ to either $\ket{n + 1}$ or $\ket{n - 1}$ must be described by a unitary operator called the *shift operator* $S$. It acts as follows:
$$
\begin{aligned}
S\ket{0}\ket{n} = \ket{0}\ket{n+1}, \\
S\ket{1}\ket{n} = \ket{1}\ket{n-1},
\end{aligned}
$$
