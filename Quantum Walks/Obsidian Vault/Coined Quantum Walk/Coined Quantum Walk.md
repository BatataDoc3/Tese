 
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
If we consider the particle initially located at the origin $\ket{n=0}$ and the coin state with spin up $\ket{0}$, that is:
$$
\ket{\psi(0)} = \ket{0}\ket{n=0}
$$
where $\ket{\psi(0)}$ denotes the state of  the quantum walk at $t=0$ and $\ket{\psi(t)}$ denotes the state at time $t$.

In the quantum context, the most used coin is the *Hadamard* operator

$$
H = \frac{1}{\sqrt{2}}
\begin{bmatrix}
1 & 1 \\
1 & -1
\end{bmatrix}
$$
One step consists in applying $H$ to the coin followed by the shift operator $S$.

After the first step, the position of the particle will be in a superposition of $n=1$ and $n=-1$. Note that the *Hadamard* operator is unbiased, even though it returns a slightly different value when applied to $\ket{0}$ or $\ket{1}$ (the difference is the minus sign but this plays no role in deciding whether the walker goes right or left).

After the first step (Hadamard + Shift operators), if we measure the result we will have a 50% chance of the walker being in either $n=1$ or $n=-1$. If we keep doing this, we will get results similar to the classical random walk, which we don't want. As such, we shouldn't measure the particle at any part of this experiment. 

If we keep applying the Hadamard and Shift operators without measurements, the quantum correlations between different positions will generate constructive and destructive interference, amplifying certain values and diminishing others. In the quantum case, the probability distribution  is not the normal distribution and the standard deviation is not $\sqrt{t}$.

We can define this "step" as a Unitary operator $U$ such as:

$$
U = S(H \otimes I)
$$
After $t$ steps, the state of the quantum walk is given by:
$$
\ket{\psi(t)} = U^t\ket{\psi(0)}
$$
For example, for $t=1$, we have:

$$
\begin{equation}
\begin{split}
\ket{\psi(1)} &= U\ket{\psi(0)} \\ 
&= S (H \otimes I)(\ket 0 \ket{0}) \\
&= S (H\ket{0}I\ket 0)\\
&= S (\frac{\ket 0 \otimes \ket 0 +\ket 1\otimes  \ket 0}{\sqrt 2})\\
&= \frac{1}{\sqrt 2}(\ket 0 \ket 1 + \ket 1 \ket{-1})
\end{split}
\end{equation}
$$
Applying this formula continuously, we will end up having the following results for $t=2$ and $t=3$:

$$
\begin{aligned}
&\ket{\psi(2)} = \frac 1 2(-\ket1 \ket{-2} + (\ket0 + \ket1)\ket0 + \ket0\ket2),\\
&\ket{\psi(3)} =\frac{1}{2\sqrt 2}(\ket1 \ket{-3} - \ket0\ket{-1}+(2\ket0+\ket1)\ket1 + \ket0\ket3)
\end{aligned}
$$

We can see that this quantum coin behaves differently from the classical counterpart. For instance, $\ket3$ is not symmetric with respect to the origin. Not only that, the probabilities are also not concentrated around the origin. We can see in the following table the probabilities after 5 steps:
![[Pasted image 20241013185338.png]]


