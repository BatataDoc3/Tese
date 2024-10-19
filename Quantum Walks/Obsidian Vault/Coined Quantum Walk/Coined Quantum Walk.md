 
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


One way to apply Quantum Walks efficiently is to use recursion. The state of the quantum walk in the computational basis is:

$$
\ket{\psi(t)} = \sum_{n= -\infty}^{\infty} (A_n(t)\ket0+B(t)\ket1)\ket{n}
$$
Where the amplitudes satisfy the constraints
$$
 \sum_{n= -\infty}^{\infty} |A_n(t)|^2 + |B_n(t)|^2 = 1
$$
When we apply the $H \otimes I$ followed by the shift operator, we get the following recursive formulas:
$$
\begin{aligned}
	A_n(t+1) = \frac{A_{n-1}(t)+B_{n-1}(t)}{\sqrt2} \\
	B_n(t+1) = \frac{A_{n+1}(t)-B_{n+1}(t)}{\sqrt2}
\end{aligned}

$$
And using the initial conditions
$$
\begin{equation}
A_n(0) =
\begin{cases}
      1, \text\ if\ n=0\\
      0,\ otherwise\\
\end{cases} 
\end{equation}
$$
And $B(0) = 0$ for all $n$, we can determine the probability for any $t$. The probability is then naturally given by:
$$
p(t,n) = |A_n(t)|^2 + |B_n(t)|^2
$$
This is a really good strategy to implement in functional programming, such as Quipper.

![[Pasted image 20241014232813.png]]
This graph represents the probability of finding the walker in various positions (positions where the probability is 0 are not present), after 100 iterations.
We see that the probability distribution is asymmetrical. The probability of finding the particle in the right hand size is much bigger than finding it on the left side. The peak is in around $\frac{100}{\sqrt2}$. This suggests that the quantum walk has a *ballistic* behavior, which means that the particle can be found away from the origin as if it is in a uniform rightward motion.

The reason for that is because the Hadamard gate introduces a negative sign when applied to the state $\ket1$. This means that there are more cancellations of terms when the coin state is $\ket1$. Since the coin state $\ket0$ induces a motion to the right and $\ket1$ to the left, the final effect is the asymmetry we see, where the probability tends to the right.

If we apply the following initial condition:
$$
\ket{\psi(0)} = -\ket1\ket{n=0}
$$
Then, we would have a mirrored version of the graph seen earlier.

Furthermore, if we apply the following initial condition:
$$
\ket{\psi(0)} = \frac{\ket0 - i\ket1}{\sqrt2}\ket{n=0}
$$
We would obtain a symmetrical (although not centered on the origin)  graph:![[Pasted image 20241014234159.png]]
The entries of the Hadamard coin are real numbers. When we apply the evolution operator, terms with the imaginary unit are not converted into terms without the imaginary unit and vice versa. There are no cancellations of terms of the walk that goes to the right with the terms of the walk that goes to the left.

Now we need to figure out the standard deviation. The formula is given by:
$$
		\sigma(t) = \sqrt{\sum_{n=-\infty}^{\infty}n^2 \ p(t,n)}
$$