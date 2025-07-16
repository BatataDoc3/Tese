# Shor's Algorithm

It's pretty well explained in the notebook. It's important to mention that:

We will be doing Shor's algorithm to find the prime factors of *15* and using $a=7$
This is based on the implementation in https://github.com/Qiskit/textbook/blob/main/notebooks/ch-algorithms/shor.ipynb

## Quiskit
```py
def c_amod15(a, power):
    # gdc(15, a) must be one
    if a not in [2, 4, 7, 8, 11, 13]:
        raise ValueError("not valid")
    U = QuantumCircuit(4)
    for _iteration in range(power):
        if a in [2,12]:
            U.swap(2,3)
            U.swap(1,2)
            U.swap(0,1)
        if a in [7,8]:
            U.swap(0,1)
            U.swap(1,2)
            U.swap(2,3)
        if a in [4,11]:
            U.swap(1,3)
            U.swap(0,2)
        if a in [7,11,13]:
            for q in range(4):
                U.x(q)
    U = U.to_gate()
    U.name = f"{a}^{power} mod"
    c_U = U.control()
    return c_U

    def qft_dagger(n):
    """n-qubit QFTdagger the first n qubits in circ"""
    qc = QuantumCircuit(n)
    # Don't forget the Swaps!
    for qubit in range(n//2):
        qc.swap(qubit, n-qubit-1)
    for j in range(n):
        for m in range(j):
            qc.cp(-np.pi/float(2**(j-m)), m, j)
        qc.h(j)
    qc.name = "QFT†"
    return qc


circuit = QuantumCircuit(12, 8)
a = 7
for q in range(8):
    circuit.h(q)
circuit.x(8)
for q in range(8):
    circuit.append(c_amod15(a, 2**q), [q] + [i + 8 for i in range(4)])
circuit.append(qft_dagger(8), range(8))
circuit.measure(range(8), range(8))
circuit.draw(output="mpl", fold=-1)
 ```


## Quipper
 ```hs
 c_amod15_aux :: Int -> Int -> (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
c_amod15_aux _ 0 c = return c
c_amod15_aux a power (q1, q2, q3, q4) = do
    when (a `elem` [2, 12]) $ do
        swap_at q3 q4
        swap_at q2 q3
        swap_at q1 q2
    when (a `elem` [7, 8]) $ do
        swap_at q1 q2
        swap_at q2 q3
        swap_at q3 q4
    when (a `elem` [4, 11]) $ do
        swap_at q2 q4
        swap_at q1 q3
    when (a `elem` [7, 11, 13]) $ do
        mapM_ gate_X [q1, q2, q3, q4]

    c_amod15_aux a (power - 1) (q1, q2, q3, q4)

c_amod15 :: Int -> Int -> (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
c_amod15 a power (q1, q2, q3, q4) = do
    c_amod15_aux a power (q1, q2, q3, q4)


controlledPhase :: Qubit -> Qubit -> Double -> Circ ()
controlledPhase control target theta = do
  with_controls (control .==. 1) $ do
    global_phase theta

qftDagger :: Int -> [Qubit] -> Circ ()
qftDagger n qs = do
  let halfN = n `div` 2
  mapM_ (\i -> swap_at (qs !! i) (qs !! (n - i - 1))) [0 .. (halfN - 1)]
  mapM_ (\j -> do
    mapM_ (\m -> controlledPhase (qs !! m) (qs !! j) (-pi / (2^(j - m)))) [0 .. (j - 1)]
    hadamard_at (qs !! j)) [0 .. (n - 1)]

-- Function to box c_amod15
boxed_c_amod15 :: Int -> Int -> (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
boxed_c_amod15 a power = box "c_amod15" (c_amod15 a power)

boxed_qftDagger :: Int -> [Qubit] -> Circ ()
boxed_qftDagger n = box "qftDagger" (qftDagger n)

shor's :: Circ [Bit]
shor's = do
    qubits <- qinit(replicate 12 False)
    mapM_ hadamard_at (take 8 qubits)
    gate_X (qubits !! 8)
    let a = 7
    mapM_ (\q -> when (q + 3 < 12) $
        void $ boxed_c_amod15 a (2^q) (qubits !! q, qubits !! (q+1), qubits !! (q+2), qubits !! (q+3))
        ) [0..7]
    boxed_qftDagger 8 (take 8 qubits)

    mapM measure (take 8 qubits)

 ```



 Here is worth mentioning how the languages differ. Qiskit is way cleaner but it might be more complicated to figure out how a funtion behaves. Quipper on the other hand as a way less user-friendly syntax. However, the functions being typed helps in understanding the function.

 We can see that Qiskit has more complex predefined gates compared to Quipper. In this case the C Phase gate, which neeeded to be constructed manually in Quipper, resulting in a more confusing syntax.

 Another point worth mentioning is how it's much easier in Qiskit to save a circuit as a "block" vissually. In the end, these two circuits appear very different when observing the final output. Since Qiskit has the more robust part of the algorithm saved as a oracle, the generated circuit appears more sound and easy to understand.