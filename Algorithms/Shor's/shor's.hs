import Quipper
import Control.Monad ( when, forM_ )

c_amod15_aux :: Int -> Int -> (Qubit, Qubit, Qubit, Qubit) -> Circ (Qubit, Qubit, Qubit, Qubit)
c_amod15_aux _ 0 c = return c  -- Base case: if power is 0, return the qubits as is
c_amod15_aux a power (q1, q2, q3, q4) = do
    -- Apply swap gates based on the value of 'a'
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

    -- Apply X gates if 'a' is in [7, 11, 13]
    when (a `elem` [7, 11, 13]) $ do
        mapM_ gate_X [q1, q2, q3, q4]

    -- Return the modified qubits
    c_amod15_aux a (power - 1) (q1, q2, q3, q4)  -- Continue with the same qubits

-- Function to apply controlled phase gate
controlled_phase :: (Qubit, Qubit) -> Phase -> Circ ()
controlled_phase (control, target) phi = do
    controlled Rz phi control target  -- Apply controlled phase gate

c_amod15 :: Int -> Int -> Circ(Qubit, Qubit, Qubit, Qubit)
c_amod15 a power = do
    q1 <- qinit False
    q2 <- qinit False
    q3 <- qinit False
    q4 <- qinit False
    (q1', q2', q3', q4') <- c_amod15_aux a power (q1, q2, q3, q4)
    return (q1', q2', q3', q4')

-- Define the QFT† (inverse Quantum Fourier Transform) on n qubits
qftDagger :: Int -> Circ [Qubit]
qftDagger n = do
    qubits <- mapM qubit [1..n]  -- Create n qubits

    -- Perform the swaps (QFT† step 1)
    forM_ [0..(n `div` 2 - 1)] $ \i -> do
        swap_at (qubits !! i) (qubits !! (n - i - 1))

    -- Perform controlled phase gates and Hadamard gates (QFT† steps 2 and 3)
    forM_ [0..(n - 1)] $ \j -> do
        -- Apply controlled phase gates (cp(-pi/2^(j-m)) in Python)
        forM_ [0..(j-1)] $ \m -> do
            controlled_phase (qubits !! m, qubits !! j) (-pi / (2 ** fromIntegral (j - m)))

        -- Apply Hadamard gate
        gate_H (qubits !! j)

    return qubits

print_final :: IO ()
print_final =  print_simple Preview (do
    c_amod15 15 7
    )