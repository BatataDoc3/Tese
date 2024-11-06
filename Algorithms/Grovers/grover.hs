import Quipper

grover :: ( Qubit , Qubit ) -> Circ ( Qubit , Qubit )
grover (q1 ,q2) = do
    q1 <- hadamard q1
    q2 <- hadamard q2

    -- Oracle - Quipper does not have a built in CZ gate, so we need to make one by doing: H(target) -> CNot -> H(target)
    q2 <- hadamard q2
    q2 <- qnot q2 `controlled` q1
    q2 <- hadamard q2

    -- Diffusion operator
    comment ""
    q1 <- hadamard q1
    q2 <- hadamard q2
    q1 <- gate_Z q1
    q2 <- gate_Z q2
    q2 <- hadamard q2
    q2 <- qnot q2 `controlled` q1
    q2 <- hadamard q2
    comment ""
    q1 <- hadamard q1
    q2 <- hadamard q2
    return (q1, q2)

-- Main function to execute the quantum circuit
print_final :: IO ()
print_final =  print_simple Preview (do
    -- Create three qubits within the Circ monad
    q1 <- qinit False
    q2 <- qinit False
    -- Call the grover function
    (q1', q2') <- grover (q1, q2)
    -- Measure the resulting qubits
    measure (q1', q2')
    )