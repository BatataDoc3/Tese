import Quipper

grover :: ( Qubit , Qubit , Qubit ) -> Circ ( Qubit , Qubit )
grover (q1 ,q2 , q3 ) = do
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    controlled (qnot_at q3) [q1, q2]
    hadamard_at q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q2
    controlled (qnot_at q2) q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    return (q1, q2)

-- Main function to execute the quantum circuit
print_final :: IO ()
print_final =  print_simple Preview (do
    -- Create three qubits within the Circ monad
    q1 <- qinit False
    q2 <- qinit False
    q3 <- qinit False
    -- Call the grover function
    (q1', q2') <- grover (q1, q2, q3)
    -- Measure the resulting qubits
    measure (q1', q2')
    )