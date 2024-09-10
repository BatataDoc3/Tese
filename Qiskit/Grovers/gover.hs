grover :: ( Qubit , Qubit , Qubit ) -> Circ ( Bit , Bit )
grover (q1 ,q2 , q3 ) = do
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    qnot_at q3 'controlled' [ q1 , q2 ]
    hadamard_at q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q2
    qnot_at q2 'controlled' q1
    hadamard_at q2
    gate_X_at q1
    gate_X_at q2
    hadamard_at q1
    hadamard_at q2
    hadamard_at q3
    measure (q1 , q2 )
