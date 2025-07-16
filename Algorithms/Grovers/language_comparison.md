# Grover's Algorithm

It's pretty well explained in the notebook. It's important to mention that this test was done with only 2 qubits and the point was to try to find the qubit |11>

## Qiskit algorithm
```py
# Oracle
oracle = QuantumCircuit(2, name='oracle')
oracle.cz(0,1)
oracle.to_gate()
oracle.draw(output="mpl")

# Main circuit
circuit = QuantumCircuit(2,2)
circuit.h([0,1])
circuit.append(oracle, [0,1]) 
circuit.draw(output="mpl")

# Diffusion Operator
circuit.barrier()
circuit.h([0,1])
circuit.z([0,1])
circuit.cz(0,1)
circuit.h([0,1])
 ```

 ## Quipper Algorithm
 ```hs 
-- cz gate
cz :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
cz (q1, q2) = do
    q2 <- hadamard q2
    q2 <- qnot q2 `controlled` q1
    q2 <- hadamard q2
    return (q1, q2)

-- oracle
oracle :: ( Qubit, Qubit ) -> Circ (Qubit , Qubit)
oracle (q1, q2) = do
    (q1, q2) <- cz(q1, q2)
    return (q1, q2)

-- main circuit
grover :: ( Qubit , Qubit ) -> Circ ( Qubit , Qubit )
grover (q1 ,q2) = do
    (q1, q2) <- map_hadamard (q1,q2)
    (q1, q2) <- oracle (q1, q2)
    (q1, q2) <- map_hadamard (q1,q2)
    q1 <- gate_Z q1
    q2 <- gate_Z q2
    (q1, q2) <- cz(q1, q2)
    (q1, q2) <- map_hadamard (q1,q2)
    return (q1, q2)
 ```

 At first glance, we can see that both algorithms can handle algorithms with a certain complexity fairly easily. Although very different syntatically due to the nature of their languages, both of these implementations are really concise and easy to understand. 
 
 From this example, we can observe some of the predefined functions that each of the languages have. From this snippet we can already see that Qiskit has a more extensive library of predefined functions and gates that ease the creation of circuits. Quipper, on the other hand, has a more limited set of function at it's disposial. This will become more aparent and troublesome as we start dwelling on more complex algorithms. In this case, since we only needed to implement a Controlled Z gate manually in Quipper, a workaround was not very difficult to create nor it polluted the code. 
 
Another difference we can see in the languages is that Quipper forces to declare the types of it's function, both input and output wise. Although this ends up created a more robust syntax, it also helps to efficiently understand what a function does and how it works, unlike Quiskit, that, in more complex algorithms, might end up creating unclear functions that are hard to properly understand.