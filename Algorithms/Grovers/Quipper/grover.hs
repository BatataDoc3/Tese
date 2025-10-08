-- ghc -package quipper-language .\vqe_quipper.hs 

import Quipper
import Quipper.Internal.Printing

oracle :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
oracle (q1, q2) = do
    q1 <- qnot q1
    gate_Z_at q1 `controlled` q2
    q1 <- qnot q1
    return (q1, q2)

grovers :: (Qubit, Qubit) -> Circ (Qubit, Qubit)
grovers (q1, q2) = do
    -- Step 1: Apply Hadamard to both qubits
    (q1, q2) <- map_hadamard (q1,q2)

    -- Step 2: Apply Oracle
    (q1, q2) <- oracle (q1, q2)
    return (q1, q2)

print_final :: IO ()
print_final =
    print_simple Preview $ do
        q1 <- qinit False
        q2 <- qinit False
        (q1, q2) <- grovers (q1, q2)
        measure (q1, q2)


main :: IO ()
main = do
  print_final