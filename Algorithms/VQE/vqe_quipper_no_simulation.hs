
import Quipper

type Params = (Double, Double)

-- Parameterized ansatz circuit
ansatz :: Params -> Circ [Qubit]
ansatz (theta0, theta1) = do
  q0 <- qinit False
  q1 <- qinit False

  gate_rot "Y" theta0 q0
  gate_rot "Y" theta1 q1

  controlled_not q0 q1
  return [q0, q1]

-- Print the circuit for Z0 measurement
circuitZ0 :: Params -> IO ()
circuitZ0 theta = print_simple Preview (do
  [q0, _] <- ansatz theta
  measure q0
  return ())

-- Print the circuit for Z1 measurement
circuitZ1 :: Params -> IO ()
circuitZ1 theta = print_simple Preview (do
  [_, q1] <- ansatz theta
  measure q1
  return ())

-- Print the circuit for Z0Z1 measurement
circuitZ0Z1 :: Params -> IO ()
circuitZ0Z1 theta = print_simple Preview (do
  [q0, q1] <- ansatz theta
  measure q0
  measure q1
  return ())

-- Print the circuit for X0X1 measurement
circuitX0X1 :: Params -> IO ()
circuitX0X1 theta = print_simple Preview (do
  [q0, q1] <- ansatz theta
  hadamard q0
  hadamard q1
  measure q0
  measure q1
  return ())

-- Main entry for generating and printing all circuits
main :: IO ()
main = do
  let theta = (1.2, 0.8)
  putStrLn "Circuit for measuring Z0:"
  circuitZ0 theta

  putStrLn "\nCircuit for measuring Z1:"
  circuitZ1 theta

  putStrLn "\nCircuit for measuring Z0Z1:"
  circuitZ0Z1 theta

  putStrLn "\nCircuit for measuring X0X1:"
  circuitX0X1 theta
