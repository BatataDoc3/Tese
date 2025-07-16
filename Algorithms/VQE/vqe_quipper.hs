-- vqe_cliffordt.hs
-- Full VQE loop with Clifford+T-only ansatz for simplified H₂ Hamiltonian

import Quipper
import Quipper.Libraries.Simulation.QuantumSimulation
import System.Random (randomRIO)
import Control.Monad (replicateM, zipWithM)

-- Define Pauli type for clarity
data Pauli = I | X | Y | Z deriving (Eq, Show)

-- H₂ Hamiltonian: simplified 2-qubit version
-- H = -1.05 * I⊗I + 0.39 * Z⊗I + 0.39 * I⊗Z - 0.01 * Z⊗Z + 0.18 * X⊗X
h2Hamiltonian :: [(Double, [Pauli])]
h2Hamiltonian =
  [ (-1.05, [I, I])
  , ( 0.39, [Z, I])
  , ( 0.39, [I, Z])
  , (-0.01, [Z, Z])
  , ( 0.18, [X, X])
  ]

-- Clifford+T ansatz with no θ parameters
ansatz :: Circ [Qubit]
ansatz = do
  q0 <- qinit False
  q1 <- qinit False

  gate_H q0
  controlled_not q0 q1
  gate_T q0
  gate_H q0
  gate_T q0
  controlled_not q0 q1
  gate_T_inv q1
  gate_H q1

  return [q0, q1]

-- Apply measurement basis depending on Pauli
applyMeasurementBasis :: Qubit -> Pauli -> Circ Qubit
applyMeasurementBasis q I = return q
applyMeasurementBasis q Z = return q
applyMeasurementBasis q X = do gate_H q; return q
applyMeasurementBasis q Y = do gate_S_inv q; gate_H q; return q

-- Measure one Hamiltonian term ⟨P₀ ⊗ P₁⟩
measureTerm :: [Pauli] -> Circ [Qubit] -> IO Double
measureTerm paulis circuit = do
  results <- replicateM 1000 $ do
    measured <- run_generic_io (1.0 :: Double) $ do
      qubits <- circuit
      rotated <- zipWithM applyMeasurementBasis qubits paulis
      bits <- measure rotated
      return bits
    let toZ b = if b == 0 then 1 else -1
    let val = product $ zipWith (\p b -> if p == I then 1 else toZ b) paulis measured
    return val
  return $ fromIntegral (sum results) / fromIntegral (length results)


-- Sum over all Hamiltonian terms
estimateEnergy :: Circ [Qubit] -> IO Double
estimateEnergy circuit = do
  terms <- mapM (\(coeff, paulis) -> do
                   ev <- measureTerm paulis circuit
                   return (coeff * ev)
                ) h2Hamiltonian
  return $ sum terms

-- Main function
main :: IO ()
main = do
  putStrLn "Estimating H2 using Clifford+T ansatz..."
  energy <- estimateEnergy ansatz
  putStrLn ("Estimated energy: " ++ show energy)
