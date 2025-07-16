from qiskit import QuantumCircuit
import matplotlib.pyplot as plt

def draw_quantum_circuit(gates, filename="circuit.png"):
    # Determinar o número máximo de qubits usados
    num_qubits = max(max(max(q[1]) for q in gates if q[1]) + 1, 2)
    qc = QuantumCircuit(num_qubits, num_qubits)
    
    for gate in gates:
        name, qubits, controls = gate
        if name == "H":
            qc.h(qubits[0])
        elif name == "X":
            qc.x(qubits[0])
        elif name == "Z":
            qc.z(qubits[0])
        elif name == "CX":
            qc.cx(controls[0], qubits[0])
        elif name == "Measure":
            qc.measure(qubits[0], qubits[0])
    
    fig = qc.draw('mpl')
    fig.savefig(filename)  # Salva a imagem

# Exemplo de uso
gates = [
    ("H", [0], []),
    ("H", [1], []),
    ("CX", [1], [0]),
    ("Z", [0], []),
    ("Measure", [0], []),
    ("Measure", [1], [])
]

draw_quantum_circuit(gates)