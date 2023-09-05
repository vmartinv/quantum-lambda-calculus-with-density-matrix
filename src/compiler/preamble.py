import typing
from qiskit import(
  QuantumCircuit,
  execute,
  Aer)


class Circuit:
    # Use Aer's qasm_simulator
    BACKEND = Aer.get_backend('qasm_simulator')
    DEBUG = False

    def __init__(self, n: int):
        assert n>0
        self.n = n
        self.circuit = QuantumCircuit(n, n)

    # creates |010+> from '010+'
    @staticmethod
    def fromstr(qubits: str):
        circuit = Circuit(len(qubits))
        for i,qubit in enumerate(qubits):
            if qubit=='0':
                pass
            elif qubit=='1':
                circuit.x(i)
            elif qubit=='+':
                circuit.h(i)
            elif qubit=='-':
                circuit.x(i)
                circuit.h(i)
            else:
                raise Exception('Unexpected qubit')
        return circuit

    def u(self, th: float, ph: float, la: float, q: int):
        self.circuit.u(th, ph, la, q)
        return self

    def cu(self, th: float, ph: float, la: float, ga: float, c: int, t: int):
        self.circuit.cu(th, ph, la, ga, c, t)
        return self

    def swap(self, q1: int, q2: int):
        self.circuit.swap(q1, q2)
        return self

    def ccnot(self, c: int, t1: int, t2: int):
        self.circuit.ccnot(c, t1, t2)
        return self

    def cswap(self, c: int, t1: int, t2: int):
        self.circuit.cswap(c, t1, t2)
        return self

    def x(self, q: int):
        self.circuit.x(q)
        return self

    def h(self, q: int):
        self.circuit.h(q)
        return self

    def cnot(self, c: int, t: int):
        self.circuit.cnot(c, t)
        return self

    def draw(self):
        return self.circuit.draw()

    def measure_all(self):
        return self.measure(*range(0, self.n, 2))

    def measure(self, *args):
        qs = list(args)
        for q in qs:
            assert 0 <= q and q < self.n
        self.circuit.measure(qs, qs)
        if Circuit.DEBUG:
            print(f"Measure called for qubits={qs}")
            print("Circuit:")
            print(self.circuit)
            job = execute(self.circuit, backend=Aer.get_backend('statevector_simulator'), shots=1, memory=True)
            job_result = job.result()
            print("Statevector:")
            print(job_result.get_statevector(self.circuit))
            shots = 500
        else:
            shots = 1
        job = execute(self.circuit, Circuit.BACKEND, shots=shots)
        if Circuit.DEBUG:
            print(job.result().get_counts())
        result = next(iter(job.result().get_counts()))
        assert len(result)==self.n
        result_int = 0
        for i,q in enumerate(qs):
            result_int = (2**i) * (result[-(q+1)]=='1')
        return result_int

    def compose(self, self2: 'Circuit', qubits, clbits):
        self.circuit \
            .compose(self2.circuit, qubits, clbits, inplace=True)
        return self

    def compose(self, self2: 'Circuit'):
        circuit1, circuit2 = self.circuit, self2.circuit
        n1, n2 = self.n, self2.n
        self.__init__(n1+n2)
        self.circuit \
            .compose(circuit1, inplace=True)
        self.circuit \
            .compose(circuit2, qubits=range(n1, n1+n2), clbits=range(n1, n1+n2), inplace=True)
        return self

def letcase(result: int, cases):
    return cases[result]()
