import typing
from qiskit import(
  QuantumCircuit,
  execute,
  Aer)


class Circuit:
    # Use Aer's qasm_simulator
    BACKEND = Aer.get_backend('qasm_simulator')

    def __init__(self, qu: int, cl: int):
        assert qu>0
        self.qu = qu
        self.cl = cl
        self.circuit = QuantumCircuit(qu, cl)

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

    def uc(self, th: float, ph: float, la: float, c: int, t: int):
        self.circuit.uc(th, ph, la, c, t)
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

    def measure(self, qs: list[int]):
        # Todo arreglar assert y verificar que la medicion devuelve solo los bits de qs (los pares)
        assert n <= self.n
        self.circuit.measure(qs, qs)
        job = execute(self.circuit, Circuit.BACKEND, shots=1)
        result = next(iter(job.result().get_counts()))
        return (int(result[:n], 2), Circuit.fromstr(result))

    def compose(self, self2: 'Circuit', qubits, clbits):
        self.circuit \
            .compose(self2.circuit, qubits, clbits, inplace=True)
        return self

    def compose(self, self2: 'Circuit'):
        circuit1, circuit2 = self.circuit, self2.circuit
        qu1, qu2 = self.qu, self2.qu
        cl1, cl2 = self.cl, self2.cl
        self.__init__(qu1+qu2, cl1+cl2)
        self.circuit \
            .compose(circuit1, inplace=True)
        self.circuit \
            .compose(circuit2, qubits=range(qu1, qu1+qu2), clbits=range(cl1, cl+cl2), inplace=True)
        return self

def letcase(result: (int, Circuit), cases):
    return cases[result[0]](result[1])
