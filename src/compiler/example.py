from preamble import *

prog = letcase(
    Circuit([
        0.7071067811865469,
        0.7071067811865471,
        0.0+6.60142560062039e-17j,
        0.0-6.601425600620388e-17j,
    ]).measure(0),
    [
        lambda: Circuit([
            1.0, 0.0, 0.0, 0.0,
        ]),
        lambda: Circuit([
            0.0, 0.0, 0.0, 1.0,
        ]),
    ]
)

print(prog)
