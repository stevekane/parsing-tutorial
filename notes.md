# Finite State Machine

Parsers are machines whose behavior is to recieve input
and to return an output as well as transition states.

In the machine below, we have two states.  Everytime the input
line hits the state machine, we transition to the other state
and emit either a 1 or a 0.  In this way, we are building a simple
clock mechanism.  Every input on the CLK line will transition
the machine and emit and output.  Thus, two inputs on the machine
will restore the state to wherever it was when this began.

       input
         |
0 <= A <-> B => 1
