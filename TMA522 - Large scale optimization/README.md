# Large_scale_opt
The purpose of this project assignment is to illustrate how a relatively difficult opti-
mization problem can be attacked by using Lagrangean duality. For the project you
should write a Matlab program for solving the Lagrangean dual problem. At your
service are two functions that are called from Matlab.
We consider a routing problem in “very large-scale integrated circuit design” (VLSI),
namely an application of the so-called “Manhattan channel routing problem”. The
mathematical classification of this problem is that of “vertex disjoint paths”. The
problem and the application are based on the article [Feo and Hochbaum, 1986].
The program to be constructed is to be used to decide whether a given placement of
a number of connections is possible to implement with respect to the wiring necessary
between the components. The circuit board, where the connections are to be placed,
is such that on one side of the board wiring can only be done horizontally, and on the
other side only vertically. On the board there are predefined “vias” (or connectors), at
which it is possible to connect the two sides of the board. Figure 1 illustrates such a
problem having six horizontal wires on one side of the board and eight vertical wires
on the other, as well as 48 vias and six contact pairs.
Given a number of contact pairs, we wish to determine whether it is possible to
connect all of them on the two-sided board.
First, we introduce the notation needed to mathematically describe the network and
how the contact pairs are interconnected. Then we present the optimization problem,
the solution of which determines whether the routing problem has a solution or not.
This problem will then be studied by using Lagrangean duality, whose formulation will
be attacked using subgradient optimization. Using the bounds obtained for the optimal
objective value we can then draw conclusions about the existence of feasible solutions
to the wiring problem.

The project description can be found here: file:///C:/Users/regin/Downloads/Project1%20Large-Scale%20Optimization%202023.pdf



