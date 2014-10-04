The sources/ directory contains a set of small program examples to test your
analyzer.

The files are ordered by increasing complexity and language features.
They are classified into categories according to the file name prefix, 
as follow:
- 00*: variable declarations, initialization with a constant
- 01*: printing, local declarations
- 02*: assignment, arithmetic operators
- 03*: if-then-else, comparisons and boolean operators
- 04*: assertions
- 05*: loops
- 06*: realistic examples mixing all of the above

The results/ directory gives the output of the analysis using our 
reference analyzer, with the constant, the interval, and the polyhedra
domains.
All the analyses are preformed using a loop unrolling of 3 and a widening 
delay of 3.
Our analyzer outputs a line for each print instruction and for each assertion
failure, as well as the abstract environment when the program stops.
Note that, due to loop unrolling, a print or assert statement may be executed
more than once. Note also that, during fixpoint computation, the output of 
print and assert statements is disabled; it is re-enabled only after the 
fixpoint is reached, so that we do not print any information about the 
iterates (which may not be invariants).

For each source file and each domain (constants, intervals, polyhedra), we
provide the file output by the reference analyzer.
We also provide for each domain a summary file (named all.*.txt) that contains
each file followed by the analysis result, which may be more convenient to
read.
