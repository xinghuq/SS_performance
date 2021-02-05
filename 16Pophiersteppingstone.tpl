//Parameters for the coalescence simulation program : fastsimcoal.exe
16 samples to simulate :
//Population effective sizes (number of genes)
NPOP0$
NPOP1$
NPOP2$
NPOP3$
NPOP4$
NPOP5$
NPOP6$
NPOP7$
NPOP8$
NPOP9$
NPOP10$
NPOP11$
NPOP12$
NPOP13$
NPOP14$
NPOP15$
//Samples sizes and samples age 
40
40
40
40
40
40
40
40
40
40
40
40
40
40
40
40
//Growth rates	: negative growth implies population expansion
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
0
//Number of migration matrices : 0 implies no migration between demes
1
//Migration matrix 0
0	M2$	0	0	0	0	0	0	0	0	0	0	0	0	0	0
M2$	0	M2$	0	0	0	0	0	0	0	0	0	0	0	0	0
0	M2$	0	M2$	0	0	0	0	0	0	0	0	0	0	0	0
0	0	M2$	0	M2$	0	0	0	0	0	0	0	0	0	0	0
0	0	0	M2$	0	M2$	0	0	0	0	0	0	0	0	0	0
0	0	0	0	M2$	0	M2$	0	0	0	0	0	0	0	0	0
0	0	0	0	0	M2$	0	M2$	0	0	0	0	0	0	0	0
0	0	0	0	0	0	M2$	0	M3$	0	0	0	0	0	0	0
0	0	0	0	0	0	0	M3$	0	M2$	0	0	0	0	0	0
0	0	0	0	0	0	0	0	M2$	0	M2$	0	0	0	0	0
0	0	0	0	0	0	0	0	0	M2$	0	M2$	0	0	0	0
0	0	0	0	0	0	0	0	0	0	M2$	0	M2$	0	0	0
0	0	0	0	0	0	0	0	0	0	0	M2$	0	M2$	0	0
0	0	0	0	0	0	0	0	0	0	0	0	M2$	0	M2$	0
0	0	0	0	0	0	0	0	0	0	0	0	0	M2$	0	M2$
0	0	0	0	0	0	0	0	0	0	0	0	0	0	M2$	0
//historical event: time, source, sink, migrants, new deme size, new growth rate, migration matrix index
0 historical event
//Number of independent loci [chromosome] 
10 0
//Per chromosome: Number of contiguous linkage block
1
//per Block: data type, num loci, rec. rate and mut rate + optional parameters
MICROSAT  1  0.0000  0.0005 0 
