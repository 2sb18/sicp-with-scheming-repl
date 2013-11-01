E = Peter  + 10
A = Paul   - 20
M = Mary   / 2

a.

EAM 100 110 90 45
EMA 100 110 55 35
AEM 100 80 90  45
AME 100 80 40  50
MEA 100 50 60  40
MAE 100 50 30  40

b. if the processes can be interleaved, then each set! procedure is really three
   different actions. 1) get balance, 2) compute new balance, 3) set balance to new
   value.

One example
   
E1 100
A1 100
E2 110
M1 100
A2  80
E3 110 set
M2  50
A3 
M3  50 set

Worse example

M1 100
A1 100
E1 100
M2  50
A2  80
E2 110
M3  50
A3  80
E3 110 set
