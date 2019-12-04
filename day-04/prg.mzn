include "globals.mzn";

int: FROM;
int: TO;
int: L = 6;
bool: PART2;
set of int: DIGIT = 0..9;

array[1..L] of var DIGIT: pass;

constraint increasing(pass);

constraint let { var int: val = sum(i in 1..L)(pow(10, L-i) * pass[i]) } in 
  val >= FROM /\ val <= TO;
  
var 1..L-1: doublePosition;
  
constraint pass[doublePosition] = pass[doublePosition+1];
constraint PART2 -> count(pass, pass[doublePosition], 2);

output [show(pass)];

solve ::int_search(pass, input_order, indomain_min) satisfy;
