(* ::Package:: *)

(*
   Compute the list of pairs you'd get from combining xs and ys
   element by element.
@param xs list of x coordinates
@param ys list of y coordinates
@return list of (x[[i]],y[[i]]) pairs
@pre Length[x] \[Equal] Length[y]
@post none
*)
Zip[xs_,ys_] :=
	Transpose[{xs,ys}];
(* 
   Compute (x,y) points given a list of xs and a list of
 lists of ys.  
@param xs list of x coordinates
@param ys list of list of y coordinates 
@return list of list of points such that all y[[i]] have x coordinate x[[i]]
@pre Length[x] \[Equal] Length[y]
@post none
*)
MakePoints[xs_,ys_] :=
Map[Zip[Table[xs[[#]],{Length[ys[[#]]]}],ys[[#]]]& ,Range[Length[xs]]];


TestReport[{VerificationTest[	
			With[{x={1,2,3,4},y={5,6,7,8}},Zip[x,y]],
			{{1,5},{2,6},{3,7},{4,8}}],	     
			VerificationTest[With[{x={0,1},y = {{1,2,3,4},{5,6,7,8}}},MakePoints[x,y]],
						 {{{0,1},{0,2},{0,3},{0,4}},{{1,5},{1,6},{1,7},{1,8}}}]}]




(*
	Compute a table of nanoseconds from a a table numbers that 
	are assumed to be seconds
@param sectab table of seconds 
@return table of nanosecond quantities 
@pre sectab values are non-quantity seconds
@post none
*)
Tons[sectab_] :=
Map[Map[Quantity[#/(10^-9),"nanoseconds"]&,#]&,sectab];

(* 
   Given a nanosecond quantity, compute the most
   appropriate larger time quantity. 
@param ns a nanosecond quantity
@return a time quantity equivalent to ns
@pre none
@post none
*)
TimeUp[ns_]:=
With[{v=QuantityMagnitude[ns]},
Which[
v<10^3,ns,
v<10^6,UnitConvert[ns,"microseconds"],
v<10^9,UnitConvert[ns,"milliseconds"],
v<60*10^9,UnitConvert[ns,"seconds"],
v<60^2*10^9,UnitConvert[ns,"minutes"],
v<24*60^2*10^9,UnitConvert[ns,"hours"],
v<7*24*60^2*10^9,UnitConvert[ns,"days"],
v<4*7*24*60^2*10^9,UnitConvert[ns,"weeks"],
v<12*4*7*24*60^2*10^9,UnitConvert[ns,"months"],
True,UnitConvert[ns,"years"]]]



