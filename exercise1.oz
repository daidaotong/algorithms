/* Exercise 1 (1)*/
declare
proc {Bubble L Bg A}/*bubble is one time loop, L is the original list ,Bg returns the biggest value of a list, A returns the list with removing the biggest value*/
   case L 
   of X|Y|L1 then 
      if X>Y
      then M in  /*if X>Y, then change X and Y position by adding X to the rest of the list, meanwhile, add Y to A(because A returns the list without the biggest) then recursive call bubble*/
	 A=Y|M
	{Bubble X|L1 Bg M}
      else N in    /*if X<=Y, then we do not need to change X and Y position. add X to A and recursive call bubble */
	 A=X|N
	 {Bubble Y|L1 Bg N}
      end
   []X|Li then Bg=X A=nil   /* if there is only one element in the list, then X will definitely be the largest, without X,the list will be empty, therefore A is null */
   []nil then Bg=nil A=nil /*if the list is empty, there is no biggest element and A returns null*/
   end
end

declare
fun {SortBubble L A} /*SortBubble is the main function, A is the accumulator and return value*/
   if L==nil then A /*if L is empty, then the output A will empty*/
   else Bg Li in 
      {Bubble L Bg Li} /*run bubble for one time, then get the biggest value and the rest list without biggest*/
      {SortBubble Li Bg|A} /*call SortBubble recursively with the rest list Li that Bubble returned, meanwhile, everytime add the biggest value in this loop to the acclumator A's left, then A will be sorted with the smallest value on the left and biggest value on the right*/
   end
end
{Browse {SortBubble [2 4 6 8 10 1 2 3 4 5] nil}} /* test the result, it's [1 2 2 3 4 4 5 6 8 10] */



/*Exercise 1(2)*/
declare
fun {Length L A} /* function Length return the length of a list,A is the accumulator*/
   case L
   of X|L1 then {Length L1 A+1}
   []nil then A
   end
end

declare
fun {GetI L I} /*function GetI returns the Ith element in the list, because I will be calculated by the Length of the list,therefore we don't need to warry if I is larger than the length of the list*/
   case L
   of X|L1 then
      if I==1 then X
      else
	 {GetI L1 I-1}
      end
   []nil then nil
   end
end

declare
fun {MyMedian L} /*MyMedian is the main function*/
   if L==nil then nil /* if L is empty then median should have no value*/
   else if {Length L 0} mod 2==0 then  M N T1 H in /*if L's Length mod 2 ==0,that means L has even numbers,then median is the average of two middle values */
	   T1={Length L 0} div 2 
	  M={GetI {SortBubble L nil} T1} /*the first middle value(Sorting is implemented here)*/
	   N={GetI {SortBubble L nil} T1+1} /*the second middle value(Sorting is implemented here)*/
	    {IntToFloat M+N}/{IntToFloat 2} /*the average of these two values*/
	else T2 in /*if L's Length mod 2 !=0,that means L has odd numbers,then median is the the middle number*/
	   T2={Length L 0} div 2 
	   {GetI {SortBubble L nil} T2+1} /*get the middle number(Sorting is implemented here)*/
	 end
   end
   
end
{Browse {SortBubble [13 18 13 14 13 16 14 21 13] nil}} /*test the result*/
{Browse {MyMedian [13 18 13 14 13 16 14 21 13]}} /*test the result*/