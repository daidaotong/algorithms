/* first*/
declare
fun {Hailstone N}
   if N == 1 then
      N
  elseif N mod 2==0 then
      N div 2
   else
      3*N+1
   end
end

{Browse{Hailstone 2}}

/* second*/
declare
fun {HsSequence N Acc}
   local L in
      L=N|L
   if N == 1 then
    {Browse{Reverse L}}
     Acc+1
   elseif N mod 2==0 then
      {HsSequence N div 2 Acc+1}
   else
      {HsSequence 3*N+1 Acc+1}
   end
   end
end
{Browse{HsSequence 14 0}}

