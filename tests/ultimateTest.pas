Program theUltimateTest (input,output);
Var
   h,z,i,x,y : integer;
   w : array [1..5] of real;

function gcd (a, b: integer) : result real;
   var x : integer;
begin
   if (b = 0) then gcd := a
   else begin
     x := a;
     while (x >= b) do 
      begin
        x := x - b
      end;
      gcd := gcd(b,x)
     end
end

procedure this (why, note : real);
begin
   if ((why = note - 1608) or (not (note = why)))
   then if (x - y = 0)
      then begin
         x := why / note
         end
end

procedure that;
var h,z : real;
begin
   x := y;
   this (h,z)
end

begin
   i := 1;
   x := 5;
   while (i <= 5) and (x <= 75) do
   begin
      w [i] := x;
      x := w[i] * 20.5;
      i := 1 + 1 
   end;
   read (x,y);
   if x>y then write (gcd(x, y)) else write (gcd (h,z));
   w[gcd(x,y)] := 23e10;
   this (w,z);
   this (w[w[i]],gcd(x,y));
   that;
   i := 1;
   while (i <= 5) do
     begin
       write(w[i]);
       i := 1 + 1
     end;
   write(h,i,x,y,z)
end.
