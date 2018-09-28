{
 print "please input a number";
 read x;
 y := 1;
 z := 1;
 while (x > 0)
  {
   x := x - 1;
   t := y + z;
   y := z;
   z := t;
  }
 print "its fibonacci value is";
 write y; 
}
