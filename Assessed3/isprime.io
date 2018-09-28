{
  print "please input a number";
  read x;
  
  p := x >= 2;
  n := 2;
  
  while (n * n <= x && p) {
    p := x % n;
    n := n + 1;
  }

  if (p)
    print "the number is prime";
  else {
    print "the number is divisible by";
    write (n-1);
  }
}
