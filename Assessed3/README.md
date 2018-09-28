# Assessed 3

You will extend the programming language discussed in the [lecture notes](https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018/blob/master/LectureNotes/interpreter/README.md) with

   * print statements (print a string followed by a newline)
   * read statements (read an integer in a line)
   * write statements (write an integer followed by a newline)

The tasks are as follows, in outline (see below for precise details):

1. Parsing, using the `Parser` monad. *Easy.*

1. Pretty print: generate nicely formated concrete program from a syntax tree. *Medium.*

1. Interpreting, using the `IO'` monad. *Hard.*

1. Optimize expressions and programs given as syntax trees. *Rather hard.*

Each question is worth 25% (a more detailed marking scheme is given below).

## Learning objectives

  * monads
  * monadic IO
  * monadic parsing
  * abstract-syntax interpretation
  * introduction to programming language processing

## The IO monad

Please study chapter 10 of the book.

## Our version of the IO monad

In order to be able to give automatic feedback, we work with our own version [`IO'`](https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018/blob/master/LectureNotes/IOPrime.md) of the `IO` monad, explained in the module [lecture notes](https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018/blob/master/LectureNotes/IOPrime.md).

## Monadic parsing

Please study Chapter 13 of the book and the [corresponding Haskell file](https://git.cs.bham.ac.uk/mhe/fp-learning-2017-2018/blob/master/Resources/Book/code/Parsing.hs).

## Sample programs in the extended language

This is the file `fibonacci.io`
```
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
```
This is the file `factorial.io`:
```
{
print "please input a number";
read x;
y := 1;
while (x > 0)
 {
 y := y * x;
 x := x - 1;
 }
print "its factorial is";
write y;
}
```
This is the file `isprime.io`:
```
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

```

## BNF of the extended language

The three new things are `read`, `write` and `print` statements:

```
Program ::= Identifier := Expr;
          | { [Program] }
          | while (Expr) Program
          | If (Expr) Program
          | If (Expr) Program else Program
          | read Identifier;   
          | write Expr;        
          | print String;

Expr  ::= Expr1 | Expr1 OrOp   Expr
Expr1 ::= Expr2 | Expr2 AndOp  Expr1
Expr2 ::= Expr3 | Expr3 EqOp   Expr2
Expr3 ::= Expr4 | Expr4 CompOp Expr3
Expr4 ::= Expr5 | Expr5 AddOp  Expr4
Expr5 ::= Expr6 | Expr6 MulOp  Expr5
Expr6 ::= Expr7 | NotOp Expr6
Expr7 ::= Constant | Identifier | (Expr)

OrOp   ::=  ||
AndOp  ::=  &&
EqOp   ::=  ==
CompOp ::=  <=  |  <  |  >=  |  >
AddOp  ::=  +   |  -
MulOp  ::=  *   |  /  |  %
NotOp  ::=  !
```

## Questions and marking scheme

The total number of points is 40, hence each point is worth 2.5% of the assignment.

  1. In the file `Parser.hs`, complete the definition of the following functions and variables, to parse according to the above grammar:
  
     * `readStatement`  (3 points)
     * `writeStatement` (3 points)
     * `printStatement` (3 points)
     * `keywords` (1 point)
    
  1. In the file `PrettyPrinting`, complete the definitions of
     * `ppExpr` (5 points)
     * `ppProgram` (5 points)

  1. In the file `Interpreter`, complete the definition of
     * `run` (9 cases, 1 point each, except the last one, which is 2 points)

  1. In the file `Optimization.hs`, complete the definitions of
     * `optExpr` (5 points)
     * `optExpr` (5 points)


## Questions and marking scheme

The total number of points is 40, hence each point is worth 2.5% of the assignment.

  1. In the file `Parser.hs`, complete the definition of the following functions and variables, to parse according to the above grammar:
  
     * `readStatement`  (3 points)
     * `writeStatement` (3 points)
     * `printStatement` (3 points)
     * `keywords` (1 point)
    
  1. In the file `PrettyPrinting`, complete the definitions of
     * `ppExpr` (5 points)
     * `ppProgram` (5 points)

  1. In the file `Interpreter`, complete the definition of
     * `run` (9 cases, 1 point each, except the last one, which is 2 points)

  1. In the file `Optimization.hs`, complete the definitions of
     * `optExpr` (5 points)
     * `optExpr` (5 points)

## Specification of pretty printing

[[Missing. Will be included before the official assignment start date.]]

## Specification of expression and program optimization

[[Missing. Will be included before the official assignment start date.]]

## Testing

We provide the file `CorrectnessTests.hs` to help you get started with your testing.
 * The function `allTests` at the end of the file tests everything.
 * But there is a collection of functions to test most of the questions, excluding `keywords`.  The questions are tested independently, to the extent that this is possible.

   *Warning:* passing these tests is no guarantee of correctness.
   Failing any of them should be, however, a guarantee of incorrectness.

   The marking program may find errors that are not accounted for
   here. The tests here are only to give you ideas for testing. There
   is no guarantee that if your submission passes all tests here it
   will necessarily pass our marking tests.

When you are confident that everything is working, you may wish to play with the file `RunIO.hs` to further test your solution.

## Submission instructions

   * Run the pre-submit script in a lab machine.

If successful, this will create a zip file with

   * `Parser.hs`
   * `Interpreter.hs`

which you should submit to [Canvas](https://birmingham.instructure.com/courses/27254).

## Further discussion

May be included here in the future, for the sake of clarification.
