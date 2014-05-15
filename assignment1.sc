import scala.util.control.Breaks._

def last(a: List[Int]): Int = {

  a match {
    case Nil => throw new NoSuchElementException
    case a::Nil => a
    case head::tail => last(tail)
  }
}
def penultimate(b: List[Int]): Int = {
  b match {
    case Nil => throw new NoSuchElementException
    case a::Nil => throw new NoSuchElementException
    case a :: c :: Nil => a
    case head::tail => penultimate(tail)

  }
}
def reverse(a: List[Int]): List[Int] =
{
  a match {
    case Nil => throw new NoSuchElementException;
    case f :: Nil => List(f);
    case head::tail =>  reverse(tail):::List(head) ;
  }
}
def compress(a: List[Any]): List[Any] =
{
  a match{
    case Nil => throw new NoSuchElementException;
    case f :: Nil => List(f);
    case b::c::tail if(b==c) => compress(List(b):::tail);
    case b::tail => List(b):::compress(tail);
  }
}
def expand(n: Int,a: List[String]): List[String] = {

  (n,a) match{
    case (_,Nil) => throw new NoSuchElementException;
    case (1,_) => a
    case (_,f :: Nil) => List(f):::expand(n-1,List(f));
    case (_,b::tail) => List(b):::expand(n-1,List(b)):::expand(n,tail);
  }
}
def drop(m:Int,a:List[Any]): List[Any] = {
  def dropIn(m: Int, n: Int, a: List[Any]): List[Any] = {

    (n, a) match {
      case (_, Nil) => a
      case (1, a :: b) => dropIn(m, m, b)
      case (_, a :: b) => a :: dropIn(m, n - 1, b)
    }
  }
  dropIn(m,m,a);
}
def slice(j: Int,k: Int,a: List[Any]): List[Any] = {

  (j,k,a) match{
    case (_,_,Nil) => a
    case (_,0,_) => Nil
    case (0,_,a::b) => List(a):::slice(0,k-1,b)
    case (_,_,a::b) => slice(j-1,k-1,b)
  }
}
def isPrime(j: Int, k: Int): Boolean =
  (j,k) match{
    case (_,0) => false
    case (_,1) => true
    case (_,_) if(j%k == 0) => false
    case (_,_) if(j%k != 0) => isPrime(j,k-1)
  }
def listPrimesinRange(j: Int,k: Int): List[Any] = {

  (j,k) match{
    case (_,_) if(j>k) => return Nil;
    case (_,_) if( isPrime(j,j/2) ) => List(j):::listPrimesinRange(j+1,k);
    case (_,_) => listPrimesinRange(j+1,k);
  }

}
def multipleof3or5(j: Int): Int = {

  j match{
    case 1 => 0
    case _ if((j-1)%3==0 || (j-1)%5==0) => multipleof3or5(j-1) + j-1 ;
    case _ => multipleof3or5(j-1);
  }
}
def fullWords(n: Int): List[String] = {
  if(n%10 == n) {
    if (n % 10 == 1)
      return  List("one");
    else if (n % 10 == 0)
      return List("zero")
    else if (n % 10 == 2)
      return List("two")
    else if (n % 10 == 3)
      return List("three")
    else if (n % 10 == 4)
      return List("four")
    else if (n % 10 == 5)
      return List("five")
    else if (n % 10 == 6)
      return List("six")
    else if (n % 10 == 7)
      return List("seven")
    else if (n % 10 == 8)
      return List("eight")
    else
      return List("nine")
  }

    if(n%10 == 1)
      return fullWords(n/10):::List("one");
    else if(n%10 == 0)
      return fullWords(n/10):::List("zero")
    else if(n%10 == 2)
      return fullWords(n/10):::List("two")
    else if(n%10 == 3)
      return fullWords(n/10):::List("three")
    else if(n%10 == 4)
      return fullWords(n/10):::List("four")
    else if(n%10 == 5)
      return fullWords(n/10):::List("five")
    else if(n%10 == 6)
      return fullWords(n/10):::List("six")
    else if(n%10 == 7)
      return fullWords(n/10):::List("seven")
    else if(n%10 == 8)
      return fullWords(n/10):::List("eight")
    else
      return fullWords(n/10):::List("nine")
}
def latticepath(m: Int, n: Int):Int ={

  if(m==1)
    return n+1;
  else if(n==1)
    return m+1;
  else
    return (latticepath(m-1,n)+latticepath(m,n-1));
}
def factorial(n: Int): Int ={
  n match{
    case 1 => 1;
    case _ => n * factorial(n-1);
  }
}
def lexicographic(n1: Int): Int ={

  var xs = List(0,1,2,3,4,5,6,7,8,9)
  var num = 0
  var count = xs.length;
  var j = 0
  var n =n1
  var i = 0

  for ( i <- 1 to 9  ) {
    j = n / factorial(10 - i);
    n = n % factorial(10 - i);
    num = num + xs(j);
    xs = xs.filterNot(xs.indexOf(_) == j)
    count -= 1
    if (n == 0)
      break
  }

  for ( l <- 0 to xs.length-1)
  {
    num = num + xs(l);
  }

  return num;
}

// Tests

assert(last(List(1,2,3,4,5)) == 5);
assert(penultimate(List(1,2,3,4,5)) == 4);
assert(reverse(List(1,23,3,43)) == List(43,3,23,1));
assert(compress(List('a','a','a','b','b','a','a','c','c','d')) == List('a','b','a','c','d'));
assert(expand(2, List("a","b","a","c","d")) == List("a","a","b","b","a","a","c","c","d","d"));
assert(drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))==List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'));
assert(slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))==List('d', 'e', 'f', 'g'));
assert(listPrimesinRange(7 ,31) == List(7, 11, 13, 17, 19, 23, 29, 31));
assert(multipleof3or5(10)== 23);
assert(fullWords(123) == List("one","two","three"));
assert(latticepath(20,10) == 30045015);
assert((lexicographic(3)) == 123456987);
