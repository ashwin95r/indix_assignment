class fraction(a: Int, b: Int){
  var num: Int = a
  var den: Int = b
  def this() = this(1,1)
}

def hcfOfTwoNumber(a: Int, b: Int): Int = {
  if(a%b == 0 )
    b
  else
    hcfOfTwoNumber(b, a % b)
}

def fraction_simplify(a: fraction): fraction={
  val c = hcfOfTwoNumber(a.num, a.den)
  val d = new fraction(a.num/c, a.den/c)
  d
}

def fraction_multiply(a: fraction, b: fraction): fraction={
  val n: Int= a.num * b.num
  val d: Int= a.den * b.den
  val c = new fraction(n,d)
  c
}
def fraction_divide(a: fraction, b: fraction): fraction={
  val n: Int= a.num * b.den
  val d: Int= a.den * b.num
  val c = new fraction(n,d)
  c
}
def addEntryGraph(matrix: Array[fraction], v1: fraction, n1: Int, v2: fraction, n2: Int): Array[fraction] = {
  matrix(n1*100+n2) = fraction_divide(v2, v1)
  matrix(n2*100+n1) = fraction_divide(v1, v2)
  matrix
}
def dfs(from: Int, to: Int, visited: List[Int],matrix: Array[fraction], curr_frac: fraction): fraction={
  val _visited: List[Int] = visited:::List(from)
  (from, to) match{
    case (_,_) if matrix(from*100+to) != null =>
      fraction_multiply(matrix(from * 100 + to), curr_frac)
    case (_,_) =>
      for(i <- from*100 to from*100 +100) {
        if (matrix(i) != null && _visited.find(x => x == i%100).isEmpty){
          return dfs(i%100, to, _visited, matrix, fraction_multiply(curr_frac,matrix(i)))
        }
      }
      new fraction(-10,1)
  }
}
def normalMap(input:List[String]):Map[String,Int] ={
  val output = mainFunction(input).distinct
  val listindex =1 to output.length
  (output zip listindex).toMap
}

def reverseMap(input:List[String]):Map[Int,String] ={
  val output = mainFunction(input).distinct
  val listindex =1 to output.length
  (listindex zip output ).toMap
}

def mainFunction(input:List[String]):List[String] ={
  input match{
    case a::Nil =>
      val tmp = a.split(" ")
      if(tmp(0) == "!"){
        List(tmp(2),tmp(5))
      }
      else
        List("")
    case a::b =>
      val tmp = a.split(" ")
      if(tmp(0) == "!"){
        List(tmp(2),tmp(5)) ++ mainFunction(b)
      }
      else
        List("")
  }
}

def generateList(input:List[String]):List[List[String]] ={
  input match{
    case a::Nil =>
      val tmp = a.split(" ")
      if(tmp(0) == "!"){
        List(List(tmp(1) ,tmp(2), tmp(4), tmp(5)))
      }
      else
        Nil
    case a::b =>
      val tmp = a.split(" ")
      if(tmp(0) == "!"){
        List(List(tmp(1) ,tmp(2), tmp(4), tmp(5))) ++ generateList(b)
      }
      else
        Nil ++ generateList(b)
  }
}

def generateQuestionList(input:List[String]):List[List[String]] ={
  input match{
    case a::Nil =>
      val tmp = a.split(" ")
      if(tmp(0) == "?"){
        List(List(tmp(1), tmp(3)))
      }
      else
        Nil
    case a::b =>
      val tmp = a.split(" ")
      if(tmp(0) == "?"){
        List(List(tmp(1) ,tmp(3))) ++ generateQuestionList(b)
      }
      else
        Nil ++ generateQuestionList(b)
  }
}

def main(inputList: List[String])  = {
  var matrix: Array[fraction] = new Array[fraction](12000)
  val mapOfitemsToIndex = normalMap(inputList)
  val mapOfIndexToItems = reverseMap(inputList)
  val informationList = generateList(inputList)
  val QuestionList = generateQuestionList(inputList)
  for(i <- 1 to informationList.length)
    matrix = addEntryGraph(matrix, new fraction(informationList(i-1)(0).toInt ,1), mapOfitemsToIndex(informationList(i-1)(1)),new fraction(informationList(i-1)(2).toInt,1), mapOfitemsToIndex(informationList(i-1)(3)))

  for( i <- 0 to QuestionList.length-1) {
    val from = mapOfitemsToIndex(QuestionList(i)(0))
    val to = mapOfitemsToIndex(QuestionList(i)(1))
    val visited = List[Int]()
    val final_frac_not_simplified = dfs(from, to, visited, matrix, new fraction(1, 1))
    val final_frac = fraction_simplify(final_frac_not_simplified)
    if(final_frac.num > 0)
      println(final_frac.den + " " + mapOfIndexToItems(from) + " = " + final_frac.num + " " + mapOfIndexToItems(to))
    else
      println("? " + mapOfIndexToItems(from) + " = ? " + mapOfIndexToItems(to))
  }
}
main(List("! 6 apple = 15 banana","! 47 grape = 9 mango","! 2 banana = 1 grape","? banana = apple","? mango = apple"))






