def mainFunction(begin : Int, end : Int,blockedChannels : List[Int],requiredChannels : List[Int]):Int = {
  val allChannels = (begin to end).toList
  val accessibleChannels = dropAllElementsInList(allChannels,blockedChannels)
  noOfDigitsInNumber(requiredChannels(0),0)+minClicksFromStartingChannel(requiredChannels,accessibleChannels,-1)
}
//
//
//minClicksFromStartingChannel TESTED AND WORKING
//Last input is -1
def minClicksFromStartingChannel(input :List[Int],accessibleChannels : List[Int],channelNoOfBack:Int):Int={
  input match{
    case a::b::Nil =>
      val usingNumbers = noOfDigitsInNumber(b,0)
      val indexOfa = indexOfElementInList(accessibleChannels,a,-1)
      val indexOfb = indexOfElementInList(accessibleChannels,b,-1)
      val usingButtonsInOneDirection = modulus(indexOfa - indexOfb)
      val usingButtonsInOtherDirection = accessibleChannels.length- modulus(indexOfa - indexOfb)
      if (b == channelNoOfBack)
        1
      else
        math.min(usingButtonsInOtherDirection,math.min(usingButtonsInOneDirection,usingNumbers))
    case a::b::c =>
      val usingNumbers = noOfDigitsInNumber(b,0)
      val indexOfa = indexOfElementInList(accessibleChannels,a,-1)
      val indexOfb = indexOfElementInList(accessibleChannels,b,-1)
      val usingButtonsInOneDirection = modulus(indexOfa - indexOfb)
      val usingButtonsInOtherDirection = accessibleChannels.length- modulus(indexOfa - indexOfb)
      if (b == channelNoOfBack)
        1 + minClicksFromStartingChannel(List(b)++c,accessibleChannels,a)
      else
        math.min(usingButtonsInOtherDirection,math.min(usingButtonsInOneDirection,usingNumbers)) +
        minClicksFromStartingChannel(List(b)++c,accessibleChannels,a)
  }
}
//minClicksFromStartingChannel(List(15,14,17,1,17) , List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,20),-1)
//minClicksFromStartingChannel(List(105,106,107,103,105),List(103,105,106,107,108),-1)
//
//
//modulus TESTED AND WORKING
def modulus(input : Int):Int ={
  if (input < 0)
    -1*input
  else
    input
}
//modulus(3)
//modulus(0)
//modulus(-5)
//
//
// noOfDigitsInNumber TESTED AND WORKING
// Last Input is Zero
def noOfDigitsInNumber(number : Int,noOfDigits:Int): Int ={
  if(number/10 > 0)
    noOfDigitsInNumber(number/10,noOfDigits+1)
  else
    noOfDigits+1
}
//noOfDigitsInNumber(1243,0)
//noOfDigitsInNumber(0,0)
//noOfDigitsInNumber(1000,0)
//
//
//indexOfElementInList TESTED AND WORKING
//Last Input is -1
//Returns -1 if Element not in list
def indexOfElementInList(input:List[Int],element : Int,indexOfElement : Int):Int ={
  input match{
    case a::Nil =>
      if(a == element)
        indexOfElement+1
      else
        -1
    case a::b =>
      if(a == element)
        indexOfElement+1
      else
        indexOfElementInList(b,element,indexOfElement+1)
    case Nil => -1
  }
}
//indexOfElementInList(List(1,3,7,2,9),1,-1)
//indexOfElementInList(List(1,3,7,2,9),7,-1)
//indexOfElementInList(List(1,3,7,2,9),9,-1)
//indexOfElementInList(List(1,3,7,2,9),8,-1)
//indexOfElementInList(List(4),4,-1)
//indexOfElementInList(List(1),4,-1)
//indexOfElementInList(List(),4,-1)
//
//
//dropAllElementsInList  TESTED AND WORKING
def dropAllElementsInList(input:List[Int],listToBeDropped: List[Int]):List[Int]= {
  listToBeDropped match {
    case a::Nil => dropByElement(input ,a)
    case a::b   => dropAllElementsInList(dropByElement(input,a),b)
    case Nil    => input
  }
}
//dropAllElementsInList(List(1,3,8,5,2),List(3,8))
//dropAllElementsInList(List(1,3,8,5,2),List(3,8,1,5,2))
//dropAllElementsInList(List(1,3,8,5,2),List())
//dropAllElementsInList(List(1,3,8,5,2),List(3,8,7))
//dropAllElementsInList(List(1,3,8,5,2),List(3,8,1,5,2,7))
//dropAllElementsInList(List(),List(3,8))
//
//
//dropByElement TESTED AND WORKING
def dropByElement(input:List[Int],valueToBeDropped: Int):List[Int]={
  input match{
    case a::Nil =>
      if(a == valueToBeDropped)
        List()
      else
        List(a)
    case a::b =>
      if(a == valueToBeDropped)
        dropByElement(b,valueToBeDropped)
      else
        List(a)++dropByElement(b,valueToBeDropped)
    case Nil => List()
  }
}
//dropByElement(List(1,4,8,2,7),4)
//dropByElement(List(1,4,8,2,7),5)
//dropByElement(List(),4)

mainFunction(1,9,List(2,4),List(1,3,7,9,1))
mainFunction(1,20,List(2,4,11),List(1,3,7,9,10))
mainFunction(11,20,List(12,14),List(11,13,17,19,20,11))
mainFunction(11,20,List(12,14),List(11,13,17,19,15,19))
