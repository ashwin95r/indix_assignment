def mainFunction(begin : Int, end : Int,blockedChannels : List[Int],requiredChannels : List[Int]):Int = {
  val allChannels = (begin to end).toList
  val accessibleChannels = dropAllElementsInList(allChannels,blockedChannels)
  noOfDigitsInNumber(requiredChannels(0),0)+minClicksFromStartingChannel(requiredChannels,accessibleChannels,-1)
}

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

def modulus(input : Int):Int ={
  if (input < 0)
    -1*input
  else
    input
}

def noOfDigitsInNumber(number : Int,noOfDigits:Int): Int ={
  if(number/10 > 0)
    noOfDigitsInNumber(number/10,noOfDigits+1)
  else
    noOfDigits+1
}

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

def dropAllElementsInList(input:List[Int],listToBeDropped: List[Int]):List[Int]= {
  listToBeDropped match {
    case a::Nil => dropByElement(input ,a)
    case a::b   => dropAllElementsInList(dropByElement(input,a),b)
    case Nil    => input
  }
}

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

mainFunction(1,9,List(2,4),List(1,3,7,9,1))
mainFunction(1,20,List(2,4,11),List(1,3,7,9,10))
mainFunction(11,20,List(12,14),List(11,13,17,19,20,11))
mainFunction(11,20,List(12,14),List(11,13,17,19,15,19))
