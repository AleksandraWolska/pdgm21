// Aleksandra Wolska

object Lista5 {

  //Zadanie 1 - podczas każdego wywołania helpera funkcja dzieli (stały), modulo (stały) i dodaje element (stała).
  // Wywołań będzie tyle, ile razy musimy podzielić wejściową liczbę, przez system. Będzie to zlożonośc liniowa

  def toHexadecimal(decimalNum: Int): List[Int] =
    def helper(temp: Int, list: List[Int]): List[Int]=
      if(temp < 16)
        then temp::list
      else helper((temp / 16), (temp % 16)::list)
    helper(decimalNum.abs, List())



  def toAnySystem(decimalNum: Int, system: Int): List[Int] =
    def helper(temp: Int, system: Int, list: List[Int]): List[Int]=
      if(temp < system)
      then temp::list
      else helper((temp / system), system, (temp % system)::list)
    if (system != 0)
    then helper(decimalNum.abs, system, List())
    else List()



//Zadanie 3

  sealed trait Tree[+A]
  case object Empty extends Tree[Nothing]
  case class Node[+A](elem:A, left:Tree[A], right:Tree[A]) extends Tree[A]


  def generate(depth: Int): Tree[Float] = {
    if (depth > 0)
      then Node(util.Random.nextFloat(), generate(depth-1), generate(depth-1))
    else Empty
  }

  //Zadanie 4

  def productOfTree(tree: Tree[Float]): Float =
    def helper(tempTree: Tree[Float], product: Float): Float =
      tempTree match {
        case Empty => 1
        case Node(value, left, right) => product * value * helper(left, product) * helper(right, product)
      }
    helper(tree, 1);


  //Zadanie 5 - tylko przejście wszerz
  def breadthBT[Float](tree: Tree[Float]) = {
    def helper[Float](nodesQueue: List[Tree[Float]]): List[Float] =
      nodesQueue match
        case Nil => Nil
        case Empty :: tail => helper(tail)
        case Node(nodeValue, leftST, rightST) :: tail =>
          nodeValue :: helper(tail ::: List(leftST, rightST))
    helper (List(tree))
  }


  def main(args: Array[String]): Unit = {

    println("\nSystem 16")

    println(toHexadecimal(31))
    println(toHexadecimal(1054))
    println(toHexadecimal(-1054))
    println(toHexadecimal(0))
    println(toHexadecimal(1))
    println(toHexadecimal(16))

    println("\nDowolny system")
    println(toAnySystem(1054, 16))
    println(toAnySystem(8, 2))
    println(toAnySystem(15, 2))
    println(toAnySystem(2, 0))

    println("\nGenerowanie drzewa")
    println(generate(0))
    val tt = generate(3)
    println(tt)

    println("\nIloczyn drzewa")
    println(productOfTree(tt))


    println("\nPrzejście wszerz")
    println(breadthBT(tt))

  }

}




