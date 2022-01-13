//Aleksandra Wolska

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Random


object lista7 {

  //1. Zrównoleglenie Quicksort. Pomiarywykazały, że dla dużego rozmiaru sortowanej tablicy zrównoleglenie wpływa korzystnie na szybkość wykonywania programu.
  // Dla małych wartości program zrównoleglony nie wykazuje żadnego przyspieszenia, a wręcz - zwalnia.
  //Wyniki są powtarzalne


  def swap (tab: Array[Int], i: Int, j: Int): Unit =
    val aux = tab(i)
    tab(i) = tab(j)
    tab(j) = aux

  def choose_pivot (tab: Array[Int], m: Int, n: Int): Int =
    tab((m + n)/2)

  def partition (tab: Array[Int], l: Int, r: Int): (Int, Int) =
    var i = l
    var j = r
    val pivot = choose_pivot(tab, l, r)
    while i <= j do
      while tab(i) < pivot do i += 1
      while pivot < tab(j) do j -= 1
      if i <= j then
        swap(tab, i, j)
        i += 1
        j -= 1
    (i,j)

  def quick (tab: Array[Int], l: Int, r: Int): Unit =
    if l < r then
      val (i, j) = partition(tab, l, r)
      if (j - l) < (r - i) then
        quick(tab, l, j)
        quick(tab, i, r)
      else
        quick(tab, i, r)
        quick(tab, l, j)


  def quickSort(tab: Array[Int]): Unit =
    quick(tab, 0, tab.length - 1)


  def quickParallel(tab: Array[Int],l: Int, r: Int): Unit = {
    if (l < r) {
      val (i, j) = partition(tab,l,r)
      if (j - l < r - i) {
        val future1 = Future{quick(tab, l, j)}
        val future2 = Future{quick(tab, i, r)}
        Await.result(future1, Duration.Inf)
        Await.result(future2, Duration.Inf)
      }
      else {
        val future1 = Future{quick(tab, i, r)}
        val future2 = Future{quick(tab, l, j)}
        Await.result(future1, Duration.Inf)
        Await.result(future2, Duration.Inf)
      }
    }
  }

  def quickSortButParallel(tab: Array[Int]): Unit =
    quickParallel(tab, 0, tab.length - 1)



  //Zrównoleglenie funkcji do obliczania liczb pierwszych, by każdy wątek sprawdzał co drugą liczbę, czy modulo wychodzi zero.
  // Rezultaty były zdecydownie poniżej oczekiwań, dla niewielkich liczb nastąpiła spore spowolnienie, przy dużych liczbach wyniki funkcji zrównoleglonej
  //wychodzą podobne do tych bez współbieżności. Nie warto.

  def isGivenNumberThePrimeNumber(number: Long): Boolean =
    def helper(num: Long, counter: Long): Boolean =
      if counter == 1
      then true
      else if num % counter == 0
      then false
      else helper(num, counter - 1)

    if number > 0
    then helper(number, number - 1)
    else false



  def isGivenNumberThePrimeNumberButParallel(number: Long): Boolean =
    def helper(num: Long, counter: Long): Boolean =
      if (counter == 1) || (counter == 0)
      then true
      else if num % counter == 0
      then false
      else helper(num, counter - 2)

    if number < 0
    then false
    else
      val future1 = Future{helper(number, number - 1)}
      val future2 = Future{helper(number, number - 2)}
      Await.result(future1, Duration.Inf)
      Await.result(future2, Duration.Inf)












  def quickSortTest(): Unit = {

    val _10_2qs = Array.fill(100)(scala.util.Random.nextInt(10000))
    val _10_2qsp = _10_2qs.clone()

    val _10_3qs = Array.fill(1000)(scala.util.Random.nextInt(10000))
    val _10_3qsp = _10_3qs.clone()

    val _10_5qs = Array.fill(100000)(scala.util.Random.nextInt(10000))
    val _10_5qsp = _10_5qs.clone()

    val _10_6qs = Array.fill(1000000)(scala.util.Random.nextInt(10000))
    val _10_6qsp = _10_6qs.clone()

    val _10_7qs = Array.fill(10000000)(scala.util.Random.nextInt(10000))
    val _10_7qsp = _10_7qs.clone()

    val _10_8qs = Array.fill(100000000)(scala.util.Random.nextInt(10000))
    val _10_8qsp = _10_8qs.clone()


    println("QuickSort test 10^5 elements")

    print("No parallel: ")
    var before = System.nanoTime()
    quickSort(_10_5qs)
    var after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")

    print("Parallel: ")

    before = System.nanoTime()
    quickSortButParallel(_10_5qsp)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")
    println()


    println("QuickSort test 10^6 elements")

    print("No parallel: ")
    before = System.nanoTime()
    quickSort(_10_6qs)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")

    print("Parallel: ")

    before = System.nanoTime()
    quickSortButParallel(_10_6qsp)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")
    println()




    println("QuickSort test 10^7 elements")

    print("No parallel: ")
    before = System.nanoTime()
    quickSort(_10_7qs)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")

    print("Parallel: ")

    before = System.nanoTime()
    quickSortButParallel(_10_7qsp)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")
    println()


    println("QuickSort test 10^8 elements")

    print("No parallel: ")
    before = System.nanoTime()
    quickSort(_10_8qs)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")

    print("Parallel: ")

    before = System.nanoTime()
    quickSortButParallel(_10_8qsp)
    after = System.nanoTime()
    println(" " + (after - before)/1000000 + "miliseconds")
    println()


  }




  def primeNumsTest(): Unit = {

    println("Dla 100")
    print("No parallel: ")
    var before = System.nanoTime()
    isGivenNumberThePrimeNumber(100)
    var after = System.nanoTime()
    println(" " + (after - before) + " nanoseconds")


    print("Parallel: ")
    before = System.nanoTime()
    isGivenNumberThePrimeNumberButParallel(100)
    after = System.nanoTime()
    println(" " + (after - before) + " nanoseconds")
    println()

    println("Dla 2000")
    print("No parallel: ")
    before = System.nanoTime()
    isGivenNumberThePrimeNumber(2000)
    after = System.nanoTime()
    println(" " + (after - before) + " nanoseconds")

    print("Parallel: ")
    before = System.nanoTime()
    isGivenNumberThePrimeNumberButParallel(2000)
    after = System.nanoTime()
    println(" " + (after - before) + " nanoseconds")
    println()

    println("Dla 10000000")
    print("No parallel: ")
    before = System.nanoTime()
    isGivenNumberThePrimeNumber(100000000)
    after = System.nanoTime()
    println(" " + (after - before)/1000 + " microseconds")

    print("Parallel: ")
    before = System.nanoTime()
    isGivenNumberThePrimeNumberButParallel(100000000)
    after = System.nanoTime()
    println(" " + (after - before)/1000 + " microseconds")
    println()
  }



  def main(args: Array[String]): Unit = {


    println("QuickSort test")
    quickSortTest()
    println()

    println("Prime nums test")
    primeNumsTest()
    println()

  }

} 