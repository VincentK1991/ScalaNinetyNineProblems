package runtime

import arithmetic.Arithmetic // import

//implicit def intToArithmetic(x:Int) = new Arithmetic(x)
object Main extends App {
  implicit def intToArithmetic(x:Int) = new Arithmetic(x)
  println("Hello please give me integer")
  val item = scala.io.StdIn.readInt()
  val number = new Arithmetic(item)
  // example
  if (number.isPrime){
    println("the integer is prime")
  } else {
    println("the integer is not prime")
    println("the prime factor of this integer is ...")
    number.primeFactors.foreach{println}
  }
}
