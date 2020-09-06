package arithmetic

class Arithmetic(n:Int){
    require(n > 0)  // add requirement to the primary constructor
    println("create arithmetic = "+ n) // print every time we create a new arithmetic object
    val num = n  // public field is accessible using .num
    private val primes = primeStream(Stream.from(2)) //private field can only be access inside class body but not outside

    /*
    this is a self-reference used to acess field within the object
    For example, this.num accesses the num constructor
    */
    def isPrime = !(2 to this.num - 1).map(this.num % _ == 0).toList.contains(true)

    def gcd(num1:Int,num2:Int):Int = {
        val small = num1 min num2
        val big = num1 max num2
        val remainder = big % small
        if (remainder == 0){
            //println(small)
            return small
        }
        else {
        return gcd(small, remainder)
        }
    }

    def isCoprimeTo(num2:Int):Boolean = {
        val result = gcd(this.num,num2)
        if (result == 1){
            return true
        }
        else{
            return false
        }
    }


    def totient = (1 to this.num).toList.filter(isCoprimeTo(_)).size
    // Note that we don't need to call this.num.isCoprimeTo because isCoprimeTo already has this.num inside

    private def primeStream(s:Stream[Int]):Stream[Int] = {
        Stream.cons(s.head, primeStream(s.tail filter {_ % s.head != 0}))
    } // private method can only be access inside class

    def primeFactors:List[Int] = {
        val listPrime = this.primes.take(this.num).toList
        var temp = this.num
        var index = 0
        var list_factor = List.empty[Int]
        while (temp.toFloat / listPrime(index) != 1.0) {
            if (index > 100){
                println("PRIME FACTORS TOO BIG EXIT NOW!!")
                return list_factor //break
            }
        if (temp % listPrime(index) == 0){
            list_factor = list_factor ::: listPrime(index) :: Nil
            temp = temp / listPrime(index)
            }
        else {
            index = index + 1
            }
        }
        list_factor = list_factor ::: listPrime(index) :: Nil
        return list_factor
    }

    def primeFactorMultiplicity:Map[Int,Int] = primeFactors.groupBy(i => i).mapValues(_.size).toMap
    /*
    similar to the totient function, when we call primeFactors it already knows to execute with this.num
    this is quite unintuitive if one comes from Python
    it executes and return the list_factor:list[Int] directly so we can
    */

    def phi:Int = primeFactorMultiplicity.keys.zip(primeFactorMultiplicity.values).map{
                    case(x$1,x$2) => (x$1-1)*math.pow((x$1),(x$2 - 1))}.foldLeft(1.0)(_*_).toInt

    def listPrimesinRange(range:Range):List[Int] = primes.take(range.max).filter{case(x$1) => (x$1< range.max) && ( x$1 > range.min)}.toList

    def goldbach:List[List[Int]] = primes.take(this.num).filter( _ < this.num).toList.combinations(2).toList.filter{
                                    case(List(x$1,x$2)) => (x$1+x$2 == this.num)}
// end of class
}
