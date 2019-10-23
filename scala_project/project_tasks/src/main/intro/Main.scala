object Hello extends App {
  println("Hello World!")
  var aList = new Array[Int](50)

  for (i <- 0 to 49) {
    aList(i) = i+1;
  }

  def add(x: Array[Int]): Int = {
    var tot = 0
    for (i <- 0 to (x.length - 1)){
      tot += x(i)
    }
    tot
  }
  //println(add(aList))

  def recursiveAdd(x: Array[Int]): Int = {
    if (x.length < 1) {
      return 0
    } else {
      return x(0) + recursiveAdd(x.tail)
    }
  }
  //println(recursiveAdd(aList))

  // INT is 32-bit long, BIGINT is 64-bit long

  def fibonacci(x: BigInt): BigInt = {
    if (x == 0 || x == 1) {
      return x
    } else {
      return fibonacci(x-1) + fibonacci(x-2)
    }
  }
  //println(fibonacci(10))

  def printStart(): Unit = { println ("Started...")}

  def createThread(callback: () => Unit): Thread = {
    val thread = new Thread {
      override def run() = callback()
    }
    thread
  }
  //println(createThread(printStart))

  private var counter: Int = 0
  def increaseCounter() =  {
    counter += 1
  }
  def printCounter() : Unit = {
    println(counter)
  }

  createThread(increaseCounter).start
  createThread(increaseCounter).start
  createThread(printCounter).start
  // b) This phenomenon is called race condition. This can be a problem when we expect eg. an ID to be atomic.

  // c)
    def atomicIncreaseCounter() = this.synchronized {
    counter += 1
  }

  // d)
  // DEADLOCK: A situation in which two or more executions wait for each other to complete an action before proceeding
  // with their own action.
  // To prevent this one can establish a total order between resources when acquiring them. Doing this ensures that no
  // set of threads wait on the resources they previously acquired.

  lazy val base = 42
  lazy val start = 42
  lazy val step = 52

  def printStart2() = {
    start.synchronized{
      println(start)
      Thread.sleep(1000)
      step.synchronized{
        println(step)
      }
    }
  }

  def printStep() = {
    step.synchronized{
      println(step)
      Thread.sleep(1000)
      start.synchronized{
        println(start)
      }
    }
  }

  val t1 = createThread(printStart2)
  val t2 = createThread(printStep)
  t1.start
  t2.start
  t1.join()
  t2.join()

}