import scala.collection.mutable

object TransactionStatus extends Enumeration {
  val SUCCESS, PENDING, FAILED = Value
}

class TransactionQueue {

    // TODO
    // project task 1.1
    // Add datastructure to contain the transactions
    var transactions_queue = new mutable.Queue[Transaction]

  // Remove and return the first element from the queue
    def pop: Transaction = this.synchronized {
      val head = transactions_queue.dequeue()
      head
    }

    // Return whether the queue is empty
    def isEmpty: Boolean = this.synchronized {
      return transactions_queue.isEmpty;
    }

    // Add new element to the back of the queue
    def push(t: Transaction): Unit = this.synchronized {
      transactions_queue += t
    }

    // Return the first element from the queue without removing it
    def peek: Transaction = this.synchronized {
      transactions_queue.head
    }

    // Return an iterator to allow you to iterate over the queue
    def iterator: Iterator[Transaction] = this.synchronized {
      transactions_queue.iterator
    }
}

class Transaction(val transactionsQueue: TransactionQueue,
                  val processedTransactions: TransactionQueue,
                  val from: Account,
                  val to: Account,
                  val amount: Double,
                  val allowedAttemps: Int) extends Runnable {

  var status: TransactionStatus.Value = TransactionStatus.PENDING
  var attempt = 0

  override def run: Unit = {

      def doTransaction() = {
          // TODO - project task 3
          // Extend this method to satisfy requirements.
          this.attempt += 1

          val fromAccount = this.from
          val toAccount = this.to

          lazy val withdrawAmount = fromAccount.withdraw(amount)
          lazy val depositAmount = toAccount.deposit(amount)

          withdrawAmount match {
              case Right(string) => println(string)
              case Left(number) => depositAmount match {
                case Right(string) => println(string)
                case Left(number) => this.status = TransactionStatus.SUCCESS
              }
          }

          if(this.attempt>=this.allowedAttemps){
            this.status = TransactionStatus.FAILED
          }
      }

      // TODO - project task 3
      // make the code below thread safe
      if (status == TransactionStatus.PENDING) {
        this.synchronized{
            doTransaction
        }
        Thread.sleep(50) // you might want this to make more room for
        // new transactions to be added to the queue
      }
    }
}
