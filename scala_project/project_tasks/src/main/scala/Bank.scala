class Bank(val allowedAttempts: Integer = 3) {

    // initialize queues
    private val transactionsQueue: TransactionQueue = new TransactionQueue()
    private val processedTransactions: TransactionQueue = new TransactionQueue()

    // creates a new transaction object and pushes it to the transactionsQueue
    def addTransactionToQueue(from: Account, to: Account, amount: Double): Unit = {
        val transaction = new Transaction(transactionsQueue, processedTransactions, from, to, amount, allowedAttempts)
        transactionsQueue.push(transaction)
        //spawns a thread that calls processTransactions
        val thread = new Thread {
            override def run: Unit = {
                processTransactions
            }
        }
        thread.start
    }

    /*
    Pops a transaction from the transactionsQueue, and runs it.
    If the transaction's status is still PENDING, it is put back to the transactionsQueue.
    If the transaction is FAILED or SUCCESS, it is pushed into the processedTransactions queue.
     */
    private def processTransactions: Unit = this.synchronized {
        val transaction = transactionsQueue.pop
        val thread = new Thread {
            override def run = {
                transaction.run()
                if (transaction.status == TransactionStatus.PENDING) {
                    transactionsQueue.push(transaction)
                    processTransactions
                } else {
                    processedTransactions.push(transaction)
                }
            }
        }
        thread.start
    }

    //Creates a new Account object.
    def addAccount(initialBalance: Double): Account = {
        new Account(this, initialBalance)
    }

    //Gets processed transactions and returns them as a list.
    def getProcessedTransactionsAsList: List[Transaction] = {
        processedTransactions.iterator.toList
    }

}
