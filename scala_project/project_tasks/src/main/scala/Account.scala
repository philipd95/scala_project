import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    /*
    Function that withdraws an amount from the account
    Takes an amount of type double
    Returns Either[Double, String]
    */
    def withdraw(amount: Double): Either[Double, String] =   this.synchronized {
        if (amount < 0) return Right("This is not a positive number")
        else if (amount > balance.amount) return Right("Funds not available")
        balance.amount -= amount
        Left(amount)
    }
    /*
    Function that deposits an amount from the account
    Takes an amount of type double
    Returns Either[Double, String]
    */
    def deposit (amount: Double): Either[Double, String] = this.synchronized {
        if (amount < 0) return Right("This is not a positive number")
        balance.amount += amount
        Left(amount)

    }
    /*
    Gets the balance in the account
    Returns a Double
    */
    def getBalanceAmount: Double = this.synchronized {
        balance.amount
    }

    /*
    Adds a transaction the the transaction queue
    Takes an account of type Account and an amount of type Double
    */
    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
