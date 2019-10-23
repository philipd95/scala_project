import exceptions._

class Account(val bank: Bank, initialBalance: Double) {

    class Balance(var amount: Double) {}

    val balance = new Balance(initialBalance)

    // TODO
    // for project task 1.2: implement functions
    // for project task 1.3: change return type and update function bodies
    def withdraw(amount: Double): Either[Double, String] =   this.synchronized {
        if (amount < 0) return Right("This is not a positive number")
        else if (amount > balance.amount) return Right("Funds not available")
        balance.amount -= amount
        Left(amount)
    }
    def deposit (amount: Double): Either[Double, String] = this.synchronized {
        if (amount < 0) return Right("This is not a positive number")
        balance.amount += amount
        Left(amount)

    }
    def getBalanceAmount: Double = this.synchronized {
        balance.amount
    }

    def transferTo(account: Account, amount: Double) = {
        bank addTransactionToQueue (this, account, amount)
    }


}
