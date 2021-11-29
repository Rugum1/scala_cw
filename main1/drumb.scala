// Main Part 1 about a really dumb investment strategy
//===================================================

object M1 {

//two test portfolios

val blchip_portfolio = List("GOOG", "AAPL", "MSFT", "IBM", "FB", "AMZN", "BIDU")
val rstate_portfolio = List("PLD", "PSA", "AMT", "AIV", "AVB", "BXP", "CCI", 
                            "DLR", "EQIX", "EQR", "ESS", "EXR", "FRT", "HCP") 


// (1) The function below takes a stock symbol and a year as arguments.
//     It should read the corresponding CSV-file and then extract the January 
//     data from the given year. The data should be collected in a list of
//     strings (one entry for each line in the CSV-file).

import io.Source
import scala.util._

def get_january_data(symbol: String, year: Int) : List[String] =
{
    
 Source.fromFile("./" ++ symbol ++ ".csv").getLines.toList.filter(element => element.startsWith(year.toString + "-01") )
    
}

// (2) From the output of the get_january_data function, the next function 
//     should extract the first line (if it exists) and the corresponding
//     first trading price in that year with type Option[Double]. If no line 
//     is generated by get_january_data then the result is None; and Some if 
//     there is a price.

def get_first_price(symbol: String, year: Int) : Option[Double] = 
{
   
   if(get_january_data(symbol,year).length == 0)
   {
       None 
   }
   else
   {   
       val firstElementInList = get_january_data(symbol,year).head.split(',').toList 
       val price = firstElementInList.lift(1).get

       Some(price.toDouble)
   }

     
}


// (3) Complete the function below that obtains all first prices
//     for the stock symbols from a portfolio (list of strings) and 
//     for the given range of years. The inner lists are for the
//     stock symbols and the outer list for the years.


def get_prices(portfolio: List[String], years: Range) : List[List[Option[Double]]] = ???



// (4) The function below calculates the change factor (delta) between
//     a price in year n and a price in year n + 1. 

def get_delta(price_old: Option[Double], price_new: Option[Double]) : Option[Double] = ???



// (5) The next function calculates all change factors for all prices (from a 
//     portfolio). The input to this function are the nested lists created by 
//     get_prices above.

def get_deltas(data: List[List[Option[Double]]]) :  List[List[Option[Double]]] = ???



// (6) Write a function that given change factors, a starting balance and an index,
//     calculates the yearly yield, i.e. new balance, according to our dumb investment 
//     strategy. Index points to a year in the data list.

def yearly_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = ???


// (7) Write a function compound_yield that calculates the overall balance for a 
//     range of years where in each year the yearly profit is compounded to the new 
//     balances and then re-invested into our portfolio. For this use the function and 
//     results generated under (6). The function investment calls compound_yield
//     with the appropriate deltas and the first index.

def compound_yield(data: List[List[Option[Double]]], balance: Long, index: Int) : Long = ???

def investment(portfolio: List[String], years: Range, start_balance: Long) : Long = ???




//Test cases for the two portfolios given above

//println("Real data: " + investment(rstate_portfolio, 1978 to 2019, 100))
//println("Blue data: " + investment(blchip_portfolio, 1978 to 2019, 100))


}
