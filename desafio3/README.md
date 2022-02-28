# desafio3

## Execute  

* Run `stack exec -- desafio3-exe` to see "We're inside the application!"
* With `stack exec -- desafio3-exe --verbose` you will see the same message, with more logging.

## Run tests

`stack test`

https://stackoverflow.com/questions/22900485/elegant-way-to-write-ordered-comparison-on-multiple-properties



# limit order

`Add Limit Order: O104, 12:03:18, INTC, BUY, 100, $33.75`

`Cancel Limit Order: 0104, 12:03:18, INTC, BUY, 100, $33.75`

| field name | value |
| --    | --    |
|orderID| O104  |
|Time   | 12:03:18|
|Security Symbol| INTC|
|Order Direction| Buy|
| limit price | $33.75 |
| size | 100 |


## sorting
### Bids (buying) 
bids are sorted by price in Descending Order

**params**
* orderID
* Time
* BidSize
* BidPrice

### Asks (selling)
asks are sorted by price in Ascending order

**params**

* orderID
* Time
* AskPrice
* AskSize

when either have the same price, they are sorted by the earliest time


# market order