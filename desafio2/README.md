# desafio2

não foi possível implementar o swagger para esse projeto porque a lib servant-multipart não suporta o swagger e vice versa ([issue](https://github.com/haskell-servant/servant-swagger/issues/106)).

por causa disso disponibilizarei os comandos curl para manipular a api neste README.md

| Endpoint                 | comando linux |
| ------------------------ | ------  |
| /calculate-daily-returns | curl -v -F upload=@../prices.csv localhost:8080/calculate-daily-returns |
| /calcluate-daily-returns/from-dates | curl -v -F upload=@../prices.csv localhost:8080/calculate-daily-returns/from-dates -F day1="02/01/2022" -F day2="05/01/2022" |
| /calculate-weekly-returns | curl -v -F upload=@../prices.csv localhost:8080/calculate-weekly-returns |