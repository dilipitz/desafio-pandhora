# Instructions

Each part of the challenge is on their respective folder as instructed on the challenge pdf. 

Every and each part is in their own, self contained haskell project, created with `stack new [projectname]`.

In order to run each part, one must run the project with `stack run [projectname]`, replacing `[projectname]` with either `desafio1`, `desafio2` or `desafio3`.

most functions are documented with haddock commentary, therefore it is possible to run `stack haddock --open [projectname]` for a better overview over the code.

for every project, i chose to implement the input and output interface as a REST API, so in order to use the functions you must open the browser on `localhost:8080` to use the functionality.

also for `desafio1` and `desafio3` i was able to make a swagger UI available to play with the functionality. It's possible to access the swagger ui via [this url](http://localhost:8080/swagger-ui).

unfortunately on `desafio2` i was not able generate a swagger-ui, because of [this issue](https://github.com/haskell-servant/servant-swagger/issues/106).

to facilitate testing the rest interface, i've made the following table:

| Endpoint                 | linux command | params |
| ------------------------ | ------  | --- |
| /calculate-daily-returns | curl -v -F upload=@../prices.csv localhost:8080/calculate-daily-returns | None |
| /calcluate-daily-returns/from-dates | curl -v -F upload=@../prices.csv localhost:8080/calculate-daily-returns/from-dates -F day1="02/01/2022" -F day2="05/01/2022" | `day1` and `day2` with the format `dd/mm/yyyy` |
| /calculate-weekly-returns | curl -v -F upload=@../prices.csv localhost:8080/calculate-weekly-returns | None |

one must execute the linux commands above from inside `desafio2` folder.

