Run as:
```
ruby prg.rb
```

I struggled most with Ruby's idea of lambdas and list iteration (each_slice).
I didn't notice that the board and the score is outputed more often than just at the end of the game which resulted in score being always 0.

I also chose a wrong algorithm to move the paddle (find out how it should have moved from previous failure).
Hacking into memory and synchronizing it with the ball was much easier.
