ABOUT:
So, this is my final for CSC3/458, Symbolic Programming. I decided to make Texas Hold 'Em poker in Common Lisp, with one AI opponent to play against. It turned out a bit harder than I expected, so there are still a couple bugs left to fix.

HOW TO PLAY:
For each round, type in the name of the round. (e.g.
(action)
(flop)
(turn)
(river)
(final-bets)
) If the two players are still left after final bets, type in (evaluate-hand) to see who wins. 

TO DO:
-fix bugs related to betting
-convert game to CLOS
-improve AI code/convert to agent