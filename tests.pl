:- ensure_loaded(cribbage).

test1 :-
    hand_value([card(7,clubs), card(queen, hearts),card(2,clubs),card(jack, clubs)], card(9,hearts), 0),
    hand_value([card(ace, spades), card(3,hearts),card(king, hearts),card(7,hearts)], card(king, spades), 2),
    hand_value([card(ace, spades), card(3,hearts),card(king, hearts),card(7,hearts)], card(2, diamonds), 5),
    hand_value([card(6,clubs), card(7,clubs),card(8,clubs),card(9,clubs)], card(8,spades), 20),
    hand_value([card(7,hearts), card(9,spades),card(8,clubs),card(7,clubs)], card(8,hearts), 24),
    hand_value([card(5,hearts), card(5,spades),card(5,clubs),card(jack,diamonds)], card(5,diamonds), 29),

    % my tests: most of these situations are techniccally impossible
    %hand_value([], card(5, hearts), 0),
    hand_value([card(jack,hearts)], card(5,hearts), 8),
    % [5,5,5,5,5,5,6,6,J,7,8,8,9,Q,A,2],K, all hearts
    % nob: 1, flush: 5, pairs: 34, runs: 123, fifteens: 146, total score: 309
    % 1     nob
    % 5     flush + start
    % 2     6 pair
    % 2     8 pair
    % 30    5 pairs 6C2 * 2 pts
    % 120   5>6>7>8>9 5 pts * (6*2*1*2*1)
    % 3     J>Q>K
    % 40    5,5,5 6C3 * 2 pts
    % 4     9,6 (1*2) * 2 pts
    % 4     8,7 (2*1) * 2 pts
    % 36    5,10 (6*3) * 2 pts
    % 8     8,1,6 (2*1*2) * 2 pts
    % 4     7,2,6 (1*1*2) * 2 pts
    % 12    9,1,5 (1*1*6) * 2 pts
    % 24    8,2,5 (2*1*6) * 2 pts
    % 12    5,7,1,2 (6*1*1*1) * 2 pts
    % 2     6,6,1,2 (1*1*1) * 2pts
    hand_value([card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(6, hearts), card(6, hearts), card(jack, hearts), card(7, hearts), card(8, hearts), card(8, hearts), card(9, hearts), card(queen, hearts), card(ace, hearts), card(2, hearts)], card(king, hearts), 309).

% run the test1 goal when this file is loaded.
:- test1.
