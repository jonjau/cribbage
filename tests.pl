:- ensure_loaded(cribbage).

test1 :-
    hand_value([
        card(7,clubs), card(queen, hearts),
        card(2,clubs),card(jack, clubs)
        ], card(9,hearts), 0),
    hand_value([
        card(ace, spades), card(3,hearts),
        card(king, hearts),card(7,hearts)
        ], card(king, spades), 2),
    hand_value([
        card(ace, spades), card(3,hearts),
        card(king, hearts),card(7,hearts)
        ], card(2, diamonds), 5),
    hand_value([
        card(6,clubs), card(7,clubs),
        card(8,clubs),card(9,clubs)
        ], card(8,spades), 20),
    hand_value([
        card(7,hearts), card(9,spades),
        card(8,clubs),card(7,clubs)
        ], card(8,hearts), 24),
    hand_value([
        card(5,hearts), card(5,spades),
        card(5,clubs),card(jack,diamonds)
        ], card(5,diamonds), 29),

    hand_value([
        card(king,spades), card(5,diamonds),
        card(3,hearts), card(3,diamonds)
        ],card(jack,diamonds),6),
    hand_value([
        card(3,hearts), card(queen,diamonds),
        card(6,clubs), card(ace,clubs)
        ],card(ace,hearts),4),
    hand_value([
        card(9,hearts), card(3,hearts),
        card(10,spades), card(9,clubs)
        ],card(7,clubs),2),
    hand_value([
        card(6,spades), card(6,clubs),
        card(3,spades), card(4,hearts)
        ],card(2,spades),11),
    hand_value([
        card(jack,clubs), card(king,diamonds),
        card(9,spades), card(2,hearts)
        ],card(7,diamonds),0),

    hand_value([
        card(ace, hearts), card(2, clubs),
        card(3, diamonds), card(jack, hearts)
        ], card(king, hearts), 8),

    select_hand([
            card(10,spades),card(jack,diamonds),card(jack,spades),
            card(2,hearts),card(7,diamonds)
        ],
        [
            card(10, spades), card(jack, diamonds),
            card(jack, spades), card(2, hearts)
        ],
        [card(7, diamonds)]),
    
    select_hand([
            card(jack,hearts),card(5,hearts),card(5,diamonds),
            card(2,diamonds),card(3,clubs)
        ],
        [
            card(jack, hearts), card(5, hearts),
            card(5, diamonds), card(3, clubs)
        ],
        [card(2, diamonds)]),

    select_hand([
            card(king,diamonds),card(4,diamonds),card(6,spades),
            card(10,spades),card(7,spades)
        ],
        [
            card(king, diamonds), card(4, diamonds),
            card(6, spades), card(7, spades)
        ],
        [card(10, spades)]),

    select_hand([
            card(4,diamonds),card(5,diamonds),card(3,diamonds),
            card(6,diamonds),card(10,spades)
        ],
        [
            card(4, diamonds), card(5, diamonds),
            card(3, diamonds), card(6, diamonds)
        ],
        [card(10, spades)]),

    select_hand([
            card(king,clubs),card(7,clubs),card(jack,clubs),
            card(2,hearts),card(8,spades)
        ],
        [
            card(7, clubs), card(jack, clubs),
            card(2, hearts), card(8, spades)
        ],
        [card(king, clubs)]),

    % also works when 6 cards are dealt
    select_hand([
        card(king,clubs),card(7,clubs),card(jack,clubs),
        card(2,hearts),card(8,spades),card(jack, spades)
    ],
    [
        card(7, clubs), card(jack, clubs),
        card(8, spades), card(jack, spades)
    ],
    [card(king, clubs), card(2, hearts)]),



    select_hand([
        card(5, clubs), card(5, diamonds), card(5, hearts),
        card(3, hearts), card(2, hearts), card(ace, hearts)
    ],
    [
        card(5, clubs), card(5, diamonds),
        card(5, hearts), card(3, hearts)
    ],
    [card(2, hearts), card(ace, hearts)]).


% run the test1 goal when this file is loaded.
:- test1.

/*
an interesting test: we keep 5s because they are really good (for 15s)
select_hand([
    card(3,clubs), card(3,diamonds),card(3,hearts),
    card(5,clubs), card(5, hearts), card(5, diamonds)
    ],X, Y).
X = [card(3, clubs), card(5, clubs), card(5, hearts), card(5, diamonds)],
Y = [card(3, diamonds), card(3, hearts)] ;
X = [card(3, diamonds), card(5, clubs), card(5, hearts), card(5, diamonds)],
Y = [card(3, clubs), card(3, hearts)] ;
X = [card(3, hearts), card(5, clubs), card(5, hearts), card(5, diamonds)],
Y = [card(3, clubs), card(3, diamonds)] ;

4s still better for 15s than 7s, so keep them
select_hand([
    card(7,clubs), card(7,diamonds),card(7,hearts),
    card(4,clubs), card(4, hearts), card(4, diamonds)
    ],X, Y).
X = [card(7, clubs), card(4, clubs), card(4, hearts), card(4, diamonds)],
Y = [card(7, diamonds), card(7, hearts)] ;
X = [card(7, diamonds), card(4, clubs), card(4, hearts), card(4, diamonds)],
Y = [card(7, clubs), card(7, hearts)] ;
X = [card(7, hearts), card(4, clubs), card(4, hearts), card(4, diamonds)],
Y = [card(7, clubs), card(7, diamonds)] ;

this is where it truly doesn't matter: either 2 8s or 2 9s
select_hand([
    card(9,clubs),
    card(9,diamonds),
    card(9,hearts),
    card(8,clubs),
    card(8, hearts),
    card(8, diamonds)
    ],X, Y).
X = [card(9, clubs), card(8, clubs), card(8, hearts), card(8, diamonds)],
Y = [card(9, diamonds), card(9, hearts)] ;
X = [card(9, clubs), card(9, diamonds), card(9, hearts), card(8, clubs)],
Y = [card(8, hearts), card(8, diamonds)] ;
X = [card(9, clubs), card(9, diamonds), card(9, hearts), card(8, diamonds)],
Y = [card(8, clubs), card(8, hearts)] ;
X = [card(9, clubs), card(9, diamonds), card(9, hearts), card(8, hearts)],
Y = [card(8, clubs), card(8, diamonds)] ;
X = [card(9, diamonds), card(8, clubs), card(8, hearts), card(8, diamonds)],
Y = [card(9, clubs), card(9, hearts)] ;
X = [card(9, hearts), card(8, clubs), card(8, hearts), card(8, diamonds)],
Y = [card(9, clubs), card(9, diamonds)] ;
*/



/*
% random tests: most of these situations are technically impossible
% these tests are not to be accounted for
%hand_value([], card(5, hearts), 0),
%hand_value([card(jack,hearts)], card(5,hearts), 8),
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
hand_value([card(5, hearts), card(5, hearts), card(5, hearts), card(5,
hearts), card(5, hearts), card(5, hearts), card(6, hearts), card(6,
hearts), card(jack, hearts), card(7, hearts), card(8, hearts), card(8,
hearts), card(9, hearts), card(queen, hearts), card(ace, hearts), card(2,
hearts)], card(king, hearts), 309),

select_hand([card(5,clubs),
card(5,diamonds),card(7,clubs),card(8,clubs),card(9, hearts), card(jack,
clubs)],[card(5, clubs), card(7, clubs), card(8, clubs), card(jack,
clubs)], [card(5, diamonds), card(9, hearts)]),

select_hand([card(5,clubs), card(5,diamonds),card(7,clubs),card(5,clubs),
card(9, hearts), card(jack, clubs)],[card(5, clubs), card(5, diamonds),
card(5, clubs), card(jack, clubs)], [card(7, clubs), card(9, hearts)] ),

select_hand([card(5,clubs),
card(5,diamonds),card(5,clubs),card(jack,clubs), card(jack, hearts),
card(jack, clubs)], [card(5, clubs), card(5, clubs), card(jack, clubs),
card(jack, clubs)], [card(5, diamonds), card(jack, hearts)]),

select_hand([card(4,clubs),
card(ace,diamonds),card(7,clubs),card(3,clubs), card(3, hearts), card(9,
clubs)],[card(4, clubs), card(7, clubs), card(3, clubs), card(9, clubs)],
[card(ace, diamonds), card(3, hearts)]),

select_hand([card(4,clubs), card(3,diamonds),card(7,clubs),card(3,clubs),
card(3, hearts), card(9, clubs)],[card(3, diamonds), card(3, clubs),
card(3, hearts), card(9, clubs)], [card(4, clubs), card(7, clubs)]),

select_hand([card(3,clubs), card(3,diamonds),card(3,clubs),card(5,clubs),
card(5, hearts), card(5, clubs)],[card(3, clubs), card(3, clubs), card(5,
clubs), card(5, clubs)], [card(3, diamonds), card(5, hearts)]),

select_hand([card(3,clubs),
card(3,diamonds),card(3,hearts),card(5,clubs), card(5, hearts), card(5,
diamonds)],[card(3, clubs), card(5, clubs), card(5, hearts), card(5,
diamonds)], [card(3, diamonds), card(3, hearts)]).
*/