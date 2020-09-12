:- ensure_loaded(cribbage).

test1 :-
    hand_value([card(7,clubs), card(queen, hearts),card(2,clubs),card(jack, clubs)], card(9,hearts), 0),
    hand_value([card(ace, spades), card(3,hearts),card(king, hearts),card(7,hearts)], card(king, spades), 2),
    hand_value([card(ace, spades), card(3,hearts),card(king, hearts),card(7,hearts)], card(2, diamonds), 5),
    hand_value([card(6,clubs), card(7,clubs),card(8,clubs),card(9,clubs)], card(8,spades), 20),
    hand_value([card(7,hearts), card(9,spades),card(8,clubs),card(7,clubs)], card(8,hearts), 24),
    hand_value([card(5,hearts), card(5,spades),card(5,clubs),card(jack,diamonds)], card(5,diamonds), 29),

    hand_value([card(king,spades),card(5,diamonds),card(3,hearts),card(3,diamonds)],card(jack,diamonds),6),
    hand_value([card(3,hearts),card(queen,diamonds),card(6,clubs),card(ace,clubs)],card(ace,hearts),4),
    hand_value([card(9,hearts),card(3,hearts),card(10,spades),card(9,clubs)],card(7,clubs),2),
    hand_value([card(6,spades),card(6,clubs),card(3,spades),card(4,hearts)],card(2,spades),11),
    hand_value([card(jack,clubs),card(king,diamonds),card(9,spades),card(2,hearts)],card(7,diamonds),0),

    select_hand([card(10,spades),card(jack,diamonds),card(jack,spades),card(2,hearts),card(7,diamonds)],[card(10, spades), card(jack, diamonds), card(jack, spades), card(2, hearts)],[card(7, diamonds)]),
    select_hand([card(jack,hearts),card(5,hearts),card(5,diamonds),card(2,diamonds),card(3,clubs)],[card(jack, hearts), card(5, hearts), card(5, diamonds), card(3, clubs)],[card(2, diamonds)]),
    select_hand([card(king,diamonds),card(4,diamonds),card(6,spades),card(10,spades),card(7,spades)],[card(king, diamonds), card(4, diamonds), card(6, spades), card(7, spades)],[card(10, spades)]),
    select_hand([card(4,diamonds),card(5,diamonds),card(3,diamonds),card(6,diamonds),card(10,spades)],[card(4, diamonds), card(5, diamonds), card(3, diamonds), card(6, diamonds)],[card(10, spades)]),
    select_hand([card(king,clubs),card(7,clubs),card(jack,clubs),card(2,hearts),card(8,spades)],[card(7, clubs), card(jack, clubs), card(2, hearts), card(8, spades)],[card(king, clubs)]).

    /*
    % my tests: most of these situations are technically impossible
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
    hand_value([card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(5, hearts), card(6, hearts), card(6, hearts), card(jack, hearts), card(7, hearts), card(8, hearts), card(8, hearts), card(9, hearts), card(queen, hearts), card(ace, hearts), card(2, hearts)], card(king, hearts), 309),

    select_hand([card(5,clubs), card(5,diamonds),card(7,clubs),card(8,clubs),card(9, hearts), card(jack, clubs)],[card(5, clubs), card(7, clubs), card(8, clubs), card(jack, clubs)], [card(5, diamonds), card(9, hearts)]),

    select_hand([card(5,clubs), card(5,diamonds),card(7,clubs),card(5,clubs), card(9, hearts), card(jack, clubs)],[card(5, clubs), card(5, diamonds), card(5, clubs), card(jack, clubs)], [card(7, clubs), card(9, hearts)] ),

    select_hand([card(5,clubs), card(5,diamonds),card(5,clubs),card(jack,clubs), card(jack, hearts), card(jack, clubs)], [card(5, clubs), card(5, clubs), card(jack, clubs), card(jack, clubs)], [card(5, diamonds), card(jack, hearts)]),

    select_hand([card(4,clubs), card(ace,diamonds),card(7,clubs),card(3,clubs), card(3, hearts), card(9, clubs)],[card(4, clubs), card(7, clubs), card(3, clubs), card(9, clubs)], [card(ace, diamonds), card(3, hearts)]),

    select_hand([card(4,clubs), card(3,diamonds),card(7,clubs),card(3,clubs), card(3, hearts), card(9, clubs)],[card(3, diamonds), card(3, clubs), card(3, hearts), card(9, clubs)], [card(4, clubs), card(7, clubs)]),

    select_hand([card(3,clubs), card(3,diamonds),card(3,clubs),card(5,clubs), card(5, hearts), card(5, clubs)],[card(3, clubs), card(3, clubs), card(5, clubs), card(5, clubs)], [card(3, diamonds), card(5, hearts)]),

    select_hand([card(3,clubs), card(3,diamonds),card(3,hearts),card(5,clubs), card(5, hearts), card(5, diamonds)],[card(3, clubs), card(5, clubs), card(5, hearts), card(5, diamonds)], [card(3, diamonds), card(3, hearts)]).
*/

% run the test1 goal when this file is loaded.
:- test1.

/*
old code dump

/*

score_runs1([], _, N, M, Acc, Score) :-
    ( N >= 3 -> 
        Score is Acc + N * M
    ; Score = Acc
    ).
score_runs1([Rank1-Count1 | Tail], Rank0-_, N, M, Acc, Score) :-
    ( Rank1 =:= Rank0 + 1 ->
        N1 is N + 1,
        M1 is M * Count1,
        score_runs1(Tail, Rank1-Count1, N1, M1, Acc, Score)
    ; N >= 3 ->
        Acc1 is Acc + N * M,
        score_runs1(Tail, Rank1-Count1, 1, Count1, Acc1, Score)
    ; score_runs1(Tail, Rank1-Count1, 1, Count1, Acc, Score)
    ).

score_runs1([Rank-Count | Counts], Score) :-
    score_runs1(Counts, Rank-Count, 1, Count, 0, Score).

% Score is the number of points from pairs of cards of the same rank,
% each pair is worth 2 points, in a collection of cards in the form of a list
% of Rank-Count pairs, where Count is the number of occurences of Rank.

score_flush1([card(Rank, Suit) | Cards], Startcard, Score) :-
    ( all_same_suit([card(Rank, Suit) | Cards]) ->
        ( Startcard = card(_, Suit) ->
            Score = 5
        ;
            % does not depend on size of hand???
            Score = 4
        )
    ; Score = 0
    ).

pair1(card(Rank, Suit), R-Suit) :-
    Mappings = [ace-1, jack-11, queen-12, king-13],
    % Suits = [clubs, diamonds, hearts, spades],
    % member(Suit, Suits),
    ( member(Rank-Value, Mappings) ->
        R = Value
    ; integer(Rank), between(2, 10, Rank) ->
        R = Rank
    ).

score_pairs1([], Acc, Acc).
score_pairs1([_-Count | Tail], Acc, Score) :-
    ( Count >= 2 ->
        choose(2, Count, NPairs),
        Acc1 is Acc + (NPairs * 2),
        score_pairs1(Tail, Acc1, Score)
    ;
        score_pairs1(Tail, Acc, Score)
    ).
score_pairs1(Counts, Score) :-
    score_pairs1(Counts, 0, Score).
sum_ranks([], Acc, Acc).
sum_ranks([R-_ | RCs], Acc, Sum) :-
    Acc1 is Acc + min(R, 10),
    sum_ranks(RCs, Acc1, Sum).

sum_ranks(Cards, Sum) :-
    sum_ranks(Cards, 0, Sum).

product_counts([], Acc, Acc).
product_counts([_-C | RCs], Acc, Product) :-
    Acc1 is Acc * C,
    product_counts(RCs, Acc1, Product).

product_counts(Cards, Product) :-
    product_counts(Cards, 1, Product).


%% fifteen(+)
%
fifteen1(Cards, Combo, Ways) :-
    combo(Cards, Combo),
    sum_ranks(Combo, 15),
    product_counts(Combo, Ways).

score_fifteens1(Cards, Score) :-
    ( setof(Combo-Ways, fifteen1(Cards, Combo, Ways), Pairs) ->
        pairs_values(Pairs, Values),
        sumlist(Values, N),
        Score is N * 2
    ;
        Score is 0
    ).
*/

% score_one_for_his_nob1(Hand, card(_, Suit), Score) :-
%     ( member(card(jack, Suit), Hand) ->
%         Score = 1
%     ; Score = 0
%     ).

%zip(Scores, Hands, ScoreHands),
% msort(ScoreHands, SortedScoreHandsAsc),
% reverse(SortedScoreHandsAsc, [Score-Hand, Score2-_, Score3-_ | _]),
%max_score(HandScores, Hand),
%max_list(ScoreHands, Hand-_),


% max_score([], CurrBestHand-_, CurrBestHand).
% max_score([Hand-Score | HSs], _-CurrBestScore, BestHand) :-
%     Score > CurrBestScore,
%     max_score(HSs, Hand-Score, BestHand).
% max_score([_-Score | HSs], CurrBestHand-CurrBestScore, BestHand) :-
%     Score =< CurrBestScore,
%     max_score(HSs, CurrBestHand-CurrBestScore, BestHand).

% max_score([Hand-Score | HSs], BestHand) :-
%     max_score(HSs, Hand-Score, BestHand)

/*
tests, and expected output:

[card(7,clubs), card(queen,hearts), card(2,clubs), card(jack,clubs)],
card(9,hearts),
0

[card(ace,spades), card(3,hearts), card(king,hearts), card(7,hearts)],card(king,spades),2

[card(ace,spades), card(3,hearts), card(king,hearts), card(7,hearts)],
card(2,diamonds)
5

[card(6,clubs), card(7,clubs), card(8,clubs), card(9,clubs)], card(8,spades)
C = [9-1, 8-1, 7-1, 6-1]
C = [6-1, 7-1, 8-1, 9-1]

?-hand_value([
    card(7,clubs),
    card(queen,hearts),
    card(2,clubs),
    card(jack,clubs)],
    card(9,hearts),
    Value).
Value = 0.

?-hand_value([
    card(ace,spades),
    card(3,hearts),
    card(king,hearts),
    card(7,hearts)],
    card(king,spades),
    Value).
Value = 2.

?-hand_value([
    card(ace,spades),
    card(3,hearts),
    card(king,hearts),
    card(7,hearts)],
    card(2,diamonds),
    Value).
Value = 5.

?-hand_value([
    card(6,clubs),
    card(7,clubs),
    card(8,clubs),
    card(9,clubs)],
    card(8,spades),
    Value).
Value = 20.

?-hand_value([
    card(7,hearts),
    card(9,spades),
    card(8,clubs),
    card(7,clubs)],
    card(8,hearts),
    Value).
Value = 24.

?-hand_value([
    card(5,hearts),
    card(5,spades),
    card(5,clubs),
    card(jack,diamonds)],
    card(5,diamonds),
    Value).
Value = 29.



*/