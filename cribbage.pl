card(Card) :-
    % FIXME: validate cards?
    true.

pair(card(Rank, Suit), R-Suit) :-
    Mappings = [ace-1, jack-11, queen-12, king-13],
    % Suits = [clubs, diamonds, hearts, spades],
    % member(Suit, Suits),
    ( member(Rank-Value, Mappings) ->
        R = Value
    ; integer(Rank), between(2, 10, Rank) ->
        R = Rank
    ).

pair2(card(Rank, Suit), R-Suit) :-
    Mappings = [ace-1, jack-11, queen-12, king-13],
    member(Rank-R, Mappings).
pair2(card(Rank, Suit), Rank-Suit) :-
    integer(Rank), between(2, 10, Rank).

score_pairs([], Acc, Acc).
score_pairs([Rank-Count | Tail], Acc, Score) :-
    Count >= 2.

fact(N, Result) :-
    fact(N, 1, Result).
fact(N, A, Result) :-
    ( N =:= 0 ->
        Result = A
    ;
        N > 0,
        N1 is N - 1,
        A1 is A * N,
        fact(N1, A1, Result)
    ).

binom(N, K, B) :-
    fact(N, F1),
    fact(K, F2),
    D is N - K,
    fact(D, F3),
    B is F1 / (F2 * F3).


% TODO: work from the back? to make use of cons
% count_rank(Tail, [Rank1-Count1, Rank0-Count0 | Tail], Counts),
% true.
count_rank([], Rank0-Count0, Acc, [Rank0-Count0 | Acc]).
count_rank([Rank1-_ | Tail], Rank1-Count0, Acc, Counts) :-
    Count1 is Count0 + 1,
    count_rank(Tail, Rank1-Count1, Acc, Counts).
count_rank([Rank1-_ | Tail], Rank0-Count0, Acc, Counts) :-
    Rank1 \= Rank0,
    count_rank(Tail, Rank1-1, [Rank0-Count0 | Acc], Counts).

count_rank(Cards, Counts) :-
    maplist(pair, Cards, Pairs),
    msort(Pairs, SortedPairs),
    SortedPairs = [Rank-_ | Tail],
    count_rank(Tail, Rank-1, [], Counts).

det(A, B) :-
    A = 1, B = 2;
    A = 2, B = 4.

det2(A, B) :-
    A = 1,
    B = 2.
det2(A, B) :-
    A = 2,
    B = 4.
    

% hand_value(+Hand, +Startcard, ?Value)
% FIXME: documentation is assessed...
hand_value(Hand, Startcard, Value) :-
    Hand = [card(X, Y), card(W, Z)],
    Startcard = card().

select_hand(Cards, Hand, Cribcards) :-
    true.



/*
tests, and expected output:

[card(7,clubs), card(queen,hearts), card(2,clubs), card(jack,clubs)],
card(9,hearts),
0

[card(ace,spades), card(3,hearts), card(king,hearts), card(7,hearts)],
card(king,spades),
2

[card(ace,spades), card(3,hearts), card(king,hearts), card(7,hearts)],
card(2,diamonds)
5

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