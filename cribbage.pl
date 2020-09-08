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

% TODO: filter >=2 then binom() then sum...

score_pairs([], Acc, Acc).
score_pairs([_-Count | Tail], Acc, Score) :-
    ( Count >= 2 ->
        binom(Count, 2, NPairs),
        Acc1 is Acc + (NPairs * 2),
        score_pairs(Tail, Acc1, Score)
    ;
        score_pairs(Tail, Acc, Score)
    ).
score_pairs(Counts, Score) :-
    score_pairs(Counts, 0, Score).


sublist_of(Sub, List) :-
    true.

% TODO: consider this syntax: [A, B | C]
% TODO: 8 space tabs???
run(Counts, Run):-
    length(Run, N),
    N >= 3,
    sublist_of(Run, Counts).

% TODO: obvious stack overflow?
combo([], []).
combo([H|T], [H|T2]) :-
    combo(T, T2).
combo([_|T], T2) :-
    combo(T, T2).

sum_ranks([], Acc, Acc).
sum_ranks([R-C | RCs], Acc, Sum) :-
    Acc1 is Acc + min(R, 10),
    sum_ranks(RCs, Acc1, Sum).

sum_ranks(Cards, Sum) :-
    sum_ranks(Cards, 0, Sum).

product_counts([], Acc, Acc).
product_counts([R-C | RCs], Acc, Product) :-
    Acc1 is Acc * C,
    product_counts(RCs, Acc1, Product).

product_counts(Cards, Product) :-
    product_counts(Cards, 1, Product).


% TODO: J, Q, K are counted as 10...
fifteen(Cards, Combo, Ways) :-
    combo(Cards, Combo),
    sum_ranks(Combo, 15),
    product_counts(Combo, Ways).

score_fifteens(Cards, Score) :-
    ( setof(Combo-Ways, fifteen(Cards, Combo, Ways), Pairs) ->
        pairs_values(Pairs, Values),
        sumlist(Values, N),
        Score is N * 2
    ;
        Score is 0
    ).

% score_fifteens2([], [], Sum, Acc, Acc).
% score_fifteens2([H1 | T1], [H | T], Acc, Sum) :-
%     member(H, [H1 | T1])



fifteen2(Ranks, Combo) :-
    combo(Ranks, Combo),
    sum_list(Combo, 15).
    

min(X, Y, X) :-
    X =< Y.
min(X, Y, Y) :-
    X > Y.

% TODO: findall/3
score_fifteens2(Cards, Score) :-
    maplist(pair, Cards, Pairs),
    pairs_keys(Pairs, Ranks),
    maplist(min(10), Ranks, Ranks1),
    findall(Combo, fifteen2(Ranks1, Combo), Fifteens),
    length(Fifteens, Ways),
    Score is Ways * 2.

    




score_runs([], _, N, M, Acc, Score) :-
    ( N >=3 -> 
        Score is Acc + N * M
    ; Score = Acc
    ).
score_runs([Rank1-Count1 | Tail], Rank0-Count0, N, M, Acc, Score) :-
    ( Rank1 =:= Rank0 + 1 ->
        N1 is N + 1,
        M1 is M * Count1,
        score_runs(Tail, Rank1-Count1, N1, M1, Acc, Score)
    ; N >= 3 ->
        Acc1 is Acc + N * M,
        score_runs(Tail, Rank1-Count1, 1, Count1, Acc1, Score)
    ; score_runs(Tail, Rank1-Count1, 1, Count1, Acc, Score)
    ).

score_runs([Rank-Count | Counts], Score) :-
    score_runs(Counts, Rank-Count, 1, Count, 0, Score).


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
    count_rank(Tail, Rank-1, [], Counts0),
    reverse(Counts0, Counts).

% listof(_, []).
% listof(X, [X | Xs]) :-
%     listof(X, Xs).

all_same_suit([]).
all_same_suit([card(_, _)]).
all_same_suit([card(_, Suit), card(Rank1, Suit) | Cards]) :-
    all_same_suit([card(Rank1, Suit) | Cards]).


score_flush([card(Rank, Suit) | Cards], Startcard, Score) :-
    ( all_same_suit([card(Rank, Suit) | Cards]) ->
        ( Startcard = card(_, Suit) ->
            Score = 5
        ;
            % does not depend on size of hand???
            Score = 4
        )
    ; Score = 0
    ).

score_one_for_his_nob(Hand, card(_, Suit), Score) :-
    ( member(card(jack, Suit), Hand) ->
        Score = 1
    ; Score = 0
    ).

% hand_value(+Hand, +Startcard, ?Value)
% FIXME: documentation is assessed...
hand_value(Hand, Startcard, Value) :-
    score_one_for_his_nob(Hand, Startcard, S0),
    score_flush(Hand, Startcard, S1),
    Cards = [Startcard | Hand],
    score_fifteens2(Cards, S2),
    count_rank(Cards, Counts),
    score_runs(Counts, S3),
    score_pairs(Counts, S4),
    Value is S0 + S1 + S2 + S3 + S4.

select_hand(Cards, Hand, Cribcards) :-
    true.



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