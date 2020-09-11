% Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
% Purpose:  Scoring cribbage hands and deciding which card(s) to keep in
%           the hand to maximise score.
%
% This Prolog program ...  
% what is cribbage



pair1(card(Rank, Suit), R-Suit) :-
    Mappings = [ace-1, jack-11, queen-12, king-13],
    % Suits = [clubs, diamonds, hearts, spades],
    % member(Suit, Suits),
    ( member(Rank-Value, Mappings) ->
        R = Value
    ; integer(Rank), between(2, 10, Rank) ->
        R = Rank
    ).

valid_card(card(Rank, Suit)) :-
    Ranks = [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
    Suits = [spades, hearts, clubs, diamonds],
    member(Rank, Ranks),
    member(Suit, Suits).

pair(card(Rank, Suit), RankValue-Suit) :-
    Mappings = [ace-1, jack-11, queen-12, king-13],
    member(Rank-RankValue, Mappings).
pair(card(Rank, Suit), Rank-Suit) :-
    integer(Rank), between(2, 10, Rank).

fact(N, Result) :-
    fact(N, 1, Result).
fact(N, Acc, Result) :-
    ( N =:= 0 ->
        Acc = Result
    ;
        N > 0,
        N1 is N - 1,
        Acc1 is Acc * N,
        fact(N1, Acc1, Result)
    ).

choose1(_, 0, 1).
choose1(N, K, B) :-
    ( 0 =< K, K =< N ->
        N1 is N - 1,
        K1 is K - 1,
        choose1(N1, K, B1),
        choose1(N1, K1, B2),
        B is B1 + B2
    ).

choose(K, N, B) :-
    fact(N, F1),
    fact(K, F2),
    D is N - K,
    fact(D, F3),
    B is F1 / (F2 * F3).

% TODO: filter >=2 then binom() then sum...

score_pairs([], Acc, Acc).
score_pairs([_-Count | Tail], Acc, Score) :-
    ( Count >= 2 ->
        choose(2, Count, NPairs),
        Acc1 is Acc + (NPairs * 2),
        score_pairs(Tail, Acc1, Score)
    ;
        score_pairs(Tail, Acc, Score)
    ).
score_pairs(Counts, Score) :-
    score_pairs(Counts, 0, Score).


score_pairs2(RankCounts, Score) :-
    pairs_values(RankCounts, Counts),
    include(call(=<, 2), Counts, Counts1),
    maplist(choose(2), Counts1, Ways),
    sum_list(Ways, NPairs),
    Score is NPairs * 2.
    


% TODO: consider this syntax: [A, B | C]
% TODO: 8 space tabs???

% sublist_of(Sub, List) :-
%     true.
% run(Counts, Run):-
%     length(Run, N),
%     N >= 3,
%     sublist_of(Run, Counts).

% TODO: obvious stack overflow?
combo([], []).
combo([H|T], [H|T2]) :-
    combo(T, T2).
combo([_|T], T2) :-
    combo(T, T2).


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


fifteen2(Ranks, Combo) :-
    combo(Ranks, Combo),
    sum_list(Combo, 15).
    
min(A, B, A) :-
    A =< B.
min(A, B, B) :-
    A > B.

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
score_runs([Rank1-Count1 | Tail], Rank0-_, N, M, Acc, Score) :-
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


score_runs2([], _-_, Acc, Acc).
score_runs2([_], Len-_, Acc, Acc) :-
    Len < 3.
score_runs2([_], Len-Ways, Acc, Score) :-
    Len >= 3,
    Score is Acc + Len * Ways.
score_runs2([Rank0-_, Rank1-Count1 | RCs], Len-_, Acc, Score) :-
    Rank1 =\= Rank0 + 1,
    Len < 3,
    score_runs2([Rank1-Count1 | RCs], 1-Count1, Acc, Score).
score_runs2([Rank0-_, Rank1-Count1 | RCs], Len-Ways, Acc, Score) :-
    Rank1 =\= Rank0 + 1,
    Len >= 3,
    Acc1 is Acc + Len * Ways,
    score_runs2([Rank1-Count1 | RCs], 1-Count1, Acc1, Score).
score_runs2([Rank0-_, Rank1-Count1 | RCs], Len-Ways, Acc, Score) :-
    Rank1 =:= Rank0 + 1,
    Len1 is Len + 1,
    Ways1 is Ways * Count1,
    score_runs2([Rank1-Count1 | RCs], Len1-Ways1, Acc, Score).

score_runs2(Cards, Score) :-
    Cards = [_-Count | _],
    score_runs2(Cards, 1-Count, 0, Score).

% TODO: work from the back? to make use of cons
% count_rank(Tail, [Rank1-Count1, Rank0-Count0 | Tail], Counts),
% true.
rank_count([Rank0-Count0], Acc, [Rank0-Count0 | Acc]).
rank_count([Rank1-Count0, Rank1-_ | Tail], Acc, Counts) :-
    Count1 is Count0 + 1,
    rank_count([Rank1-Count1 | Tail], Acc, Counts).
rank_count([Rank0-Count0, Rank1-_ | Tail], Acc, Counts) :-
    Rank1 \= Rank0,
    rank_count([Rank1-1 | Tail], [Rank0-Count0 | Acc], Counts).

rank_count(Cards, Counts) :-
    maplist(pair, Cards, Pairs),
    msort(Pairs, SortedPairs),
    SortedPairs = [Rank-_ | Tail],
    rank_count([Rank-1 | Tail], [], Counts0),
    reverse(Counts0, Counts).

% listof(_, []).
% listof(X, [X | Xs]) :-
%     listof(X, Xs).

% TODO: empty hand necessary?

%% all_same_suit(+Cards)
% 
% Holds if Cards is a list of card(Rank, Suit) terms,
% where all the cards are of the same suit.
all_same_suit([]).
all_same_suit([card(_, _)]).
all_same_suit([card(_, Suit), card(Rank, Suit) | RSs]) :-
    all_same_suit([card(Rank, Suit) | RSs]).


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

%% score_flush(+Cards, +Startcard, ?Score)
%
score_flush2(Cards, card(_, Suit), 5) :-
    all_same_suit(Cards),
    Cards = [card(_, Suit) | _].
score_flush2(Cards, card(_, Suit2), 4) :-
    all_same_suit(Cards),
    Cards = [card(_, Suit1) | _],
    Suit1 \= Suit2.
score_flush2(Cards, card(_, _), 0) :-
    \+ all_same_suit(Cards).

score_one_for_his_nob(Hand, card(_, Suit), Score) :-
    ( member(card(jack, Suit), Hand) ->
        Score = 1
    ; Score = 0
    ).

score_one_for_his_nob2(Hand, card(_, Suit), 1) :-
    member(card(jack, Suit), Hand).
score_one_for_his_nob2(Hand, card(_, Suit), 0) :-
    \+ memberchk(card(jack, Suit), Hand).



hand_value([], _, 0).
% hand_value(+Hand, +Startcard, ?Value)
% FIXME: documentation is assessed...
hand_value(Hand, Startcard, Value) :-
    maplist(valid_card, Hand),
    score_one_for_his_nob2(Hand, Startcard, S0),
    score_flush2(Hand, Startcard, S1),
    Cards = [Startcard | Hand],
    score_fifteens2(Cards, S2),
    rank_count(Cards, Counts),
    score_runs2(Counts, S3),
    score_pairs2(Counts, S4),
    Value is S0 + S1 + S2 + S3 + S4.

hand(Cards, Hand) :-
    length(Hand, 4),
    combo(Cards, Hand).

%% is_in(?List, ?Elem)
%
% Holds if Elem is a member of List.
member_of(List, Elem) :-
    member(Elem, List).

expected_hand_score(Hand, Score) :-
    findall(Card, valid_card(Card), AllCards),
    exclude(member_of(Hand), AllCards, Startcards),
    maplist(hand_value(Hand), Startcards, Scores),
    sum_list(Scores, SumScores),
    length(Scores, LenScores),
    Score is SumScores / LenScores.

% max_score([], CurrBestHand-_, CurrBestHand).
% max_score([Hand-Score | HSs], _-CurrBestScore, BestHand) :-
%     Score > CurrBestScore,
%     max_score(HSs, Hand-Score, BestHand).
% max_score([_-Score | HSs], CurrBestHand-CurrBestScore, BestHand) :-
%     Score =< CurrBestScore,
%     max_score(HSs, CurrBestHand-CurrBestScore, BestHand).

% max_score([Hand-Score | HSs], BestHand) :-
%     max_score(HSs, Hand-Score, BestHand).

% what strategy
select_hand(Cards, Hand, Cribcards) :-
    maplist(valid_card, Cards),
    setof(Hand0, hand(Cards, Hand0), Hands),
    maplist(expected_hand_score, Hands, Scores),
    pairs_keys_values(ScoreHands, Scores, Hands),
    max_list(Scores, MaxScore),
    member(MaxScore-Hand, ScoreHands),
    exclude(member_of(Hand), Cards, Cribcards).


%zip(Scores, Hands, ScoreHands),
% msort(ScoreHands, SortedScoreHandsAsc),
% reverse(SortedScoreHandsAsc, [Score-Hand, Score2-_, Score3-_ | _]),
%max_score(HandScores, Hand),
%max_list(ScoreHands, Hand-_),


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

RUBRIC:

Signature

Author clearly identified with name and id near the top of the file.
1.0
Some identification, but missing name or id, or too far from the top of the
file.
0.5
No author identification.
0.0
1 or 2 line summary of file purpose

Good, clear, helpful one line summary near the top of the file explaining
what the program does (not just what subject or project it's for).
1.0
Unclear or not very helpful summary near top of file.
0.5
No brief summary near top of file.
0.0
Quality of file-level documentation

Clear, comprehensive but not overwhelming description of the purpose of the
file and the approach taken, without assuming the reader knows the project.
3.0
Clear description of the purpose of the file and the approach taken, but
incomplete or too much irrelevant information.
2.0
Brief description of the purpose of the file and the approach taken, but
significant detail omitted or assumes knowledge of the project.
1.0
Little or no file-level documentation.
0.0
Quality of function/predicate/type level documentation

Outstanding documentation of every function/predicate/type explaining
purpose, meaning of arguments and output, and any implementation subtleties.
No unhelpful comments (eg, saying "increment i" next to a statement that
increments i).
5.0
Excellent documentation of almost every function/predicate/type explaining
purpose and meaning of arguments and output. Subtleties explained, but no
unhelpful comments.
4.0
Good, clear documentation of important functions/predicates/types explaining
meaning of arguments and output. No unexplained mysterious code, nor
excessive comments about self-explanatory code.
3.0
Some documentation for many functions/predicates/types explaining key
arguments and output, but some missing documentation or excessive comments.
2.0
Some documentation of some functions/predicates/types, but not particularly
helpful or too distracting.
1.0
Little or no useful documentation of functions/predicates/types or purposes,
or excessive unhelpful comments making the code harder to understand.
0.0
Readability

Beautifully readable, with neat, consistent layout. A pleasure to read.
5.0
Very readable and tidy, with good code layout. Easy to navigate.
4.0
Well presented, neat, mostly consistent layout.
3.0
Mostly readable, but with some messy parts. Layout inconsistent in places or
some long lines.
2.0
Hard to read, with inconsistent or poor layout or many long lines.
1.0
Little evidence of effort to layout the code. Illegible.
0.0
Understandability

A clear approach, well organised, with any subtleties clearly explained.
Names well chosen and clear but not too long.
5.0
Cleanly organised structure, with subtleties explained. Names are
understandable.
4.0
Functions/predicates are pretty understandable. Names are mostly clear
enough. Some effort to organise the presentation.
3.0
Some of the code is unclear without explanation. Some names are a bit
cryptic. Not much organisation of the code.
2.0
Much of the code and/or many of the names are hard to understand. Little
effort to group related things together.
1.0
Code is completely cryptic. Names give little hint of purpose.
0.0
Abstraction

Well structured code, each part with a single purpose, no code repetition, no
divided responsibilities, succinct definitions. Simple, elegant code.
5.0
Well thought-out structure, no code repetition, no long definitions, simple
code.
4.0
Good structure, no code repetition, reasonable length definitions, code could
be a bit simpler.
3.0
Some apparent structure, not a lot of redundant code, but code could have
been simpler with a bit more abstraction.
2.0
Not much evident structure; could could be substantially improved with some
abstraction.
1.0
Spaghetti code.
0.0
Use of Language and Libraries

Elegant use of language and library facilities; no wheels reinvented; code
reflects mastery of the language and libraries.
5.0
Excellent use of language and library facilities; not much could be improved
with better language or library use; code reflects good understanding of the
language and libraries.
4.0
Program demonstrats good use of language and library facilities in many
places, although some some opportunities for more sophisticated usage were
missed.
3.0
Program makes good use of language and library facilities in a few places,
but misses many opportunities for better usage.
2.0
Program mostly uses the most basic language and library facilities, but
misses many opportunities for better usage.
1.0
Program hardly uses any language and library facilities, reflecting little
understanding of the language or library.
0.0

*/