% Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
% Purpose:  Evaluating cribbage hands and deciding which card(s) to keep in
%           the hand to maximise score.
%
% COMP30020 Project 1, S2 2020.
%
% The main predicates in this Prolog program are:
% - hand_value(+Hand, +Startcard, ?Value):
%           Evaluates a 5-card cribbage hand [Startcard|Hand], as in the show.
% - select_hand(+Cards, +Hand, ?Cribcards):
%           Selects cards to keep in the hand (and to discard to the crib),
%           to maximise expected hand value when the start card is revealed,
%           by averaging the scores across all possible card arrangements.
%
% Background:
%
% Cribbage is a card game that involves playing and grouping cards. The game
% begins with the dealer dealing each player 5 or 6 cards (depending on the
% number of players). Players choose 4 cards to keep in their hand and put the
% rest in the crib. The discarded cards form the dealer's 4-card hand. The
% dealer then cuts the deck to select an extra card, called the start card.
%
% Then, the players take turns to play single cards (putting them face-up).
% After the play comes the show, where players establish the value of a
% 5-card hand (their 4-card hand and the start card).
%
% The scoring rules are as follows:
% - (15s) 2 points for each distinct combination of cards whose ranks sum
%           to 15. For this purpose, an ace counts as 1 and a jack, queen, or
%           king counts as 10.
% - (Pairs) 2 points for each pair of cards with the same rank.
% - (Runs) 1 point for each card in a run of 3 or more consecutive cards
%           (suit does not matter). Runs do not overlap; 4 consecutive cards
%           is a 4-card run, not 2 3-card runs. Runs cannot wrap around.
% - (Flushes) 4 points if all the cards in the hand are of the same suit, and
%           1 further point if the start card is also of that suit.
% - (One for his nob) 1 point if the hand contains a jack of the same suit as
%           the start card
%
% Following the show of each player's hand, the dealer claims points by
% counting the crib as a second hand. Then the next player becomes the dealer
% for the next hand. The first player to reach 121 points wins.

% Card predicates =============================================================

%% valid_card(?Card)
% Card is a valid card(Rank, Suit) term, representing a French-suited
% playing card, i.e. Suit is one of spades, hearts, clubs or diamonds.
valid_card(card(Rank, Suit)) :-
        Ranks = [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
        Suits = [spades, hearts, clubs, diamonds],
        member(Rank, Ranks),
        member(Suit, Suits).

%% rank_suit_pair(?Card, ?RankSuitPair)
% Card is a card(Rank, Suit) term and RankSuitPair is a pair of the numerical
% value of the rank of the card and its suit. An ace is counted as 1.
% e.g. card(king, spades) corresponds to 13-spades.
rank_suit_pair(card(Rank, Suit), RankValue-Suit) :-
        Mappings = [ace-1, jack-11, queen-12, king-13],
        member(Rank-RankValue, Mappings).
rank_suit_pair(card(Rank, Suit), Rank-Suit) :-
        integer(Rank),
        between(2, 10, Rank).

%% rank_count(+Cards, ?RankCounts)
% RankCounts is a sorted (by Rank then Count) list of Rank-Count pairs
% corresponding to Cards, which is a non-empty list of card(Rank, Suit) terms.
% "Count" refers to the number of occurrences of cards of that rank in Cards.
rank_count(Cards, RankCounts) :-
        maplist(rank_suit_pair, Cards, RankSuits),
        pairs_keys(RankSuits, Ranks),
        msort(Ranks, SortedRanks),
        rank_count(SortedRanks, 1, [], RankCountsDesc),
        reverse(RankCountsDesc, RankCounts).

% rank_count(+Ranks, +Count, +Acc, -RankCounts)
% auxiliary predicate of rank_count/2
rank_count([Rank0], Count0, Acc, [Rank0-Count0 | Acc]).
rank_count([Rank1, Rank1 | Rs], Count0, Acc, RankCounts) :-
        Count1 is Count0 + 1,
        rank_count([Rank1 | Rs], Count1, Acc, RankCounts).
rank_count([Rank0, Rank1 | Rs], Count0, Acc, RankCounts) :-
        Rank1 \= Rank0,
        rank_count([Rank1 | Rs], 1, [Rank0-Count0 | Acc], RankCounts).

% Scoring 15s =================================================================

%% combo(+List, ?Combo)
% Combo is a combination of elements from List.
% List's (hence Combo's) elements need not be unique.
combo([], []).
combo([_|Tail1], Combo) :-
        Combo = Tail2,
        combo(Tail1, Tail2).
combo([Head|Tail1], Combo) :-
        Combo = [Head | Tail2],
        combo(Tail1, Tail2).

%% fifteen(+Ranks, ?Combo)
% Combo is a combination of cards whose ranks sum to 15.
% Cards are represented by their rank's numerical value in Ranks.
fifteen(Ranks, Combo) :-
        combo(Ranks, Combo),
        sum_list(Combo, 15).

%% min(+A, +B, ?Min)
% Min is the smaller term between A and B.
min(A, B, A) :-
        A @=< B.
min(A, B, B) :-
        A @> B.

%% score_fifteens (+Cards, ?Score)
% Score is the number of points from "15s": combinations of cards in Cards
% (a list of card(Rank, Suit) terms), whose ranks sum to 15. Each 15 is worth
% 2 points. In scoring 15s, jacks, queens, and kings have value 10, and aces
% have value 1 still.
score_fifteens(Cards, Score) :-
        maplist(rank_suit_pair, Cards, RankSuits),
        pairs_keys(RankSuits, Ranks),
        maplist(min(10), Ranks, Ranks1),
        findall(Combo, fifteen(Ranks1, Combo), Fifteens),
        length(Fifteens, NFifteens),
        Score is NFifteens * 2.

% Scoring pairs ===============================================================

%% factorial(+N, -Result)
% Result is the computed factorial of N. N >= 0.
factorial(N, Result) :-
        factorial(N, 1, Result).
factorial(N, Acc, Result) :-
        ( N =:= 0 ->
                Acc = Result
        ;       N > 0,
                N1 is N - 1,
                Acc1 is Acc * N,
                factorial(N1, Acc1, Result)
        ).

%% choose(+K, +N, -B)
% B is the computed binomial coefficient "N choose K". N >= K >= 0.
choose(K, N, B) :-
        N >= K,
        factorial(N, F1),
        factorial(K, F2),
        D is N - K,
        factorial(D, F3),
        B is F1 / (F2 * F3).

%% score_pairs(+RankCounts, ?Score)
% Score is the number of points from pairs of cards of the same rank, each pair
% being worth 2 points, in a collection of cards in the form of a list
% of Rank-Count pairs, where Count is the number of occurences of Rank.
score_pairs(RankCounts, Score) :-
        pairs_values(RankCounts, Counts),
        include(call(=<, 2), Counts, Counts2OrGreater),
        maplist(choose(2), Counts2OrGreater, AllWays),
        sum_list(AllWays, NPairs),
        Score is NPairs * 2.
    
% Scoring runs ================================================================

%% score_runs(+RankCounts, -Score)
% Score is the number of points from any run of 3 or more consecutive cards in
% a collection of cards, represented as a *sorted* non-empty list of Rank-Count
% pairs, where Count is the number of occurrences of cards of rank Rank.
% - Suits do not matter
% - Runs do not wrap around
% - Runs do not overlap: 4 consecutive cards is a 4-card run, not 2 3-card runs
% - The length of a run is the number of points it's worth
score_runs(RankCounts, Score) :-
        RankCounts = [_-Count | _],
        score_runs(RankCounts, 1-Count, 0, Score).

% score_runs(+RankCounts, +RunStatus, +Acc, -Score)
% auxiliary predicate for score_runs/2.
% RunStatus is of the form Len-Ways, where Len is the length of the current
% run and Ways is the number of ways that run can be formed with the cards.
score_runs([_], Len-Ways, Acc, Score) :-
        ( Len >= 3 ->
                Score is Acc + Len * Ways
        ; Score = Acc
        ).
score_runs([Rank0-_, Rank1-Count1 | RCs], Len-Ways, Acc, Score) :-
        ( Rank1 =:= Rank0 + 1 ->
                Len1 is Len + 1,
                Ways1 is Ways * Count1,
                score_runs([Rank1-Count1 | RCs], Len1-Ways1, Acc, Score)
        ; Len >= 3 ->
                Acc1 is Acc + Len * Ways,
                score_runs([Rank1-Count1 | RCs], 1-Count1, Acc1, Score)
        ; score_runs([Rank1-Count1 | RCs], 1-Count1, Acc, Score)
        ).

% Scoring flushes =============================================================

%% all_same_suit(+Cards)
% Cards is a non-empty list of card(Rank, Suit) terms, where all the
% cards are of the same suit.
all_same_suit([card(_, _)]).
all_same_suit([card(_, Suit), card(Rank, Suit) | RSs]) :-
        all_same_suit([card(Rank, Suit) | RSs]).

%% score_flush(+Hand, +Startcard, ?Score)
% Score is 4 if all 4 card(Rank, Suit) terms in Hand are of the same suit.
% Further, if Startcard is of that same suit as well, then Score is 5.
% Otherwise Score is 0. Cards here are card(Rank, Suit) terms.
score_flush(Hand, card(_, Suit), 5) :-
        all_same_suit(Hand),
        Hand = [card(_, Suit) | _].
score_flush(Hand, card(_, Suit), 4) :-
        all_same_suit(Hand),
        Hand = [card(_, Suit1) | _],
        Suit1 \= Suit.
score_flush(Hand, card(_, _), 0) :-
        \+ all_same_suit(Hand).

% Scoring one for his nob ====================================================

%% score_one_for_his_nob(+Hand, +Startcard, ?Score)
% Score is 1 if Hand contains a jack of the same suit as Startcard, and
% 0 otherwise. Cards here are card(Rank, Suit) terms.
score_one_for_his_nob(Hand, card(_, Suit), 1) :-
        member(card(jack, Suit), Hand).
score_one_for_his_nob(Hand, card(_, Suit), 0) :-
        \+ member(card(jack, Suit), Hand).

% Evaluating hands ============================================================

%% hand_value(+Hand, +Startcard, ?Value)
% Value is the total value of the 5-card cribbage hand consisting of the
% 4-card hand Hand and Startcard. A hand is a list of valid
% card(Rank, Suit) terms. The 5-card hand does not contain duplicate cards.
hand_value(Hand, Startcard, Value) :-
        length(Hand, 4),
        Cards = [Startcard | Hand],
        is_set(Cards),
        maplist(valid_card, Cards),
        score_one_for_his_nob(Hand, Startcard, S1),
        score_flush(Hand, Startcard, S2),
        score_fifteens(Cards, S3),
        rank_count(Cards, RankCounts),
        score_runs(RankCounts, S4),
        score_pairs(RankCounts, S5),
        Value is S1 + S2 + S3 + S4 + S5.

% Selecting hands =============================================================

%% hand(+Cards, ?Hand)
% Hand is a possible 4-card cribbage hand from the collection of cards Cards.
% Cards here are card(Rank, Suit) terms.
hand(Cards, Hand) :-
        length(Hand, 4),
        combo(Cards, Hand).

%% expected_hand_score(+Startcards, +Hand, ?Score)
% Score is the expected value of Hand, a list of card(Rank, Suit) terms.
% The expected value of a 5-card hand is the average value across all
% start card possibilities listed in Startcards. If 6 cards are dealt,
% Startcards corresponds to 52 - 6 = 46 possibilities.
expected_hand_score(Startcards, Hand, Score) :-
        maplist(hand_value(Hand), Startcards, Scores),
        sum_list(Scores, SumScores),
        length(Scores, LenScores),
        Score is SumScores / LenScores.

%% select_hand(+Cards, ?Hand, ?Cribcards)
% Selects a hand from Cards, a list of 5 or 6 valid, distinct card(Rank, Suit)
% terms such that the expected value, as defined by expected_hand_scores/3 is
% maximised. There may be multiple best hands; this predicate only returns one
% and does not backtrack. Hand contains the 4 cards to be kept and
% Cribcards contains the cards not to be kept, card(Rank, Suit) terms.
% Both Hand and Cribcards are not explicitly sorted.
select_hand(Cards, Hand, Cribcards) :-
        length(Cards, Len),
        between(5, 6, Len),
        is_set(Cards),
        maplist(valid_card, Cards),
        setof(Hand0, hand(Cards, Hand0), Hands),
        findall(Card, valid_card(Card), AllCards),
        subtract(AllCards, Cards, Startcards),
        maplist(expected_hand_score(Startcards), Hands, Scores),
        pairs_keys_values(ScoreHands, Scores, Hands),
        msort(ScoreHands, SortedScoreHands),
        reverse(SortedScoreHands, [_-Hand | _]),
        subtract(Cards, Hand, Cribcards).

% =============================================================================
