module Poker where
--Student Name: Mahamudul Islam
--Student Number: 500963051
--Student Name: Naushad Sayeed
--Student Number: 500589793
    import Control.Exception (assert)
    import Data.List (permutations, sort, sortOn, intersect, findIndices, (\\))
    import Data.Function (on)

    deal = trans
        . uncurry (winningHand 5)
        . hands
        . form where
            trans = sort . map (\n -> (show (([2..13] ++ [1]) !! (n % 13))) ++ [("CDHS" !! (n % 4))])
            hands (a:b:c:d:flop) = (a:c:flop, b:d:flop)
            indexOf x = head . findIndices (x==)
            encode (n:ns) [rank, suit]
                | n % 13 == indexOf rank "23456789TJQKA" && n % 4 == indexOf suit "CDHS" = n
                | otherwise = encode ns [rank, suit]
            form = map (\n -> encode [0..] [("A23456789TJQK" !! ((n - 1) % 13)), ("CDHS" !! ((n - 1) // 13))])

    winningHand n cards1 cards2
        | null cards1 = assert (null cards2) []
        | n < 1 = []
        | ranking1 > ranking2 = makeHand cards1 hand1
        | ranking1 < ranking2 = makeHand cards2 hand2
        | tieBroken == 1 = makeHand cards1 hand1
        | tieBroken == 2 = makeHand cards2 hand2
        | otherwise = hand1 ++ winningHand
                        (n - assert (length hand1 == length hand2) (length hand1))
                        (cards1 \\ hand1)
                        (cards2 \\ hand2)
        where
            makeHand cards hand = hand ++ (take (n - length hand) (sortOn ((13 -) . (%13)) (cards \\ hand))) 
            (ranking1, hand1) = bestHand cards1
            (ranking2, hand2) = bestHand cards2
            tieBroken = tieBreaker hand1 hand2

    bestHand hand = foldr1 (\xs ys -> if null (snd ys) then xs else ys) (map ($ hand) pokerHands)

    tieBreaker = tieBreaker' `on` highestVals where
        highestVals = reverse . arrangeVals . map (% 13) where
            arrangeVals vals = if vals `intersect` straightWithLowAce == straightWithLowAce
                then [-1..3]
                else sort vals
        tieBreaker' [] [] = 0
        tieBreaker' (card1:cards1) (card2:cards2)
            | card1 > card2 = 1
            | card1 < card2 = 2
            | otherwise = tieBreaker' cards1 cards2

    straightFlush = straight . flush
    fourOfAKind = eqOn 4 (% 13)
    fullHouse = pairAnd threeOfAKind
    flush = eqOn 5 (% 4)
    straight cards = (sort (map (% 13) cards) `elem` cons5) ? cards where
        cons5 = straightWithLowAce : [[n..n+4] | n <- [0..8]]
    threeOfAKind = eqOn 3 (% 13)
    twoPair = pairAnd pair
    pair = eqOn 2 (% 13)
    highCard = eqOn 1 id
    pokerHands = zipWith rank [1..] [
        highCard
        , pair
        , twoPair
        , threeOfAKind
        , straight
        , flush
        , fullHouse
        , fourOfAKind
        , straightFlush
        ] where
            rank ranking pokerHand cards = (ranking, handResult) where
                handResult = foldr1 bestVersionOfHand (map (pokerHand . take 5) $ permutations cards) where
                    bestVersionOfHand [] ys = ys
                    bestVersionOfHand xs [] = xs
                    bestVersionOfHand xs ys
                        | tieBreaker xs ys < 2 = xs
                        | otherwise = ys

    (%) = mod
    (//) = div
    straightWithLowAce = [0,1,2,3,12]
    tryTake n xs
        | length xs < n = []
        | otherwise = take n xs
    cond ? xs
        | cond = xs
        | otherwise = []
    eqOn n f cards = allEq (tryTake n $ map f cards) ? tryTake n cards
    allEq [] = False
    allEq (x:xs) = all (== x) xs
    pairAnd pokerHand cards
        | any null [picked1, picked2] = []
        | otherwise = picked1 ++ picked2
        where
            picked1 = pokerHand cards
            picked2 = pair $ cards \\ picked1