module Tema1 (
        solveSimple,
        solveCosts
        ) where

import Data.Array
import Data.List
import Data.Maybe


-- FUNCTII AJUTATOARE SolveSimple

-- functie care returneaza costul drumului dintre 2 orase
-- sau infinit daca nu exista drum intre cele 2 orase
getCost [] _ _= 99999
getCost ((x1, x2, cost) : t) y1 y2 = if (y1 == y2) then 0
                                    else if ((x1 == y1) && (x2 == y2)) ||
                                        ((x1 == y2) && (x2 == y1)) then cost
                                     else (getCost t y1 y2)
first3 (a, b, c) = a
second3 (a, b, c) = b
third3 (a, b, c) = c
first2 (a, b) = a
second2 (a, b) = b


getNeighbour i (x1, x2, cost) = if x1 == i then x2
                                else x1
-- functie care intoarce lista oraselor vecine cu orasul x
getAllNeighbours list x = map (getNeighbour x) 
                        (filter (\(x1,x2,_)-> x1 == x || x2 == x ) list)

getMat mat i j = mat ! (i, j)   -- accesez elementul (i,j) din matrice


-- calcularea costului in matrice la pozitia (i, j) si aflarea oraselor prin care s-a trecut

auxList i j mat list = ( (getMat mat (i-1) j):(map (getMat mat (i-1)) (getAllNeighbours list j)) ) --lista de dp[i-1][k]
                                                                                        -- unde k este oras vecin sau j

addCostList j list = map (getCost list j) (j:(getAllNeighbours list j))         -- cost(k, j)
finalCostList i j mat list = zipWith (+) (map second2 (auxList i j mat list)) (addCostList j list)  -- lista coturi

minCost i j mat list = minimum (finalCostList i j mat list) -- costul minim

indexMin i j mat list = fromJust (elemIndex (minCost i j mat list) (finalCostList i j mat list))
listPathList i j mat list = listArray (0, (length (auxList i j mat list))) (auxList i j mat list)

pathList i j mat list = let          -- lista oraselor prin care trecem pentru a obtine cost minim
            auxVar i j mat list = (first2 ((listPathList i j mat list) ! (indexMin i j mat list))) 
            result i j mat list = if (head (auxVar i j mat list) == j) then (auxVar i j mat list)
                else j:(auxVar i j mat list)
            in result i j mat list
                                                                                        


-- IMPLEMENTARE solveSimple

solveSimple :: (Int, [(Int, Int, Int)]) -> Maybe ([Int], Int)
solveSimple arg = let

    n = first2 arg
    list = second2 arg

    bounds = ((1,1),(n,n))
    mat = listArray bounds [cost i j | (i, j) <- range bounds]

    cost i j
        | i == 1 = (j:1:[], (getCost list 1 j))
        | i == n && j == n = if (minCost i j mat list >= 99999) then ([], 0)    -- daca nu exista drum
                            else
                                (reverse(pathList i j mat list), minCost i j mat list)
        | otherwise = (pathList i j mat list, minCost i j mat list)

    getResult arg = if ( (second2 (mat ! (n, n))) == 0) then Nothing
                    else Just (mat ! (n, n) )
    in (getResult arg)






-- FUNCTII AJUTATOARE SolveCosts


first4 (a, b, c, d) = a
second4 (a, b, c, d) = b
third4 (a, b, c, d) = c
fourth4 (a, b, c, d) = d

getMoneyLeft i j initSum feeList = if (i == j) then initSum  -- adica nu plecam din orasul curent
                                    else let
                                        feeArray = listArray (1, length feeList) feeList
                                    in initSum - (feeArray ! j)


-- (list, costK) reprezinta un element dp[i-1][k] si le voi selecta pe cele cu suma ramasa > 0
filterMoneyLeft nextCity feeList (list, costK) = let

                lastCityIndex = first2 (head list)
                lastCitySum = second2 (head list)
                getFilterMoney = (getMoneyLeft lastCityIndex nextCity lastCitySum feeList) >= 0

                in getFilterMoney

getMoneyLeft2 nextCity feeList (list, costK) = let

                lastCityIndex = first2 (head list)
                lastCitySum = second2 (head list)

                in getMoneyLeft lastCityIndex nextCity lastCitySum feeList


resultMatrix i j list feeList mat = let
                
    auxList2 = filter (filterMoneyLeft j feeList) (auxList i j mat list)
    costsCitiesPassed = map second2 auxList2
    listLastCityArrived = map (first2.head.first2) auxList2
    costsToCurrentCity = map (getCost list j) listLastCityArrived
    totalCostList = zipWith (+) costsCitiesPassed costsToCurrentCity


    minCost2 = if (length totalCostList) == 0 then 99999    -- nu exista oras in care pot intra
                                                            -- cu suma curenta
                else minimum totalCostList -- costul minim

    indexMin2 = if (length totalCostList) == 0 then -1  -- nu exista oras in care pot intra
                                                        -- cu suma curenta

                else let    -- considerand ca ar putea fi mai multe orase din care rezulta cost minim
                    auxFilter (l, z) = (z + (getCost list j ((first2.head)  l) ) == minCost2)
                    listMinCosts = filter auxFilter auxList2

                    auxSumList = map (getMoneyLeft2 j feeList) listMinCosts
                    minSum = maximum auxSumList
                    indexMinSum = fromJust (elemIndex minSum auxSumList)

                    arr2 = listArray (1, length listMinCosts) listMinCosts
                    minSumAndCost = arr2 ! (indexMinSum + 1)
                    indexMinSumAndCost = fromJust (elemIndex minSumAndCost auxList2)

                in indexMinSumAndCost   -- il alegem pe cel care ne ajuta sa ramanem cu mai multi bani

    arr1 = listArray (1, length auxList2) auxList2
    resultAux = if (indexMin2 == -1) then ([(1,0)], 99999)
                else arr1 ! (indexMin2 + 1)  -- result1 si resultAux sunt de tip dp[i-1][k]

    constrFinalResult (list2, cost) = if cost == 99999 then ([(1,0)], 99999)    -- nu exista oras in care pot intra
                                                                                -- cu suma curenta
                else let
                    lastElem = head list2
                    lastCity = first2 lastElem
                    lastSum = second2 lastElem
                    finalSum = getMoneyLeft lastCity j lastSum feeList

                    result1 = if(lastCity == j) -- adica nu am plecat spre un alt oras
                            then (list2, cost)
                            else ((j,finalSum):list2, minCost2)
                in result1

    in constrFinalResult resultAux



solveCosts :: (Int, Int, [Int], [(Int, Int, Int)]) -> Maybe ([(Int, Int)], Int)
solveCosts arg = let

        n = first4 arg
        m = second4 arg
        feeList = third4 arg
        list = fourth4 arg

        bounds = ((1,1),(n,n))
        mat = listArray bounds [cost i j | (i, j) <- range bounds]

        cost i j
            | i == 1 = ( (j, (getMoneyLeft 1 j m feeList)):(1, m):[], (getCost list 1 j))
            | i == n && j == n = if (second2 (resultMatrix i j list feeList mat) >= 99999)
                            then ([], 0)    -- daca nu exista drum
                            else let
                                revResult (x,y) = (reverse x,y)
                            in revResult (resultMatrix i j list feeList mat)


            | otherwise = (resultMatrix i j list feeList mat)

        getResult = if ( (second2 (mat ! (n, n))) == 0) then Nothing
                    else Just (mat ! (n, n) )
    in getResult
