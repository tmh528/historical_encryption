module EnigmaMachine where

import Data.List
import Data.Maybe


--wheels
--plain wheel


--plwe = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
plwe = ['A'..'Z']
--Enigma 1
we1= "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
we2= "AJDKSIRUXBLHWTMCQGZNPYFVOE"
we3= "BDFHJLCPRTXVZNYEIWGAKMUSQO"
--M3 Army
we4= "ESOVPZJAYQUIRHXLNFTGKDCMWB"
we5= "VZBRGITYUPSDNHLXAWMJQOFECK"

--reflectors
--Enigma 1
refA ="EJMZALYXVBWFCRQUONTSPIKHGD"
--Enigma M3
refB="AYBRCUDHEQFSGLIPJXKNMOTZVW"
refC="AFBVCPDJEIGOHYKRLZMXNWTQSU"

--getPlugboard :: undefined
--plugboard <- getline


whichWheel :: Int -> (String, Char)
-- functionally a dictionary of the wheels and their turnover points
whichWheel 1 = (we1, 'Q')
whichWheel 2 = (we2, 'E')
whichWheel 3 = (we3, 'V')
whichWheel 4 = (we4, 'J')
whichWheel 5 = (we5, 'Z')
-- whichWheel _ = Nothing?



plugboarding :: Char -> String -> Char
--(could do list of tuples? )
--plugboarding l plug | AH ELEM MORE DIFICULT, GOT IT
-- | l `notElem` plug = l

plugboarding l plug | isNothing ind = l
                    | fromJust ind `mod` 2 == 0 = plug !! (fromJust ind + 1)
                    | otherwise = plug !! (fromJust ind - 1)
                    where
                        ind = elemIndex l plug

                

{-
simpleRotateWheel :: String -> String
{-but what about the turn next letter??? | x == turnLetter (then turnNext) = 1-}
simpleRotateWheel (x:xs) = xs ++ [x]
-}

-- might work better with tuples instead?
--[(String,Char),(String,Char),(String,Char)]  


rotateWheels :: [(String,Char)] -> [(String,Char)] 
rotateWheels [(xs,xTurn),(ys,yTurn),(zs,zTurn)]
                    | isNextTurn xs xTurn && isNextTurn ys yTurn = [(simpleRotateWheel xs,xTurn),(simpleRotateWheel ys,yTurn),(simpleRotateWheel zs,zTurn)]
                    | isNextTurn xs xTurn = [(simpleRotateWheel xs,xTurn),(simpleRotateWheel ys,yTurn),(zs,zTurn)]
                    | otherwise = [(simpleRotateWheel xs,xTurn),(ys,yTurn),(zs,zTurn)]
                    where simpleRotateWheel (first:tail) = tail ++ [first]
                          isNextTurn (first:_) turn = first == turn

freeWheel :: [Int] -> [(String,Char)] -> [(String,Char)]
--set Grundstellung: the starting configurations
freeWheel [xTurns,yTurns,zTurns] --unfinished XXXXX

