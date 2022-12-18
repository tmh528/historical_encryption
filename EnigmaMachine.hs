module EnigmaMachine where

import Data.List
import Data.Maybe
--TODO: ringstellung

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

                

simpleRotateWheel :: String -> String
simpleRotateWheel (x:xs) = xs ++ [x]

--[(String,Char),(String,Char),(String,Char)]  

rotateWheels :: [(String,Char)] -> [(String,Char)]
-- called with the exsisting wheel configurations to turn the wheels once
rotateWheels [(xs,xTurn),(ys,yTurn),(zs,zTurn)]
                    | isNextTurn xs xTurn && isNextTurn ys yTurn = [(simpleRotateWheel xs,xTurn),(simpleRotateWheel ys,yTurn),(simpleRotateWheel zs,zTurn)]
                    | isNextTurn xs xTurn = [(simpleRotateWheel xs,xTurn),(simpleRotateWheel ys,yTurn),(zs,zTurn)]
                    | otherwise = [(simpleRotateWheel xs,xTurn),(ys,yTurn),(zs,zTurn)]
                    where isNextTurn (first:_) turn = first == turn

freeWheel :: [Char] -> [(String,Char)] -> [(String,Char)]
--set Grundstellung: the starting configurations
--takes the letter that each wheel should be starting at (the one which would be visible through the screen)
freeWheel [xStart,yStart,zStart] [(xs,xTurn),(ys,yTurn),(zs,zTurn)] =
            [(spin xStart xs,xTurn),(spin yStart ys,yTurn),(spin zStart zs,zTurn)]
            where spin start (first:tail) | start == first = first:tail
                                          | otherwise = spin start (simpleRotateWheel (first:tail))
