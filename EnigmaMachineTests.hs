module EnigmaMachineTests where
import EnigmaMachine

plugboardTests :: Bool
--tests of the plug board
plugboardTests = 
        --an odd indexed letter in plugboard
        plugboarding 'a' "abcdef" == 'b'
        -- an even indexed letter in plugboard
        && plugboarding 'b' "abcdef" == 'a'
        -- a letter not in plugboard (return letter)
        && plugboarding 'z' "abcdef" == 'z'
        -- plugboard wrong length
        {- TODO:
        && plugboarding 'a' "abcdefg" == --uh oh
        && plugboarding 'g' "abcdefg" == --oh no -}

freeWheelTests :: Bool
--test of freewheeling mechanism
freeWheelTests =
    -- where nothing could catch
    freeWheel ['G','O','B'] [whichWheel 1, whichWheel 2, whichWheel 3]
    == [("GDQVZNTOWYHXUSPAIBRCJEKMFL",'Q'),("OEAJDKSIRUXBLHWTMCQGZNPYFV",'E'),("BDFHJLCPRTXVZNYEIWGAKMUSQO",'V')]
    -- where, if catches were active, the second wheel would be turned by the first
    && freeWheel ['H','E','Y'] [whichWheel 1, whichWheel 2, whichWheel 3]
     == [("HXUSPAIBRCJEKMFLGDQVZNTOWY",'Q'),("EAJDKSIRUXBLHWTMCQGZNPYFVO",'E'),("YEIWGAKMUSQOBDFHJLCPRTXVZN",'V')]


rotateWheelTests :: Bool
--test of standard and un-rotated wheels:
rotateWheelTests =
    --test of wheels where there is no catches (only turn wheel 1)
    rotateWheels [whichWheel 1, whichWheel 2, whichWheel 3]
     == [("KMFLGDQVZNTOWYHXUSPAIBRCJE",'Q'),("AJDKSIRUXBLHWTMCQGZNPYFVOE",'E'),("BDFHJLCPRTXVZNYEIWGAKMUSQO",'V')]
    --test of wheels where the first wheel catches (turning wheels 1,2)
    && rotateWheels (freeWheel ['Z','O','O'] [whichWheel 5, whichWheel 4, whichWheel 3])
     == [("BRGITYUPSDNHLXAWMJQOFECKVZ",'Z'),("VPZJAYQUIRHXLNFTGKDCMWBESO",'J'),("OBDFHJLCPRTXVZNYEIWGAKMUSQ",'V')]
    --test of wheels where the first and second wheel catch (turning wheels 1,2,3)
    && rotateWheels (freeWheel ['Q','E','Y'] [whichWheel 1, whichWheel 2, whichWheel 3])
     == [("VZNTOWYHXUSPAIBRCJEKMFLGDQ",'Q'),("AJDKSIRUXBLHWTMCQGZNPYFVOE",'E'),("EIWGAKMUSQOBDFHJLCPRTXVZNY",'V')]