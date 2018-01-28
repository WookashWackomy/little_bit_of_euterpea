module MelodicFunctions
    (Progression,Mod,oo, heynow,accMaj,accMin,accDim,composeMelody,composeWhole,run2
    ) where
--Author: Mikolaj Pniak, Lukasz Lakomy
import Euterpea
import Test.HUnit




----------------------------------------------------------------------
--Example functions for hard-coded music:
heynow = line [
 b 4 en, g 4 en, enr, g 4 sn, e 4 sn, g 4 en, g 4 en, enr, g 4 sn, e 4 sn, --single tact
 g 4 en, g 4 en, enr, g 4 en, enr, b 4 qn, enr,
 b 4 en, g 4 en, enr, g 4 sn, e 4 sn, g 4 en, g 4 en, enr, g 4 sn, e 4 sn,
 g 4 en, g 4 en, enr, g 4 en, enr, b 4 qn, enr,
 b 4 qn, d 5 qn, c 5 en, b 4 en, a 4 en, a 4 en,
 g 4 hn, g 4 en, g 4 en, a 4 en, g 4 en,
 b 4 en, a 4 dqn, a 4 en, g 4 qn, a 4 en,
 b 4 en, e 4 (5/8), qnr]


--Type 'play heynow' to find out :)
--Notation above means e.g. "a 4 dqn" == dotted quarter note, A in 4th octave. "enr" == eighth note rest.
-- | 'forever' function plays music note or piece indefinitely
-- forever    :: Music a -> Music a
-- forever m  = m :+: forever m

weee = play $ forever $ progress hn (Fs,oo 3) Maj [I,IV,I,VI,II,V,I,V]
sbody = play $ forever $ progress hn (G,oo 3) Maj [I,V,II,IV]
--Some progression functions. Press Ctrl+C to interrupt a loop.
----------------------------------------------------------------------
x1 = (c 4 en :=: g 4 en) :+: (g 4 en :=: d 4 en) :+: (d 4 en :=: a 4 en)
x1p = line[c 4 en :=: g 4 en,g 4 en :=: d 4 en,d 4 en :=: a 4 en]
x1pp = line(map fn [(c,g),(g,d),(d,a)])
       where fn(n1,n2) = n1 4 en :=: n2 4 en
----------------------------------------------------------------------
-- | 'delayM' function applies interval of silence lasting d to m
delayM      :: Dur -> Music a -> Music a
delayM d m  = rest d :+: m
-- | 'rep' is a recursive function that applies two functions f,g to a note,transforms it and invokes itself n times.
rep :: (Music a -> Music a) -> (Music a -> Music a) -> Int ->
       Music a -> Music a
rep f g 0 m = rest 0
rep f g n m = m :=: g (rep f g (n-1) (f m))
run       = rep (transpose 5) (delayM tn) 8 (c 4 tn)
run2 = rep (transpose 5) (delayM tn) 8 (run)





----------------------------------------------------------------------
-- |Function wrapping Int into Octave.
oo :: Int -> Octave
oo x = x::Octave


--IMPORTANT NOTE: a :: Octave -> Dur -> Music Pitch
--Example: (Gs,oo 3) means a pitch of G sharp in 5th octave.
--Proper use of the following functions: e.g. accMaj qn (Ef,oo 5)

-- |Given duration and a (PitchClass,Octave) tuple, returns a Maj chord based on given pitch.
--Example: accMaj (5/8) (F,oo 2)
accMaj :: Dur -> a -> Music a
accMaj d a = (note d a) :=: transpose 4 (note d a) :=: transpose 7 (note d a)


-- |Given duration and a, returns a Min chord based on a given pitch.
--Example: accMin sn (Ds,oo 4)
accMin :: Dur -> a -> Music a
accMin d a = (note d a) :=: transpose 3 (note d a) :=: transpose 7 (note d a)



-- |Given duration and a, returns a diminished chord based on a given pitch.
--Example: accDim qn (Fss,oo 3)
accDim :: Dur -> a -> Music a
accDim d a = (note d a) :=: transpose 3 (note d a) :=: transpose 6 (note d a)


-- |Given duration, a, mode and a list of chords (in scale given in a + mode), returns progression.
--Example: progress hn (Bf,oo 4) Maj [I,IV,V,I]
--Note: Change tempo by using "tempo r m" (where r - ratio x/60bpm, m - melody).
data Mod = Min | Maj
data Progression = I|II|III|IV|V|VI|VII
progress :: Dur -> a -> Mod -> [Progression] -> Music a
progress d a Maj [] = (rest 0)
progress d a Min [] = (rest 0)
progress d a Maj (n:ns) = case n of I   -> (accMaj d a)               :+: (progress d a Maj ns)
                                    II  -> (transpose 2 (accMin d a)) :+: (progress d a Maj ns)
                                    III -> (transpose 4 (accMin d a)) :+: (progress d a Maj ns)
                                    IV  -> (transpose 5 (accMaj d a)) :+: (progress d a Maj ns)
                                    V   -> (transpose 7 (accMaj d a)) :+: (progress d a Maj ns)
                                    VI  -> (transpose 9 (accMaj d a)) :+: (progress d a Maj ns) -- should be minor but whatever
                                    VII -> (transpose 11 (accDim d a)) :+: (progress d a Maj ns)
progress d a Min (n:ns) = case n of I   -> (accMaj d a)               :+: (progress d a Min ns)
                                    II  -> (transpose 2 (accDim d a)) :+: (progress d a Min ns)
                                    III -> (transpose 3 (accMaj d a)) :+: (progress d a Min ns)
                                    IV  -> (transpose 5 (accMin d a)) :+: (progress d a Min ns)
                                    V   -> (transpose 7 (accMaj d a)) :+: (progress d a Min ns)
                                    VI  -> (transpose 8 (accMaj d a)) :+: (progress d a Min ns)
                                    VII -> (transpose 11 (accDim d a)) :+: (progress d a Min ns)



-- | Function which combines modulo and signum.
md :: Int -> Int -> Int
md x m = if x >= 0
 then mod x m
 else -(mod x m)

-- | Function which combines random melody with a simple progression (lasting 8 tacts)
composeWhole :: a -> Int -> Music a
composeWhole t r = ((progress wn t Maj [I,I,V,IV]) :=: (composeMelody t r))

-- | Function which creates pseudo-random melody, based on given seed and key.
-- Combines 8 tacts (2/4 measure) of melody, matching (kind of) the progression.
composeMelody :: a -> Int -> Music a
composeMelody t r = (composeTact t (md 64535 r)) :+: (composeTact t (md 53874 (2*r))) :+: (composeTact t (md 40000 (7*r+2))) :+: (composeTact t (md 93948 (51*r))) :+: transpose 7 (composeTact t (md 65938 (67*r))) :+: transpose 7 (composeTact t (md 65392 ((-5)*r))) :+: transpose 5 (composeTact t (md 29381 r)) :+: transpose 5 (composeTact t (md 44431 (2*(r+1))))

-- | Function which "randomly" chooses rhythm within a tact, for more "randomness".
composeTact :: a -> Int -> Music a
composeTact t r = if md r 3 == (-2) then (type3 t r)
             else if md r 3 == (-1) then (type4 t r)
             else if md r 3 == 0    then (type0 t r)
             else if md r 3 == 1    then (type1 t r)
             else (type2 t r )

-- | Function which "randomly" chooses a pitch from a pentatonic based on a tonic of the active progression chord.
getPitch :: Int -> Int
getPitch r = if md r 3 == (-2) then (-5)
             else if md r 3 == (-1) then (-2)
             else if md r 3 == 0    then 0
             else if md r 3 == 1    then 2
             else 5


type0 :: a -> Int -> Music a
type0 t r = (transpose (getPitch (md 22 (74*r+1))) (note hn t))

type1 :: a -> Int -> Music a
type1 t r = ((transpose (getPitch (md 25 (7*r+2))) (note qn t)) :+: (transpose (getPitch (md 40 (17*r+2))) (note qn t)))

type2 :: a -> Int -> Music a
type2 t r = ((transpose (getPitch (md 29 (78*r))) (note dqn t)) :+: (transpose (getPitch (md 20 (37*r+5))) (note en t)))

type3 :: a -> Int -> Music a
type3 t r = ((transpose (getPitch (md 13 (37*r+8))) (note qn t)) :+: (transpose (getPitch (md 20 (44*r+2))) (note en t)) :+: (transpose (getPitch (md 65 (7*r+35))) (note en t)))

-- | Above functions return created parts of melody.
type4 :: a -> Int -> Music a
type4 t r = ((transpose (getPitch (md 92 (70*r))) (note qn t)) :+: (rest qn))
