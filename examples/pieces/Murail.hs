
import Music.Prelude
import Util

{-
Harmonic material from Murail's Gondwana
-}
carr = 392                                 :: Hertz
modu = carr*(intone (c,1) justT (c.+^ m3)) :: Hertz


fsum  i = abs $ carr + modu*(fromInteger i)
fdiff i = abs $ carr - modu*(fromInteger i)

chord1 = fmap fdiff [1..9] `merge` fmap fsum [1..9]


main = display $ chord1
{-
>>> map fsum [1..9]
[599.65 Hz,807.3 Hz,1014.95 Hz,1222.6 Hz,1430.25 Hz,1637.9 Hz,1845.55 Hz,2053.2 Hz,2260.8500000000004 Hz]
>>> map fdiff [1..9]
[184.35 Hz,23.30000000000001 Hz,230.95000000000005 Hz,438.6 Hz,646.25 Hz,853.9000000000001 Hz,1061.55 Hz,1269.2 Hz,1476.8500000000001 Hz]
-}


{-
>>> 
>>> 
>>> let c = 392
>>> let m = 207.65
>>> 
>>> let c = 392::Hertz
>>> let m = 207.65::Hertz
>>> 
>>> let fsum i = c + m^*i

<interactive>:76:19:
    No instance for (VectorSpace Hertz) arising from a use of ‘^*’
    In the second argument of ‘(+)’, namely ‘m ^* i’
    In the expression: c + m ^* i
    In an equation for ‘fsum’: fsum i = c + m ^* i
>>> let fsum i = c + m*i
>>> let fsum i = c + m*(fromInteger i)
>>> :t fsum
fsum :: Integer -> Hertz
>>> let fsum i = abs $ c + m*(fromInteger i)
>>> let fdiff i = abs $ c - m*(fromInteger i)
>>> 
>>> fsum [1..9]

<interactive>:83:6:
    Couldn't match expected type ‘Integer’ with actual type ‘[t0]’
    In the first argument of ‘fsum’, namely ‘[1 .. 9]’
    In the expression: fsum [1 .. 9]
    In an equation for ‘it’: it = fsum [1 .. 9]

>>> map fsum [1..9]
[599.65 Hz,807.3 Hz,1014.95 Hz,1222.6 Hz,1430.25 Hz,1637.9 Hz,1845.55 Hz,2053.2 Hz,2260.8500000000004 Hz]
>>> map fdiff [1..9]
[184.35 Hz,23.30000000000001 Hz,230.95000000000005 Hz,438.6 Hz,646.25 Hz,853.9000000000001 Hz,1061.55 Hz,1269.2 Hz,1476.8500000000001 Hz]
-}
