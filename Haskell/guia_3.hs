-- !Mi resolución de la guía 3 de Haskell para Intro a la Programación.

-- *Ej 1.a
ej_1a_f :: Integer -> Integer
ej_1a_f 1 = 8
ej_1a_f 4 = 131
ej_1a_f 16 = 16

-- *Ej 1.b
ej_1b_g :: Integer -> Integer
ej_1b_g 8 = 16
ej_1b_g 16 = 4
ej_1b_g 131 = 1

-- *Ej 1.c
ej_1c_h :: Integer -> Integer
ej_1c_h = ej_1a_f . ej_1b_g

ej_1c_k :: Integer -> Integer
ej_1c_k = ej_1b_g . ej_1a_f

-- *Ej 2.a
{--
    problema absoluto(x: Z): Z {
        Requiere: {True}
        Asegura: {res = x <=> x>=0}
        Asegura: {res = x <=> x < 0}
    }
--}
ej_2a_absoluto :: Integer -> Integer
-- ej_2a_absoluto = abs
ej_2a_absoluto x
  | x >= 0 = x
  | otherwise = -x

-- *Ej 2.b
{--
    problema maximoAbsoluto(x:Z, y:Z): Z {
        Requiere: {True}
        Asegura: {res = absoluto(x) <=> absoluto(x)>=absoluto(y)}
        Asegura: {res = absoluto(y) <=> absoluto(x) < absoluto(y)}
    }
--}
ej_2b_maximoAbsoluto :: Integer -> Integer -> Integer
ej_2b_maximoAbsoluto x y
 | ej_2a_absoluto x >=ej_2a_absoluto y = x
 | otherwise = y
