
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
      |Mod E E -- Mod E E: retorna o resto da divisão de E1 por E2
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | Twice C   ---- Executa o comando C 2 vezes
    | RepeatUntil C B --- Repeat C until B: executa C até que B seja verdadeiro
    | ExecN C E      ---- ExecN C n: executa o comando C n vezes
    | Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
    | Swap E E --- recebe duas variáveis e troca o conteúdo delas
    | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
   deriving(Eq,Show)   

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

--- EXPRESSÕES ARITMÉTICAS
smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)  
                        
smallStepE (Sub (Num n1) (Num n2), s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e, s)          = let (el, sl) = smallStepE (e,s)
                                         in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)               = let (el, sl) = smallStepE (e1,s)
                                         in (Sub el e2, sl)

smallStepE (Mod (Num n1) (Num n2), s)  = (Num (mod n1 n2), s)
smallStepE (Mod (Num n) e, s)          = let (el, sl) = smallStepE (e,s)
                                         in (Mod (Num n) el, sl)
smallStepE (Mod e1 e2,s)               = let (el, sl) = smallStepE (e1,s)
                                         in (Mod el e2, sl)

--- EXPRESSÕES BOOLEANAS
smallStepB :: (B,Memoria) -> (B, Memoria)
smallStepB (Not b,s) 
    | not (isFinalB b)              =   let (bl, sl) = smallStepB (b,s) in (Not bl, sl)
smallStepB (Not TRUE,s)             =   (FALSE,s)
smallStepB (Not FALSE,s)            =   (TRUE,s)

smallStepB (And TRUE b2, s) = (b2, s)
smallStepB (And FALSE _, s) = (FALSE, s)
smallStepB (And b1 b2, s) 
    | not (isFinalB b1) = let (bl, sl) = smallStepB (b1, s) in (And bl b2, sl)
    | otherwise = let (br, sr) = smallStepB (b2, s) in (And b1 br, sr)

smallStepB (Or TRUE _, s) = (TRUE, s)
smallStepB (Or FALSE b2, s) = (b2, s)
smallStepB (Or b1 b2, s)
    | not (isFinalB b1) = let (bl, sl) = smallStepB (b1, s) in (Or bl b2, sl)
    | otherwise = let (br, sr) = smallStepB (b2, s) in (Or b1 br, sr)

smallStepB (Leq (Num n1) (Num n2), s)
    | n1 <= n2 = (TRUE, s)
    | otherwise = (FALSE, s)
smallStepB (Leq (Num n) e, s)       =   let (el, sl) = smallStepE (e,s)
                                        in (Leq (Num n) el, sl)
smallStepB (Leq e1 e2, s)           =   let (el, sl) = smallStepE (e1,s)
                                        in (Leq el e2, sl)

smallStepB (Igual (Num n1) (Num n2), s)
    | n1 == n2 = (TRUE, s)
    | otherwise = (FALSE, s)
smallStepB (Igual (Num n) e, s)     =   let (el, sl) = smallStepE (e,s)
                                        in (Igual (Num n) el, sl)
smallStepB (Igual e1 e2, s)         =   let (el, sl) = smallStepE (e1,s)
                                        in (Igual el e2, sl)


--- COMANDOS
smallStepC (Atrib (Var x) e,s) 
    | not (isFinalE e) = let (el, sl) = smallStepE (e,s) in (Atrib (Var x) el, sl)
smallStepC (Atrib (Var x) (Num n),s) = (Skip, mudaVar s x n)

smallStepC (Seq c1 c2,s) 
    | not (isFinalC c1) = let (cl, sl) = smallStepC (c1,s) in (Seq cl c2, sl)
smallStepC (Seq Skip c2,s) = (c2,s)

smallStepC (If b c1 c2,s) 
    | not (isFinalB b) = let (bl, sl) = smallStepB (b,s) in (If bl c1 c2, sl)
smallStepC (If TRUE c1 c2,s) = (c1,s)
smallStepC (If FALSE c1 c2,s) = (c2,s)

smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

smallStepC (Twice c, s) = (Seq c c, s)

smallStepC (RepeatUntil c b, s) = (While (Not b) c, s)

smallStepC (ExecN c e, s) = (Seq c (If (Igual e (Num 1)) Skip (ExecN c (Sub e (Num 1)))), s)

smallStepC (Assert b c, s) = (If b c Skip, s) --- Assert B C: caso B seja verdadeiro, executa o comando C

smallStepC (Swap (Var x) (Var y), s) = (DAtrrib (Var x) (Var y) (Var y) (Var x), s)

  ---  | DAtrrib E E E E -- Dupla atribuição: recebe duas variáveis "e1" e "e2" e duas expressões "e3" e "e4". Faz e1:=e3 e e2:=e4.
smallStepC (DAtrrib (Var x) (Var y) e3 e4, s)
    | not (isFinalE e3) = let (el3, sl3) = smallStepE (e3, s) in (DAtrrib (Var x) (Var y) el3 e4, sl3)
smallStepC (DAtrrib (Var x) (Var y) (Num n1) e4, s)
    | not (isFinalE e4) = let (el4, sl4) = smallStepE (e4, s) in (DAtrrib (Var x) (Var y) (Num n1) el4, sl4)
smallStepC (DAtrrib (Var x) (Var y) (Num n1) (Num n2), s) = (Skip, mudaVar (mudaVar s x n1) y n2)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas


isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

-- Descomentar quanto a função smallStepB estiver implementada:

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

-- Descomentar quando a função smallStepC estiver implementada:

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---


exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


--- Para rodar os próximos programas é necessário primeiro implementar as regras que faltam
--- e descomentar os respectivos interpretadores


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y"))) 
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


--- O ALUNO DEVE IMPLEMENTAR EXEMPLOS DE PROGRAMAS QUE USEM:
--  * RepeatUntil C B --- Repeat C until B: executa C até que B seja verdadeiro
 -- * ExecN C E      ---- ExecN C n: executa o comando C n vezes
 -- * Assert B C --- Assert B C: caso B seja verdadeiro, executa o comando C
 -- * Swap E E --- recebe duas variáveis e troca o conteúdo delas
 -- *  DAtrrib E E E E

--- Programas Criados
fibo :: C -- Retorna o z-ésimo número da sequência de Fibonacci
--- DAtrrib e RepeatUntil
fibo = (Seq (DAtrrib (Var "x") (Var "y") (Num 1) (Num 1))
            (RepeatUntil (Seq (If (Leq (Mod (Var "z")(Num 2)) (Num 0)) 
                    (Atrib (Var "x") (Soma (Var "x") (Var "y"))) 
                    (Atrib (Var "y") (Soma (Var "x") (Var "y")))) 
                    (Atrib (Var "z") (Sub (Var "z") (Num 1)))) 
                (Igual (Var "z") (Num 0))))


allEqual :: C -- Recebe 3 variáveis e verifica se todas são iguais, se sim, seta "x" para -1
--- Assert
allEqual = Assert (Igual (Var "x") (Var "y")) (Assert (Igual (Var "y") (Var "z")) (Atrib (Var "x") (Sub (Num 0) (Num 1))))


potencia :: C -- Recebe 2 variáveis e seta "x" para a potência de "y" elevado a "z"
--- ExecN
potencia = (Seq (Atrib (Var "x") (Num 1))
                (ExecN (Atrib (Var "x") (Mult (Var "x") (Var "y"))) (Var "z")))


circularSwap :: C -- Recebe 3 variáveis e faz uma troca circular entre elas
--- Swap
circularSwap = Seq (Swap (Var "z") (Var "y")) (Swap (Var "y") (Var "x"))
