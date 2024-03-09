{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Grisette
import GHC.Generics
import Data.Time
import Data.Hashable
import Data.Maybe
import Data.Proxy
import Control.DeepSeq
import Control.Monad.Except
import System.CPUTime
import System.Timeout as SysTimeout

type Ident = String
      
data Unit =
      Unit
    deriving stock (Generic, Show, Eq)
    deriving (Mergeable, EvaluateSym, ToCon Unit, ExtractSymbolics)
        via (Default Unit)

instance SimpleMergeable Unit where
  mrgIte cond l r = go cond l r
    where
      go cond Unit Unit = Unit
      go _ _ _ = error "Should not happen"

------pre output end-----

------program space begin----
data EnvValue
  = Env0 Unit
  | Env1 SymInteger
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
          Env1 _ -> 1 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          1 -> SimpleStrategy $ \cond (Env1 l) (Env1 r) -> Env1 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
      go cond (Env1 l) (Env1 r) = Env1 $ mrgIte cond l r
      go _ _ _ = error "Should not happen"

$(makeUnionWrapper "u" ''EnvValue)

-- store all var from context
newtype RefEnv = RefEnv {unRefEnv :: [(Ident, EnvValue)]}
    deriving (Show, Generic)
    deriving (EvaluateSym) via (Default RefEnv)

instance Mergeable RefEnv where
    rootStrategy = SimpleStrategy $ \cond (RefEnv l) (RefEnv r) -> RefEnv $ go cond l r
        where
            go _ [] [] = []
            go cond ((li, lv) : l) ((ri, rv) : r)
                | li == ri = (ri, mrgIte cond lv rv) : go cond l r
            go _ _ _ = error "Should not happen"

instance SimpleMergeable RefEnv where
    mrgIte cond (RefEnv l) (RefEnv r) = RefEnv $ go cond l r
        where
            go _ [] [] = []
            go cond ((li, lv) : l) ((ri, rv) : r)
                | li == ri = (ri, mrgIte cond lv rv) : go cond l r
            go _ _ _ = error "Should not happen"

evalFunc :: RefEnv -> Ident -> EnvValue
evalFunc (RefEnv env) x = case lookup x env of
  Just v -> v
  Nothing -> error "Bad program: variable not in scope"

evalVar0 :: RefEnv -> Ident -> Unit
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> SymInteger
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
Unit
SymInteger
-}

-- output_type: Unit
-- param_list: a tmp1 t b h
data Expr0_0
  = Param10_0
  | Param30_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

-- output_type: Unit
-- param_list: xs tmp2
data Expr1_0
  = Unit1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

-- output_type: Int
-- param_list: l p n
data Expr2_0
  = Param22_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_1) (UnionM Expr2_0) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Ceq2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cless2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cleq2_1 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cand2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cor2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cnot2_1 (UnionM Expr2_1)
  | CFalse2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

-- output_type: Unit
-- param_list: sol n m subres remain tailres choice sols
data Expr3_0
  = Unit3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr3_0)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam10_0] ++ [mrgParam30_0]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        res <- chooseUnionFresh (genSingle0)
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgUnit1_0]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        res <- chooseUnionFresh (genSingle0)
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam22_0] ++ [mrgCzero2_0]
    genSingle1 = [mrgCFalse2_1]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e1_0 e0_4 e0_5])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq2_1 e0_0 e0_1] ++ [mrgCless2_1 e0_2 e0_3] ++ [mrgCleq2_1 e0_4 e0_5] ++ [mrgCand2_1 e1_0 e1_1] ++ [mrgCor2_1 e1_2 e1_3] ++ [mrgCnot2_1 e1_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgUnit3_0]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        res <- chooseUnionFresh (genSingle0)
        return res

eval0_0 :: RefEnv -> Expr0_0 -> Unit
eval0_0 env (Param10_0) = evalVar0 env "tmp1"
eval0_0 env (Param30_0) = evalVar0 env "b"

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> Unit
evalU0_0 env = onUnion (eval0_0 env)

eval1_0 :: RefEnv -> Expr1_0 -> Unit
eval1_0 env (Unit1_0) = Unit

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> Unit
evalU1_0 env = onUnion (eval1_0 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Param22_0) = evalVar1 env "n"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymBool
eval2_1 env (Ceq2_1 p0 p1) =  (evalU2_0 env p0) ==~ (evalU2_0 env p1) 
eval2_1 env (Cless2_1 p0 p1) =  (evalU2_0 env p0) <~ (evalU2_0 env p1) 
eval2_1 env (Cleq2_1 p0 p1) =  (evalU2_0 env p0) <=~ (evalU2_0 env p1) 
eval2_1 env (Cand2_1 p0 p1) =  (evalU2_1 env p0) &&~ (evalU2_1 env p1) 
eval2_1 env (Cor2_1 p0 p1) =  (evalU2_1 env p0) ||~ (evalU2_1 env p1) 
eval2_1 env (Cnot2_1 p0) =  mrgIte ((evalU2_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_1 env (CFalse2_1) = (toSym False)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymBool
evalU2_1 env = onUnion (eval2_1 env)

eval3_0 :: RefEnv -> Expr3_0 -> Unit
eval3_0 env (Unit3_0) = Unit

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> Unit
evalU3_0 env = onUnion (eval3_0 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
a tmp1 t b h 

Hole grammar for #1
xs tmp2 

Hole grammar for #2
l p n 

Hole grammar for #3
sol n m subres remain tailres choice sols 
-}

data List
  = Nil Unit
  | Cons SymInteger List
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)

instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go cond (Cons l1 r1) (Nil r2) = Cons l1 r1
      go cond (Nil l) (Cons l2 r2) = Cons l2 r2
      go _ _ _ = error "Should not happen"

data NList
  = Nnil Unit
  | Ncons List NList
  deriving stock (Generic, Show, Eq)
  deriving (Mergeable, EvaluateSym, ToCon NList, ExtractSymbolics)
    via (Default NList)

instance SimpleMergeable NList where
  mrgIte cond l r = go cond l r
    where
      go cond (Ncons l1 r1) (Ncons l2 r2) = Ncons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nnil l) (Nnil r) = Nnil Unit
      go cond (Ncons l1 r1) (Nnil r2) = Ncons l1 r1
      go cond (Nnil l) (Ncons l2 r2) = Ncons l2 r2
      go _ _ _ = error "Should not happen"

div' :: SymInteger -> SymInteger -> SymInteger
div' a b = 
    let 
        a1 = toCon (ssym "a" :: SymInteger) :: Maybe Integer 
    in
    let
        b1 = toCon (ssym "b" :: SymInteger) :: Maybe Integer 
    in
        case a1 of 
            Nothing -> 0 :: SymInteger
            Just a2 ->
                case b1 of
                    Nothing -> 0 :: SymInteger
                    Just b2 -> if b2 == 0 then 0::SymInteger else let c = a2 `div` b2 in (toSym c)

get1from3 (a, _, _) = a
get2from3 (_, b, _) = b
get3from3 (_, _, c) = c

get1from4 (a, _, _, _) = a
get2from4 (_, b, _, _) = b
get3from4 (_, _, c, _) = c
get4from4 (_, _, _, d) = d

get1from5 (a, _, _, _, _) = a
get2from5 (_, b, _, _, _) = b
get3from5 (_, _, c, _, _) = c
get4from5 (_, _, _, d, _) = d
get5from5 (_, _, _, _, e) = e

get1from6 (a, _, _, _, _, _) = a
get2from6 (_, b, _, _, _, _) = b
get3from6 (_, _, c, _, _, _) = c
get4from6 (_, _, _, d, _, _) = d
get5from6 (_, _, _, _, e, _) = e
get6from6 (_, _, _, _, _, f) = f

length' = 
  let
    f xs = 
      case xs of
        Nil _ -> 0
        Cons h t -> 1 +  (f t)
      
  in
  f 

sum' = 
  let
    f xs = 
      case xs of
        Nil _ -> 0
        Cons h t -> h +  (f t)
      
  in
  f 

append w = 
  let
    f a = 
      case a of
        Nil _ -> Cons w (Nil Unit)
        Cons h t -> Cons h (f t)
      
  in
  f 

cat = 
  let
    f a b = 
      case a of
        Nil _ -> b
        Cons h t -> 
          let
            tmp1 =
              (f t b)
          in
          evalU0_0 (RefEnv [("tmp1", (Env0 tmp1)), ("b", (Env0 b))]) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0))
      
  in
  f 

concat' = 
  let
    f xs = 
      case xs of
        Nnil _ -> 
          let
            tmp2 =
              (Nil Unit)
          in
          evalU1_0 (RefEnv []) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))
        Ncons h t -> cat h (f t)
      
  in
  f 

safe p l n = 
  let
    m =
      (1 +  (evalU2_0 (RefEnv [("n", (Env1 n))]) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))))
  in
  let
    f xs i = 
      case xs of
        Nil _ -> (toSym True)
        Cons j t -> mrgIte ((j ==~  n) ||~  (((i +  j) ==~  (n +  m)) ||~  ((i -  j) ==~  (m -  n))))
            ((toSym False))
            (f t (i +  1))
      
  in
  f  p 1

queens n = 
  let
    f m = 
      mrgIte (m ==~  0)
        (Ncons (Nil Unit) (Nnil Unit))
        (let
          subres =
            (f (m -  1))
        in
        let
          enum =
            (let
              g sols choice = 
                case sols of
                  Nnil _ -> mrgIte (choice ==~  n)
                      (Nnil Unit)
                      (g subres (choice +  1))
                  Ncons sol remain -> 
                    let
                      tailres =
                        (g remain choice)
                    in
                    mrgIte (safe sol (evalU3_0 (RefEnv []) ((genSym (4::Int) "hole3") :: (UnionM Expr3_0))) choice)
                      (Ncons (append choice sol) tailres)
                      (tailres)
                
            in
            g )
        in
        enum subres 1)
  in
  f  n

main' n = 
  mrgIte (n >~  0)
    (queens n)
    (Nnil Unit)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(SymInteger, NList)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(SymInteger, NList)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = (if ((main' x) == y) then (toSym True) else (toSym False)) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((-1))), (Nnil Unit))
                , ((((2))), (Nnil Unit))
                , ((((5))), (Ncons ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit)))))))))))) ((Nnil Unit))))))))))))))))))))))
                , ((((4))), (Ncons ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Nil Unit)))))))))) ((Ncons ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Nil Unit)))))))))) ((Nnil Unit))))))
                , ((((-2))), (Nnil Unit))
                , ((((-1))), (Nnil Unit))
                , ((((0))), (Nnil Unit))
                , ((((3))), (Nnil Unit))
                , ((((0))), (Nnil Unit))
                , ((((-4))), (Nnil Unit))
                , ((((-1))), (Nnil Unit))
                , ((((4))), (Ncons ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Nil Unit)))))))))) ((Ncons ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Nil Unit)))))))))) ((Nnil Unit))))))
                , ((((3))), (Nnil Unit))
                , ((((-3))), (Nnil Unit))
                , ((((3))), (Nnil Unit))
                , ((((-4))), (Nnil Unit))
                , ((((1))), (Ncons ((Cons ((1)) ((Nil Unit)))) ((Nnil Unit))))
                , ((((-2))), (Nnil Unit))
                , ((((-1))), (Nnil Unit))
                , ((((5))), (Ncons ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((2)) ((Cons ((5)) ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Cons ((2)) ((Cons ((4)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((2)) ((Cons ((5)) ((Nil Unit)))))))))))) ((Ncons ((Cons ((2)) ((Cons ((4)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit)))))))))))) ((Nnil Unit))))))))))))))))))))))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
