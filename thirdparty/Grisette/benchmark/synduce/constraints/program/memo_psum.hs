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
    deriving stock (Generic, Show)
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
  = Env0 SymBool
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
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

evalVar0 :: RefEnv -> Ident -> SymBool
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

{- env_type_list: 
SymBool
-}

-- output_type: Unit
-- param_list: a tmp1 e
data Expr0_0
  = Unit0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

-- output_type: Unit
-- param_list: e s
data Expr1_0
  = Unit1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

-- output_type: Unit
-- param_list: e s
data Expr2_0
  = Unit2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

-- output_type: Int
-- param_list: e tmp2 inp
data Expr3_0
  = Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_1) (UnionM Expr3_0) (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Ceq3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cless3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cleq3_1 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cand3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cor3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cnot3_1 (UnionM Expr3_1)
  | CFalse3_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgUnit0_0]
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
    genSingle0 = [mrgUnit2_0]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        res <- chooseUnionFresh (genSingle0)
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero3_0]
    genSingle1 = [mrgCFalse3_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e1_0 e0_4 e0_5])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq3_1 e0_0 e0_1] ++ [mrgCless3_1 e0_2 e0_3] ++ [mrgCleq3_1 e0_4 e0_5] ++ [mrgCand3_1 e1_0 e1_1] ++ [mrgCor3_1 e1_2 e1_3] ++ [mrgCnot3_1 e1_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> Unit
eval0_0 env (Unit0_0) = Unit

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> Unit
evalU0_0 env = onUnion (eval0_0 env)

eval1_0 :: RefEnv -> Expr1_0 -> Unit
eval1_0 env (Unit1_0) = Unit

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> Unit
evalU1_0 env = onUnion (eval1_0 env)

eval2_0 :: RefEnv -> Expr2_0 -> Unit
eval2_0 env (Unit2_0) = Unit

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> Unit
evalU2_0 env = onUnion (eval2_0 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymInteger
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymBool
eval3_1 env (Ceq3_1 p0 p1) =  (evalU3_0 env p0) ==~ (evalU3_0 env p1) 
eval3_1 env (Cless3_1 p0 p1) =  (evalU3_0 env p0) <~ (evalU3_0 env p1) 
eval3_1 env (Cleq3_1 p0 p1) =  (evalU3_0 env p0) <=~ (evalU3_0 env p1) 
eval3_1 env (Cand3_1 p0 p1) =  (evalU3_1 env p0) &&~ (evalU3_1 env p1) 
eval3_1 env (Cor3_1 p0 p1) =  (evalU3_1 env p0) ||~ (evalU3_1 env p1) 
eval3_1 env (Cnot3_1 p0) =  mrgIte ((evalU3_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_1 env (CFalse3_1) = (toSym False)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymBool
evalU3_1 env = onUnion (eval3_1 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
a tmp1 e 

Hole grammar for #1
e s 

Hole grammar for #2
e s 

Hole grammar for #3
e tmp2 inp 
-}

data AExpr
  = Int SymInteger
  | Plus AExpr AExpr
  | Minus AExpr AExpr
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon AExpr, ExtractSymbolics)
    via (Default AExpr)

data NExpr
  = Nint SymInteger
  | Nplus SymInteger NExpr NExpr
  | Nminus SymInteger NExpr NExpr
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon NExpr, ExtractSymbolics)
    via (Default NExpr)
{-
instance SimpleMergeable List where
  mrgIte cond l r = go cond l r
    where
      go cond (Cons l1 r1) (Cons l2 r2) = Cons (mrgIte cond l1 l2) (mrgIte cond r1 r2)
      go cond (Nil l) (Nil r) = Nil Unit
      go cond (Cons l1 r1) (Nil r2) = Cons l1 r1
      go cond (Nil l) (Cons l2 r2) = Cons l2 r2
      go _ _ _ = error "Should not happen"
      -}

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

repr = 
  let
    f e = 
      case e of
        Nint a -> Int a
        Nplus _ a b -> Plus (f a) (f b)
        Nminus _ a b -> Minus (f a) (f b)
      
  in
  f 

memo e = 
  case e of
    Nint a -> a
    Nplus a _ _ -> a
    Nminus a _ _ -> a
  

is_memo = 
  let
    f e = 
      case e of
        Nint a -> (toSym True)
        Nplus n e1 e2 -> (n ==~  ((memo e1) +  (memo e2))) &&~  ((f e1) &&~  (f e2))
        Nminus n e1 e2 -> (n ==~  ((memo e1) -  (memo e2))) &&~  ((f e1) &&~  (f e2))
      
  in
  f 

spec = 
  let
    f e = 
      case e of
        Int a -> a
        Plus e1 e2 -> (f e1) +  (f e2)
        Minus e1 e2 -> (f e1) -  (f e2)
      
  in
  f 

target = 
  let
    f e = 
      case e of
        Nint a -> 
          let
            tmp1 =
              (Nint a)
          in
          evalU0_0 (RefEnv []) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0))
        Nplus s _ _ -> 
          evalU1_0 (RefEnv []) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))
        Nminus s _ _ -> 
          evalU2_0 (RefEnv []) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0))
      
  in
  f 

gen = 
  let
    f e = 
      case e of
        Int a -> Nint a
        Plus a b -> 
          let
            res =
              ((f a), (f b))
          in
          Nplus ((memo (fst res)) +  (memo (snd res))) (fst res) (snd res)
        Minus a b -> 
          let
            res =
              ((f a), (f b))
          in
          Nminus ((memo (fst res)) -  (memo (snd res))) (fst res) (snd res)
      
  in
  f 

main' e = 
  let
    inp =
      (gen e)
  in
  mrgIte (is_memo inp)
    (let
      tmp2 =
        (target inp)
    in
    evalU3_0 (RefEnv []) ((genSym (3::Int) "hole3") :: (UnionM Expr3_0)))
    (0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(AExpr, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(AExpr, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Minus ((Minus ((Int (-4))) ((Int (3))))) ((Minus ((Int (3))) ((Int (1)))))))), (-9))
                , ((((Int (-2)))), (-2))
                , ((((Minus ((Plus ((Minus ((Int (-5))) ((Int (-3))))) ((Plus ((Int (-2))) ((Int (-5))))))) ((Int (3)))))), (-12))
                , ((((Minus ((Int (5))) ((Minus ((Int (0))) ((Minus ((Plus ((Int (-2))) ((Int (5))))) ((Int (3)))))))))), (5))
                , ((((Minus ((Plus ((Int (3))) ((Plus ((Int (-2))) ((Plus ((Int (-1))) ((Int (1))))))))) ((Int (3)))))), (-2))
                , ((((Minus ((Int (3))) ((Minus ((Minus ((Int (-2))) ((Int (-4))))) ((Int (2)))))))), (3))
                , ((((Plus ((Int (-3))) ((Minus ((Int (4))) ((Int (-1)))))))), (2))
                , ((((Minus ((Int (-2))) ((Int (-3)))))), (1))
                , ((((Plus ((Int (-2))) ((Int (5)))))), (3))
                , ((((Plus ((Int (2))) ((Plus ((Int (-1))) ((Int (-1)))))))), (0))
                , ((((Int (2)))), (2))
                , ((((Minus ((Plus ((Plus ((Int (5))) ((Int (3))))) ((Plus ((Int (2))) ((Int (0))))))) ((Int (-2)))))), (12))
                , ((((Plus ((Minus ((Int (5))) ((Plus ((Int (5))) ((Int (-4))))))) ((Int (-2)))))), (2))
                , ((((Plus ((Int (1))) ((Plus ((Int (1))) ((Int (3)))))))), (5))
                , ((((Plus ((Int (0))) ((Plus ((Minus ((Int (-1))) ((Int (0))))) ((Int (2)))))))), (1))
                , ((((Minus ((Minus ((Int (4))) ((Int (3))))) ((Plus ((Int (-3))) ((Minus ((Int (1))) ((Int (-4)))))))))), (-1))
                , ((((Int (-4)))), (-4))
                , ((((Minus ((Int (4))) ((Int (3)))))), (1))
                , ((((Plus ((Int (1))) ((Plus ((Int (1))) ((Int (2)))))))), (4))
                , ((((Int (2)))), (2))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
