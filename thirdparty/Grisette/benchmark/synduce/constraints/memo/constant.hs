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
-- param_list: tmp1 t a
data Expr0_0
  = Unit0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

-- output_type: Unit
-- param_list: r l a tmp2 t n
data Expr1_0
  = Unit1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

-- output_type: Int
-- param_list: mt tmp3
data Expr2_0
  = Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
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

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)

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
    genSingle0 = [mrgCzero2_0]
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

eval0_0 :: RefEnv -> Expr0_0 -> Unit
eval0_0 env (Unit0_0) = Unit

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> Unit
evalU0_0 env = onUnion (eval0_0 env)

eval1_0 :: RefEnv -> Expr1_0 -> Unit
eval1_0 env (Unit1_0) = Unit

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> Unit
evalU1_0 env = onUnion (eval1_0 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
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


------program space end----

------spec begin-------
{-
Hole grammar for #0
tmp1 t a 

Hole grammar for #1
r l a tmp2 t n 

Hole grammar for #2
mt tmp3 
-}

data Tree
  = Leaf SymInteger
  | Node SymInteger Tree Tree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

data TreeMemo
  = Mleaf SymInteger
  | Mnode SymInteger SymInteger TreeMemo TreeMemo
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon TreeMemo, ExtractSymbolics)
    via (Default TreeMemo)
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

memo t = 
  case t of
    Mleaf _ -> 1
    Mnode x _ _ _ -> x
  

is_memo = 
  let
    f t = 
      case t of
        Mleaf x -> (toSym True)
        Mnode n a l r -> (n ==~  (1 +  ((memo l) +  (memo r)))) &&~  ((f l) &&~  (f r))
      
  in
  f 

repr = 
  let
    f t = 
      case t of
        Mleaf a -> Leaf a
        Mnode n a l r -> Node a (f l) (f r)
      
  in
  f 

target = 
  let
    f t = 
      case t of
        Mleaf a -> 
          let
            tmp1 =
              (Mleaf a)
          in
          evalU0_0 (RefEnv []) ((genSym (4::Int) "hole0") :: (UnionM Expr0_0))
        Mnode n a l r -> 
          let
            tmp2 =
              (Mnode n a l r)
          in
          evalU1_0 (RefEnv []) ((genSym (4::Int) "hole1") :: (UnionM Expr1_0))
      
  in
  f 

spec t = 
  case t of
    Leaf a -> 1
    Node a l r -> 1
  

gen = 
  let
    f t = 
      case t of
        Leaf a -> Mleaf a
        Node a l r -> 
          let
            res =
              ((f l), (f r))
          in
          Mnode (1 +  ((memo (fst res)) +  (memo (snd res)))) a (fst res) (snd res)
      
  in
  f 

main' mt = 
  mrgIte (is_memo mt)
    (let
      tmp3 =
        (target mt)
    in
    evalU2_0 (RefEnv []) ((genSym (4::Int) "hole2") :: (UnionM Expr2_0)))
    (0)

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(TreeMemo, Integer)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(TreeMemo, Integer)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Mnode ((-2)) ((3)) ((Mnode ((-5)) ((5)) ((Mnode ((-1)) ((1)) ((Mleaf (5))) ((Mnode ((-3)) ((3)) ((Mleaf (5))) ((Mleaf (-5))))))) ((Mleaf (-3))))) ((Mleaf (-4)))))), (0))
                , ((((Mnode ((4)) ((-2)) ((Mnode ((-2)) ((2)) ((Mleaf (-3))) ((Mleaf (-2))))) ((Mleaf (-5)))))), (0))
                , ((((Mnode ((-5)) ((-5)) ((Mnode ((-4)) ((5)) ((Mnode ((-3)) ((3)) ((Mleaf (4))) ((Mnode ((5)) ((3)) ((Mleaf (5))) ((Mleaf (-5))))))) ((Mleaf (4))))) ((Mleaf (4)))))), (0))
                , ((((Mleaf (-4)))), (1))
                , ((((Mnode ((0)) ((-1)) ((Mleaf (1))) ((Mnode ((3)) ((1)) ((Mleaf (-3))) ((Mnode ((-4)) ((-2)) ((Mleaf (4))) ((Mleaf (-2)))))))))), (0))
                , ((((Mnode ((-3)) ((5)) ((Mleaf (2))) ((Mnode ((4)) ((0)) ((Mleaf (-4))) ((Mleaf (2)))))))), (0))
                , ((((Mnode ((-4)) ((-2)) ((Mleaf (2))) ((Mnode ((4)) ((2)) ((Mleaf (-3))) ((Mleaf (-3)))))))), (0))
                , ((((Mleaf (-3)))), (1))
                , ((((Mnode ((5)) ((5)) ((Mleaf (-3))) ((Mleaf (2)))))), (0))
                , ((((Mnode ((-2)) ((-2)) ((Mnode ((2)) ((-4)) ((Mleaf (1))) ((Mleaf (-1))))) ((Mnode ((-3)) ((3)) ((Mnode ((-5)) ((-1)) ((Mleaf (5))) ((Mleaf (-1))))) ((Mleaf (-2)))))))), (0))
                , ((((Mnode ((-4)) ((3)) ((Mnode ((4)) ((4)) ((Mleaf (3))) ((Mleaf (4))))) ((Mleaf (3)))))), (0))
                , ((((Mleaf (3)))), (1))
                , ((((Mleaf (5)))), (1))
                , ((((Mnode ((-4)) ((4)) ((Mleaf (3))) ((Mleaf (0)))))), (0))
                , ((((Mleaf (3)))), (1))
                , ((((Mnode ((3)) ((-5)) ((Mnode ((-3)) ((-4)) ((Mleaf (-2))) ((Mleaf (-3))))) ((Mnode ((-1)) ((1)) ((Mleaf (1))) ((Mleaf (5)))))))), (0))
                , ((((Mnode ((-4)) ((-4)) ((Mnode ((-4)) ((-1)) ((Mleaf (5))) ((Mleaf (2))))) ((Mleaf (-1)))))), (0))
                , ((((Mleaf (-4)))), (1))
                , ((((Mnode ((-5)) ((-2)) ((Mnode ((5)) ((-1)) ((Mleaf (-3))) ((Mleaf (-2))))) ((Mnode ((3)) ((-2)) ((Mnode ((4)) ((-2)) ((Mleaf (-4))) ((Mleaf (2))))) ((Mleaf (4)))))))), (0))
                , ((((Mleaf (4)))), (1))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
