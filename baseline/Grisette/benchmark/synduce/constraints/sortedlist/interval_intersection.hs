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
  = Env0 (SymBool, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> (SymBool, SymInteger)
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
(SymBool, SymInteger)
SymInteger
-}

-- output_type: {Bool,Int}
-- param_list: xs
data Expr0_0
  = Prod0_0 (UnionM Expr0_2) (UnionM Expr0_1)
  | CFalseZero0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Access0_3_0_1 (UnionM Expr0_3)
  | Access1_3_0_1 (UnionM Expr0_3)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Access0_0_0_2 (UnionM Expr0_0)
  | Ceq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cless0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cleq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cand0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cor0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cnot0_2 (UnionM Expr0_2)
  | CFalse0_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

data Expr0_3
  = Prod0_3 (UnionM Expr0_1) (UnionM Expr0_1)
  | CZero20_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_3)
    via (Default Expr0_3)

-- output_type: {Bool,Int}
-- param_list: b xs t tmp1 a
data Expr1_0
  = Param31_0
  | Prod1_0 (UnionM Expr1_2) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param01_1
  | Param41_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access0_3_1_1 (UnionM Expr1_3)
  | Access1_3_1_1 (UnionM Expr1_3)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Access0_0_1_2 (UnionM Expr1_0)
  | Ceq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_2 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  | CFalse1_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

data Expr1_3
  = Prod1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | CZero21_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_3)
    via (Default Expr1_3)

-- output_type: Bool
-- param_list: xs tmp2
data Expr2_0
  = Access0_1_2_0 (UnionM Expr2_1)
  | Ceq2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cless2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cleq2_0 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cand2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cor2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cnot2_0 (UnionM Expr2_0)
  | CFalse2_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param12_1
  | Prod2_1 (UnionM Expr2_0) (UnionM Expr2_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Cadd2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Csub2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Czero2_2
  | CIte2_2 (UnionM Expr2_0) (UnionM Expr2_2) (UnionM Expr2_2)
  | Access1_1_2_2 (UnionM Expr2_1)
  | Access0_3_2_2 (UnionM Expr2_3)
  | Access1_3_2_2 (UnionM Expr2_3)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

data Expr2_3
  = Prod2_3 (UnionM Expr2_2) (UnionM Expr2_2)
  | CZero22_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_3)
    via (Default Expr2_3)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr0_3)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr1_3)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr2_3)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalseZero0_0]
    genSingle1 = [mrgCzero0_1]
    genSingle2 = [mrgCFalse0_2]
    genSingle3 = [mrgCZero20_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e2_0 e1_0])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_0_1 e0_0] ++ [mrgAccess0_3_0_1 e3_0] ++ [mrgAccess1_3_0_1 e3_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_0_2 e0_0] ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgProd0_3 e1_0 e1_1])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam31_0]
    genSingle1 = [mrgParam01_1] ++ [mrgParam41_1] ++ [mrgCzero1_1]
    genSingle2 = [mrgCFalse1_2]
    genSingle3 = [mrgCZero21_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e2_0 e1_0])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess1_0_1_1 e0_0] ++ [mrgAccess0_3_1_1 e3_0] ++ [mrgAccess1_3_1_1 e3_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess0_0_1_2 e0_0] ++ [mrgCeq1_2 e1_0 e1_1] ++ [mrgCless1_2 e1_2 e1_3] ++ [mrgCleq1_2 e1_4 e1_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgProd1_3 e1_0 e1_1])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse2_0]
    genSingle1 = [mrgParam12_1]
    genSingle2 = [mrgCzero2_2]
    genSingle3 = [mrgCZero22_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess0_1_2_0 e1_0] ++ [mrgCeq2_0 e2_0 e2_1] ++ [mrgCless2_0 e2_2 e2_3] ++ [mrgCleq2_0 e2_4 e2_5] ++ [mrgCand2_0 e0_0 e0_1] ++ [mrgCor2_0 e0_2 e0_3] ++ [mrgCnot2_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd2_1 e0_0 e2_0])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd2_2 e2_0 e2_1] ++ [mrgCsub2_2 e2_2 e2_3] ++ [mrgCIte2_2 e0_0 e2_4 e2_5] ++ [mrgAccess1_1_2_2 e1_0] ++ [mrgAccess0_3_2_2 e3_0] ++ [mrgAccess1_3_2_2 e3_1])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgProd2_3 e2_0 e2_1])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymInteger)
eval0_0 env (Prod0_0 p0 p1) = ((evalU0_2 env p0), (evalU0_1 env p1))
eval0_0 env (CFalseZero0_0) = ((toSym False),0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access1_0_0_1 p0) = snd (evalU0_0 env p0)
eval0_1 env (Access0_3_0_1 p0) = fst (evalU0_3 env p0)
eval0_1 env (Access1_3_0_1 p0) = snd (evalU0_3 env p0)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Access0_0_0_2 p0) = fst (evalU0_0 env p0)
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval0_3 :: RefEnv -> Expr0_3 -> (SymInteger, SymInteger)
eval0_3 env (Prod0_3 p0 p1) = ((evalU0_1 env p0), (evalU0_1 env p1))
eval0_3 env (CZero20_3) = (0,0)

evalU0_3 :: RefEnv -> UnionM Expr0_3 -> (SymInteger, SymInteger)
evalU0_3 env = onUnion (eval0_3 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymInteger)
eval1_0 env (Param31_0) = evalVar0 env "tmp1"
eval1_0 env (Prod1_0 p0 p1) = ((evalU1_2 env p0), (evalU1_1 env p1))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param01_1) = evalVar1 env "b"
eval1_1 env (Param41_1) = evalVar1 env "a"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access1_0_1_1 p0) = snd (evalU1_0 env p0)
eval1_1 env (Access0_3_1_1 p0) = fst (evalU1_3 env p0)
eval1_1 env (Access1_3_1_1 p0) = snd (evalU1_3 env p0)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Access0_0_1_2 p0) = fst (evalU1_0 env p0)
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval1_3 :: RefEnv -> Expr1_3 -> (SymInteger, SymInteger)
eval1_3 env (Prod1_3 p0 p1) = ((evalU1_1 env p0), (evalU1_1 env p1))
eval1_3 env (CZero21_3) = (0,0)

evalU1_3 :: RefEnv -> UnionM Expr1_3 -> (SymInteger, SymInteger)
evalU1_3 env = onUnion (eval1_3 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymBool
eval2_0 env (Access0_1_2_0 p0) = fst (evalU2_1 env p0)
eval2_0 env (Ceq2_0 p0 p1) =  (evalU2_2 env p0) ==~ (evalU2_2 env p1) 
eval2_0 env (Cless2_0 p0 p1) =  (evalU2_2 env p0) <~ (evalU2_2 env p1) 
eval2_0 env (Cleq2_0 p0 p1) =  (evalU2_2 env p0) <=~ (evalU2_2 env p1) 
eval2_0 env (Cand2_0 p0 p1) =  (evalU2_0 env p0) &&~ (evalU2_0 env p1) 
eval2_0 env (Cor2_0 p0 p1) =  (evalU2_0 env p0) ||~ (evalU2_0 env p1) 
eval2_0 env (Cnot2_0 p0) =  mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_0 env (CFalse2_0) = (toSym False)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymBool
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (SymBool, SymInteger)
eval2_1 env (Param12_1) = evalVar0 env "tmp2"
eval2_1 env (Prod2_1 p0 p1) = ((evalU2_0 env p0), (evalU2_2 env p1))

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> (SymBool, SymInteger)
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymInteger
eval2_2 env (Cadd2_2 p0 p1) =  (evalU2_2 env p0) + (evalU2_2 env p1) 
eval2_2 env (Csub2_2 p0 p1) =  (evalU2_2 env p0) - (evalU2_2 env p1) 
eval2_2 env (Czero2_2) = 0
eval2_2 env (CIte2_2 p0 p1 p2) = mrgIte ((evalU2_0 env p0) ==~ (toSym True)) (evalU2_2 env p1) (evalU2_2 env p2)
eval2_2 env (Access1_1_2_2 p0) = snd (evalU2_1 env p0)
eval2_2 env (Access0_3_2_2 p0) = fst (evalU2_3 env p0)
eval2_2 env (Access1_3_2_2 p0) = snd (evalU2_3 env p0)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymInteger
evalU2_2 env = onUnion (eval2_2 env)

eval2_3 :: RefEnv -> Expr2_3 -> (SymInteger, SymInteger)
eval2_3 env (Prod2_3 p0 p1) = ((evalU2_2 env p0), (evalU2_2 env p1))
eval2_3 env (CZero22_3) = (0,0)

evalU2_3 :: RefEnv -> UnionM Expr2_3 -> (SymInteger, SymInteger)
evalU2_3 env = onUnion (eval2_3 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs 

Hole grammar for #1
b xs t tmp1 a 

Hole grammar for #2
xs tmp2 
-}

data List
  = Elt SymInteger SymInteger
  | Cons SymInteger SymInteger List
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon List, ExtractSymbolics)
    via (Default List)
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

head' xs = 
  case xs of
    Elt a b -> (a, b)
    Cons a b t -> (a, b)
  

sorted = 
  let
    f xs = 
      case xs of
        Elt a b -> a <~  b
        Cons a b t -> (a <~  b) &&~  ((a <~  (fst (head' t))) &&~  (f t))
      
  in
  f 

inter a b = 
  let
    f xs = 
      case xs of
        Elt c d -> (mrgIte ((b <~  c) ==~ (toSym True)) (toSym False) (toSym True)) &&~  (mrgIte ((a >~  d) ==~ (toSym True)) (toSym False) (toSym True))
        Cons c d t -> (f t) ||~  ((mrgIte ((b <~  c) ==~ (toSym True)) (toSym False) (toSym True)) &&~  (mrgIte ((a >~  d) ==~ (toSym True)) (toSym False) (toSym True)))
      
  in
  f 

spec xs = 
  (get1from3 (let
    f xs = 
      case xs of
        Elt a b -> ((toSym False), a, b)
        Cons a b t -> 
          let
            res =
              (f t)
          in
          (((get1from3 res) ||~  (inter a b t)), a, b)
      
  in
  f  xs))

target = 
  let
    f xs = 
      case xs of
        Elt _ _ -> 
          evalU0_0 (RefEnv []) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
        Cons a b t -> 
          let
            tmp1 =
              (f t)
          in
          evalU1_0 (RefEnv [("tmp1", (Env0 tmp1)), ("b", (Env1 b)), ("a", (Env1 a))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))
      
  in
  f 

main' xs = 
  mrgIte (sorted xs)
    (let
      tmp2 =
        (target xs)
    in
    evalU2_0 (RefEnv [("tmp2", (Env0 tmp2))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0)))
    ((toSym False))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(List, SymBool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(List, SymBool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Cons ((-5)) ((-2)) ((Cons ((-3)) ((-1)) ((Cons ((-2)) ((0)) ((Cons ((0)) ((3)) ((Elt ((1)) ((4))))))))))))), (toSym True))
                , ((((Cons ((-5)) ((-3)) ((Cons ((-4)) ((-3)) ((Cons ((-3)) ((-1)) ((Elt ((-1)) ((1))))))))))), (toSym True))
                , ((((Cons ((-4)) ((5)) ((Cons ((-3)) ((-1)) ((Cons ((-2)) ((5)) ((Elt ((-1)) ((2))))))))))), (toSym True))
                , ((((Cons ((-5)) ((1)) ((Cons ((-4)) ((-3)) ((Cons ((-2)) ((0)) ((Elt ((-1)) ((3))))))))))), (toSym True))
                , ((((Cons ((-5)) ((3)) ((Cons ((-4)) ((0)) ((Cons ((-2)) ((4)) ((Elt ((-1)) ((4))))))))))), (toSym True))
                , ((((Cons ((-5)) ((-3)) ((Cons ((-3)) ((-2)) ((Cons ((0)) ((3)) ((Elt ((4)) ((5))))))))))), (toSym True))
                , ((((Cons ((-3)) ((-2)) ((Cons ((-2)) ((3)) ((Cons ((-1)) ((2)) ((Elt ((1)) ((3))))))))))), (toSym True))
                , ((((Cons ((-5)) ((0)) ((Cons ((-4)) ((3)) ((Cons ((-3)) ((4)) ((Elt ((-1)) ((0))))))))))), (toSym True))
                , ((((Cons ((-5)) ((-3)) ((Cons ((-4)) ((3)) ((Cons ((-1)) ((3)) ((Elt ((0)) ((2))))))))))), (toSym True))
                , ((((Cons ((-5)) ((1)) ((Cons ((-4)) ((3)) ((Cons ((-3)) ((-2)) ((Elt ((2)) ((4))))))))))), (toSym True))
                , ((((Cons ((-4)) ((-3)) ((Cons ((-3)) ((3)) ((Cons ((-1)) ((0)) ((Elt ((0)) ((5))))))))))), (toSym True))
                , ((((Cons ((-5)) ((-3)) ((Cons ((-4)) ((0)) ((Cons ((-3)) ((3)) ((Elt ((0)) ((1))))))))))), (toSym True))
                , ((((Cons ((-5)) ((1)) ((Cons ((-4)) ((0)) ((Cons ((-3)) ((1)) ((Elt ((0)) ((4))))))))))), (toSym True))
                , ((((Cons ((-5)) ((3)) ((Cons ((-3)) ((3)) ((Cons ((-1)) ((5)) ((Elt ((0)) ((4))))))))))), (toSym True))
                , ((((Cons ((-4)) ((-1)) ((Cons ((-3)) ((3)) ((Cons ((0)) ((2)) ((Elt ((1)) ((2))))))))))), (toSym True))
                , ((((Cons ((-4)) ((5)) ((Cons ((-2)) ((4)) ((Cons ((-1)) ((4)) ((Elt ((0)) ((2))))))))))), (toSym True))
                , ((((Cons ((-5)) ((0)) ((Cons ((-3)) ((4)) ((Cons ((-2)) ((3)) ((Elt ((1)) ((3))))))))))), (toSym True))
                , ((((Cons ((-5)) ((4)) ((Cons ((-3)) ((1)) ((Cons ((-1)) ((3)) ((Elt ((2)) ((3))))))))))), (toSym True))
                , ((((Cons ((-4)) ((5)) ((Cons ((-2)) ((0)) ((Cons ((-1)) ((0)) ((Elt ((0)) ((5))))))))))), (toSym True))
                , ((((Cons ((-4)) ((5)) ((Cons ((-1)) ((5)) ((Cons ((0)) ((1)) ((Elt ((2)) ((4))))))))))), (toSym True))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
