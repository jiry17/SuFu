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

------pre output end-----

------program space begin----
data EnvValue
  = Env0 (SymBool, SymInteger, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> (SymBool, SymInteger, SymInteger)
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

{- env_type_list: 
(SymBool, SymInteger, SymInteger)
-}

-- output_type: {Bool,Int,Int}
-- param_list: xs
data Expr0_0
  = Prod0_0 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | CFalseZero0_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access10_1 (UnionM Expr0_0)
  | Access20_1 (UnionM Expr0_0)
  | Al_inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Access00_2 (UnionM Expr0_0)
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

-- output_type: {Bool,Int,Int}
-- param_list: xs
data Expr1_0
  = Prod1_0 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | CFalseZero1_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_2) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access11_1 (UnionM Expr1_0)
  | Access21_1 (UnionM Expr1_0)
  | Al_inf1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Access01_2 (UnionM Expr1_0)
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

-- output_type: {Bool,Int,Int}
-- param_list: sp xs tmp2 tmp1
data Expr2_0
  = Param22_0
  | Param32_0
  | Prod2_0 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access12_1 (UnionM Expr2_0)
  | Access22_1 (UnionM Expr2_0)
  | Al_inf2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Access02_2 (UnionM Expr2_0)
  | Ceq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: Bool
-- param_list: xs tmp3
data Expr3_0
  = Access03_0 (UnionM Expr3_1)
  | Ceq3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cless3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cleq3_0 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cand3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cor3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cnot3_0 (UnionM Expr3_0)
  | CFalse3_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Prod3_1 (UnionM Expr3_0) (UnionM Expr3_2) (UnionM Expr3_2)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Cadd3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Csub3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Czero3_2
  | CIte3_2 (UnionM Expr3_0) (UnionM Expr3_2) (UnionM Expr3_2)
  | Access13_2 (UnionM Expr3_1)
  | Access23_2 (UnionM Expr3_1)
  | Al_inf3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr3_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalseZero0_0]
    genSingle1 = [mrgCzero0_1] ++ [mrgAl_inf0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e2_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess10_1 e0_0] ++ [mrgAccess20_1 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess00_2 e0_0] ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalseZero1_0]
    genSingle1 = [mrgCzero1_1] ++ [mrgAl_inf1_1]
    genSingle2 = [mrgCFalse1_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd1_0 e2_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e2_0 e1_4 e1_5] ++ [mrgAccess11_1 e0_0] ++ [mrgAccess21_1 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess01_2 e0_0] ++ [mrgCeq1_2 e1_0 e1_1] ++ [mrgCless1_2 e1_2 e1_3] ++ [mrgCleq1_2 e1_4 e1_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam22_0] ++ [mrgParam32_0]
    genSingle1 = [mrgCzero2_1] ++ [mrgAl_inf2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e2_0 e1_0 e1_1])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess12_1 e0_0] ++ [mrgAccess22_1 e0_1])
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgAccess02_2 e0_0] ++ [mrgCeq2_2 e1_0 e1_1] ++ [mrgCless2_2 e1_2 e1_3] ++ [mrgCleq2_2 e1_4 e1_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCFalse3_0]
    genSingle1 = [mrgParam13_1]
    genSingle2 = [mrgCzero3_2] ++ [mrgAl_inf3_2]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess03_0 e1_0] ++ [mrgCeq3_0 e2_0 e2_1] ++ [mrgCless3_0 e2_2 e2_3] ++ [mrgCleq3_0 e2_4 e2_5] ++ [mrgCand3_0 e0_0 e0_1] ++ [mrgCor3_0 e0_2 e0_3] ++ [mrgCnot3_0 e0_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgProd3_1 e0_0 e2_0 e2_1])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        e2_5 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCadd3_2 e2_0 e2_1] ++ [mrgCsub3_2 e2_2 e2_3] ++ [mrgCIte3_2 e0_0 e2_4 e2_5] ++ [mrgAccess13_2 e1_0] ++ [mrgAccess23_2 e1_1])
        return res

get1th :: (SymBool, SymInteger, SymInteger) -> SymBool
get1th (a, _, _) = a

get2th :: (SymBool, SymInteger, SymInteger) -> SymInteger
get2th (_, b, _) = b

get3th :: (SymBool, SymInteger, SymInteger) -> SymInteger
get3th (_, _, c) = c

eval0_0 :: RefEnv -> Expr0_0 -> (SymBool, SymInteger, SymInteger)
eval0_0 env (Prod0_0 p0 p1 p2) = ((evalU0_2 env p0), (evalU0_1 env p1), (evalU0_1 env p2))
eval0_0 env (CFalseZero0_0) = ((toSym False),0,0)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymBool, SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access10_1 p0) = get2th (evalU0_0 env p0)
eval0_1 env (Access20_1 p0) = get3th (evalU0_0 env p0)
eval0_1 env (Al_inf0_1) = (100 :: SymInteger)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Access00_2 p0) = get1th (evalU0_0 env p0)
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymBool, SymInteger, SymInteger)
eval1_0 env (Prod1_0 p0 p1 p2) = ((evalU1_2 env p0), (evalU1_1 env p1), (evalU1_1 env p2))
eval1_0 env (CFalseZero1_0) = ((toSym False),0,0)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymBool, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access11_1 p0) = get2th (evalU1_0 env p0)
eval1_1 env (Access21_1 p0) = get3th (evalU1_0 env p0)
eval1_1 env (Al_inf1_1) = (100 :: SymInteger)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Access01_2 p0) = get1th (evalU1_0 env p0)
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymBool, SymInteger, SymInteger)
eval2_0 env (Param22_0) = evalVar0 env "tmp2"
eval2_0 env (Param32_0) = evalVar0 env "tmp1"
eval2_0 env (Prod2_0 p0 p1 p2) = ((evalU2_2 env p0), (evalU2_1 env p1), (evalU2_1 env p2))

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymBool, SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access12_1 p0) = get2th (evalU2_0 env p0)
eval2_1 env (Access22_1 p0) = get3th (evalU2_0 env p0)
eval2_1 env (Al_inf2_1) = (100 :: SymInteger)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Access02_2 p0) = get1th (evalU2_0 env p0)
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymBool
eval3_0 env (Access03_0 p0) = get1th (evalU3_1 env p0)
eval3_0 env (Ceq3_0 p0 p1) =  (evalU3_2 env p0) ==~ (evalU3_2 env p1) 
eval3_0 env (Cless3_0 p0 p1) =  (evalU3_2 env p0) <~ (evalU3_2 env p1) 
eval3_0 env (Cleq3_0 p0 p1) =  (evalU3_2 env p0) <=~ (evalU3_2 env p1) 
eval3_0 env (Cand3_0 p0 p1) =  (evalU3_0 env p0) &&~ (evalU3_0 env p1) 
eval3_0 env (Cor3_0 p0 p1) =  (evalU3_0 env p0) ||~ (evalU3_0 env p1) 
eval3_0 env (Cnot3_0 p0) =  mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_0 env (CFalse3_0) = (toSym False)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymBool
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (SymBool, SymInteger, SymInteger)
eval3_1 env (Param13_1) = evalVar0 env "tmp3"
eval3_1 env (Prod3_1 p0 p1 p2) = ((evalU3_0 env p0), (evalU3_2 env p1), (evalU3_2 env p2))

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (SymBool, SymInteger, SymInteger)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymInteger
eval3_2 env (Cadd3_2 p0 p1) =  (evalU3_2 env p0) + (evalU3_2 env p1) 
eval3_2 env (Csub3_2 p0 p1) =  (evalU3_2 env p0) - (evalU3_2 env p1) 
eval3_2 env (Czero3_2) = 0
eval3_2 env (CIte3_2 p0 p1 p2) = mrgIte ((evalU3_0 env p0) ==~ (toSym True)) (evalU3_2 env p1) (evalU3_2 env p2)
eval3_2 env (Access13_2 p0) = get2th (evalU3_1 env p0)
eval3_2 env (Access23_2 p0) = get3th (evalU3_1 env p0)
eval3_2 env (Al_inf3_2) = (100 :: SymInteger)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymInteger
evalU3_2 env = onUnion (eval3_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
xs 

Hole grammar for #1
xs 

Hole grammar for #2
sp xs tmp2 tmp1 

Hole grammar for #3
xs tmp3 
-}

data List
  = Cons SymInteger List
  | Nil Unit
  deriving stock (Generic, Show)
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

length' = 
  let
    f x = 
      case x of
        Cons h t -> (f t) +  1
        Nil _ -> 0
      
  in
  f 

concat = 
  let
    f x y = 
      case x of
        Cons h t -> Cons h (f t y)
        Nil _ -> y
      
  in
  f 

split xs = 
  let
    f x n = 
      mrgIte (n <~  1)
        (((Nil Unit), x))
        (case x of
          Cons h t -> 
            let
              res =
                (f t (n -  1))
            in
            ((Cons h (fst res)), (snd res))
          _ -> ((Nil Unit), (Nil Unit))
        )
  in
  f  xs (div' (length' xs) 2)

dac v xs = 
  let
    run =
      (let
        f xs = 
          case xs of
            Nil _ -> 
              evalU0_0 (RefEnv []) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))
            Cons _ (Nil _) -> 
              evalU1_0 (RefEnv []) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))
            _ -> 
              let
                sp =
                  (split xs)
              in
              let
                tmp1 =
                  (f (fst sp))
              in
              let
                tmp2 =
                  (f (snd sp))
              in
              evalU2_0 (RefEnv [("tmp2", (Env0 tmp2)), ("tmp1", (Env0 tmp1))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0))
          
      in
      f )
  in
  let
    tmp3 =
      (run xs)
  in
  evalU3_0 (RefEnv [("tmp3", (Env0 tmp3))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0))

inf = 
  100

balanced :: List -> SymBool
balanced = 
  let
    f cnt xs = 
      case xs of
        Nil _ -> (toSym True)
        Cons h t -> 
          let
            cnt =
              (cnt +  h)
          in
          mrgIte (cnt <~  0)
            ((toSym False))
            (f cnt t)
      
  in
  f  0

main' = 
  dac balanced

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(List, Bool)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(List, Bool)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = main' x ==~ (toSym y) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((Cons (1) (Cons (1) (Cons (-1) (Cons (0) (Cons (-1) (Cons (1) (Cons (1) (Cons (1) (Nil Unit))))))))), True)
                , ((Cons (-1) (Nil Unit)), False)
                , ((Cons (-1) (Cons (-1) (Cons (0) (Cons (1) (Cons (-1) (Cons (-1) (Cons (0) (Cons (-1) (Cons (1) (Nil Unit)))))))))), False)
                , ((Cons (-1) (Cons (1) (Cons (-1) (Cons (0) (Cons (1) (Cons (0) (Cons (1) (Cons (-1) (Cons (-1) (Cons (1) (Cons (0) (Cons (0) (Cons (0) (Cons (1) (Cons (-1) (Cons (1) (Cons (-1) (Cons (0) (Cons (1) (Nil Unit)))))))))))))))))))), False)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (1) (Cons (-1) (Cons (-1) (Cons (-1) (Cons (-1) (Cons (-1) (Cons (0) (Cons (1) (Nil Unit)))))))))))), False)
                , ((Cons (0) (Cons (0) (Cons (-1) (Cons (0) (Cons (1) (Cons (1) (Cons (0) (Nil Unit)))))))), False)
                , ((Cons (0) (Cons (1) (Cons (1) (Cons (0) (Cons (-1) (Cons (-1) (Nil Unit))))))), True)
                , ((Cons (0) (Cons (0) (Cons (0) (Cons (1) (Cons (0) (Cons (0) (Nil Unit))))))), True)
                , ((Nil Unit), True)
                , ((Cons (-1) (Cons (0) (Cons (-1) (Cons (0) (Cons (-1) (Cons (-1) (Cons (-1) (Nil Unit)))))))), False)
                , ((Cons (-1) (Cons (0) (Cons (0) (Cons (-1) (Cons (0) (Cons (1) (Cons (0) (Cons (0) (Cons (-1) (Cons (0) (Cons (1) (Cons (-1) (Cons (-1) (Cons (0) (Cons (0) (Cons (0) (Cons (1) (Nil Unit)))))))))))))))))), False)
                , ((Cons (0) (Cons (0) (Cons (1) (Cons (-1) (Cons (0) (Cons (-1) (Cons (-1) (Cons (-1) (Cons (1) (Cons (1) (Cons (1) (Cons (-1) (Cons (1) (Nil Unit)))))))))))))), False)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (1) (Cons (1) (Cons (1) (Cons (0) (Cons (0) (Cons (1) (Cons (-1) (Cons (-1) (Cons (1) (Cons (0) (Cons (-1) (Nil Unit))))))))))))))), True)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (1) (Cons (-1) (Cons (0) (Cons (-1) (Cons (0) (Nil Unit))))))))), True)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (-1) (Nil Unit))))), True)
                , ((Cons (1) (Cons (-1) (Cons (1) (Cons (-1) (Cons (1) (Nil Unit)))))), True)
                , ((Cons (-1) (Cons (0) (Cons (-1) (Cons (0) (Cons (1) (Cons (1) (Cons (-1) (Cons (-1) (Cons (0) (Cons (1) (Cons (0) (Cons (0) (Nil Unit))))))))))))), False)
                , ((Cons (1) (Cons (1) (Cons (1) (Cons (0) (Nil Unit))))), True)
                , ((Cons (-1) (Cons (0) (Nil Unit))), False)
                , ((Cons (0) (Cons (1) (Cons (1) (Cons (-1) (Cons (1) (Nil Unit)))))), True)
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
