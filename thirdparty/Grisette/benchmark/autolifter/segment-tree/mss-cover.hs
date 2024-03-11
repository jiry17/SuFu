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
type Tag = SymInteger

type Result = (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)

type NodeInfo = (Tag, Result)

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

data SegTree
  = Empty Unit
  | Node NodeInfo SegTree SegTree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon SegTree)
    via (Default SegTree)

instance SimpleMergeable SegTree where
  mrgIte cond l r = go cond l r
    where
      go cond (Empty l) (Empty r) = Empty Unit
      go cond (Node l1 l2 l3) (Node r1 r2 r3) = Node (mrgIte cond l1 r1) (mrgIte cond l2 r2) (mrgIte cond l3 r3)
      go cond (Empty l) (Node r1 r2 r3) = Node r1 r2 r3
      go cond (Node l1 l2 l3) (Empty r) = Node l1 l2 l3
      go _ _ _ = error "Should not happen"

data Operation
  = Update SymInteger SymInteger Tag
  | Query SymInteger SymInteger
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Operation, ExtractSymbolics)
    via (Default Operation)

instance SimpleMergeable Operation where
  mrgIte cond l r = go cond l r
    where
      go cond (Update l1 l2 l3) (Update r1 r2 r3) = Update (mrgIte cond r1 l1) (mrgIte cond l2 r2) (mrgIte cond l3 r3)
      go cond (Query l1 l2) (Query r1 r2) = Query (mrgIte cond r1 l1) (mrgIte cond l2 r2)
      go cond (Update l1 l2 l3) (Query r1 r2) = Update (mrgIte cond r1 l1) (mrgIte cond l2 r2) l3
      go cond (Query l1 l2) (Update r1 r2 r3) = Update (mrgIte cond r1 l1) (mrgIte cond l2 r2) r3
      go _ _ _ = error "Should not happen"

data OpList
  = Onil Unit
  | Ocons Operation OpList
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon OpList, ExtractSymbolics)
    via (Default OpList)

instance SimpleMergeable OpList where
  mrgIte cond l r = go cond l r
    where
      go cond (Onil l) (Onil r) = Onil Unit
      go cond (Ocons l1 l2) (Ocons r1 r2) = Ocons (mrgIte cond r1 l1) (mrgIte cond l2 r2)
      go cond (Onil l) (Ocons r1 r2) = Ocons r1 r2
      go cond (Ocons l1 l2) (Onil r) = Ocons l1 l2
      go _ _ _ = error "Should not happen"

data EnvValue
  = Env0 (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
  | Env1 SymInteger
  | Env2 (SymInteger, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
  | Env3 (SegTree, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
  deriving (Show, Generic)
  deriving (EvaluateSym) via (Default EnvValue)

instance Mergeable EnvValue where
  rootStrategy =
    SortedStrategy
      ( \case
          Env0 _ -> 0 :: Int
          Env1 _ -> 1 :: Int
          Env2 _ -> 2 :: Int
          Env3 _ -> 3 :: Int
      )
      ( htmemo $ \case
          0 -> SimpleStrategy $ \cond (Env0 l) (Env0 r) -> Env0 $ mrgIte cond l r
          1 -> SimpleStrategy $ \cond (Env1 l) (Env1 r) -> Env1 $ mrgIte cond l r
          2 -> SimpleStrategy $ \cond (Env2 l) (Env2 r) -> Env2 $ mrgIte cond l r
          3 -> SimpleStrategy $ \cond (Env3 l) (Env3 r) -> Env3 $ mrgIte cond l r
          _ -> error "Should not happen"
      )

instance SimpleMergeable EnvValue where
  mrgIte cond l r = go cond l r
    where
      go cond (Env0 l) (Env0 r) = Env0 $ mrgIte cond l r
      go cond (Env1 l) (Env1 r) = Env1 $ mrgIte cond l r
      go cond (Env2 l) (Env2 r) = Env2 $ mrgIte cond l r
      go cond (Env3 l) (Env3 r) = Env3 $ mrgIte cond l r
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

evalVar0 :: RefEnv -> Ident -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
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

evalVar2 :: RefEnv -> Ident -> (SymInteger, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
evalVar2 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env2 sym -> sym
      _ -> error "evalVar2: variable type not matched"

evalVar3 :: RefEnv -> Ident -> (SegTree, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
evalVar3 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env3 sym -> sym
      _ -> error "evalVar3: variable type not matched"

{- env_type_list: 
(SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
SymInteger
(SymInteger, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
(SegTree, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
-}

-- output_type: {Int,Int,Int,Int,Int}
-- param_list: y x default_tag
data Expr0_0
  = Param00_0
  | Param10_0
  | Prod0_0 (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1) (UnionM Expr0_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Param20_1
  | Cadd0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Csub0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Czero0_1
  | CIte0_1 (UnionM Expr0_2) (UnionM Expr0_1) (UnionM Expr0_1)
  | Access0_0_0_1 (UnionM Expr0_0)
  | Access1_0_0_1 (UnionM Expr0_0)
  | Access2_0_0_1 (UnionM Expr0_0)
  | Access3_0_0_1 (UnionM Expr0_0)
  | Access4_0_0_1 (UnionM Expr0_0)
  | Cmul0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Al_inf0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

data Expr0_2
  = Ceq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cless0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cleq0_2 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cand0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cor0_2 (UnionM Expr0_2) (UnionM Expr0_2)
  | Cnot0_2 (UnionM Expr0_2)
  | CFalse0_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_2)
    via (Default Expr0_2)

-- output_type: {Int,Int,Int,Int,Int}
-- param_list: l r tmp2 n tag default_tag info
data Expr1_0
  = Param21_0
  | Access1_2_1_0 (UnionM Expr1_2)
  | Prod1_0 (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1) (UnionM Expr1_1)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param41_1
  | Param51_1
  | Cadd1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Csub1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Czero1_1
  | CIte1_1 (UnionM Expr1_3) (UnionM Expr1_1) (UnionM Expr1_1)
  | Access0_0_1_1 (UnionM Expr1_0)
  | Access1_0_1_1 (UnionM Expr1_0)
  | Access2_0_1_1 (UnionM Expr1_0)
  | Access3_0_1_1 (UnionM Expr1_0)
  | Access4_0_1_1 (UnionM Expr1_0)
  | Access0_2_1_1 (UnionM Expr1_2)
  | Cmul1_1 (UnionM Expr1_1) (UnionM Expr1_1)
  | Al_inf1_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Param61_2
  | Prod1_2 (UnionM Expr1_1) (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

data Expr1_3
  = Ceq1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cless1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cleq1_3 (UnionM Expr1_1) (UnionM Expr1_1)
  | Cand1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cor1_3 (UnionM Expr1_3) (UnionM Expr1_3)
  | Cnot1_3 (UnionM Expr1_3)
  | CFalse1_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_3)
    via (Default Expr1_3)

-- output_type: {Int,Int,Int,Int,Int}
-- param_list: l r tmp3 default_tag h xs t
data Expr2_0
  = Prod2_0 (UnionM Expr2_1) (UnionM Expr2_1) (UnionM Expr2_1) (UnionM Expr2_1) (UnionM Expr2_1)
  | CZero52_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param02_1
  | Param12_1
  | Param32_1
  | Cadd2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Csub2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Czero2_1
  | CIte2_1 (UnionM Expr2_2) (UnionM Expr2_1) (UnionM Expr2_1)
  | Access0_0_2_1 (UnionM Expr2_0)
  | Access1_0_2_1 (UnionM Expr2_0)
  | Access2_0_2_1 (UnionM Expr2_0)
  | Access3_0_2_1 (UnionM Expr2_0)
  | Access4_0_2_1 (UnionM Expr2_0)
  | Cmul2_1 (UnionM Expr2_1) (UnionM Expr2_1)
  | Al_inf2_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Ceq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cless2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cleq2_2 (UnionM Expr2_1) (UnionM Expr2_1)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: {Int,Int,Int,Int,Int}
-- param_list: l ql n qr tmp4 r default_tag
data Expr3_0
  = Prod3_0 (UnionM Expr3_1) (UnionM Expr3_1) (UnionM Expr3_1) (UnionM Expr3_1) (UnionM Expr3_1)
  | CZero53_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param03_1
  | Param13_1
  | Param33_1
  | Param53_1
  | Param63_1
  | Cadd3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Csub3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Czero3_1
  | CIte3_1 (UnionM Expr3_2) (UnionM Expr3_1) (UnionM Expr3_1)
  | Access0_0_3_1 (UnionM Expr3_0)
  | Access1_0_3_1 (UnionM Expr3_0)
  | Access2_0_3_1 (UnionM Expr3_0)
  | Access3_0_3_1 (UnionM Expr3_0)
  | Access4_0_3_1 (UnionM Expr3_0)
  | Cmul3_1 (UnionM Expr3_1) (UnionM Expr3_1)
  | Al_inf3_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Ceq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cless3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cleq3_2 (UnionM Expr3_1) (UnionM Expr3_1)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: Int
-- param_list: ops h len r t init l root tmp5 res default_tag
data Expr4_0
  = Param24_0
  | Param34_0
  | Param64_0
  | Param104_0
  | Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_3) (UnionM Expr4_0) (UnionM Expr4_0)
  | Access0_1_4_0 (UnionM Expr4_1)
  | Access1_1_4_0 (UnionM Expr4_1)
  | Access2_1_4_0 (UnionM Expr4_1)
  | Access3_1_4_0 (UnionM Expr4_1)
  | Access4_1_4_0 (UnionM Expr4_1)
  | Cmul4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Al_inf4_0
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param84_1
  | Access1_2_4_1 (UnionM Expr4_2)
  | Prod4_1 (UnionM Expr4_0) (UnionM Expr4_0) (UnionM Expr4_0) (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Param94_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_2)
    via (Default Expr4_2)

data Expr4_3
  = Ceq4_3 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cless4_3 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cleq4_3 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cand4_3 (UnionM Expr4_3) (UnionM Expr4_3)
  | Cor4_3 (UnionM Expr4_3) (UnionM Expr4_3)
  | Cnot4_3 (UnionM Expr4_3)
  | CFalse4_3
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_3)
    via (Default Expr4_3)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr0_2)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr1_3)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr3_2)
$(makeUnionWrapper "mrg" ''Expr4_0)
$(makeUnionWrapper "mrg" ''Expr4_1)
$(makeUnionWrapper "mrg" ''Expr4_2)
$(makeUnionWrapper "mrg" ''Expr4_3)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam00_0] ++ [mrgParam10_0]
    genSingle1 = [mrgParam20_1] ++ [mrgCzero0_1] ++ [mrgAl_inf0_1]
    genSingle2 = [mrgCFalse0_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd0_0 e1_0 e1_1 e1_2 e1_3 e1_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd0_1 e1_0 e1_1] ++ [mrgCsub0_1 e1_2 e1_3] ++ [mrgCIte0_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_0_1 e0_0] ++ [mrgAccess1_0_0_1 e0_1] ++ [mrgAccess2_0_0_1 e0_2] ++ [mrgAccess3_0_0_1 e0_3] ++ [mrgAccess4_0_0_1 e0_4] ++ [mrgCmul0_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq0_2 e1_0 e1_1] ++ [mrgCless0_2 e1_2 e1_3] ++ [mrgCleq0_2 e1_4 e1_5] ++ [mrgCand0_2 e2_0 e2_1] ++ [mrgCor0_2 e2_2 e2_3] ++ [mrgCnot0_2 e2_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam21_0]
    genSingle1 = [mrgParam41_1] ++ [mrgParam51_1] ++ [mrgCzero1_1] ++ [mrgAl_inf1_1]
    genSingle2 = [mrgParam61_2]
    genSingle3 = [mrgCFalse1_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgAccess1_2_1_0 e2_0] ++ [mrgProd1_0 e1_0 e1_1 e1_2 e1_3 e1_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd1_1 e1_0 e1_1] ++ [mrgCsub1_1 e1_2 e1_3] ++ [mrgCIte1_1 e3_0 e1_4 e1_5] ++ [mrgAccess0_0_1_1 e0_0] ++ [mrgAccess1_0_1_1 e0_1] ++ [mrgAccess2_0_1_1 e0_2] ++ [mrgAccess3_0_1_1 e0_3] ++ [mrgAccess4_0_1_1 e0_4] ++ [mrgAccess0_2_1_1 e2_0] ++ [mrgCmul1_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgProd1_2 e1_0 e0_0])
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq1_3 e1_0 e1_1] ++ [mrgCless1_3 e1_2 e1_3] ++ [mrgCleq1_3 e1_4 e1_5] ++ [mrgCand1_3 e3_0 e3_1] ++ [mrgCor1_3 e3_2 e3_3] ++ [mrgCnot1_3 e3_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero52_0]
    genSingle1 = [mrgParam02_1] ++ [mrgParam12_1] ++ [mrgParam32_1] ++ [mrgCzero2_1] ++ [mrgAl_inf2_1]
    genSingle2 = [mrgCFalse2_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd2_0 e1_0 e1_1 e1_2 e1_3 e1_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd2_1 e1_0 e1_1] ++ [mrgCsub2_1 e1_2 e1_3] ++ [mrgCIte2_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_2_1 e0_0] ++ [mrgAccess1_0_2_1 e0_1] ++ [mrgAccess2_0_2_1 e0_2] ++ [mrgAccess3_0_2_1 e0_3] ++ [mrgAccess4_0_2_1 e0_4] ++ [mrgCmul2_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq2_2 e1_0 e1_1] ++ [mrgCless2_2 e1_2 e1_3] ++ [mrgCleq2_2 e1_4 e1_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCZero53_0]
    genSingle1 = [mrgParam03_1] ++ [mrgParam13_1] ++ [mrgParam33_1] ++ [mrgParam53_1] ++ [mrgParam63_1] ++ [mrgCzero3_1] ++ [mrgAl_inf3_1]
    genSingle2 = [mrgCFalse3_2]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgProd3_0 e1_0 e1_1 e1_2 e1_3 e1_4])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e1_5 <- (gen1 (gendepth - 1))
        e1_6 <- (gen1 (gendepth - 1))
        e1_7 <- (gen1 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgCadd3_1 e1_0 e1_1] ++ [mrgCsub3_1 e1_2 e1_3] ++ [mrgCIte3_1 e2_0 e1_4 e1_5] ++ [mrgAccess0_0_3_1 e0_0] ++ [mrgAccess1_0_3_1 e0_1] ++ [mrgAccess2_0_3_1 e0_2] ++ [mrgAccess3_0_3_1 e0_3] ++ [mrgAccess4_0_3_1 e0_4] ++ [mrgCmul3_1 e1_6 e1_7])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
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
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq3_2 e1_0 e1_1] ++ [mrgCless3_2 e1_2 e1_3] ++ [mrgCleq3_2 e1_4 e1_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam24_0] ++ [mrgParam34_0] ++ [mrgParam64_0] ++ [mrgParam104_0] ++ [mrgCzero4_0] ++ [mrgAl_inf4_0]
    genSingle1 = [mrgParam84_1]
    genSingle2 = [mrgParam94_2]
    genSingle3 = [mrgCFalse4_3]
    gen0 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle0
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e0_6 <- (gen0 (gendepth - 1))
        e0_7 <- (gen0 (gendepth - 1))
        e1_0 <- (gen1 (gendepth - 1))
        e1_1 <- (gen1 (gendepth - 1))
        e1_2 <- (gen1 (gendepth - 1))
        e1_3 <- (gen1 (gendepth - 1))
        e1_4 <- (gen1 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd4_0 e0_0 e0_1] ++ [mrgCsub4_0 e0_2 e0_3] ++ [mrgCIte4_0 e3_0 e0_4 e0_5] ++ [mrgAccess0_1_4_0 e1_0] ++ [mrgAccess1_1_4_0 e1_1] ++ [mrgAccess2_1_4_0 e1_2] ++ [mrgAccess3_1_4_0 e1_3] ++ [mrgAccess4_1_4_0 e1_4] ++ [mrgCmul4_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle1 ++ [mrgAccess1_2_4_1 e2_0] ++ [mrgProd4_1 e0_0 e0_1 e0_2 e0_3 e0_4])
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        res <- chooseUnionFresh (genSingle2)
        return res
    gen3 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle3
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e3_0 <- (gen3 (gendepth - 1))
        e3_1 <- (gen3 (gendepth - 1))
        e3_2 <- (gen3 (gendepth - 1))
        e3_3 <- (gen3 (gendepth - 1))
        e3_4 <- (gen3 (gendepth - 1))
        res <- chooseUnionFresh (genSingle3 ++ [mrgCeq4_3 e0_0 e0_1] ++ [mrgCless4_3 e0_2 e0_3] ++ [mrgCleq4_3 e0_4 e0_5] ++ [mrgCand4_3 e3_0 e3_1] ++ [mrgCor4_3 e3_2 e3_3] ++ [mrgCnot4_3 e3_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
eval0_0 env (Param00_0) = evalVar0 env "y"
eval0_0 env (Param10_0) = evalVar0 env "x"
eval0_0 env (Prod0_0 p0 p1 p2 p3 p4) = ((evalU0_1 env p0), (evalU0_1 env p1), (evalU0_1 env p2), (evalU0_1 env p3), (evalU0_1 env p4))

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymInteger
eval0_1 env (Param20_1) = evalVar1 env "default_tag"
eval0_1 env (Cadd0_1 p0 p1) =  (evalU0_1 env p0) + (evalU0_1 env p1) 
eval0_1 env (Csub0_1 p0 p1) =  (evalU0_1 env p0) - (evalU0_1 env p1) 
eval0_1 env (Czero0_1) = 0
eval0_1 env (CIte0_1 p0 p1 p2) = mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (evalU0_1 env p1) (evalU0_1 env p2)
eval0_1 env (Access0_0_0_1 p0) = get1from5 (evalU0_0 env p0)
eval0_1 env (Access1_0_0_1 p0) = get2from5 (evalU0_0 env p0)
eval0_1 env (Access2_0_0_1 p0) = get3from5 (evalU0_0 env p0)
eval0_1 env (Access3_0_0_1 p0) = get4from5 (evalU0_0 env p0)
eval0_1 env (Access4_0_0_1 p0) = get5from5 (evalU0_0 env p0)
eval0_1 env (Cmul0_1 p0 p1) =  (evalU0_1 env p0) * (evalU0_1 env p1) 
eval0_1 env (Al_inf0_1) = (100 :: SymInteger)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymInteger
evalU0_1 env = onUnion (eval0_1 env)

eval0_2 :: RefEnv -> Expr0_2 -> SymBool
eval0_2 env (Ceq0_2 p0 p1) =  (evalU0_1 env p0) ==~ (evalU0_1 env p1) 
eval0_2 env (Cless0_2 p0 p1) =  (evalU0_1 env p0) <~ (evalU0_1 env p1) 
eval0_2 env (Cleq0_2 p0 p1) =  (evalU0_1 env p0) <=~ (evalU0_1 env p1) 
eval0_2 env (Cand0_2 p0 p1) =  (evalU0_2 env p0) &&~ (evalU0_2 env p1) 
eval0_2 env (Cor0_2 p0 p1) =  (evalU0_2 env p0) ||~ (evalU0_2 env p1) 
eval0_2 env (Cnot0_2 p0) =  mrgIte ((evalU0_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_2 env (CFalse0_2) = (toSym False)

evalU0_2 :: RefEnv -> UnionM Expr0_2 -> SymBool
evalU0_2 env = onUnion (eval0_2 env)

eval1_0 :: RefEnv -> Expr1_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
eval1_0 env (Param21_0) = evalVar0 env "tmp2"
eval1_0 env (Access1_2_1_0 p0) = snd (evalU1_2 env p0)
eval1_0 env (Prod1_0 p0 p1 p2 p3 p4) = ((evalU1_1 env p0), (evalU1_1 env p1), (evalU1_1 env p2), (evalU1_1 env p3), (evalU1_1 env p4))

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> SymInteger
eval1_1 env (Param41_1) = evalVar1 env "tag"
eval1_1 env (Param51_1) = evalVar1 env "default_tag"
eval1_1 env (Cadd1_1 p0 p1) =  (evalU1_1 env p0) + (evalU1_1 env p1) 
eval1_1 env (Csub1_1 p0 p1) =  (evalU1_1 env p0) - (evalU1_1 env p1) 
eval1_1 env (Czero1_1) = 0
eval1_1 env (CIte1_1 p0 p1 p2) = mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (evalU1_1 env p1) (evalU1_1 env p2)
eval1_1 env (Access0_0_1_1 p0) = get1from5 (evalU1_0 env p0)
eval1_1 env (Access1_0_1_1 p0) = get2from5 (evalU1_0 env p0)
eval1_1 env (Access2_0_1_1 p0) = get3from5 (evalU1_0 env p0)
eval1_1 env (Access3_0_1_1 p0) = get4from5 (evalU1_0 env p0)
eval1_1 env (Access4_0_1_1 p0) = get5from5 (evalU1_0 env p0)
eval1_1 env (Access0_2_1_1 p0) = fst (evalU1_2 env p0)
eval1_1 env (Cmul1_1 p0 p1) =  (evalU1_1 env p0) * (evalU1_1 env p1) 
eval1_1 env (Al_inf1_1) = (100 :: SymInteger)

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> SymInteger
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> (SymInteger, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
eval1_2 env (Param61_2) = evalVar2 env "info"
eval1_2 env (Prod1_2 p0 p1) = ((evalU1_1 env p0), (evalU1_0 env p1))

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> (SymInteger, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
evalU1_2 env = onUnion (eval1_2 env)

eval1_3 :: RefEnv -> Expr1_3 -> SymBool
eval1_3 env (Ceq1_3 p0 p1) =  (evalU1_1 env p0) ==~ (evalU1_1 env p1) 
eval1_3 env (Cless1_3 p0 p1) =  (evalU1_1 env p0) <~ (evalU1_1 env p1) 
eval1_3 env (Cleq1_3 p0 p1) =  (evalU1_1 env p0) <=~ (evalU1_1 env p1) 
eval1_3 env (Cand1_3 p0 p1) =  (evalU1_3 env p0) &&~ (evalU1_3 env p1) 
eval1_3 env (Cor1_3 p0 p1) =  (evalU1_3 env p0) ||~ (evalU1_3 env p1) 
eval1_3 env (Cnot1_3 p0) =  mrgIte ((evalU1_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_3 env (CFalse1_3) = (toSym False)

evalU1_3 :: RefEnv -> UnionM Expr1_3 -> SymBool
evalU1_3 env = onUnion (eval1_3 env)

eval2_0 :: RefEnv -> Expr2_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
eval2_0 env (Prod2_0 p0 p1 p2 p3 p4) = ((evalU2_1 env p0), (evalU2_1 env p1), (evalU2_1 env p2), (evalU2_1 env p3), (evalU2_1 env p4))
eval2_0 env (CZero52_0) = (0,0,0,0,0)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> SymInteger
eval2_1 env (Param02_1) = evalVar1 env "l"
eval2_1 env (Param12_1) = evalVar1 env "r"
eval2_1 env (Param32_1) = evalVar1 env "default_tag"
eval2_1 env (Cadd2_1 p0 p1) =  (evalU2_1 env p0) + (evalU2_1 env p1) 
eval2_1 env (Csub2_1 p0 p1) =  (evalU2_1 env p0) - (evalU2_1 env p1) 
eval2_1 env (Czero2_1) = 0
eval2_1 env (CIte2_1 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_1 env p1) (evalU2_1 env p2)
eval2_1 env (Access0_0_2_1 p0) = get1from5 (evalU2_0 env p0)
eval2_1 env (Access1_0_2_1 p0) = get2from5 (evalU2_0 env p0)
eval2_1 env (Access2_0_2_1 p0) = get3from5 (evalU2_0 env p0)
eval2_1 env (Access3_0_2_1 p0) = get4from5 (evalU2_0 env p0)
eval2_1 env (Access4_0_2_1 p0) = get5from5 (evalU2_0 env p0)
eval2_1 env (Cmul2_1 p0 p1) =  (evalU2_1 env p0) * (evalU2_1 env p1) 
eval2_1 env (Al_inf2_1) = (100 :: SymInteger)

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> SymInteger
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_1 env p0) ==~ (evalU2_1 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_1 env p0) <~ (evalU2_1 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_1 env p0) <=~ (evalU2_1 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
eval3_0 env (Prod3_0 p0 p1 p2 p3 p4) = ((evalU3_1 env p0), (evalU3_1 env p1), (evalU3_1 env p2), (evalU3_1 env p3), (evalU3_1 env p4))
eval3_0 env (CZero53_0) = (0,0,0,0,0)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> SymInteger
eval3_1 env (Param03_1) = evalVar1 env "l"
eval3_1 env (Param13_1) = evalVar1 env "ql"
eval3_1 env (Param33_1) = evalVar1 env "qr"
eval3_1 env (Param53_1) = evalVar1 env "r"
eval3_1 env (Param63_1) = evalVar1 env "default_tag"
eval3_1 env (Cadd3_1 p0 p1) =  (evalU3_1 env p0) + (evalU3_1 env p1) 
eval3_1 env (Csub3_1 p0 p1) =  (evalU3_1 env p0) - (evalU3_1 env p1) 
eval3_1 env (Czero3_1) = 0
eval3_1 env (CIte3_1 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_1 env p1) (evalU3_1 env p2)
eval3_1 env (Access0_0_3_1 p0) = get1from5 (evalU3_0 env p0)
eval3_1 env (Access1_0_3_1 p0) = get2from5 (evalU3_0 env p0)
eval3_1 env (Access2_0_3_1 p0) = get3from5 (evalU3_0 env p0)
eval3_1 env (Access3_0_3_1 p0) = get4from5 (evalU3_0 env p0)
eval3_1 env (Access4_0_3_1 p0) = get5from5 (evalU3_0 env p0)
eval3_1 env (Cmul3_1 p0 p1) =  (evalU3_1 env p0) * (evalU3_1 env p1) 
eval3_1 env (Al_inf3_1) = (100 :: SymInteger)

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> SymInteger
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_1 env p0) ==~ (evalU3_1 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_1 env p0) <~ (evalU3_1 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_1 env p0) <=~ (evalU3_1 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> SymInteger
eval4_0 env (Param24_0) = evalVar1 env "len"
eval4_0 env (Param34_0) = evalVar1 env "r"
eval4_0 env (Param64_0) = evalVar1 env "l"
eval4_0 env (Param104_0) = evalVar1 env "h"
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_3 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Access0_1_4_0 p0) = get1from5 (evalU4_1 env p0)
eval4_0 env (Access1_1_4_0 p0) = get2from5 (evalU4_1 env p0)
eval4_0 env (Access2_1_4_0 p0) = get3from5 (evalU4_1 env p0)
eval4_0 env (Access3_1_4_0 p0) = get4from5 (evalU4_1 env p0)
eval4_0 env (Access4_1_4_0 p0) = get5from5 (evalU4_1 env p0)
eval4_0 env (Cmul4_0 p0 p1) =  (evalU4_0 env p0) * (evalU4_0 env p1) 
eval4_0 env (Al_inf4_0) = (100 :: SymInteger)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
eval4_1 env (Param84_1) = evalVar0 env "tmp5"
eval4_1 env (Access1_2_4_1 p0) = snd (evalU4_2 env p0)
eval4_1 env (Prod4_1 p0 p1 p2 p3 p4) = ((evalU4_0 env p0), (evalU4_0 env p1), (evalU4_0 env p2), (evalU4_0 env p3), (evalU4_0 env p4))

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger)
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> (SegTree, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
eval4_2 env (Param94_2) = evalVar3 env "res"

evalU4_2 :: RefEnv -> UnionM Expr4_2 -> (SegTree, (SymInteger, SymInteger, SymInteger, SymInteger, SymInteger))
evalU4_2 env = onUnion (eval4_2 env)

eval4_3 :: RefEnv -> Expr4_3 -> SymBool
eval4_3 env (Ceq4_3 p0 p1) =  (evalU4_0 env p0) ==~ (evalU4_0 env p1) 
eval4_3 env (Cless4_3 p0 p1) =  (evalU4_0 env p0) <~ (evalU4_0 env p1) 
eval4_3 env (Cleq4_3 p0 p1) =  (evalU4_0 env p0) <=~ (evalU4_0 env p1) 
eval4_3 env (Cand4_3 p0 p1) =  (evalU4_3 env p0) &&~ (evalU4_3 env p1) 
eval4_3 env (Cor4_3 p0 p1) =  (evalU4_3 env p0) ||~ (evalU4_3 env p1) 
eval4_3 env (Cnot4_3 p0) =  mrgIte ((evalU4_3 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval4_3 env (CFalse4_3) = (toSym False)

evalU4_3 :: RefEnv -> UnionM Expr4_3 -> SymBool
evalU4_3 env = onUnion (eval4_3 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
y x default_tag 

Hole grammar for #1
l r tmp2 n tag default_tag info 

Hole grammar for #2
l r tmp3 default_tag h xs t 

Hole grammar for #3
l ql n qr tmp4 r default_tag 

Hole grammar for #4
ops h len r t init l root tmp5 res default_tag 
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

concat' = 
  let
    f x y = 
      case x of
        Cons h t -> Cons h (f t y)
        Nil _ -> y
      
  in
  f 

length' = 
  let
    f x = 
      case x of
        Cons h t -> (f t) +  1
        Nil _ -> 0
      
  in
  f 

map' g = 
  let
    f xs = 
      case xs of
        Nil _ -> xs
        Cons h t -> Cons (g h) (f t)
      
  in
  f 

solve' merge_tag default_tag apply target init ops = 
  let
    get_info n =
      (case n of
        Node info _ _ -> info
      )
  in
  let
    merge_res x y =
      (evalU0_0 (RefEnv [("y", (Env0 y)), ("x", (Env0 x)), ("default_tag", (Env1 default_tag))]) ((genSym (5::Int) "hole0") :: (UnionM Expr0_0)))
  in
  let
    merge l r =
      (let
        linfo =
          (get_info l)
      in
      let
        rinfo =
          (get_info r)
      in
      Node (default_tag, (merge_res (snd linfo) (snd rinfo))) l r)
  in
  let
    apply2 n tag =
      (case n of
        Node info l r -> Node ((merge_tag (fst info) tag), (let
            tmp1 =
              (map' (apply tag))
          in
          let
            tmp2 =
              (snd info)
          in
          evalU1_0 (RefEnv [("tmp2", (Env0 tmp2)), ("tag", (Env1 tag)), ("default_tag", (Env1 default_tag)), ("info", (Env2 info))]) ((genSym (5::Int) "hole1") :: (UnionM Expr1_0)))) l r
      )
  in
  let
    pushdown n =
      (case n of
        Node info l r -> Node (default_tag, (snd info)) (apply2 l (fst info)) (apply2 r (fst info))
      )
  in
  let
    buildtree =
      (let
        f l r xs = 
          mrgIte (l ==~  r)
            (case xs of
              Cons h t -> (t, (Node (default_tag, (let
                  tmp3 =
                    (Cons h (Nil Unit))
                in
                evalU2_0 (RefEnv [("l", (Env1 l)), ("r", (Env1 r)), ("default_tag", (Env1 default_tag)), ("h", (Env1 h))]) ((genSym (5::Int) "hole2") :: (UnionM Expr2_0)))) (Empty Unit) (Empty Unit)))
            )
            (let
              mid =
                (div' (l +  r) 2)
            in
            let
              lres =
                (f l mid xs)
            in
            let
              rres =
                (f (mid +  1) r (fst lres))
            in
            ((fst rres), (merge (snd lres) (snd rres))))
      in
      f )
  in
  let
    update_tree ql qr tag =
      (let
        f l r n = 
          mrgIte ((l >~  ql) ||~  (r <~  qr))
            (n)
            (mrgIte ((ql <=~  l) &&~  (r <=~  qr))
              (apply2 n tag)
              (let
                mid =
                  (div' (l +  r) 2)
              in
              case pushdown n of
                Node info lnode rnode -> 
                  let
                    lres =
                      (f l mid lnode)
                  in
                  let
                    rres =
                      (f (mid +  1) r rnode)
                  in
                  merge lres rres
              ))
      in
      f )
  in
  let
    query_tree ql qr =
      (let
        f l r n = 
          mrgIte ((l >~  ql) ||~  (r <~  qr))
            ((n, (let
              tmp4 =
                (Nil Unit)
            in
            evalU3_0 (RefEnv [("l", (Env1 l)), ("ql", (Env1 ql)), ("qr", (Env1 qr)), ("r", (Env1 r)), ("default_tag", (Env1 default_tag))]) ((genSym (5::Int) "hole3") :: (UnionM Expr3_0)))))
            (mrgIte ((ql <=~  l) &&~  (r <=~  qr))
              ((n, (snd (get_info n))))
              (let
                mid =
                  (div' (l +  r) 2)
              in
              let
                new_node =
                  (pushdown n)
              in
              case new_node of
                Node info lnode rnode -> 
                  let
                    lres =
                      (f l mid lnode)
                  in
                  let
                    rres =
                      (f (mid +  1) r rnode)
                  in
                  ((Node info (fst lres) (fst rres)), (merge_res (snd lres) (snd rres)))
              ))
      in
      f )
  in
  let
    len =
      (length' init)
  in
  mrgIte (len ==~  0)
    (Nil Unit)
    (let
      is_range_valid l r =
        ((1 <=~  l) &&~  ((l <=~  r) &&~  (r <=~  len)))
    in
    let
      root =
        (snd (buildtree 1 len init))
    in
    let
      f root ops = 
        case ops of
          Onil _ -> Nil Unit
          Ocons h t -> 
            case h of
              Query l r -> mrgIte (is_range_valid l r)
                  (let
                    res =
                      (query_tree l r 1 len root)
                  in
                  Cons (let
                    tmp5 =
                      (snd res)
                  in
                  evalU4_0 (RefEnv [("len", (Env1 len)), ("r", (Env1 r)), ("l", (Env1 l)), ("tmp5", (Env0 tmp5)), ("res", (Env3 res))]) ((genSym (5::Int) "hole4") :: (UnionM Expr4_0))) (f (fst res) t))
                  (f root t)
              Update l r tag -> mrgIte (is_range_valid l r)
                  (let
                    res =
                      (update_tree l r tag 1 len root)
                  in
                  f res t)
                  (f root t)
            
        
    in
    f  root ops)

default_tag = 
  100

apply_tag t w = 
  mrgIte (default_tag ==~  t)
    (w)
    (t)

merge_tag x y = 
  mrgIte (default_tag ==~  y)
    (x)
    (y)

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

mss xs = 
  (fst (let
    f xs = 
      case xs of
        Nil _ -> (0, 0)
        Cons h t -> 
          let
            res =
              (f t)
          in
          let
            new_mps =
              (max' 0 (h +  (snd res)))
          in
          ((max' new_mps (fst res)), new_mps)
      
  in
  f  xs))

main' = 
  solve' merge_tag default_tag apply_tag mss

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [((List, OpList), List)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [((List, OpList), List)] -> SymBool
        constraint [] = con True
        constraint (((x1,x2), y) : xs) = (if ((main' x1 x2) == y) then (toSym True) else (toSym False)) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Cons ((-4)) ((Cons ((1)) ((Cons ((-1)) ((Nil Unit)))))))), ((Ocons ((Query ((4)) ((5)))) ((Ocons ((Update ((-2)) ((-1)) ((5)))) ((Ocons ((Update ((-3)) ((-5)) ((-2)))) ((Ocons ((Query ((-2)) ((1)))) ((Ocons ((Update ((0)) ((-4)) ((-4)))) ((Ocons ((Update ((4)) ((-3)) ((-4)))) ((Ocons ((Update ((4)) ((1)) ((0)))) ((Onil Unit))))))))))))))))), (Nil Unit))
                , ((((Cons ((-3)) ((Cons ((2)) ((Cons ((-1)) ((Nil Unit)))))))), ((Ocons ((Query ((-3)) ((-2)))) ((Ocons ((Query ((-3)) ((-5)))) ((Ocons ((Update ((-3)) ((-4)) ((-4)))) ((Ocons ((Query ((5)) ((-4)))) ((Ocons ((Update ((-4)) ((3)) ((3)))) ((Ocons ((Query ((3)) ((0)))) ((Onil Unit))))))))))))))), (Nil Unit))
                , ((((Cons ((4)) ((Nil Unit)))), ((Ocons ((Query ((4)) ((0)))) ((Ocons ((Query ((0)) ((5)))) ((Ocons ((Query ((-3)) ((4)))) ((Ocons ((Update ((4)) ((3)) ((1)))) ((Ocons ((Update ((2)) ((-4)) ((-1)))) ((Ocons ((Update ((5)) ((2)) ((5)))) ((Ocons ((Query ((1)) ((4)))) ((Ocons ((Update ((2)) ((2)) ((-2)))) ((Onil Unit))))))))))))))))))), (Nil Unit))
                , ((((Cons ((-3)) ((Cons ((-2)) ((Cons ((-2)) ((Nil Unit)))))))), ((Ocons ((Update ((1)) ((-5)) ((-2)))) ((Ocons ((Update ((5)) ((-3)) ((-2)))) ((Ocons ((Query ((3)) ((-4)))) ((Onil Unit))))))))), (Nil Unit))
                , ((((Cons ((-4)) ((Cons ((-3)) ((Cons ((-1)) ((Nil Unit)))))))), ((Onil Unit))), (Nil Unit))
                , ((((Cons ((4)) ((Cons ((-5)) ((Cons ((-2)) ((Cons ((1)) ((Cons ((-2)) ((Cons ((-4)) ((Cons ((-1)) ((Cons ((-3)) ((Cons ((5)) ((Nil Unit)))))))))))))))))))), ((Ocons ((Query ((-5)) ((0)))) ((Ocons ((Query ((-3)) ((-3)))) ((Ocons ((Update ((-2)) ((0)) ((3)))) ((Onil Unit))))))))), (Nil Unit))
                , ((((Cons ((2)) ((Nil Unit)))), ((Ocons ((Update ((4)) ((5)) ((-2)))) ((Ocons ((Update ((2)) ((-2)) ((3)))) ((Ocons ((Update ((-4)) ((-4)) ((-1)))) ((Onil Unit))))))))), (Nil Unit))
                , ((((Cons ((-1)) ((Cons ((3)) ((Cons ((-2)) ((Cons ((1)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((-2)) ((Cons ((-2)) ((Nil Unit)))))))))))))))))), ((Ocons ((Update ((4)) ((4)) ((2)))) ((Ocons ((Query ((0)) ((-2)))) ((Ocons ((Query ((-1)) ((4)))) ((Ocons ((Update ((1)) ((-5)) ((-2)))) ((Ocons ((Update ((-5)) ((2)) ((2)))) ((Ocons ((Query ((0)) ((-5)))) ((Ocons ((Query ((-4)) ((4)))) ((Ocons ((Query ((-2)) ((4)))) ((Onil Unit))))))))))))))))))), (Nil Unit))
                , ((((Cons ((-4)) ((Cons ((0)) ((Cons ((-4)) ((Nil Unit)))))))), ((Ocons ((Update ((1)) ((-1)) ((-1)))) ((Onil Unit))))), (Nil Unit))
                , ((((Cons ((5)) ((Cons ((-5)) ((Cons ((-5)) ((Cons ((-4)) ((Nil Unit)))))))))), ((Ocons ((Update ((1)) ((-4)) ((1)))) ((Ocons ((Update ((0)) ((-5)) ((-3)))) ((Ocons ((Update ((-3)) ((-2)) ((2)))) ((Ocons ((Query ((1)) ((-5)))) ((Ocons ((Update ((2)) ((4)) ((0)))) ((Onil Unit))))))))))))), (Nil Unit))
                , ((((Nil Unit)), ((Ocons ((Query ((-4)) ((3)))) ((Ocons ((Query ((2)) ((1)))) ((Ocons ((Update ((3)) ((1)) ((5)))) ((Ocons ((Query ((-4)) ((-1)))) ((Ocons ((Query ((2)) ((-5)))) ((Ocons ((Update ((0)) ((1)) ((4)))) ((Ocons ((Query ((1)) ((-2)))) ((Ocons ((Query ((-3)) ((3)))) ((Ocons ((Query ((0)) ((-5)))) ((Onil Unit))))))))))))))))))))), (Nil Unit))
                , ((((Cons ((3)) ((Cons ((-4)) ((Cons ((1)) ((Nil Unit)))))))), ((Ocons ((Query ((4)) ((5)))) ((Onil Unit))))), (Nil Unit))
                , ((((Cons ((3)) ((Nil Unit)))), ((Ocons ((Update ((-3)) ((-2)) ((-3)))) ((Ocons ((Query ((-2)) ((5)))) ((Ocons ((Query ((-2)) ((2)))) ((Ocons ((Update ((2)) ((-3)) ((-2)))) ((Ocons ((Query ((5)) ((-5)))) ((Ocons ((Query ((-2)) ((2)))) ((Onil Unit))))))))))))))), (Nil Unit))
                , ((((Cons ((0)) ((Cons ((1)) ((Cons ((-1)) ((Cons ((-1)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit)))))))))))))), ((Ocons ((Update ((3)) ((1)) ((-5)))) ((Ocons ((Query ((-5)) ((4)))) ((Ocons ((Update ((3)) ((1)) ((0)))) ((Ocons ((Update ((1)) ((-1)) ((2)))) ((Ocons ((Update ((5)) ((-3)) ((3)))) ((Ocons ((Update ((2)) ((0)) ((-5)))) ((Ocons ((Query ((-1)) ((3)))) ((Onil Unit))))))))))))))))), (Nil Unit))
                , ((((Cons ((3)) ((Nil Unit)))), ((Ocons ((Query ((0)) ((4)))) ((Onil Unit))))), (Nil Unit))
                , ((((Cons ((-5)) ((Cons ((5)) ((Cons ((2)) ((Cons ((1)) ((Cons ((-2)) ((Cons ((-1)) ((Nil Unit)))))))))))))), ((Ocons ((Query ((-3)) ((4)))) ((Ocons ((Update ((-3)) ((-5)) ((2)))) ((Ocons ((Update ((-3)) ((-1)) ((1)))) ((Ocons ((Update ((4)) ((-2)) ((-3)))) ((Ocons ((Update ((2)) ((-4)) ((3)))) ((Ocons ((Query ((3)) ((0)))) ((Ocons ((Update ((-4)) ((1)) ((1)))) ((Ocons ((Query ((0)) ((2)))) ((Onil Unit))))))))))))))))))), (Nil Unit))
                , ((((Cons ((1)) ((Cons ((3)) ((Cons ((-2)) ((Cons ((4)) ((Cons ((3)) ((Cons ((0)) ((Cons ((5)) ((Nil Unit)))))))))))))))), ((Ocons ((Query ((4)) ((2)))) ((Ocons ((Update ((-5)) ((2)) ((-3)))) ((Ocons ((Update ((-4)) ((5)) ((2)))) ((Ocons ((Update ((3)) ((1)) ((-2)))) ((Onil Unit))))))))))), (Nil Unit))
                , ((((Cons ((-5)) ((Cons ((0)) ((Cons ((-2)) ((Cons ((1)) ((Cons ((-5)) ((Cons ((1)) ((Nil Unit)))))))))))))), ((Ocons ((Update ((2)) ((2)) ((-5)))) ((Ocons ((Update ((-5)) ((0)) ((4)))) ((Ocons ((Update ((2)) ((-4)) ((3)))) ((Ocons ((Query ((4)) ((-1)))) ((Ocons ((Update ((1)) ((-3)) ((4)))) ((Ocons ((Update ((-4)) ((-4)) ((-3)))) ((Ocons ((Query ((-4)) ((-4)))) ((Onil Unit))))))))))))))))), (Nil Unit))
                , ((((Cons ((-3)) ((Cons ((2)) ((Cons ((-1)) ((Cons ((-5)) ((Cons ((5)) ((Cons ((1)) ((Cons ((3)) ((Cons ((5)) ((Nil Unit)))))))))))))))))), ((Ocons ((Update ((-2)) ((2)) ((-2)))) ((Ocons ((Query ((-2)) ((5)))) ((Ocons ((Query ((-5)) ((3)))) ((Onil Unit))))))))), (Nil Unit))
                , ((((Cons ((3)) ((Cons ((1)) ((Cons ((4)) ((Cons ((5)) ((Cons ((-1)) ((Cons ((3)) ((Cons ((0)) ((Cons ((-5)) ((Nil Unit)))))))))))))))))), ((Ocons ((Update ((-4)) ((-2)) ((-4)))) ((Ocons ((Query ((-2)) ((4)))) ((Ocons ((Update ((1)) ((-3)) ((-5)))) ((Ocons ((Update ((-4)) ((-3)) ((2)))) ((Ocons ((Update ((-1)) ((-5)) ((4)))) ((Ocons ((Query ((-3)) ((-5)))) ((Ocons ((Update ((3)) ((-1)) ((-2)))) ((Ocons ((Update ((-4)) ((2)) ((-3)))) ((Ocons ((Update ((-1)) ((3)) ((2)))) ((Onil Unit))))))))))))))))))))), (Nil Unit))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
