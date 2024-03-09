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
data Tree
  = Leaf SymInteger
  | Node Tree Tree
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Tree, ExtractSymbolics)
    via (Default Tree)

{-instance SimpleMergeable Tree where
  mrgIte cond l r = go cond l r
    where
      go cond (Leaf l) (Leaf r) = Leaf (mrgIte cond l r)
      go cond (Node l1 l2) (Node r1 r2) = Node (mrgIte cond l1 r1) (mrgIte cond l2 r2)
      go cond (Leaf l) (Node r1 r2) = Node r1 r2
      go cond (Node l1 l2) (Leaf r) = Node l1 l2
      go _ _ _ = error "Should not happen"-}

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
      

data EnvValue
  = Env0 SymInteger
  | Env1 (List, SymInteger)
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

evalVar0 :: RefEnv -> Ident -> SymInteger
evalVar0 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env0 sym -> sym
      _ -> error "evalVar0: variable type not matched"

evalVar1 :: RefEnv -> Ident -> (List, SymInteger)
evalVar1 (RefEnv env) x =
    let v = evalFunc (RefEnv env) x in
    case v of
      Env1 sym -> sym
      _ -> error "evalVar1: variable type not matched"

{- env_type_list: 
SymInteger
(List, SymInteger)
-}

-- output_type: Int
-- param_list: w t
data Expr0_0
  = Param00_0
  | Cadd0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Csub0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  | Czero0_0
  | CIte0_0 (UnionM Expr0_1) (UnionM Expr0_0) (UnionM Expr0_0)
  | Max0_0 (UnionM Expr0_0) (UnionM Expr0_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_0)
    via (Default Expr0_0)

data Expr0_1
  = Ceq0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cless0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cleq0_1 (UnionM Expr0_0) (UnionM Expr0_0)
  | Cand0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cor0_1 (UnionM Expr0_1) (UnionM Expr0_1)
  | Cnot0_1 (UnionM Expr0_1)
  | CFalse0_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr0_1)
    via (Default Expr0_1)

-- output_type: Int
-- param_list: l lres t tmp1 r rres
data Expr1_0
  = Param31_0
  | Cadd1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Csub1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  | Czero1_0
  | CIte1_0 (UnionM Expr1_2) (UnionM Expr1_0) (UnionM Expr1_0)
  | Access1_1_1_0 (UnionM Expr1_1)
  | Max1_0 (UnionM Expr1_0) (UnionM Expr1_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_0)
    via (Default Expr1_0)

data Expr1_1
  = Param11_1
  | Param51_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_1)
    via (Default Expr1_1)

data Expr1_2
  = Ceq1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cless1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cleq1_2 (UnionM Expr1_0) (UnionM Expr1_0)
  | Cand1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cor1_2 (UnionM Expr1_2) (UnionM Expr1_2)
  | Cnot1_2 (UnionM Expr1_2)
  | CFalse1_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr1_2)
    via (Default Expr1_2)

-- output_type: Int
-- param_list: tmp2 t lres l r rres
data Expr2_0
  = Param02_0
  | Cadd2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Csub2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  | Czero2_0
  | CIte2_0 (UnionM Expr2_2) (UnionM Expr2_0) (UnionM Expr2_0)
  | Access1_1_2_0 (UnionM Expr2_1)
  | Max2_0 (UnionM Expr2_0) (UnionM Expr2_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_0)
    via (Default Expr2_0)

data Expr2_1
  = Param22_1
  | Param52_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_1)
    via (Default Expr2_1)

data Expr2_2
  = Ceq2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cless2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cleq2_2 (UnionM Expr2_0) (UnionM Expr2_0)
  | Cand2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cor2_2 (UnionM Expr2_2) (UnionM Expr2_2)
  | Cnot2_2 (UnionM Expr2_2)
  | CFalse2_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr2_2)
    via (Default Expr2_2)

-- output_type: Int
-- param_list: r rres t lres l
data Expr3_0
  = Cadd3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Csub3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  | Czero3_0
  | CIte3_0 (UnionM Expr3_2) (UnionM Expr3_0) (UnionM Expr3_0)
  | Access1_1_3_0 (UnionM Expr3_1)
  | Max3_0 (UnionM Expr3_0) (UnionM Expr3_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_0)
    via (Default Expr3_0)

data Expr3_1
  = Param13_1
  | Param33_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_1)
    via (Default Expr3_1)

data Expr3_2
  = Ceq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cless3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cleq3_2 (UnionM Expr3_0) (UnionM Expr3_0)
  | Cand3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cor3_2 (UnionM Expr3_2) (UnionM Expr3_2)
  | Cnot3_2 (UnionM Expr3_2)
  | CFalse3_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr3_2)
    via (Default Expr3_2)

-- output_type: Int
-- param_list: tmp3 l lres t r rres
data Expr4_0
  = Param04_0
  | Cadd4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Csub4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  | Czero4_0
  | CIte4_0 (UnionM Expr4_2) (UnionM Expr4_0) (UnionM Expr4_0)
  | Access1_1_4_0 (UnionM Expr4_1)
  | Max4_0 (UnionM Expr4_0) (UnionM Expr4_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_0)
    via (Default Expr4_0)

data Expr4_1
  = Param24_1
  | Param54_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_1)
    via (Default Expr4_1)

data Expr4_2
  = Ceq4_2 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cless4_2 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cleq4_2 (UnionM Expr4_0) (UnionM Expr4_0)
  | Cand4_2 (UnionM Expr4_2) (UnionM Expr4_2)
  | Cor4_2 (UnionM Expr4_2) (UnionM Expr4_2)
  | Cnot4_2 (UnionM Expr4_2)
  | CFalse4_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr4_2)
    via (Default Expr4_2)

-- output_type: Int
-- param_list: l lres t tmp4 r rres
data Expr5_0
  = Param35_0
  | Cadd5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Csub5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  | Czero5_0
  | CIte5_0 (UnionM Expr5_2) (UnionM Expr5_0) (UnionM Expr5_0)
  | Access1_1_5_0 (UnionM Expr5_1)
  | Max5_0 (UnionM Expr5_0) (UnionM Expr5_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_0)
    via (Default Expr5_0)

data Expr5_1
  = Param15_1
  | Param55_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_1)
    via (Default Expr5_1)

data Expr5_2
  = Ceq5_2 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cless5_2 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cleq5_2 (UnionM Expr5_0) (UnionM Expr5_0)
  | Cand5_2 (UnionM Expr5_2) (UnionM Expr5_2)
  | Cor5_2 (UnionM Expr5_2) (UnionM Expr5_2)
  | Cnot5_2 (UnionM Expr5_2)
  | CFalse5_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr5_2)
    via (Default Expr5_2)

-- output_type: Int
-- param_list: r rres t lres l
data Expr6_0
  = Cadd6_0 (UnionM Expr6_0) (UnionM Expr6_0)
  | Csub6_0 (UnionM Expr6_0) (UnionM Expr6_0)
  | Czero6_0
  | CIte6_0 (UnionM Expr6_2) (UnionM Expr6_0) (UnionM Expr6_0)
  | Access1_1_6_0 (UnionM Expr6_1)
  | Max6_0 (UnionM Expr6_0) (UnionM Expr6_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr6_0)
    via (Default Expr6_0)

data Expr6_1
  = Param16_1
  | Param36_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr6_1)
    via (Default Expr6_1)

data Expr6_2
  = Ceq6_2 (UnionM Expr6_0) (UnionM Expr6_0)
  | Cless6_2 (UnionM Expr6_0) (UnionM Expr6_0)
  | Cleq6_2 (UnionM Expr6_0) (UnionM Expr6_0)
  | Cand6_2 (UnionM Expr6_2) (UnionM Expr6_2)
  | Cor6_2 (UnionM Expr6_2) (UnionM Expr6_2)
  | Cnot6_2 (UnionM Expr6_2)
  | CFalse6_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr6_2)
    via (Default Expr6_2)

-- output_type: Int
-- param_list: r rres t lres l
data Expr7_0
  = Cadd7_0 (UnionM Expr7_0) (UnionM Expr7_0)
  | Csub7_0 (UnionM Expr7_0) (UnionM Expr7_0)
  | Czero7_0
  | CIte7_0 (UnionM Expr7_2) (UnionM Expr7_0) (UnionM Expr7_0)
  | Access1_1_7_0 (UnionM Expr7_1)
  | Max7_0 (UnionM Expr7_0) (UnionM Expr7_0)
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr7_0)
    via (Default Expr7_0)

data Expr7_1
  = Param17_1
  | Param37_1
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr7_1)
    via (Default Expr7_1)

data Expr7_2
  = Ceq7_2 (UnionM Expr7_0) (UnionM Expr7_0)
  | Cless7_2 (UnionM Expr7_0) (UnionM Expr7_0)
  | Cleq7_2 (UnionM Expr7_0) (UnionM Expr7_0)
  | Cand7_2 (UnionM Expr7_2) (UnionM Expr7_2)
  | Cor7_2 (UnionM Expr7_2) (UnionM Expr7_2)
  | Cnot7_2 (UnionM Expr7_2)
  | CFalse7_2
  deriving stock (Generic, Show)
  deriving (Mergeable, EvaluateSym, ToCon Expr7_2)
    via (Default Expr7_2)

$(makeUnionWrapper "mrg" ''Expr0_0)
$(makeUnionWrapper "mrg" ''Expr0_1)
$(makeUnionWrapper "mrg" ''Expr1_0)
$(makeUnionWrapper "mrg" ''Expr1_1)
$(makeUnionWrapper "mrg" ''Expr1_2)
$(makeUnionWrapper "mrg" ''Expr2_0)
$(makeUnionWrapper "mrg" ''Expr2_1)
$(makeUnionWrapper "mrg" ''Expr2_2)
$(makeUnionWrapper "mrg" ''Expr3_0)
$(makeUnionWrapper "mrg" ''Expr3_1)
$(makeUnionWrapper "mrg" ''Expr3_2)
$(makeUnionWrapper "mrg" ''Expr4_0)
$(makeUnionWrapper "mrg" ''Expr4_1)
$(makeUnionWrapper "mrg" ''Expr4_2)
$(makeUnionWrapper "mrg" ''Expr5_0)
$(makeUnionWrapper "mrg" ''Expr5_1)
$(makeUnionWrapper "mrg" ''Expr5_2)
$(makeUnionWrapper "mrg" ''Expr6_0)
$(makeUnionWrapper "mrg" ''Expr6_1)
$(makeUnionWrapper "mrg" ''Expr6_2)
$(makeUnionWrapper "mrg" ''Expr7_0)
$(makeUnionWrapper "mrg" ''Expr7_1)
$(makeUnionWrapper "mrg" ''Expr7_2)

instance GenSym (Int) Expr0_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr0_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam00_0] ++ [mrgCzero0_0]
    genSingle1 = [mrgCFalse0_1]
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
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd0_0 e0_0 e0_1] ++ [mrgCsub0_0 e0_2 e0_3] ++ [mrgCIte0_0 e1_0 e0_4 e0_5] ++ [mrgMax0_0 e0_6 e0_7])
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
        res <- chooseUnionFresh (genSingle1 ++ [mrgCeq0_1 e0_0 e0_1] ++ [mrgCless0_1 e0_2 e0_3] ++ [mrgCleq0_1 e0_4 e0_5] ++ [mrgCand0_1 e1_0 e1_1] ++ [mrgCor0_1 e1_2 e1_3] ++ [mrgCnot0_1 e1_4])
        return res

instance GenSym (Int) Expr1_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr1_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam31_0] ++ [mrgCzero1_0]
    genSingle1 = [mrgParam11_1] ++ [mrgParam51_1]
    genSingle2 = [mrgCFalse1_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd1_0 e0_0 e0_1] ++ [mrgCsub1_0 e0_2 e0_3] ++ [mrgCIte1_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_1_0 e1_0] ++ [mrgMax1_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq1_2 e0_0 e0_1] ++ [mrgCless1_2 e0_2 e0_3] ++ [mrgCleq1_2 e0_4 e0_5] ++ [mrgCand1_2 e2_0 e2_1] ++ [mrgCor1_2 e2_2 e2_3] ++ [mrgCnot1_2 e2_4])
        return res

instance GenSym (Int) Expr2_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr2_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam02_0] ++ [mrgCzero2_0]
    genSingle1 = [mrgParam22_1] ++ [mrgParam52_1]
    genSingle2 = [mrgCFalse2_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd2_0 e0_0 e0_1] ++ [mrgCsub2_0 e0_2 e0_3] ++ [mrgCIte2_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_2_0 e1_0] ++ [mrgMax2_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq2_2 e0_0 e0_1] ++ [mrgCless2_2 e0_2 e0_3] ++ [mrgCleq2_2 e0_4 e0_5] ++ [mrgCand2_2 e2_0 e2_1] ++ [mrgCor2_2 e2_2 e2_3] ++ [mrgCnot2_2 e2_4])
        return res

instance GenSym (Int) Expr3_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr3_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero3_0]
    genSingle1 = [mrgParam13_1] ++ [mrgParam33_1]
    genSingle2 = [mrgCFalse3_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd3_0 e0_0 e0_1] ++ [mrgCsub3_0 e0_2 e0_3] ++ [mrgCIte3_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_3_0 e1_0] ++ [mrgMax3_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq3_2 e0_0 e0_1] ++ [mrgCless3_2 e0_2 e0_3] ++ [mrgCleq3_2 e0_4 e0_5] ++ [mrgCand3_2 e2_0 e2_1] ++ [mrgCor3_2 e2_2 e2_3] ++ [mrgCnot3_2 e2_4])
        return res

instance GenSym (Int) Expr4_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr4_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam04_0] ++ [mrgCzero4_0]
    genSingle1 = [mrgParam24_1] ++ [mrgParam54_1]
    genSingle2 = [mrgCFalse4_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd4_0 e0_0 e0_1] ++ [mrgCsub4_0 e0_2 e0_3] ++ [mrgCIte4_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_4_0 e1_0] ++ [mrgMax4_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq4_2 e0_0 e0_1] ++ [mrgCless4_2 e0_2 e0_3] ++ [mrgCleq4_2 e0_4 e0_5] ++ [mrgCand4_2 e2_0 e2_1] ++ [mrgCor4_2 e2_2 e2_3] ++ [mrgCnot4_2 e2_4])
        return res

instance GenSym (Int) Expr5_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr5_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgParam35_0] ++ [mrgCzero5_0]
    genSingle1 = [mrgParam15_1] ++ [mrgParam55_1]
    genSingle2 = [mrgCFalse5_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd5_0 e0_0 e0_1] ++ [mrgCsub5_0 e0_2 e0_3] ++ [mrgCIte5_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_5_0 e1_0] ++ [mrgMax5_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq5_2 e0_0 e0_1] ++ [mrgCless5_2 e0_2 e0_3] ++ [mrgCleq5_2 e0_4 e0_5] ++ [mrgCand5_2 e2_0 e2_1] ++ [mrgCor5_2 e2_2 e2_3] ++ [mrgCnot5_2 e2_4])
        return res

instance GenSym (Int) Expr6_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr6_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero6_0]
    genSingle1 = [mrgParam16_1] ++ [mrgParam36_1]
    genSingle2 = [mrgCFalse6_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd6_0 e0_0 e0_1] ++ [mrgCsub6_0 e0_2 e0_3] ++ [mrgCIte6_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_6_0 e1_0] ++ [mrgMax6_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq6_2 e0_0 e0_1] ++ [mrgCless6_2 e0_2 e0_3] ++ [mrgCleq6_2 e0_4 e0_5] ++ [mrgCand6_2 e2_0 e2_1] ++ [mrgCor6_2 e2_2 e2_3] ++ [mrgCnot6_2 e2_4])
        return res

instance GenSym (Int) Expr7_0 where 
  fresh :: forall m. (MonadFresh m) => Int -> m (UnionM Expr7_0)
  fresh gendepth = gen0 gendepth 
    where
    genSingle0 = [mrgCzero7_0]
    genSingle1 = [mrgParam17_1] ++ [mrgParam37_1]
    genSingle2 = [mrgCFalse7_2]
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
        e2_0 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle0 ++ [mrgCadd7_0 e0_0 e0_1] ++ [mrgCsub7_0 e0_2 e0_3] ++ [mrgCIte7_0 e2_0 e0_4 e0_5] ++ [mrgAccess1_1_7_0 e1_0] ++ [mrgMax7_0 e0_6 e0_7])
        return res
    gen1 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle1
      | otherwise = do
        res <- chooseUnionFresh (genSingle1)
        return res
    gen2 gendepth
      | gendepth <= 0 = chooseUnionFresh genSingle2
      | otherwise = do
        e0_0 <- (gen0 (gendepth - 1))
        e0_1 <- (gen0 (gendepth - 1))
        e0_2 <- (gen0 (gendepth - 1))
        e0_3 <- (gen0 (gendepth - 1))
        e0_4 <- (gen0 (gendepth - 1))
        e0_5 <- (gen0 (gendepth - 1))
        e2_0 <- (gen2 (gendepth - 1))
        e2_1 <- (gen2 (gendepth - 1))
        e2_2 <- (gen2 (gendepth - 1))
        e2_3 <- (gen2 (gendepth - 1))
        e2_4 <- (gen2 (gendepth - 1))
        res <- chooseUnionFresh (genSingle2 ++ [mrgCeq7_2 e0_0 e0_1] ++ [mrgCless7_2 e0_2 e0_3] ++ [mrgCleq7_2 e0_4 e0_5] ++ [mrgCand7_2 e2_0 e2_1] ++ [mrgCor7_2 e2_2 e2_3] ++ [mrgCnot7_2 e2_4])
        return res

eval0_0 :: RefEnv -> Expr0_0 -> SymInteger
eval0_0 env (Param00_0) = evalVar0 env "w"
eval0_0 env (Cadd0_0 p0 p1) =  (evalU0_0 env p0) + (evalU0_0 env p1) 
eval0_0 env (Csub0_0 p0 p1) =  (evalU0_0 env p0) - (evalU0_0 env p1) 
eval0_0 env (Czero0_0) = 0
eval0_0 env (CIte0_0 p0 p1 p2) = mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (evalU0_0 env p1) (evalU0_0 env p2)
eval0_0 env (Max0_0 p0 p1) = max' (evalU0_0 env p0) (evalU0_0 env p1)

evalU0_0 :: RefEnv -> UnionM Expr0_0 -> SymInteger
evalU0_0 env = onUnion (eval0_0 env)

eval0_1 :: RefEnv -> Expr0_1 -> SymBool
eval0_1 env (Ceq0_1 p0 p1) =  (evalU0_0 env p0) ==~ (evalU0_0 env p1) 
eval0_1 env (Cless0_1 p0 p1) =  (evalU0_0 env p0) <~ (evalU0_0 env p1) 
eval0_1 env (Cleq0_1 p0 p1) =  (evalU0_0 env p0) <=~ (evalU0_0 env p1) 
eval0_1 env (Cand0_1 p0 p1) =  (evalU0_1 env p0) &&~ (evalU0_1 env p1) 
eval0_1 env (Cor0_1 p0 p1) =  (evalU0_1 env p0) ||~ (evalU0_1 env p1) 
eval0_1 env (Cnot0_1 p0) =  mrgIte ((evalU0_1 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval0_1 env (CFalse0_1) = (toSym False)

evalU0_1 :: RefEnv -> UnionM Expr0_1 -> SymBool
evalU0_1 env = onUnion (eval0_1 env)

eval1_0 :: RefEnv -> Expr1_0 -> SymInteger
eval1_0 env (Param31_0) = evalVar0 env "tmp1"
eval1_0 env (Cadd1_0 p0 p1) =  (evalU1_0 env p0) + (evalU1_0 env p1) 
eval1_0 env (Csub1_0 p0 p1) =  (evalU1_0 env p0) - (evalU1_0 env p1) 
eval1_0 env (Czero1_0) = 0
eval1_0 env (CIte1_0 p0 p1 p2) = mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (evalU1_0 env p1) (evalU1_0 env p2)
eval1_0 env (Access1_1_1_0 p0) = snd (evalU1_1 env p0)
eval1_0 env (Max1_0 p0 p1) = max' (evalU1_0 env p0) (evalU1_0 env p1)

evalU1_0 :: RefEnv -> UnionM Expr1_0 -> SymInteger
evalU1_0 env = onUnion (eval1_0 env)

eval1_1 :: RefEnv -> Expr1_1 -> (List, SymInteger)
eval1_1 env (Param11_1) = evalVar1 env "lres"
eval1_1 env (Param51_1) = evalVar1 env "rres"

evalU1_1 :: RefEnv -> UnionM Expr1_1 -> (List, SymInteger)
evalU1_1 env = onUnion (eval1_1 env)

eval1_2 :: RefEnv -> Expr1_2 -> SymBool
eval1_2 env (Ceq1_2 p0 p1) =  (evalU1_0 env p0) ==~ (evalU1_0 env p1) 
eval1_2 env (Cless1_2 p0 p1) =  (evalU1_0 env p0) <~ (evalU1_0 env p1) 
eval1_2 env (Cleq1_2 p0 p1) =  (evalU1_0 env p0) <=~ (evalU1_0 env p1) 
eval1_2 env (Cand1_2 p0 p1) =  (evalU1_2 env p0) &&~ (evalU1_2 env p1) 
eval1_2 env (Cor1_2 p0 p1) =  (evalU1_2 env p0) ||~ (evalU1_2 env p1) 
eval1_2 env (Cnot1_2 p0) =  mrgIte ((evalU1_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval1_2 env (CFalse1_2) = (toSym False)

evalU1_2 :: RefEnv -> UnionM Expr1_2 -> SymBool
evalU1_2 env = onUnion (eval1_2 env)

eval2_0 :: RefEnv -> Expr2_0 -> SymInteger
eval2_0 env (Param02_0) = evalVar0 env "tmp2"
eval2_0 env (Cadd2_0 p0 p1) =  (evalU2_0 env p0) + (evalU2_0 env p1) 
eval2_0 env (Csub2_0 p0 p1) =  (evalU2_0 env p0) - (evalU2_0 env p1) 
eval2_0 env (Czero2_0) = 0
eval2_0 env (CIte2_0 p0 p1 p2) = mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (evalU2_0 env p1) (evalU2_0 env p2)
eval2_0 env (Access1_1_2_0 p0) = snd (evalU2_1 env p0)
eval2_0 env (Max2_0 p0 p1) = max' (evalU2_0 env p0) (evalU2_0 env p1)

evalU2_0 :: RefEnv -> UnionM Expr2_0 -> SymInteger
evalU2_0 env = onUnion (eval2_0 env)

eval2_1 :: RefEnv -> Expr2_1 -> (List, SymInteger)
eval2_1 env (Param22_1) = evalVar1 env "lres"
eval2_1 env (Param52_1) = evalVar1 env "rres"

evalU2_1 :: RefEnv -> UnionM Expr2_1 -> (List, SymInteger)
evalU2_1 env = onUnion (eval2_1 env)

eval2_2 :: RefEnv -> Expr2_2 -> SymBool
eval2_2 env (Ceq2_2 p0 p1) =  (evalU2_0 env p0) ==~ (evalU2_0 env p1) 
eval2_2 env (Cless2_2 p0 p1) =  (evalU2_0 env p0) <~ (evalU2_0 env p1) 
eval2_2 env (Cleq2_2 p0 p1) =  (evalU2_0 env p0) <=~ (evalU2_0 env p1) 
eval2_2 env (Cand2_2 p0 p1) =  (evalU2_2 env p0) &&~ (evalU2_2 env p1) 
eval2_2 env (Cor2_2 p0 p1) =  (evalU2_2 env p0) ||~ (evalU2_2 env p1) 
eval2_2 env (Cnot2_2 p0) =  mrgIte ((evalU2_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval2_2 env (CFalse2_2) = (toSym False)

evalU2_2 :: RefEnv -> UnionM Expr2_2 -> SymBool
evalU2_2 env = onUnion (eval2_2 env)

eval3_0 :: RefEnv -> Expr3_0 -> SymInteger
eval3_0 env (Cadd3_0 p0 p1) =  (evalU3_0 env p0) + (evalU3_0 env p1) 
eval3_0 env (Csub3_0 p0 p1) =  (evalU3_0 env p0) - (evalU3_0 env p1) 
eval3_0 env (Czero3_0) = 0
eval3_0 env (CIte3_0 p0 p1 p2) = mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (evalU3_0 env p1) (evalU3_0 env p2)
eval3_0 env (Access1_1_3_0 p0) = snd (evalU3_1 env p0)
eval3_0 env (Max3_0 p0 p1) = max' (evalU3_0 env p0) (evalU3_0 env p1)

evalU3_0 :: RefEnv -> UnionM Expr3_0 -> SymInteger
evalU3_0 env = onUnion (eval3_0 env)

eval3_1 :: RefEnv -> Expr3_1 -> (List, SymInteger)
eval3_1 env (Param13_1) = evalVar1 env "rres"
eval3_1 env (Param33_1) = evalVar1 env "lres"

evalU3_1 :: RefEnv -> UnionM Expr3_1 -> (List, SymInteger)
evalU3_1 env = onUnion (eval3_1 env)

eval3_2 :: RefEnv -> Expr3_2 -> SymBool
eval3_2 env (Ceq3_2 p0 p1) =  (evalU3_0 env p0) ==~ (evalU3_0 env p1) 
eval3_2 env (Cless3_2 p0 p1) =  (evalU3_0 env p0) <~ (evalU3_0 env p1) 
eval3_2 env (Cleq3_2 p0 p1) =  (evalU3_0 env p0) <=~ (evalU3_0 env p1) 
eval3_2 env (Cand3_2 p0 p1) =  (evalU3_2 env p0) &&~ (evalU3_2 env p1) 
eval3_2 env (Cor3_2 p0 p1) =  (evalU3_2 env p0) ||~ (evalU3_2 env p1) 
eval3_2 env (Cnot3_2 p0) =  mrgIte ((evalU3_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval3_2 env (CFalse3_2) = (toSym False)

evalU3_2 :: RefEnv -> UnionM Expr3_2 -> SymBool
evalU3_2 env = onUnion (eval3_2 env)

eval4_0 :: RefEnv -> Expr4_0 -> SymInteger
eval4_0 env (Param04_0) = evalVar0 env "tmp3"
eval4_0 env (Cadd4_0 p0 p1) =  (evalU4_0 env p0) + (evalU4_0 env p1) 
eval4_0 env (Csub4_0 p0 p1) =  (evalU4_0 env p0) - (evalU4_0 env p1) 
eval4_0 env (Czero4_0) = 0
eval4_0 env (CIte4_0 p0 p1 p2) = mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (evalU4_0 env p1) (evalU4_0 env p2)
eval4_0 env (Access1_1_4_0 p0) = snd (evalU4_1 env p0)
eval4_0 env (Max4_0 p0 p1) = max' (evalU4_0 env p0) (evalU4_0 env p1)

evalU4_0 :: RefEnv -> UnionM Expr4_0 -> SymInteger
evalU4_0 env = onUnion (eval4_0 env)

eval4_1 :: RefEnv -> Expr4_1 -> (List, SymInteger)
eval4_1 env (Param24_1) = evalVar1 env "lres"
eval4_1 env (Param54_1) = evalVar1 env "rres"

evalU4_1 :: RefEnv -> UnionM Expr4_1 -> (List, SymInteger)
evalU4_1 env = onUnion (eval4_1 env)

eval4_2 :: RefEnv -> Expr4_2 -> SymBool
eval4_2 env (Ceq4_2 p0 p1) =  (evalU4_0 env p0) ==~ (evalU4_0 env p1) 
eval4_2 env (Cless4_2 p0 p1) =  (evalU4_0 env p0) <~ (evalU4_0 env p1) 
eval4_2 env (Cleq4_2 p0 p1) =  (evalU4_0 env p0) <=~ (evalU4_0 env p1) 
eval4_2 env (Cand4_2 p0 p1) =  (evalU4_2 env p0) &&~ (evalU4_2 env p1) 
eval4_2 env (Cor4_2 p0 p1) =  (evalU4_2 env p0) ||~ (evalU4_2 env p1) 
eval4_2 env (Cnot4_2 p0) =  mrgIte ((evalU4_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval4_2 env (CFalse4_2) = (toSym False)

evalU4_2 :: RefEnv -> UnionM Expr4_2 -> SymBool
evalU4_2 env = onUnion (eval4_2 env)

eval5_0 :: RefEnv -> Expr5_0 -> SymInteger
eval5_0 env (Param35_0) = evalVar0 env "tmp4"
eval5_0 env (Cadd5_0 p0 p1) =  (evalU5_0 env p0) + (evalU5_0 env p1) 
eval5_0 env (Csub5_0 p0 p1) =  (evalU5_0 env p0) - (evalU5_0 env p1) 
eval5_0 env (Czero5_0) = 0
eval5_0 env (CIte5_0 p0 p1 p2) = mrgIte ((evalU5_2 env p0) ==~ (toSym True)) (evalU5_0 env p1) (evalU5_0 env p2)
eval5_0 env (Access1_1_5_0 p0) = snd (evalU5_1 env p0)
eval5_0 env (Max5_0 p0 p1) = max' (evalU5_0 env p0) (evalU5_0 env p1)

evalU5_0 :: RefEnv -> UnionM Expr5_0 -> SymInteger
evalU5_0 env = onUnion (eval5_0 env)

eval5_1 :: RefEnv -> Expr5_1 -> (List, SymInteger)
eval5_1 env (Param15_1) = evalVar1 env "lres"
eval5_1 env (Param55_1) = evalVar1 env "rres"

evalU5_1 :: RefEnv -> UnionM Expr5_1 -> (List, SymInteger)
evalU5_1 env = onUnion (eval5_1 env)

eval5_2 :: RefEnv -> Expr5_2 -> SymBool
eval5_2 env (Ceq5_2 p0 p1) =  (evalU5_0 env p0) ==~ (evalU5_0 env p1) 
eval5_2 env (Cless5_2 p0 p1) =  (evalU5_0 env p0) <~ (evalU5_0 env p1) 
eval5_2 env (Cleq5_2 p0 p1) =  (evalU5_0 env p0) <=~ (evalU5_0 env p1) 
eval5_2 env (Cand5_2 p0 p1) =  (evalU5_2 env p0) &&~ (evalU5_2 env p1) 
eval5_2 env (Cor5_2 p0 p1) =  (evalU5_2 env p0) ||~ (evalU5_2 env p1) 
eval5_2 env (Cnot5_2 p0) =  mrgIte ((evalU5_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval5_2 env (CFalse5_2) = (toSym False)

evalU5_2 :: RefEnv -> UnionM Expr5_2 -> SymBool
evalU5_2 env = onUnion (eval5_2 env)

eval6_0 :: RefEnv -> Expr6_0 -> SymInteger
eval6_0 env (Cadd6_0 p0 p1) =  (evalU6_0 env p0) + (evalU6_0 env p1) 
eval6_0 env (Csub6_0 p0 p1) =  (evalU6_0 env p0) - (evalU6_0 env p1) 
eval6_0 env (Czero6_0) = 0
eval6_0 env (CIte6_0 p0 p1 p2) = mrgIte ((evalU6_2 env p0) ==~ (toSym True)) (evalU6_0 env p1) (evalU6_0 env p2)
eval6_0 env (Access1_1_6_0 p0) = snd (evalU6_1 env p0)
eval6_0 env (Max6_0 p0 p1) = max' (evalU6_0 env p0) (evalU6_0 env p1)

evalU6_0 :: RefEnv -> UnionM Expr6_0 -> SymInteger
evalU6_0 env = onUnion (eval6_0 env)

eval6_1 :: RefEnv -> Expr6_1 -> (List, SymInteger)
eval6_1 env (Param16_1) = evalVar1 env "rres"
eval6_1 env (Param36_1) = evalVar1 env "lres"

evalU6_1 :: RefEnv -> UnionM Expr6_1 -> (List, SymInteger)
evalU6_1 env = onUnion (eval6_1 env)

eval6_2 :: RefEnv -> Expr6_2 -> SymBool
eval6_2 env (Ceq6_2 p0 p1) =  (evalU6_0 env p0) ==~ (evalU6_0 env p1) 
eval6_2 env (Cless6_2 p0 p1) =  (evalU6_0 env p0) <~ (evalU6_0 env p1) 
eval6_2 env (Cleq6_2 p0 p1) =  (evalU6_0 env p0) <=~ (evalU6_0 env p1) 
eval6_2 env (Cand6_2 p0 p1) =  (evalU6_2 env p0) &&~ (evalU6_2 env p1) 
eval6_2 env (Cor6_2 p0 p1) =  (evalU6_2 env p0) ||~ (evalU6_2 env p1) 
eval6_2 env (Cnot6_2 p0) =  mrgIte ((evalU6_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval6_2 env (CFalse6_2) = (toSym False)

evalU6_2 :: RefEnv -> UnionM Expr6_2 -> SymBool
evalU6_2 env = onUnion (eval6_2 env)

eval7_0 :: RefEnv -> Expr7_0 -> SymInteger
eval7_0 env (Cadd7_0 p0 p1) =  (evalU7_0 env p0) + (evalU7_0 env p1) 
eval7_0 env (Csub7_0 p0 p1) =  (evalU7_0 env p0) - (evalU7_0 env p1) 
eval7_0 env (Czero7_0) = 0
eval7_0 env (CIte7_0 p0 p1 p2) = mrgIte ((evalU7_2 env p0) ==~ (toSym True)) (evalU7_0 env p1) (evalU7_0 env p2)
eval7_0 env (Access1_1_7_0 p0) = snd (evalU7_1 env p0)
eval7_0 env (Max7_0 p0 p1) = max' (evalU7_0 env p0) (evalU7_0 env p1)

evalU7_0 :: RefEnv -> UnionM Expr7_0 -> SymInteger
evalU7_0 env = onUnion (eval7_0 env)

eval7_1 :: RefEnv -> Expr7_1 -> (List, SymInteger)
eval7_1 env (Param17_1) = evalVar1 env "rres"
eval7_1 env (Param37_1) = evalVar1 env "lres"

evalU7_1 :: RefEnv -> UnionM Expr7_1 -> (List, SymInteger)
evalU7_1 env = onUnion (eval7_1 env)

eval7_2 :: RefEnv -> Expr7_2 -> SymBool
eval7_2 env (Ceq7_2 p0 p1) =  (evalU7_0 env p0) ==~ (evalU7_0 env p1) 
eval7_2 env (Cless7_2 p0 p1) =  (evalU7_0 env p0) <~ (evalU7_0 env p1) 
eval7_2 env (Cleq7_2 p0 p1) =  (evalU7_0 env p0) <=~ (evalU7_0 env p1) 
eval7_2 env (Cand7_2 p0 p1) =  (evalU7_2 env p0) &&~ (evalU7_2 env p1) 
eval7_2 env (Cor7_2 p0 p1) =  (evalU7_2 env p0) ||~ (evalU7_2 env p1) 
eval7_2 env (Cnot7_2 p0) =  mrgIte ((evalU7_2 env p0) ==~ (toSym True)) (toSym False) (toSym True)
eval7_2 env (CFalse7_2) = (toSym False)

evalU7_2 :: RefEnv -> UnionM Expr7_2 -> SymBool
evalU7_2 env = onUnion (eval7_2 env)


------program space end----

------spec begin-------
{-
Hole grammar for #0
w t 

Hole grammar for #1
l lres t tmp1 r rres 

Hole grammar for #2
tmp2 t lres l r rres 

Hole grammar for #3
r rres t lres l 

Hole grammar for #4
tmp3 l lres t r rres 

Hole grammar for #5
l lres t tmp4 r rres 

Hole grammar for #6
r rres t lres l 

Hole grammar for #7
r rres t lres l 
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

cat = 
  let
    f a b = 
      case a of
        Nil _ -> b
        Cons h t -> Cons h (f t b)
      
  in
  f 

max' a b = 
  mrgIte (a <~  b)
    (b)
    (a)

depth :: Tree -> SymInteger
depth = 
  let
    f t = 
      case t of
        Leaf w -> 0
        Node l r -> 1 +  (max' (f l) (f r))
  in
  f 

deepest = 
  let
    f t = 
      case t of
        Leaf w -> ((Cons w (Nil Unit)), (evalU0_0 (RefEnv [("w", (Env0 w))]) ((genSym (1::Int) "hole0") :: (UnionM Expr0_0))))
        Node l r -> 
          let
            lres =
              (f l)
          in
          let
            rres =
              (f r)
          in
          mrgIte ((let
            tmp1 =
              (snd lres)
          in
          evalU1_0 (RefEnv [("tmp1", (Env0 tmp1)), ("lres", (Env1 lres)), ("rres", (Env1 rres))]) ((genSym (1::Int) "hole1") :: (UnionM Expr1_0))) >~  (let
            tmp2 =
              (snd rres)
          in
          evalU2_0 (RefEnv [("tmp2", (Env0 tmp2)), ("lres", (Env1 lres)), ("rres", (Env1 rres))]) ((genSym (1::Int) "hole2") :: (UnionM Expr2_0))))
            (((fst lres), (evalU3_0 (RefEnv [("rres", (Env1 rres)), ("lres", (Env1 lres))]) ((genSym (1::Int) "hole3") :: (UnionM Expr3_0)))))
            (mrgIte ((let
              tmp3 =
                (snd lres)
            in
            evalU4_0 (RefEnv [("tmp3", (Env0 tmp3)), ("lres", (Env1 lres)), ("rres", (Env1 rres))]) ((genSym (1::Int) "hole4") :: (UnionM Expr4_0))) ==~  (let
              tmp4 =
                (snd rres)
            in
            evalU5_0 (RefEnv [("tmp4", (Env0 tmp4)), ("lres", (Env1 lres)), ("rres", (Env1 rres))]) ((genSym (1::Int) "hole5") :: (UnionM Expr5_0))))
              (((cat (fst lres) (fst rres)), (evalU6_0 (RefEnv [("rres", (Env1 rres)), ("lres", (Env1 lres))]) ((genSym (1::Int) "hole6") :: (UnionM Expr6_0)))))
              (((fst rres), (evalU7_0 (RefEnv [("rres", (Env1 rres)), ("lres", (Env1 lres))]) ((genSym (1::Int) "hole7") :: (UnionM Expr7_0))))))
      
  in
  f 

main' t = 
  (fst (deepest t))

------spec end-------

------main function-----

solverConfig :: GrisetteSMTConfig 16
solverConfig = approx Proxy z3
 
ioPair :: [(Tree, List)] -> IO ()
ioPair pairs = do
    res <- solve solverConfig (constraint pairs)
    case res of
        Left _ -> do
            print "fail!"
        Right model -> do
            print "success!"
    where
        constraint :: [(Tree, List)] -> SymBool
        constraint [] = con True
        constraint ((x, y) : xs) = (if ((main' x) == y) then (toSym True) else (toSym False)) &&~ constraint xs

main :: IO ()
main = do
    let pairs = [
                ((((Node ((Node ((Leaf (3))) ((Node ((Node ((Node ((Node ((Node ((Leaf (-4))) ((Leaf (3))))) ((Leaf (1))))) ((Leaf (4))))) ((Leaf (5))))) ((Leaf (3))))))) ((Node ((Leaf (5))) ((Leaf (-1)))))))), (Cons ((-4)) ((Cons ((3)) ((Nil Unit))))))
                , ((((Node ((Node ((Node ((Leaf (1))) ((Leaf (4))))) ((Leaf (1))))) ((Node ((Leaf (0))) ((Node ((Leaf (4))) ((Node ((Node ((Leaf (2))) ((Leaf (-3))))) ((Node ((Node ((Leaf (-5))) ((Leaf (4))))) ((Leaf (5)))))))))))))), (Cons ((-5)) ((Cons ((4)) ((Nil Unit))))))
                , ((((Leaf (5)))), (Cons ((5)) ((Nil Unit))))
                , ((((Node ((Node ((Node ((Node ((Node ((Leaf (2))) ((Leaf (0))))) ((Leaf (3))))) ((Leaf (3))))) ((Node ((Leaf (1))) ((Node ((Leaf (2))) ((Leaf (-3))))))))) ((Node ((Leaf (-3))) ((Leaf (-3)))))))), (Cons ((2)) ((Cons ((0)) ((Nil Unit))))))
                , ((((Node ((Node ((Leaf (2))) ((Node ((Node ((Leaf (-4))) ((Leaf (0))))) ((Leaf (-3))))))) ((Node ((Node ((Leaf (2))) ((Leaf (4))))) ((Node ((Leaf (-4))) ((Node ((Leaf (-5))) ((Leaf (-1)))))))))))), (Cons ((-4)) ((Cons ((0)) ((Cons ((-5)) ((Cons ((-1)) ((Nil Unit))))))))))
                , ((((Node ((Leaf (2))) ((Node ((Leaf (-4))) ((Leaf (5)))))))), (Cons ((-4)) ((Cons ((5)) ((Nil Unit))))))
                , ((((Node ((Node ((Node ((Leaf (5))) ((Node ((Leaf (-5))) ((Node ((Leaf (2))) ((Leaf (-1))))))))) ((Node ((Leaf (5))) ((Leaf (1))))))) ((Node ((Leaf (-4))) ((Leaf (-2)))))))), (Cons ((2)) ((Cons ((-1)) ((Nil Unit))))))
                , ((((Node ((Leaf (1))) ((Node ((Node ((Leaf (-3))) ((Leaf (-5))))) ((Node ((Leaf (2))) ((Leaf (-5)))))))))), (Cons ((-3)) ((Cons ((-5)) ((Cons ((2)) ((Cons ((-5)) ((Nil Unit))))))))))
                , ((((Node ((Node ((Node ((Leaf (0))) ((Node ((Leaf (-4))) ((Leaf (1))))))) ((Node ((Leaf (2))) ((Leaf (-2))))))) ((Leaf (-4)))))), (Cons ((-4)) ((Cons ((1)) ((Nil Unit))))))
                , ((((Leaf (-3)))), (Cons ((-3)) ((Nil Unit))))
                , ((((Node ((Leaf (-1))) ((Leaf (-3)))))), (Cons ((-1)) ((Cons ((-3)) ((Nil Unit))))))
                , ((((Node ((Node ((Node ((Leaf (-4))) ((Leaf (0))))) ((Node ((Node ((Node ((Leaf (-3))) ((Leaf (1))))) ((Leaf (2))))) ((Leaf (3))))))) ((Node ((Node ((Node ((Leaf (3))) ((Leaf (-5))))) ((Leaf (-4))))) ((Leaf (-2)))))))), (Cons ((-3)) ((Cons ((1)) ((Nil Unit))))))
                , ((((Node ((Node ((Leaf (5))) ((Node ((Node ((Leaf (-5))) ((Leaf (-2))))) ((Leaf (-4))))))) ((Node ((Leaf (-4))) ((Node ((Leaf (5))) ((Leaf (2)))))))))), (Cons ((-5)) ((Cons ((-2)) ((Nil Unit))))))
                , ((((Node ((Leaf (1))) ((Node ((Leaf (-5))) ((Node ((Node ((Leaf (5))) ((Leaf (5))))) ((Leaf (-5)))))))))), (Cons ((5)) ((Cons ((5)) ((Nil Unit))))))
                , ((((Node ((Node ((Leaf (-5))) ((Leaf (4))))) ((Node ((Leaf (1))) ((Leaf (-2)))))))), (Cons ((-5)) ((Cons ((4)) ((Cons ((1)) ((Cons ((-2)) ((Nil Unit))))))))))
                , ((((Node ((Leaf (-3))) ((Leaf (-5)))))), (Cons ((-3)) ((Cons ((-5)) ((Nil Unit))))))
                , ((((Node ((Node ((Node ((Leaf (3))) ((Leaf (-5))))) ((Leaf (-5))))) ((Node ((Node ((Leaf (2))) ((Leaf (4))))) ((Node ((Leaf (-4))) ((Leaf (-3)))))))))), (Cons ((3)) ((Cons ((-5)) ((Cons ((2)) ((Cons ((4)) ((Cons ((-4)) ((Cons ((-3)) ((Nil Unit))))))))))))))
                , ((((Node ((Leaf (-4))) ((Leaf (3)))))), (Cons ((-4)) ((Cons ((3)) ((Nil Unit))))))
                , ((((Node ((Node ((Leaf (1))) ((Leaf (3))))) ((Node ((Node ((Leaf (-3))) ((Node ((Node ((Leaf (5))) ((Leaf (-5))))) ((Leaf (4))))))) ((Node ((Node ((Leaf (5))) ((Node ((Leaf (3))) ((Leaf (-3))))))) ((Leaf (-5)))))))))), (Cons ((5)) ((Cons ((-5)) ((Cons ((3)) ((Cons ((-3)) ((Nil Unit))))))))))
                , ((((Leaf (-1)))), (Cons ((-1)) ((Nil Unit))))
                ]
    startTime <- getCurrentTime
    result <- SysTimeout.timeout (10 * 60 * 1000000) $ ioPair pairs
    endTime <- getCurrentTime
    let elapsedTime = diffUTCTime endTime startTime
    case result of
        Just _ -> putStrLn $ "Time: " ++ show elapsedTime ++ " seconds"
        Nothing -> putStrLn "Timeout occurred"
