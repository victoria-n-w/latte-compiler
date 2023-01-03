{-# LANGUAGE LambdaCase #-}
module Postprocess where

import SSA
import Quadruples (Quadruple (..), Loc, LabelName, Op (..), Arg (..), Type (..), TopDef'(..))
import Control.Monad.Writer
import Data.Bifunctor (second)
import Data.Map qualified as Map

postprocess :: [SSA.TopDef] -> [SSA.TopDef]
postprocess = map postprocessTopDef

postprocessTopDef :: SSA.TopDef -> SSA.TopDef
postprocessTopDef (TopDef' name args blocks) =
    let remaps = execWriter (mapM gatherRemapBlock blocks)
    in TopDef' name args (map (postprocessBlock remaps) blocks)

postprocessBlock :: VarMap -> SSA.SSABlock -> SSA.SSABlock
postprocessBlock remaps (SSABlock label qs phiMap next prev) =
    let qs' = filter (\case
                        (Assign {}) -> False
                        _ -> True) qs
      in
        SSA.rename remaps (SSABlock label qs' phiMap next prev)


gatherRemapBlock :: SSA.SSABlock -> Writer VarMap ()
gatherRemapBlock (SSABlock _ qs _ _ _) = mapM_ gatherRemap qs

type VarMap = Map.Map Loc Arg

gatherRemap :: Quadruple -> Writer VarMap ()
gatherRemap q =
    case q of
        (Assign t (Var loc) res) -> tell (Map.singleton res (Var loc))
        (Assign t (Const i) res) -> tell (Map.singleton res (Const i))
        _ -> return ()
