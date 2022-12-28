module Liveness where

import Data.List (intercalate)
import Data.Map
import Data.Set (Set)
import Quadruples (Arg (..), LabelName, Loc, Op (..), Quadruple (..))
import SSA
import Text.Printf (printf)

data Liveness = Liveness
  { inLive :: Set Loc,
    out :: Set Loc,
    kill :: Set Loc,
    use :: Set Loc
  }

instance Show Liveness where
  show :: Liveness -> String
  show (Liveness inLive out kill use) =
    printf "in: %s, out: %s, kill: %s, use: %s" (show inLive) (show out) (show kill) (show use)

data LBlock = LBlock
  { label :: LabelName,
    block :: [(Quadruple, Liveness)],
    phiMap :: PhiMap,
    next :: [LabelName],
    previous :: [LabelName],
    liveness :: Liveness
  }

instance Show LBlock where
  show :: LBlock -> String
  show (LBlock label block phiMap next prvs liveness) =
    "---\n"
      ++ label
      ++ ":\n"
      ++ "previous:\n"
      -- add indentation
      ++ unlines (Prelude.map ("\t" ++) prvs)
      ++ "phi:\n"
      ++ unlines
        ( Prelude.map
            ( \(loc, phi) ->
                printf
                  "\tphi(%d) = %s"
                  loc
                  $ intercalate
                    ","
                    ( Prelude.map
                        (\(label, loc) -> printf "%s:%s" label (show loc))
                        (Data.Map.toList phi)
                    )
            )
            (Data.Map.toList phiMap)
        )
      ++ "block:\n"
      ++ unlines (Prelude.map (\(q, l) -> show q ++ "\t" ++ show l) block)
      ++ "next:\n"
      ++ unlines (Prelude.map ("\t" ++) next)
      ++ "liveness:\n"
      ++ "\t"
      ++ show liveness
