module LLVM where

import SSA (TopDef, SSABlock (..), Phi(..))
import Data.List (intercalate)
import Quadruples (TopDef'(TopDef'), Loc, Type( ..), Quadruple(..), Op (..), CmpOp(..), SingOp(..), Arg(..))
import Text.Printf (printf)
import Data.Map qualified as Map

translate :: [TopDef] -> String
translate topdefs =
    intercalate "\n" $ map transTopDef topdefs


transTopDef :: TopDef -> String
transTopDef (TopDef' name args blocks) =
    let args' = map transArgDef $ Map.toList args
        blocks' = map transBlock blocks
     in printf "define i32 @%s(%s) {\n%s\n}\n"
          name
          (unlines args')
          (unlines blocks')

transArgDef :: (Loc, Type) -> String
transArgDef (loc, type_) =
    printf "%s %%%d" (transType type_) loc

transType :: Type -> String
transType (Int x) = "i" ++ show x
transType Bool = "i1"
transType Void = "void"
transType (Ptr x) = transType x ++ "*"

transBlock :: SSABlock -> String
transBlock (SSABlock label block phiMap _ _) =
    let phi' = map transPhi $ Map.toList phiMap
        block' = map transQuadruple block
     in printf "%s:\n%s\n%s" label (intercalate "\n" phi') (intercalate "\n" block')


transPhi :: (Loc, Phi) -> String
transPhi (loc, phi) =
    let mapping' = map transMapping $ Map.toList $ mapping phi
     in printf "%%%d = phi %s %s" loc (transType $ type_ phi) (intercalate ", " mapping')

transMapping :: (String, Loc) -> String
transMapping (label, loc) =
    printf "[%%%d, %s]" loc label

transQuadruple :: Quadruple -> String
transQuadruple (BinOp t op arg1 arg2 loc) =
    printf "%%%d = %s %s %s, %s" loc (transOp op) (transType t) (transArg arg1) (transArg arg2)
transQuadruple (SingleArgOp t Neg arg loc) =
    printf "%%%d = %s imul %s, -1" loc (transType t) (transArg arg)
transQuadruple (SingleArgOp t Not arg loc) =
    printf "%%%d = %s xor %s, 1" loc (transType t) (transArg arg)
transQuadruple (CmpBinOp t op arg1 arg2 loc) =
    printf "%%%d = %s %s %s, %s" loc (transCmpOp op) (transType t) (transArg arg1) (transArg arg2)
transQuadruple (Assign t arg loc) =
    printf "%%%d = %s %s" loc (transType t) (transArg arg)
transQuadruple (Call loc t name args) =
    printf "%%%d = call %s @%s(%s)" loc (transType t) name (intercalate ", " (map (\(t, arg) -> transArg arg) args))
transQuadruple (Label label) =
    printf "%s:" label
transQuadruple (Jump label) =
    printf "br label %s" label
transQuadruple (JumpIf arg label1 label2) =
    printf "br i1 %s, label %s, label %s" (transArg arg) label1 label2
transQuadruple ReturnVoid =
    printf "ret void"
transQuadruple (Return t arg) =
    printf "ret %s %s" (transType t) (transArg arg)
transQuadruple Nop = ""

transOp :: Op -> String
transOp Add = "iadd"
transOp Sub = "isub"
transOp Mul = "imul"
transOp Div = "sdiv"
transOp Mod = "srem"
transOp And = "and"
transOp Or = "or"

transSingOp :: SingOp -> String
transSingOp Neg = "neg"
transSingOp Not = "not"

transCmpOp :: CmpOp -> String
transCmpOp Eq = "icmp eq"
transCmpOp Neq = "icmp ne"
transCmpOp Lt = "icmp slt"
transCmpOp Gt = "icmp sgt"
transCmpOp Le = "icmp sle"
transCmpOp Ge = "icmp sge"

transArg :: Arg -> String
transArg (Var loc) = printf "%%%d" loc
transArg (Const x) = show x
