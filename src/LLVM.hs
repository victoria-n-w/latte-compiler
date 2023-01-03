module LLVM where

import SSA (TopDef, SSABlock (..), Phi(..))
import Data.List (intercalate)
import Quadruples (TopDef'(TopDef'), Loc, Type( ..), Quadruple(..), Op (..), CmpOp(..), SingOp(..), Arg(..))
import Text.Printf (printf)
import Data.Map qualified as Map

translate :: [TopDef] -> String
translate topdefs =
    header ++ intercalate "\n" (map transTopDef topdefs)

header :: String
header =
  "declare void @printInt(i32)\n"
  ++ "declare void @printString(i8*)\n"
  ++ "declare i32 @readInt()\n"
  ++ "declare i8* @readString()\n"
  ++ "declare i8* @concat(i8*, i8*)\n"



transTopDef :: TopDef -> String
transTopDef (TopDef' name type_ args blocks) =
    let args' = map transArgDef $ Map.toList args
        blocks' = map transBlock blocks
     in printf "define %s @%s(%s) {\n%s}\n"
          (transType type_)
          name
          (intercalate ", " args')
          (unlines blocks')

transArgDef :: (Loc, Type) -> String
transArgDef (loc, type_) =
    transType type_ ++ " " ++ transLoc loc

transType :: Type -> String
transType (Int x) = "i" ++ show x
transType Bool = "i1"
transType Void = "void"
transType (Ptr x) = transType x ++ "*"

transBlock :: SSABlock -> String
transBlock (SSABlock label block phiMap _ _) =
    let phi' = map (\p -> "\t" ++ transPhi p) $ Map.toList phiMap
        block' = map (\q -> "\t" ++ transQuadruple q) block
     in printf "%s:\n%s%s" label (unlines phi') (intercalate "\n" block')


transPhi :: (Loc, Phi) -> String
transPhi (loc, phi) =
    let mapping' = map transMapping $ Map.toList $ mapping phi
     in printf "%s = phi %s %s" (transLoc loc) (transType $ type_ phi) (intercalate ", " mapping')

transMapping :: (String, Arg) -> String
transMapping (label, arg) =
    printf "[%s, %%%s]" (transArg arg) label

transQuadruple :: Quadruple -> String
transQuadruple (BinOp t op arg1 arg2 loc) =
    printf "%s = %s %s %s, %s" (transLoc loc) (transOp op) (transType t) (transArg arg1) (transArg arg2)
transQuadruple (SingleArgOp t Neg arg loc) =
    printf "%s = mul %s %s, -1" (transLoc loc) (transType t) (transArg arg)
transQuadruple (SingleArgOp t Not arg loc) =
    printf "%s = xor %s %s, 1" (transLoc loc) (transType t) (transArg arg)
transQuadruple (CmpBinOp t op arg1 arg2 loc) =
    printf "%s = %s %s %s, %s" (transLoc loc) (transCmpOp op) (transType t) (transArg arg1) (transArg arg2)
transQuadruple (Assign t arg loc) =
    printf "%s = %s %s" (transLoc loc) (transType t) (transArg arg)
transQuadruple (Call loc t name args) =
    if t == Void
        then printf "call %s @%s(%s)" (transType t) name (intercalate ", " $ map transArgCall args)
      else
        printf "%s = call %s @%s(%s)" (transLoc loc) (transType t) name (intercalate ", " $ map transArgCall args)
transQuadruple (Label label) =
    printf "%s:" label
transQuadruple (Jump label) =
    printf "br label %%%s" label
transQuadruple (JumpIf arg label1 label2) =
    printf "br i1 %s, label %%%s, label %%%s" (transArg arg) label1 label2
transQuadruple ReturnVoid =
    printf "ret void"
transQuadruple (Return t arg) =
    printf "ret %s %s" (transType t) (transArg arg)
transQuadruple Nop = ""

-- | For location i, prints %ri
transLoc :: Loc -> String
transLoc loc = "%r" ++ show loc

transArgCall :: (Type, Arg) -> String
transArgCall (t, arg) =
    printf "%s %s" (transType t) (transArg arg)

transOp :: Op -> String
transOp Add = "add"
transOp Sub = "sub"
transOp Mul = "mul"
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
transArg (Var loc) = transLoc loc
transArg (Const x) = show x
