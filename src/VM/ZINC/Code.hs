module VM.ZINC.Code where


data ZAMValue =
  IntVal Int
  | BoolVal Bool
  | ClosVal ZAMCode ZAMEnv
  deriving Show

type ZAMStack = [ZAMValue]
type ZAMEnv = [ZAMValue]

type Address = Int
data ZAMInst =
  Ldi Int     -- 整数をload（スタックに積む）
  | Ldb Bool  -- bool値をload
  | Access Address
  | Closure ZAMCode
  | Apply
  | Return
  | Let
  | EndLet
  | Test ZAMCode ZAMCode
  | ArithBinOp (Int -> Int -> Int)
  | CompBinOp (Int -> Int -> Bool)

instance Show ZAMInst where
  show (Ldi n) = "ldi " ++ show n
  show (Ldb b) = "ldb " ++ show b

type ZAMCode = [ZAMInst]
