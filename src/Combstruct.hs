{-# INCLUDE "../combstruct/src/combstruct.h" #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Combstruct (defsToOracle) where

import Foreign
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Control.Monad
import Data.Generics
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Def

data CombEqStr = CombEqStr
type CombEq = Ptr CombEqStr
data CombSysStr = CombSysStr
type CombSys = Ptr CombSysStr

foreign import ccall epsilon :: IO CombEq

foreign import ccall atom :: IO CombEq

foreign import ccall "sum" union :: CombEq -> CombEq -> IO CombEq

foreign import ccall prod :: CombEq -> CombEq -> IO CombEq

foreign import ccall seq :: CombEq -> IO CombEq

foreign import ccall ref :: CInt -> IO CombEq

foreign import ccall free_eq :: CombEq -> IO ()

foreign import ccall empty_sys :: IO CombSys

foreign import ccall add_sys :: CombSys -> CombEq -> IO ()

foreign import ccall sys_len :: CombSys -> CInt

foreign import ccall free_sys :: CombSys -> IO ()

foreign import ccall eval_sys 
  :: CombSys -> CDouble -> CDouble -> Ptr CDouble -> IO ()

foreign import ccall eval_point_sys 
  :: CombSys -> CDouble -> CDouble -> Ptr CDouble -> IO ()

foreign import ccall sing_sys
  :: CombSys -> CDouble -> CDouble -> Ptr CDouble -> IO CDouble

foreign import ccall mean_sys 
  :: CombSys -> CDouble -> CDouble -> CDouble -> Ptr CDouble -> IO CDouble

refToComb :: Ref -> IO CombEq
refToComb (Ref 0 s) = error "Impossible: nil reference"
refToComb (Ref i s) = ref (fromIntegral i)

prodToComb :: Prod -> IO CombEq
prodToComb (Prod rs) = do
  lr <- mapM refToComb rs
  a <- atom
  foldM prod a lr

sumToComb :: Sum -> IO CombEq
sumToComb (Sum ps) = do
  lp <- mapM prodToComb ps
  case lp of
    []  -> epsilon
    h:t -> foldM union h t

defToComb :: Def -> IO CombEq
defToComb (Def _ s) = sumToComb s

defsToComb :: Defs -> IO CombSys
defsToComb (Defs m) = do
  let ds = map fst $ sortBy (\(_, i) -> \(_, j) -> compare i j) (Map.elems m)
  sys <- empty_sys
  cs <- mapM defToComb ds
  mapM_ (add_sys sys) cs
  return sys

defsToVals :: [Def] -> IO [Double]
defsToVals ds = do
  sys <- empty_sys
  cs <- mapM defToComb ds
  mapM_ (add_sys sys) cs
  y <- newArray (replicate (length ds) (0.0::CDouble))
  z <- sing_sys sys (1.0e-6::CDouble) (1.0e-4::CDouble) y
  res <- peekArray (length ds) y
  free y
  free_sys sys
  return $ map realToFrac res

defsToOracle :: Defs -> Oracle
{-# NOINLINE defsToOracle #-}
defsToOracle (Defs m) = do
  let ds = map fst $ sortBy (\(_, i) -> \(_, j) -> compare i j) (Map.elems m)
  let l = unsafePerformIO $ defsToVals ds
  Map.fromList $ zip (map (\(Def d _) -> dataTypeName d) ds) l
