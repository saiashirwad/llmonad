{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell splices and QuasiQuoters for tools and prompts.
module LLMonad.TH (
    prompt,
    makeTool,
    makeToolNamed,
) where

import Data.Text (Text)
import Data.Text qualified as T
import LLMonad.TH.QuasiQuoter (prompt)
import LLMonad.Tools (mkTool, toolSync)
import Language.Haskell.TH

-- | Automatically create a 'Tool' whose spec name comes from the function name.
makeTool :: Name -> Q Exp
makeTool = makeToolWorker "makeTool" Nothing

{- | Automatically create a 'Tool' under an explicit name, overriding the
function-name-derived default.
-}
makeToolNamed :: Text -> Name -> Q Exp
makeToolNamed toolName = makeToolWorker "makeToolNamed" (Just toolName)

makeToolWorker :: String -> Maybe Text -> Name -> Q Exp
makeToolWorker api toolNameOverride name = do
    info <- reify name
    case info of
        VarI _ typ _ ->
            generateToolExp api (maybe (T.pack (nameBase name)) id toolNameOverride) name typ
        _ -> fail (api ++ ": Expected a function variable name, got: " ++ show name)

{- | Inspect the function type and generate the 'Tool' splice.

Each supported arity maps to exactly one argument adapter: zero arguments
bind a unit, one argument passes through, and two to four arguments uncurry
from a fresh-binder tuple. IO-typed functions wrap in 'mkTool', pure ones
in 'toolSync'.
-}
generateToolExp :: String -> Text -> Name -> Type -> Q Exp
generateToolExp api toolName name typ = do
    let unrolled = unrollType typ
        argCount = length unrolled - 1
        retType = last unrolled
        isIO = case retType of
            AppT (ConT io) _ -> io == ''IO
            _ -> False
        ctor = if isIO then [|mkTool|] else [|toolSync|]
    adapter <- case argCount of
        0 -> [|\(() :: ()) -> $(varE name)|]
        1 -> varE name
        n
            | n <= 4 -> do
                vars <- mapM (newName . (: [])) (take n ['a' ..])
                lamE [tupP (map varP vars)] $ foldl appE (varE name) (map varE vars)
        _ -> fail (api ++ ": Functions with " ++ show argCount ++ " arguments are not supported (max 4)")
    [|$ctor (T.pack $(stringE (T.unpack toolName))) (T.pack $(stringE ("Execute " ++ nameBase name))) $(pure adapter)|]

-- | Unroll arrows from a function type into a list of argument and return types.
unrollType :: Type -> [Type]
unrollType (ForallT _ _ t) = unrollType t
unrollType (AppT (AppT ArrowT a) b) = a : unrollType b
unrollType (SigT t _) = unrollType t
unrollType other = [other]
