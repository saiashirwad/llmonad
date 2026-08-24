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

-- | Automatically create a Tool from a function name.
makeTool :: Name -> Q Exp
makeTool name = makeToolNamed (T.pack (nameBase name)) name

-- | Automatically create a named Tool from a function name.
makeToolNamed :: Text -> Name -> Q Exp
makeToolNamed toolName name = do
    info <- reify name
    case info of
        VarI _ typ _ -> generateToolExp toolName name typ
        _ -> fail ("makeTool: Expected a function variable name, got: " ++ show name)

-- | Inspect function type and generate the Tool splice.
generateToolExp :: Text -> Name -> Type -> Q Exp
generateToolExp toolName name typ = do
    let unrolled = unrollType typ
        retType = last unrolled
        argCount = length unrolled - 1
        isIO = case retType of
            AppT (ConT io) _ -> io == ''IO
            _ -> False
        toolNameStr = T.unpack toolName
        descStr = "Execute " ++ nameBase name
    case argCount of
        0 ->
            if isIO
                then [|mkTool (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(() :: ()) -> $(varE name))|]
                else [|toolSync (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(() :: ()) -> $(varE name))|]
        1 ->
            if isIO
                then [|mkTool (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) $(varE name)|]
                else [|toolSync (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) $(varE name)|]
        2 ->
            if isIO
                then [|mkTool (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (uncurry $(varE name))|]
                else [|toolSync (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (uncurry $(varE name))|]
        3 ->
            if isIO
                then [|mkTool (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(a, b, c) -> $(varE name) a b c)|]
                else [|toolSync (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(a, b, c) -> $(varE name) a b c)|]
        4 ->
            if isIO
                then [|mkTool (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(a, b, c, d) -> $(varE name) a b c d)|]
                else [|toolSync (T.pack $(stringE toolNameStr)) (T.pack $(stringE descStr)) (\(a, b, c, d) -> $(varE name) a b c d)|]
        _ ->
            fail ("makeTool: Functions with " ++ show argCount ++ " arguments are not supported (max 4)")

-- | Unroll arrows from a function type into a list of argument and return types.
unrollType :: Type -> [Type]
unrollType (ForallT _ _ t) = unrollType t
unrollType (AppT (AppT ArrowT a) b) = a : unrollType b
unrollType (SigT t _) = unrollType t
unrollType other = [other]
