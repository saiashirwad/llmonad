{- | First-class model middleware.

A 'Middleware' wraps a 'ModelRuntime' so cache, tracing, and rate-limiting
attach to individual agents instead of whole effect scopes. Compose with
'(<>)' or '(.)'; either way the left operand runs outermost and observes
traffic first:

> let runtime =
>         applyMiddleware (traced emit <> cached store <> rateLimited limiter) $
>             model provider "deepseek-v4-flash"
-}
module LLMonad.Middleware (
    Middleware (..),
) where

import LLMonad.Model (ModelRuntime)

-- | A transformation of one model runtime. This is the middleware seam.
newtype Middleware es = Middleware
    { applyMiddleware :: ModelRuntime es -> ModelRuntime es
    }

{- | Left operand composes outermost: @(f '<>' g) \`applyMiddleware\` rt = f (g rt)@,
so @f@ sees every request before @g@ does.
-}
instance Semigroup (Middleware es) where
    Middleware f <> Middleware g = Middleware (f . g)

instance Monoid (Middleware es) where
    mempty = Middleware id
