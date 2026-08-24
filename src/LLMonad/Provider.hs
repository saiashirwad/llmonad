{- | The provider abstraction: a record of functions that knows how to turn
a provider-neutral 'CompletionRequest' into a 'CompletionResponse'.

Records-of-functions (rather than a typeclass) keep providers first-class
values: you can store them in config, swap them at runtime, or build a
mock for tests in three lines.
-}
module LLMonad.Provider (
    -- * Providers
    Provider (..),
    StructuredMode (..),

    -- * Helpers
    nonStreamingFallback,
) where

import Data.Text (Text)
import LLMonad.Error (LLMError)
import LLMonad.Types (
    CompletionRequest,
    CompletionResponse,
    StreamEvent (..),
 )

-- | How a provider prefers to receive structured-output requests.
data StructuredMode
    = {- | Native JSON-Schema enforcement (OpenAI @json_schema@, Anthropic
      forced tool-use).
      -}
      StructuredNative
    | {- | Only legacy \"JSON object\" mode is available; the schema travels
      inside the prompt.
      -}
      StructuredJsonObjectOnly
    | -- | No JSON mode at all; schema lives entirely in the prompt.
      StructuredPromptOnly
    deriving (Eq, Show)

-- | A chat-completions backend.
data Provider = Provider
    { providerName :: Text
    -- ^ Human-readable name, used in errors and traces.
    , providerStructured :: StructuredMode
    -- ^ Best structured-output capability of this endpoint.
    , providerComplete :: CompletionRequest -> IO (Either LLMError CompletionResponse)
    -- ^ One-shot completion.
    , providerStream :: CompletionRequest -> (StreamEvent -> IO ()) -> IO (Either LLMError CompletionResponse)
    {- ^ Streaming completion. Implementations must emit exactly one
    'SEFinished' as the final event (or return an error).
    -}
    }

{- | Adapt a non-streaming completion function into a streaming one by
emitting a single final event. Used by mocks and simple backends.
-}
nonStreamingFallback ::
    (CompletionRequest -> IO (Either LLMError CompletionResponse)) ->
    (CompletionRequest -> (StreamEvent -> IO ()) -> IO (Either LLMError CompletionResponse))
nonStreamingFallback complete req cb = do
    er <- complete req
    case er of
        Left e -> pure (Left e)
        Right resp -> do
            cb (SEFinished resp)
            pure (Right resp)
