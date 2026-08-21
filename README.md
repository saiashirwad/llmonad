# LLMonad

A Type-Safe, Composable Haskell DSL for Large Language Models built on top of `effectful`.

`LLMonad` provides an elegant, expressive, and type-safe functional interface for interacting with Large Language Models across major providers (OpenAI, Anthropic, DeepSeek, OpenRouter, Groq, Ollama, and custom endpoints).

---

## Key Capabilities

- **`effectful` Architecture**: First-class `LLM` dynamic effect with pluggable network (`runLLMHTTP`) and pure in-memory mock (`runLLMMock`) interpreters.
- **Curried Functional API (`ask`, `ask'`)**: Curried functions where the return type drives automatic JSON Schema derivation and decoding.
- **Type-Driven Structured Outputs (`HasSchema`)**: Automatic GHC Generics JSON Schema derivation supporting strict OpenAI schemas and Anthropic tool schemas.
- **Self-Correcting Error Recovery (`extractWithRetry`)**: Automatic feedback loop that feeds JSON validation errors back to the model for multi-turn repair.
- **Autonomous Tool Agents (`runAgent`, `runAgentStructured`)**: Multi-step ReAct agent execution with cycle detection, configurable step bounds, and structured results.
- **Template Haskell QuasiQuoting (`[prompt| ... |]`)**: Compile-time variable interpolation with `#{var}` syntax and `makeTool` function splices.
- **Higher-Order Middleware**: Transparent in-memory response caching (`withCache`), request/response telemetry tracing (`withTrace`), and client-side token-bucket rate limiting (`withRateLimit`).
- **Composable Message Algebra & Real-Time Streaming**: `Prompt` monoid with `IsString`, `fewShot` templating, message smart constructors, and Server-Sent Events (SSE) streaming (`streamSSE`, `streamText`).

---

## Installation

Add `llmonad` and `effectful` to your `build-depends` in your `.cabal` file or `package.yaml`:

```cabal
build-depends:
    base >= 4.16 && < 5,
    llmonad,
    effectful,
    aeson,
    text
```

---

## Quick Tour

### 1. High-Level Curried Functional API (`ask`, `ask'`)

Define LLM queries as ordinary curried Haskell functions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad

data Sentiment = Positive | Negative | Neutral
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data SentimentReport = SentimentReport
  { sentiment :: Sentiment
  , confidence :: Double
  , explanation :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | 1-argument curried function returning plain Text
summarize :: (LLM :> es) => Text -> Eff es Text
summarize = ask "Summarize this input in one concise sentence"

-- | 1-argument curried function returning structured record
analyzeSentiment :: (LLM :> es) => Text -> Eff es SentimentReport
analyzeSentiment = ask "Analyze the sentiment of this text with confidence and explanation"

-- | 2-argument curried function returning Bool
compareThemes :: (LLM :> es) => Text -> Text -> Eff es Bool
compareThemes = ask' "Do these two text snippets discuss the same primary theme?"

-- | Applicative (<*>) and Monadic composition
workflow :: (LLM :> es) => Text -> Eff es (Text, SentimentReport)
workflow input = (,) <$> summarize input <*> analyzeSentiment input
```

---

### 2. Type-Driven Structured Extraction with Self-Correction

Extract strongly-typed data structures with automatic error recovery:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad

data Priority = Low | Medium | High | Critical
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data BugReport = BugReport
  { title :: Text
  , priority :: Priority
  , affectedModules :: [Text]
  , estimatedFixHours :: Double
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | Direct structured extraction
extractBug :: (LLM :> es) => Text -> Eff es BugReport
extractBug input = askStructured @BugReport ("Extract bug report:\n" <> input)

-- | Self-correcting retry loop (retries up to N times on decoding failure)
extractBugSafe :: (LLM :> es) => Text -> Eff es BugReport
extractBugSafe input = extractWithRetry @BugReport 3 ("Extract bug report:\n" <> input)
```

---

### 3. Autonomous Tool-Calling Agents

Register type-safe tools and let the autonomous agent plan, call tools, and return either text or structured outputs:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import GHC.Generics (Generic)
import LLMonad

data StockArgs = StockArgs { ticker :: Text }
  deriving (Show, Generic, FromJSON, ToSchema)

data ConvertArgs = ConvertArgs { amount :: Double, toCurrency :: Text }
  deriving (Show, Generic, FromJSON, ToSchema)

-- | Tool with IO side-effects
stockTool :: Tool
stockTool = mkTool "stock_price" "Look up stock price in USD" $ \(args :: StockArgs) -> do
  putStrLn ("Looking up ticker: " ++ T.unpack (ticker args))
  pure (225.50 :: Double)

-- | Synchronous pure tool
convertTool :: Tool
convertTool = toolSync "convert_currency" "Convert USD to target currency" $ \(args :: ConvertArgs) ->
  amount args * 0.92

myTools :: [Tool]
myTools = [stockTool, convertTool]

agentWorkflow :: (LLM :> es, IOE :> es) => Eff es Text
agentWorkflow = runAgent myTools "What is the price of AAPL in EUR?"
```

---

### 4. Template Haskell: `[prompt| ... |]` and `makeTool`

Interpolate variables at compile-time and automatically generate tools from Haskell functions:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.Text (Text)
import Effectful
import GHC.Generics (Generic)
import LLMonad

data TaxArgs = TaxArgs { country :: Text, amount :: Double }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- | Ordinary function
computeTax :: TaxArgs -> IO Double
computeTax args = pure (amount args * 0.19)

-- Close declaration group for Template Haskell reification
$(return [])

-- Automatically generate Tool from function
taxTool :: Tool
taxTool = $(makeTool 'computeTax)

promptWorkflow :: (LLM :> es, IOE :> es) => Text -> Double -> Eff es Text
promptWorkflow customer price = do
  let query = [prompt|Customer #{customer} purchased items totaling $#{price}. Compute tax.|]
  runAgent [taxTool] query
```

---

### 5. Pluggable Interpreters & Middleware Stacking

Stack transparent caching, rate limiting, and telemetry tracing over HTTP or Mock interpreters:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Effectful
import LLMonad
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "OPENAI_API_KEY"
  cache <- newInMemoryCache
  limiter <- newRateLimiter 10 1.0 -- 10 req/sec

  let tracer :: Trace -> IO ()
      tracer = \case
        TraceRequest m sys _ -> putStrLn ("[TRACE] Request to " ++ show m)
        TraceResponse txt _ _ -> putStrLn ("[TRACE] Response length: " ++ show (length (show txt)))
        TraceToolExecuted name ok _ -> putStrLn ("[TRACE] Tool " ++ show name ++ " status: " ++ show ok)
        TraceError err -> putStrLn ("[TRACE] Error: " ++ show err)

  let cfg = defaultConfig (openAIProvider (T.pack apiKey)) "gpt-4o-mini"

  runEff
    . runLLMHTTP cfg
    . withRateLimit limiter
    . withCache cache
    . withTrace tracer
    $ do
      ans <- generateText "Hello, world!"
      liftIO (putStrLn ans)
```

---

### 6. Offline Testing with Pure In-Memory Mock Interpreter

Test complex LLM interactions, tool calls, and structured extraction in pure unit tests without API keys or network requests:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import Effectful
import LLMonad

testScript :: [Either LLMError CompletionResponse]
testScript =
  [ Right (textResp "Hello from mock model!")
  ]

runTest :: (Text, [CompletionRequest])
runTest = runPureEff $ runLLMMock testScript (generateText "Say hello")
```

---

## Supported Providers

| Provider | Constructor | Default Model | Protocol Format |
| :--- | :--- | :--- | :--- |
| **OpenAI** | `openAIProvider key` | `gpt-4o-mini` | OpenAI-compatible (`/v1/chat/completions`) |
| **Anthropic** | `anthropicProvider key` | `claude-sonnet-4-5` | Anthropic Messages API (`/v1/messages`) |
| **DeepSeek** | `deepSeekProvider key` | `deepseek-chat` | OpenAI-compatible (`https://api.deepseek.com`) |
| **OpenRouter** | `openRouterProvider key` | `openai/gpt-4o-mini` | OpenAI-compatible (`https://openrouter.ai/api/v1`) |
| **Groq** | `groqProvider key` | `llama-3.3-70b-versatile` | OpenAI-compatible (`https://api.groq.com/openai/v1`) |
| **Ollama** | `ollamaProvider host` | `llama3.2` | Local OpenAI-compatible endpoint |

---

## Standalone Examples

Explore standalone, runnable example programs in the `examples/` directory:

1. **`examples/01_CurriedAPI.hs`**: Curried `ask` & `ask'`, 0/1/2 arguments, records, and Applicative `<*>` composition.
2. **`examples/02_StructuredOutput.hs`**: Schema derivation with GHC Generics, `askStructured`, and `extractWithRetry` self-correction.
3. **`examples/03_AgentWithTools.hs`**: Multi-step ReAct agent loops, `mkTool`, `toolSync`, `runAgent`, `runAgentStructured`, and cycle detection.
4. **`examples/04_QuasiQuotes.hs`**: `[prompt| ... |]` compile-time interpolation and `makeTool` splices.
5. **`examples/05_EffectfulHandlers.hs`**: Middleware stacking with `withCache`, `withTrace`, and `withRateLimit`.

Run any example with:

```bash
cabal exec ghc -- -package llmonad examples/01_CurriedAPI.hs -o /tmp/ex01 && /tmp/ex01
```

---

## Running the Demo Executable

Run the guided tour CLI demo with:

```bash
cabal run llmonad
```

If API keys are present (`ANTHROPIC_API_KEY`, `OPENAI_API_KEY`, `DEEPSEEK_API_KEY`, etc.), the demo connects live. Otherwise, it runs smoothly in offline mock mode showcasing all 5 feature tiers.

---

## License

MIT License. Copyright (c) Sai Ashirwad.
