# LLMonad

A Type-Safe, Composable Haskell Domain-Specific Language (DSL) for Large Language Models.

`LLMonad` provides an expressive, monadic interface for interacting with LLMs across major providers (OpenAI, Anthropic, DeepSeek, OpenRouter, Ollama, and custom endpoints) with first-class support for:

- **Type-Driven Structured Outputs** (Generic JSON Schema derivation with automatic self-correcting retry loops)
- **Autonomous ReAct Agents** (Type-safe function calling with automatic execution and result forwarding)
- **Multi-Turn Stateful Conversations** (Automatic dialogue history tracking and prompt composition)
- **Universal Provider Dispatch** (OpenAI-compatible endpoints and Anthropic Messages API)
- **Real-Time Token Streaming** (Server-Sent Events streaming with chunk callbacks)

---

## Quick Start

### 1. Basic Conversational Chat

```haskell
{-# LANGUAGE OverloadedStrings #-}
import LLMonad
import Data.Text.IO qualified as TIO

main :: IO ()
main = do
  -- Use DeepSeek, OpenAI, Anthropic, or local Ollama
  let cfg = defaultConfig (deepseek "sk-...")
  
  result <- runLLM cfg $ do
    ans1 <- ask "What is the difference between a Functor and a Monad?"
    liftIO $ TIO.putStrLn ans1
    
    ans2 <- ask "Can you give a practical Haskell example?"
    liftIO $ TIO.putStrLn ans2

  print result
```

---

### 2. Type-Safe Structured Extraction

Derive `HasSchema` and `FromJSON` using GHC Generics to extract typed data with schema validation and automatic self-correcting error recovery:

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

data Sentiment = Positive | Neutral | Negative
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

data CodeReview = CodeReview
  { summary      :: Text
  , sentiment    :: Sentiment
  , issuesFound  :: [Text]
  , qualityScore :: Int
  }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

reviewCode :: Text -> LLM CodeReview
reviewCode snippet =
  askStructured ("Perform a code review on:\n" <> snippet)
```

---

### 3. ReAct Agents & Tool Calling

Define type-safe tools in pure Haskell and let the autonomous agent plan and execute multi-step tool calls:

```haskell
data CalcArgs = CalcArgs { op :: Text, a :: Double, b :: Double }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

data CalcRes = CalcRes { result :: Double }
  deriving (Show, Generic, FromJSON, ToJSON, HasSchema)

calcTool :: Tool
calcTool = defToolSync "calculator" "Perform arithmetic calculation" $ \(CalcArgs op a b) ->
  case op of
    "add" -> CalcRes (a + b)
    "mul" -> CalcRes (a * b)
    _     -> CalcRes 0.0

main :: IO ()
main = do
  let cfg = defaultConfig (openai "sk-...")
  res <- runLLM cfg $
    runAgentWith [calcTool] "What is 42 multiplied by 137, plus 10?"
  print res
```

---

## Supported Providers

| Provider | Constructor | Default Model | Protocol Format |
| :--- | :--- | :--- | :--- |
| **DeepSeek** | `deepseek "api-key"` | `deepseek-chat` | OpenAI-compatible |
| **OpenAI** | `openai "api-key"` | `gpt-4o-mini` | OpenAI-compatible |
| **Anthropic** | `anthropic "api-key"` | `claude-3-5-sonnet-20241022` | Anthropic Messages API |
| **OpenRouter** | `openrouter "api-key"` | `deepseek/deepseek-chat` | OpenAI-compatible |
| **Ollama** | `ollama` | `llama3.2` | Local (`localhost:11434`) |
| **Custom OpenAI** | `openaiCompatible "name" "url" maybeKey` | Configurable | OpenAI-compatible |
| **Custom Anthropic** | `anthropicCompatible "name" "url" "key"` | Configurable | Anthropic Messages API |

---

## Architecture

- **`LLMonad.Core`**: Monad transformer `LLMT m a`, `LLM a`, `MonadLLM` class, configuration, runners.
- **`LLMonad.Types`**: Unified message representation (`Role`, `Message`, `ToolCall`, `ChatRequest`, `ChatResponse`).
- **`LLMonad.Schema`**: GHC Generics-based JSON Schema engine.
- **`LLMonad.Structured`**: Type-driven extraction with self-correcting validation loops.
- **`LLMonad.Tools`**: Type-safe tool definition and dynamic invocation.
- **`LLMonad.Agent`**: Autonomous multi-step ReAct agent execution loop.
- **`LLMonad.Prompt`**: Conversational primitives, few-shot helpers, and template substitution.
- **`LLMonad.Streaming`**: SSE token streaming with callback hooks.
- **`LLMonad.Client`**: Universal HTTP dispatcher across OpenAI and Anthropic protocol styles.

---

## License

MIT License.
