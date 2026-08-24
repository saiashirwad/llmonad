# AGENTS.md

Instructions for AI coding agents working in this repository.

LLMonad is an Effectful-based Haskell DSL for typed LLM agents, tools, and
workflows. Workflow code must stay model-agnostic: models attach only at the
program edge via `bind`.

## Workflow commands

- `make format` — format everything (fourmolu is pinned via DotSlash)
- `make check` — format-check, build, and test; run before finishing any task
- `cabal repl lib:llmonad` — experiment in GHCi
- Tests are hspec. Full suite must pass; never weaken an assertion to make a
  refactor pass — fix the code or prove the old assertion was wrong.

## Haskell style bar

These rules come from real refactors in this repo (see the QuasiQuoter pass).
Every new module is held to them. When reviewing your own diff, check each one.

1. **Guards mirror the specification.** Each guard branch corresponds to one
   rule of whatever the function implements (a grammar, a state machine, a
   policy), written in specification order. If branches don't line up with the
   spec, either the code or the spec description is lying.

   ```haskell
   -- Each guard IS one grammar production:
   go text
       | Just rest <- T.stripPrefix "\\#{" text = cons (ChunkLit "#{") rest
       | Just rest <- T.stripPrefix "#{" text = tag rest
       | otherwise = plain text
   ```

2. **Standard combinators over hand-rolled scanners.** Index arithmetic,
   character-by-character recursion, and ad-hoc span/break loops hide intent.
   Prefer named combinators (`stripPrefix`, `breakOn`, `unsnoc`, `foldl'`,
   `mapAccumL`) whose names say what happens.

3. **No accumulator invariants in the reader's head.** Reverse-accumulators
   ("acc is reversed, flush reverses again"), paired mutable-ish state, and
   flush logic duplicated across branches force reviewers to simulate
   execution. Restructure until data flows forward and each helper does one
   thing. A small, single-purpose accumulator inside one obviously-named
   local function is acceptable; distributed bookkeeping is not.

4. **Parse completely, then interpret.** When consuming structured input,
   produce a pure-data intermediate (exported!), then transform it in a
   separate layer. Each half becomes independently testable.

5. **Duplicated equations are unmerged abstractions.** Four near-identical
   case branches usually mean one missing traversal or class instance.

6. **Dead bindings are lies.** A `do` block whose only purpose is to discard
   a value (`_ <- find ...`) is noise left by an earlier design — delete it.

7. **Document semantics where features collide.** Where two mechanisms touch
   (escaping × tag boundaries, cancellation × concurrency, caching × tool
   rounds), write the intended behavior as a sentence at the implementation
   site, and pin it with a test. Correct behavior should be stated, never an
   emergent property of control flow.

8. **Exact assertions make refactors fearless.** Exported pure functions get
   exact-equality tests on representative inputs, including corner cases.
   Vague assertions ("should not crash", property-only) do not permit safe
   rewriting later.

9. **Name things for what they mean, not how they work.** `beforeTag`,
   `close`, `flush` — not `acc`, `tmp`, `go2`.

## Architecture conventions

- **Effects**: dynamic effects with smart constructors that call `send`;
  interpreters live at program edges. Handlers compose via `interpose`
  middleware; prefer making middleware first-class values (transformers)
  over scoped brackets when it attaches to a specific component.
- **The model seam**: `ModelRuntime es` is a natural transformation
  `forall a. Eff (LLM : es) a -> Eff es a`. Providers are adapters behind it;
  `mockModel` scripts are the test adapter. Nothing above the seam names a
  provider, model, or HTTP detail — term level or type level.
- **Function-face abstractions**: model concepts on newtypes over functions
  and derive standard classes (`Category`, `Functor`) instead of inventing
  combinator zoos. An `Agent es i o` is callable; workflows are plain
  functions taking agents.
- **Seams earn their existence**: one adapter = hypothetical seam (don't
  build it); two adapters = real seam. Enforce invariants with additional
  interpreters (e.g. a read-only World) rather than phantom type parameters.
- **Smart constructors validate at construction time**, so failures happen
  before tokens are spent, not mid-invocation.
- **Capability via effect rows**: tools declare required effects
  (`World :> es`) at construction; rows discharge once at the edge.

## Repo facts

- GHC2021 + default extensions from `llmonad.cabal`; `-Wall` must stay clean.
- Formatting is fourmolu-pinned; never hand-reformat formatted code.
- Public API changes update the README examples and relevant specs in the
  same commit.
