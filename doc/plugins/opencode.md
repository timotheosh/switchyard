# opencode plugin

Runs an agent with [opencode](https://opencode.ai) in non-interactive mode
(`opencode run`). This is the default plugin.

## Selecting it

In `switchyard.toml`:

```toml
plugin = "opencode"
```

## Harness

```clojure
{:harnesses
 {:opencode
  {:command "opencode"
   :model {:parameter "-m"}
   :extra-params ["--auto"]
   :exit-codes {}}}}
```

| Key             | Meaning                                                              |
|-----------------|----------------------------------------------------------------------|
| `:command`      | The executable.                                                      |
| `:model`        | `{:parameter "-m"}`: an agent's `:model` is passed as `-m <model>`.  |
| `:extra-params` | Extra arguments, passed through as-is before the prompt.             |
| `:exit-codes`   | Optional `{code reason}` entries that extend the defaults below.     |

There is no `:skill` mapping: opencode has no skills flag, so an agent that
lists `:skills` is reported as a problem before anything runs.

## Agent

```clojure
{:agents
 {:implementer
  {:harness :opencode
   :model "openrouter/~deepseek/deepseek-pro-latest"
   :writes ["src/"]}}}
```

The model is `provider/model`. List what your install knows with
`opencode models`. The provider needs credentials in `opencode auth`.

## How it runs

By default the agent's working directory is the run's own git worktree (see
"Run worktrees" in the README), not your checkout; with `--no-worktree` it is
the project root.

The command is `opencode run [-m model] [extra-params]`. The prompt (the
prompt file plus the specification) goes on **stdin**, so a long prompt never
hits the operating system's argument-length limit and a prompt that starts
with `-` can't be mistaken for a flag.

`--auto` makes opencode approve every permission request that isn't explicitly
denied. Switchyard checks an agent's `:writes` after each step and rolls the
step back on a violation; it does not stop the agent from writing during the
step, and it cannot see changes outside the repository or to ignored paths.
Treat `--auto` accordingly.

## Exit codes

Exit codes name a crash in the run record and failure message.

| Code | Reason         | Meaning                                                      |
|------|----------------|--------------------------------------------------------------|
| 1    | `:error`       | Every error opencode reports itself: a bad model, missing credentials, a server error, a missing message, a bad flag. They are not distinguishable. |
| 130  | `:interrupted` | SIGINT or SIGTERM: opencode 2.x handles both and exits 130. |
| 137  | `:killed`      | SIGKILL (also how switchyard stops a timed-out process tree). |

Other non-zero codes are `:unknown`. Add your own with `:exit-codes` on the
harness.

## Verified

These behaviors were checked against the real `opencode` binary with the free
`opencode/big-pickle` model: success (exit 0), a bad model (exit 1), stdin
prompts including a 200 KB prompt, running with `GIT_DIR` pointing nowhere, the
signals above, and a violation being rolled back and retried.
