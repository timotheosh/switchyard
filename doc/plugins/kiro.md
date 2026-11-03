# kiro plugin

Runs an agent with [Kiro CLI](https://kiro.dev/docs/cli/) in non-interactive
mode (`kiro-cli chat --no-interactive`).

## Selecting it

In `switchyard.toml`:

```toml
plugin = "kiro"
```

## Harness

```clojure
{:harnesses
 {:kiro
  {:command "kiro-cli"
   :model {:parameter "--model"}
   :extra-params ["--trust-tools=fs_read,fs_write"]
   :exit-codes {}}}}
```

| Key             | Meaning                                                                  |
|-----------------|--------------------------------------------------------------------------|
| `:command`      | The executable.                                                          |
| `:model`        | `{:parameter "--model"}`: an agent's `:model` is passed as `--model <model>`. |
| `:extra-params` | Extra arguments, passed through as-is after the model flag.              |
| `:exit-codes`   | Optional `{code reason}` entries that extend the defaults below.         |

There is no `:skill` mapping: kiro-cli has no skills flag, so an agent that
lists `:skills` is reported as a problem before anything runs. To give Kiro
skills, attach `skill://` resources to a Kiro agent (see below).

## Agent

```clojure
{:agents
 {:implementer
  {:harness :kiro
   :model "claude-sonnet-5.5"
   :writes ["src/"]}}}
```

The model is a Kiro model id; list them with `kiro-cli chat --list-models`.
An unknown model exits 1 and prints the available ids.

## How it runs

The agent's working directory is the run's git worktree (see "Run worktrees"
in the README), or the project root with `--no-worktree`.

The command is `kiro-cli chat --no-interactive [--model m] [extra-params]`.
The prompt goes on **stdin**, so a long prompt never hits the argument-length
limit and a prompt that starts with `-` can't be mistaken for a flag. Kiro's
stdout contains ANSI escape codes.

## Permissions

In `--no-interactive` mode there is nobody to approve a tool call. A tool that
is not pre-approved is **denied and the run carries on with exit 0** (it does
not hang or fail), so a missing approval shows up as the agent not doing the
work, not as a crash. Pre-approve what the agent needs:

- `--trust-tools=<list>` for tool-level scoping, e.g.
  `--trust-tools=fs_read,fs_write` trusts reading and writing but not shell.
  `--trust-tools=` trusts nothing.
- A scoped Kiro agent for path- and command-level scoping (`allowedTools`,
  `toolsSettings.write.allowedPaths`, shell allow/deny lists).
- `--trust-all-tools` approves everything. Switchyard checks an agent's
  `:writes` after each step and rolls the step back on a violation; it does not
  stop the agent from writing during the step, and cannot see changes outside
  the repository or to ignored paths. Treat it like opencode's `--auto`.

## Using a Kiro agent

Switchyard has no per-agent field for Kiro agents; define one harness per Kiro
agent and pass `--agent`:

```clojure
{:harnesses
 {:kiro-reviewer
  {:command "kiro-cli"
   :model {:parameter "--model"}
   :extra-params ["--agent" "reviewer"]}}}
```

Kiro looks for agents in `.kiro/agents/` of the working directory, then in
`~/.kiro/agents/`; a local agent wins over a global one of the same name. The
working directory is the run worktree, so a project-local agent is found only
if `.kiro/` is committed (not gitignored). Fallback: a global agent in
`~/.kiro/agents/` is found regardless of cwd, but isn't version-controlled with
the project. A missing agent exits 4.

## Exit codes

Exit codes name a crash in the run record and failure message. 1, 3 and 4 are
from [Kiro's exit-codes reference](https://kiro.dev/docs/reference/exit-codes/).

| Code | Reason                 | Meaning                                                          |
|------|------------------------|------------------------------------------------------------------|
| 1    | `:error`               | General failure: auth error, invalid args (e.g. unknown model), operation failed. |
| 3    | `:mcp-startup-failed`  | A server required by `--require-mcp-startup` did not start.      |
| 4    | `:agent-not-found`     | The agent named by `--agent` was unavailable.                    |
| 143  | `:interrupted`         | SIGTERM (plain signal death).                                    |

Other non-zero codes are `:unknown`. Add your own (e.g. `137` for SIGKILL,
which is how switchyard stops a timed-out process tree) with `:exit-codes` on
the harness.

## Verified

Checked against kiro-cli 2.18.0: a stdin prompt (exit 0), an unknown model
(exit 1), SIGTERM (exit 143), a denied unapproved write (exit 0), and writes
succeeding under `--trust-tools=fs_read,fs_write` and `--trust-tools=read,write`.
Not yet checked: `--model` vs an agent's own `model`, whether agent hooks fire
under `--no-interactive`, and a full switchyard pipeline run.
