# hermes plugin

Runs an agent with [hermes](https://github.com/NousResearch/hermes-agent) in
one-shot mode (`-z/--oneshot`).

> **Status:** the argument mapping and exit codes below come from the plugin's
> code, its tests, and `hermes --help`. Unlike opencode, a real hermes run has
> not been exercised through the pipeline runner.

## Selecting it

In `switchyard.toml`:

```toml
plugin = "hermes"
```

## Harness

```clojure
{:harnesses
 {:hermes
  {:command "hermes"
   :model {:parameter "-m"}
   :skill {:parameter "--skills" :plural true}
   :extra-params ["--accept-hooks"]
   :exit-codes {}}}}
```

| Key             | Meaning                                                                 |
|-----------------|-------------------------------------------------------------------------|
| `:command`      | The executable.                                                         |
| `:model`        | `{:parameter "-m"}`: an agent's `:model` is passed as `-m <model>`.     |
| `:skill`        | `{:parameter "--skills" :plural true}`: each of an agent's `:skills` becomes its own `--skills <name>`. Without this mapping an agent that lists skills is a problem. |
| `:extra-params` | Extra arguments, passed through as-is before the prompt. For unattended runs consider `--accept-hooks` (approve unseen shell hooks) and, at your own risk, `--yolo` (skip dangerous-command approvals). |
| `:exit-codes`   | Optional `{code reason}` entries that extend the defaults below.        |

## Agent

```clojure
{:agents
 {:reviewer
  {:harness :hermes
   :model "~deepseek/deepseek-flash-latest"
   :skills [:acd :review]
   :writes []}}}
```

Skills are named in the timetable's `:skills` section and passed to hermes by
name.

## How it runs

By default the agent's working directory is the run's own git worktree (see
"Run worktrees" in the README), not your checkout; with `--no-worktree` it is
the project root.

The command is `hermes [-m model] [--skills name]... [extra-params]
--oneshot=<prompt>`. Hermes prints only the final response on stdout in this
mode. The prompt is joined onto `--oneshot=` in a single argument so a prompt
starting with `-` is not parsed as a flag.

Because the prompt travels in the argument list, the operating system's limit
on one argument (128 KiB on Linux) applies. A prompt over that limit is reported
as a problem before anything runs. (Whether hermes can read its prompt from
stdin is not yet checked.)

## Exit codes

| Code | Reason         |
|------|----------------|
| 1    | `:no-output`   |
| 2    | `:failed`      |
| 130  | `:interrupted` |

Other non-zero codes are `:unknown`. Extend or override them with
`:exit-codes` on the harness.
