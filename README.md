# jax/git-mode

Magit-inspired buffer-based git interface for the [Jax](https://github.com/jax-editor/jax) text editor.

## Features

- **Status buffer** — dashboard showing HEAD, staged/unstaged/untracked files, recent commits, stashes
- **Staging granularity** — stage/unstage at file level (hunk and region staging planned)
- **Transient menus** — commit, branch, push, pull, fetch, log, diff, stash with argument toggles
- **Section navigation** — collapsible sections with TAB, n/p navigation
- **Inline diffs** — expand file diffs inline in the status buffer
- **Commit message buffer** — compose commits with C-c C-c / C-c C-k
- **Log viewer** — browsable commit history with commit inspection
- **Diff viewer** — syntax-highlighted diff display
- **Process buffer** — transparent log of every git command
- **Parallel execution** — concurrent git subprocess calls for fast status assembly

## Installation

```
jax -p install jax/git-mode
```

## Configuration

In your `init.janet`:

```janet
# git-mode activates automatically when installed.
# Optional configuration:
(import jax/git-mode :as git-mode)

# Disable refresh-on-save (default: true)
(git-mode/set-refresh-on-save false)

# Set max log entries (default: 256)
(git-mode/set-log-max-count 100)
```

## Key Bindings

### Global

| Key | Command | Description |
|-----|---------|-------------|
| `C-x g` | `git-status` | Open git status buffer |
| `C-x G` | `git-dispatch` | Open top-level git transient |

### Status Buffer

| Key | Command | Description |
|-----|---------|-------------|
| `g` | Refresh | Refresh status buffer |
| `s` | Stage | Stage file at point |
| `u` | Unstage | Unstage file at point |
| `x` | Discard | Discard changes at point (with confirmation) |
| `TAB` | Toggle section | Expand/collapse section or inline diff |
| `n` / `p` | Navigate | Next/previous section |
| `M-n` / `M-p` | Siblings | Next/previous sibling section |
| `^` | Parent | Jump to parent section |
| `1`–`4` | Level | Set section visibility level |
| `RET` | Visit | Open file or show commit diff |
| `q` | Quit | Bury the git buffer |
| `$` | Process | Show process buffer |
| `c` | Commit | Open commit transient |
| `b` | Branch | Open branch transient |
| `P` | Push | Open push transient |
| `F` | Pull | Open pull transient |
| `f` | Fetch | Open fetch transient |
| `l` | Log | Open log transient |
| `d` | Diff | Open diff transient |
| `z` | Stash | Open stash transient |

### Commit Buffer

| Key | Command | Description |
|-----|---------|-------------|
| `C-c C-c` | Finish | Finalize the commit |
| `C-c C-k` | Cancel | Abort the commit |

### Log Buffer

| Key | Command | Description |
|-----|---------|-------------|
| `RET` | Show commit | Show diff for commit at point |
| `n` / `p` | Navigate | Next/previous entry |
| `q` | Quit | Bury the log buffer |

## Transient Menus

### Commit (`c` from status)
- `-a` Stage all modified
- `-e` Allow empty commit
- `-n` Skip hooks
- `-s` Add Signed-off-by
- `c` Commit / `a` Amend / `f` Fixup / `w` Reword

### Branch (`b` from status)
- `b` Checkout / `c` Create & checkout / `n` Create
- `r` Rename / `k` Delete

### Push (`P` from status)
- `-f` Force with lease / `-u` Set upstream / `-n` Dry run
- `p` Push to pushremote / `o` Push to other

### Pull (`F` from status)
- `-r` Rebase / `-n` No rebase
- `p` Pull from upstream / `o` Pull from other

### Fetch (`f` from status)
- `-p` Prune / `-a` All remotes
- `f` Fetch from upstream / `o` Fetch from other

### Stash (`z` from status)
- `-u` Include untracked / `-a` All / `-k` Keep index
- `z` Save / `p` Pop / `a` Apply / `d` Drop / `l` List

### Log (`l` from status)
- `-n` Max count / `-a` All branches / `-g` Graph
- `--author` / `--since` / `--grep` filters
- `l` Current / `o` Other branch / `f` File

### Diff (`d` from status)
- `-w` Ignore whitespace / `--stat` Stat only
- `d` Dwim (unstaged) / `s` Staged / `c` Commit / `r` Range

## Dependencies

None — uses only core Jax APIs and the `git` CLI.

## License

MIT
