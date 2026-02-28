# git-mode — Magit-inspired buffer-based git interface for Jax
#
# Provides a complete git workflow without leaving the editor:
# status buffer, staging/unstaging, commit, branch, push/pull/fetch,
# log, diff, stash — all driven by transient menus.
#
# Entry points:
#   C-x g   — git-status (open status buffer)
#   C-x G   — git-dispatch (top-level transient)

(import jax/buffer :as buf)
(import jax/command)
(import jax/editor :as editor)
(import jax/hook)
(import jax/keymap)
(import jax/mode)
(import jax/faces)
(import jax/overlay)
(import jax/pane)
(import jax/prompt)
(import jax/transient)
(import jax/display-buffer :as db)
(import jax/core :prefix "")
(import jax/movement :as move)
(import jax/undo :as undo)
(import jax/kill-ring)
(import jax/package)

(import ./plumbing :as git)
(import ./sections :as sec)

# ════════════════════════════════════════════════════════════════════
# Helpers
# ════════════════════════════════════════════════════════════════════

(defn- use-buffer-root
  "Set (dyn :git-root) from the current buffer's stored git root.
  Call at the start of any operation that runs git commands."
  []
  (setdyn :git-root (get-in (buffer) [:locals :git-root])))

# ════════════════════════════════════════════════════════════════════
# Faces
# ════════════════════════════════════════════════════════════════════

(faces/defface :git-header
  "Git buffer header line."
  @{:bold true})

(faces/defface :git-section-heading
  "Git section heading."
  @{:bold true :fg "#7cafc2"})

(faces/defface :git-branch-local
  "Local branch name."
  @{:fg "#7cafc2"})

(faces/defface :git-branch-remote
  "Remote branch name."
  @{:fg "#a1b56c"})

(faces/defface :git-hash
  "Commit hash."
  @{:fg "#888888"})

(faces/defface :git-author
  "Commit author."
  @{:fg "#ba8baf"})

(faces/defface :git-date
  "Commit date."
  @{:fg "#888888"})

(faces/defface :git-tag
  "Tag name."
  @{:fg "#f7ca88"})

(faces/defface :git-diff-add
  "Added line in diff."
  @{:fg "#a1b56c"})

(faces/defface :git-diff-remove
  "Removed line in diff."
  @{:fg "#ab4642"})

(faces/defface :git-diff-hunk-header
  "Diff hunk header."
  @{:fg "#7cafc2"})

(faces/defface :git-diff-file-header
  "Diff file header."
  @{:bold true})

(faces/defface :git-graph
  "Graph art characters in log view."
  @{:fg "#ab4642"})

(faces/defface :git-subject
  "Commit subject in log view."
  @{})

(faces/defface :git-refs
  "Ref decorations in log view (branches, tags)."
  @{:fg "#f7ca88" :bold true})

(faces/defface :git-staged
  "Staged file indicator."
  @{:fg "#a1b56c"})

(faces/defface :git-unstaged
  "Unstaged file indicator."
  @{:fg "#ab4642"})

(faces/defface :git-untracked
  "Untracked file indicator."
  @{:fg "#888888"})

# ════════════════════════════════════════════════════════════════════
# Dynamic variables
# ════════════════════════════════════════════════════════════════════

(var refresh-on-save true)
(var log-max-count 256)

(defn set-refresh-on-save [v] (set refresh-on-save v))
(defn set-log-max-count [n] (set log-max-count n))

# ════════════════════════════════════════════════════════════════════
# Status buffer
# ════════════════════════════════════════════════════════════════════

# Per-repo status buffers keyed by repo root path
(var- status-bufs @{})
(var status-buf nil)
(var- refresh-pending false)
(var- refresh-generation 0)
(def- refresh-debounce-delay 0.3)

# Forward declarations for mutual references
(var show-commit-diff nil)
(var apply-diff-overlays nil)
(var diff-mode nil)

# --- Status mode keymap ---

(def status-keymap (keymap/new))

# --- Cursor helpers (needed early by line selection and navigation) ---

(defn- cursor-line []
  (def b (buffer))
  (- (first (buf/line-col b (cursor))) 1))

(defn- goto-line [b line]
  (when line
    (def pos (buf/line-byte-offset b line))
    (when pos (set-cursor pos))))

# --- Line selection for region staging ---
# Native line-selection using buffer locals and overlays.
# Tracks selected line range in [:locals :line-select] as
# @{:anchor <line> :end <line>} and highlights with overlays.

(defn- line-select-state [b]
  (get-in b [:locals :line-select]))

(defn- line-select-range [b]
  "Return [first-line last-line] of the current selection, or nil."
  (when-let [sel (line-select-state b)]
    [(min (sel :anchor) (sel :end))
     (max (sel :anchor) (sel :end))]))

(defn- line-select-update-overlays [b]
  "Update selection highlight overlays to match current selection range."
  (overlay/clear-overlays-by-tag b :git-line-select)
  (when-let [[first-line last-line] (line-select-range b)]
    (for i first-line (+ last-line 1)
      (def start (buf/line-byte-offset b i))
      (def end (buf/line-end-byte-offset b i))
      (when (and start end)
        (overlay/add-overlay b
          (overlay/make-overlay b start end :selection
            {:tag :git-line-select :priority 20}))))))

(defn- line-select-start [b line]
  "Enter line-selection mode at the given line."
  (put-in b [:locals :line-select] @{:anchor line :end line})
  (line-select-update-overlays b))

(defn- line-select-extend [b line]
  "Extend the selection to the given line."
  (when-let [sel (line-select-state b)]
    (put sel :end line)
    (line-select-update-overlays b)))

(defn- line-select-clear [b]
  "Cancel line selection."
  (put-in b [:locals :line-select] nil)
  (overlay/clear-overlays-by-tag b :git-line-select))

(defn- line-select-move [delta]
  "Move selection end by delta lines (+1 or -1) and move cursor."
  (def b (buffer))
  (def sel (line-select-state b))
  (unless sel (break))
  (def new-end (+ (sel :end) delta))
  (when (and (>= new-end 0) (< new-end (buf/line-count b)))
    (put sel :end new-end)
    (line-select-update-overlays b)
    (when-let [pos (buf/line-byte-offset b new-end)]
      (set-cursor pos))))

(command/defcmd git-toggle-line-select
  "Toggle line selection mode for region staging."
  :label "Toggle Line Select"
  []
  (def b (buffer))
  (if (line-select-state b)
    (line-select-clear b)
    (line-select-start b (cursor-line))))

(command/defcmd git-clear-selection
  "Clear line selection."
  :label "Clear Selection"
  []
  (line-select-clear (buffer)))

(command/defcmd git-next-line
  "Move to the next line, extending selection if active."
  :label "Next Line"
  []
  (if (line-select-state (buffer))
    (line-select-move 1)
    (move/next-line 1)))

(command/defcmd git-prev-line
  "Move to the previous line, extending selection if active."
  :label "Previous Line"
  []
  (if (line-select-state (buffer))
    (line-select-move -1)
    (move/prev-line 1)))

(def status-mode
  @{:name "git-status"
    :display-name "Git Status"
    :keymap status-keymap})

(mode/register status-mode)

# --- Rendering ---

(defn- status-line-xy-to-label
  "Convert a porcelain v2 XY code to a human-readable change type."
  [xy]
  (def index-char (string/slice xy 0 1))
  (def worktree-char (string/slice xy 1 2))
  (cond
    (= index-char "M") "modified"
    (= index-char "A") "new file"
    (= index-char "D") "deleted"
    (= index-char "R") "renamed"
    (= index-char "C") "copied"
    (= worktree-char "M") "modified"
    (= worktree-char "D") "deleted"
    "changed"))

(defn- render-status-buffer
  "Render the status buffer from parsed git data.
  Reads expanded/collapsed state from buffer locals to render inline
  diffs and hide collapsed sections. This is the single rendering path —
  both refresh and toggle operations call this."
  [b results]
  (let [status-data (results :status)
        branch (status-data :branch)
        entries (status-data :entries)
        log-data (results :log)
        stash-data (results :stash)
        diff-unstaged (results :diff-unstaged)
        diff-staged (results :diff-staged)
        unpushed-data (or (results :unpushed) @[])
        unpulled-data (or (results :unpulled) @[])
        # Read view state from buffer locals
        expanded-files (or (get-in b [:locals :expanded-files]) @{})
        expanded-commits (or (get-in b [:locals :expanded-commits]) @{})
        commit-diffs (or (get-in b [:locals :commit-diffs]) @{})
        collapsed-sections (or (get-in b [:locals :collapsed-sections]) @{})
        # Save cursor position for restoration
        saved-cursor (or (get-in b [:locals :saved-cursor]) 0)]

  # Clear buffer
  (put b :readonly false)
  (unless (b :undo-root) (undo/init b))
  (when (> (buf/length b) 0)
    (buf/delete-forward b 0 (buf/length b)))

  # Build content lines and sections
  (def lines @[])
  (def sections @[])
  (def overlays-list @[])

  # Helper to add a line and track position
  (defn add-line [text &opt face]
    (def line-num (length lines))
    (array/push lines text)
    (when face
      (array/push overlays-list @{:line line-num :face face}))
    line-num)

  # Helper to render inline diff lines for an expanded file
  (defn add-diff-lines [file-diff file-section]
    (def hunk-children @[])
    (each hunk (file-diff :hunks)
      (def hunk-start (length lines))
      (add-line (string "    " (hunk :header)) :git-diff-hunk-header)
      (each line (hunk :lines)
        (def face
          (cond
            (string/has-prefix? "+" line) :git-diff-add
            (string/has-prefix? "-" line) :git-diff-remove
            nil))
        (add-line (string "    " line) face))
      (def hunk-end (- (length lines) 1))
      (array/push hunk-children
        (sec/make-section :hunk hunk-start hunk-end
                          :data @{:hunk hunk :file-diff file-diff}
                          :face :git-diff-hunk-header)))
    (put file-section :children hunk-children)
    (put file-section :end (- (length lines) 1)))

  # Helper to render a file section (with optional inline diff)
  (defn add-file-section [entry change-type status-key face diff-data]
    (def path (entry :path))
    # Expansion key distinguishes staged vs unstaged for same file
    (def expand-key (string (or status-key "") ":" path))
    (def file-line (add-line (string "  " change-type "  " path) face))
    (def file-section
      (sec/make-section :file file-line file-line
                        :data @{:path path
                                :status status-key
                                :entry entry
                                :expand-key expand-key}
                        :face face))
    # Store diff data for future expansion
    (when diff-data
      (def file-diff (find |(= ($ :file) path) diff-data))
      (when file-diff
        (put-in file-section [:data :diff] file-diff)
        # Render inline diff if this file is expanded
        (when (expanded-files expand-key)
          (add-diff-lines file-diff file-section))))
    file-section)

  # --- Header ---
  (def head-display (or (branch :head) "(detached)"))
  (def oid-short (if (branch :oid)
                   (string/slice (branch :oid) 0 (min 7 (length (branch :oid))))
                   ""))
  (add-line (string "Head:     " head-display
                    (when (> (length oid-short) 0) (string " (" oid-short ")")))
            :git-header)

  (when (branch :upstream)
    (def ab-str
      (string
        (when (and (branch :ahead) (> (branch :ahead) 0))
          (string "ahead " (branch :ahead)))
        (when (and (branch :ahead) (> (branch :ahead) 0)
                   (branch :behind) (> (branch :behind) 0))
          ", ")
        (when (and (branch :behind) (> (branch :behind) 0))
          (string "behind " (branch :behind)))
        (when (and (or (nil? (branch :ahead)) (= (branch :ahead) 0))
                   (or (nil? (branch :behind)) (= (branch :behind) 0)))
          "up to date")))
    (add-line (string "Upstream: " (branch :upstream)
                      (when (> (length ab-str) 0) (string " (" ab-str ")")))
              :git-header))

  (add-line "")

  # --- Categorize entries ---
  (def untracked @[])
  (def unstaged @[])
  (def staged @[])

  (each entry entries
    (case (entry :type)
      :untracked (array/push untracked entry)
      :unmerged (array/push unstaged entry)
      (do
        (when (entry :unstaged) (array/push unstaged entry))
        (when (entry :staged) (array/push staged entry)))))

  # --- Render a group of entries as a collapsible section ---
  (defn render-section [section-key heading-text items face diff-data]
    (when (> (length items) 0)
      (def section-start (length lines))
      (add-line (string heading-text " (" (length items) ")")
                :git-section-heading)
      (def children @[])
      (unless (collapsed-sections section-key)
        (each entry items
          (def change-type
            (case section-key
              :untracked ""
              :staged (status-line-xy-to-label (entry :xy))
              (if (= (entry :type) :unmerged) "unmerged"
                (status-line-xy-to-label
                  (string "." (string/slice (entry :xy) 1 2))))))
          (def file-section
            (if (= section-key :untracked)
              (do
                (def file-line (add-line (string "  " (entry :path)) face))
                (sec/make-section :file file-line file-line
                                  :data @{:path (entry :path)
                                          :status section-key}
                                  :face face))
              (add-file-section entry change-type section-key face diff-data)))
          (array/push children file-section)))
      (def section-end (- (length lines) 1))
      (array/push sections
        (sec/make-section :section-header section-start section-end
                          :data @{:status section-key}
                          :children children
                          :collapsed (truthy? (collapsed-sections section-key))
                          :face :git-section-heading))
      (add-line "")))

  (render-section :untracked "Untracked files" untracked
                  :git-untracked nil)
  (render-section :unstaged "Unstaged changes" unstaged
                  :git-unstaged diff-unstaged)
  (render-section :staged "Staged changes" staged
                  :git-staged diff-staged)

  # Helper to render inline commit diff lines for an expanded commit
  (defn add-commit-diff-lines [hash commit-section]
    (def cached-diff (commit-diffs hash))
    (unless cached-diff (break))
    (def file-children @[])
    (each file-diff cached-diff
      (def file-start (length lines))
      (add-line (string "    " (or (file-diff :header) (string "diff " (file-diff :file))))
                :git-diff-file-header)
      (def hunk-children @[])
      (each hunk (file-diff :hunks)
        (def hunk-start (length lines))
        (add-line (string "    " (hunk :header)) :git-diff-hunk-header)
        (each line (hunk :lines)
          (def face
            (cond
              (string/has-prefix? "+" line) :git-diff-add
              (string/has-prefix? "-" line) :git-diff-remove
              nil))
          (add-line (string "    " line) face))
        (def hunk-end (- (length lines) 1))
        (array/push hunk-children
          (sec/make-section :hunk hunk-start hunk-end
                            :data @{:hunk hunk :file-diff file-diff}
                            :face :git-diff-hunk-header)))
      (def file-end (- (length lines) 1))
      (array/push file-children
        (sec/make-section :file file-start file-end
                          :data @{:path (file-diff :file)
                                  :commit-diff true}
                          :children hunk-children
                          :face :git-diff-file-header)))
    (put commit-section :children file-children)
    (put commit-section :end (- (length lines) 1)))

  # Helper to render a commit list section
  (defn render-commit-section [section-key heading commits]
    (when (> (length commits) 0)
      (def section-start (length lines))
      (add-line (string heading " (" (length commits) ")")
                :git-section-heading)
      (def children @[])
      (unless (collapsed-sections section-key)
        (each commit commits
          (def refs-str (if (commit :refs) (string " (" (commit :refs) ")") ""))
          (def commit-line
            (add-line (string "  " (commit :hash) " "
                             (commit :date) "  "
                             (commit :subject) refs-str)
                      :git-hash))
          (def commit-section
            (sec/make-section :commit commit-line commit-line
                              :data commit
                              :face :git-hash))
          # Render inline diff if this commit is expanded
          (when (expanded-commits (commit :hash))
            (add-commit-diff-lines (commit :hash) commit-section))
          (array/push children commit-section)))
      (def section-end (- (length lines) 1))
      (array/push sections
        (sec/make-section :section-header section-start section-end
                          :data @{:status section-key}
                          :children children
                          :collapsed (truthy? (collapsed-sections section-key))
                          :face :git-section-heading))
      (add-line "")))

  # --- Unpushed / Unpulled commits ---
  (render-commit-section :unpushed
    (string "Unpushed to " (or (branch :upstream) "upstream"))
    unpushed-data)
  (render-commit-section :unpulled
    (string "Unpulled from " (or (branch :upstream) "upstream"))
    unpulled-data)

  # --- Recent commits ---
  (render-commit-section :log "Recent commits" log-data)

  # --- Stashes ---
  (when (and stash-data (> (length stash-data) 0))
    (def section-start (length lines))
    (add-line (string "Stashes (" (length stash-data) ")")
              :git-section-heading)
    (def children @[])
    (unless (collapsed-sections :stash)
      (each stash stash-data
        (def stash-line
          (add-line (string "  " (stash :ref) " " (stash :message))
                    :git-hash))
        (array/push children
          (sec/make-section :stash stash-line stash-line
                            :data stash
                            :face :git-hash))))
    (def section-end (- (length lines) 1))
    (array/push sections
      (sec/make-section :section-header section-start section-end
                        :data @{:status :stash}
                        :children children
                        :collapsed (truthy? (collapsed-sections :stash))
                        :face :git-section-heading)))

  # --- Write to buffer ---
  (def content (string/join lines "\n"))
  (when (> (length content) 0)
    (buf/insert b 0 content))
  (put b :readonly true)
  (put b :modified false)

  # Store sections
  (sec/build-section-tree b sections)

  # Store parsed data for staging operations
  (put-in b [:locals :git-data] results)

  # Apply overlays for faces
  (overlay/clear-overlays-by-tag b :git-face)
  (overlay/clear-overlays-by-tag b :git-diff)
  (each ov-info overlays-list
    (def line (ov-info :line))
    (def face (ov-info :face))
    (def start (buf/line-byte-offset b line))
    (def end (buf/line-end-byte-offset b line))
    (def tag (if (or (= face :git-diff-add)
                     (= face :git-diff-remove)
                     (= face :git-diff-hunk-header))
               :git-diff :git-face))
    (overlay/add-overlay b
      (overlay/make-overlay b start end face
        {:tag tag :priority 10})))

  # Restore cursor position (clamp to buffer length)
  (def max-pos (max 0 (- (buf/length b) 1)))
  (put-in b [:locals :saved-cursor] nil)
  (when (> saved-cursor 0)
    (set-cursor (min saved-cursor max-pos)))

  # Fire hook
  (hook/fire :git-status-refreshed b)))

(defn- re-render-status
  "Re-render the status buffer from cached git data.
  Preserves cursor position. Used by toggle operations."
  [b]
  (def results (get-in b [:locals :git-data]))
  (unless results (break))
  (put-in b [:locals :saved-cursor] (cursor))
  (render-status-buffer b results))

# --- Refresh ---

(defn- do-status-refresh
  "Refresh the status buffer content."
  [b]
  (when refresh-pending (break))
  (set refresh-pending true)

  # Use the stored root, or detect it
  (def root (or (get-in b [:locals :git-root]) (git/repo-root)))
  (unless root
    (editor-message "Not in a git repository")
    (set refresh-pending false)
    (break))

  # Ensure git commands target this repo
  (setdyn :git-root root)
  (put-in b [:locals :git-root] root)

  # Fire concurrent git commands
  (def results @{})
  (def ch (ev/chan))
  (def log-fmt (string "--format=%h%x00%s%x00%ar%x00%an%x00%D"))

  # Detect upstream for unpushed/unpulled
  (def upstream (git/upstream-ref))

  (def commands
    @[[:status "status" "--porcelain=v2" "--branch"]
      [:diff-unstaged-raw "diff"]
      [:diff-staged-raw "diff" "--cached"]
      [:log-raw "log" log-fmt (string "-" (string log-max-count))]
      [:stash-raw "stash" "list"]])

  (when upstream
    (array/push commands
      [:unpushed-raw "log" log-fmt (string upstream "..HEAD")])
    (array/push commands
      [:unpulled-raw "log" log-fmt (string "HEAD.." upstream)]))

  (def num-commands (length commands))
  (each [key & args] commands
    (ev/go (fn []
             (def result (git/run ;args))
             (ev/give ch [key result]))))

  # Collect results
  (repeat num-commands
    (def [key result] (ev/take ch))
    (put results key result))

  # Parse results
  (def parsed @{})
  (put parsed :status
       (git/parse-status (get-in results [:status :stdout] "")))
  (put parsed :diff-unstaged
       (git/parse-diff (get-in results [:diff-unstaged-raw :stdout] "")))
  (put parsed :diff-staged
       (git/parse-diff (get-in results [:diff-staged-raw :stdout] "")))
  (put parsed :log
       (git/parse-log (get-in results [:log-raw :stdout] "")))
  (put parsed :stash
       (git/parse-stash-list (get-in results [:stash-raw :stdout] "")))
  (put parsed :unpushed
       (if upstream
         (git/parse-log (get-in results [:unpushed-raw :stdout] ""))
         @[]))
  (put parsed :unpulled
       (if upstream
         (git/parse-log (get-in results [:unpulled-raw :stdout] ""))
         @[]))

  # Render
  (render-status-buffer b parsed)

  (set refresh-pending false))

# --- Section navigation commands ---

(command/defcmd git-next-section
  "Move to the next section."
  :label "Next Section"
  []
  (def b (buffer))
  (def line (sec/next-section-line b (cursor-line)))
  (goto-line b line))

(command/defcmd git-prev-section
  "Move to the previous section."
  :label "Previous Section"
  []
  (def b (buffer))
  (def line (sec/prev-section-line b (cursor-line)))
  (goto-line b line))

(command/defcmd git-next-sibling
  "Move to the next sibling section."
  :label "Next Sibling Section"
  []
  (def b (buffer))
  (def line (sec/next-sibling-line b (cursor-line)))
  (goto-line b line))

(command/defcmd git-prev-sibling
  "Move to the previous sibling section."
  :label "Previous Sibling Section"
  []
  (def b (buffer))
  (def line (sec/prev-sibling-line b (cursor-line)))
  (goto-line b line))

(command/defcmd git-section-parent
  "Move to the parent section."
  :label "Section Parent"
  []
  (def b (buffer))
  (def line (sec/parent-section-line b (cursor-line)))
  (goto-line b line))

(command/defcmd git-toggle-section
  "Toggle visibility of the section at point."
  :label "Toggle Section"
  []
  (def b (buffer))
  (def line (cursor-line))
  (def section (sec/section-at-line b line))
  (when section
    (def expanded-files
      (or (get-in b [:locals :expanded-files]) @{}))
    (def collapsed-sections
      (or (get-in b [:locals :collapsed-sections]) @{}))
    (case (section :type)
      # File sections toggle inline diff expansion
      :file
      (let [expand-key (get-in section [:data :expand-key])]
        (when expand-key
          (if (expanded-files expand-key)
            (put expanded-files expand-key nil)
            (put expanded-files expand-key true))
          (put-in b [:locals :expanded-files] expanded-files)
          (re-render-status b)))

      # Section headers toggle collapse
      :section-header
      (let [status-key (get-in section [:data :status])]
        (when status-key
          (if (collapsed-sections status-key)
            (put collapsed-sections status-key nil)
            (put collapsed-sections status-key true))
          (put-in b [:locals :collapsed-sections] collapsed-sections)
          (re-render-status b)))

      # Commit sections toggle inline diff expansion
      :commit
      (let [hash (get-in section [:data :hash])]
        (when hash
          (def expanded-commits
            (or (get-in b [:locals :expanded-commits]) @{}))
          (def commit-diffs
            (or (get-in b [:locals :commit-diffs]) @{}))
          (if (expanded-commits hash)
            (put expanded-commits hash nil)
            (do
              (put expanded-commits hash true)
              # Fetch and cache the commit diff if not already cached
              (unless (commit-diffs hash)
                (use-buffer-root)
                (def result (git/run "show" "--format=" hash))
                (when (= (result :exit) 0)
                  (put commit-diffs hash
                       (git/parse-diff (result :stdout)))))))
          (put-in b [:locals :expanded-commits] expanded-commits)
          (put-in b [:locals :commit-diffs] commit-diffs)
          (re-render-status b)))

      # Hunk sections — toggle parent file
      :hunk
      (when-let [parent (section :parent)]
        (when-let [expand-key (get-in parent [:data :expand-key])]
          (put expanded-files expand-key nil)
          (put-in b [:locals :expanded-files] expanded-files)
          (re-render-status b))))))

(defn- set-visibility-level [level]
  (def b (buffer))
  # Level 1 = collapse all, level 4 = expand all
  (def collapsed-sections
    (or (get-in b [:locals :collapsed-sections]) @{}))
  (if (<= level 1)
    (do
      (each key [:untracked :unstaged :staged :log :stash]
        (put collapsed-sections key true))
      (put-in b [:locals :expanded-files] @{}))
      (do
        (each key [:untracked :unstaged :staged :log :stash]
          (put collapsed-sections key nil))
        (when (>= level 3)
          # Level 3+ expands files too — but we'd need to know which
          # files exist, so just clear collapsed state for now
          nil)))
    (put-in b [:locals :collapsed-sections] collapsed-sections)
    (re-render-status b))

(command/defcmd git-visibility-level-1
  "Collapse all sections." :label "Visibility Level 1" []
  (set-visibility-level 1))

(command/defcmd git-visibility-level-2
  "Show section headers only." :label "Visibility Level 2" []
  (set-visibility-level 2))

(command/defcmd git-visibility-level-3
  "Show section headers and files." :label "Visibility Level 3" []
  (set-visibility-level 3))

(command/defcmd git-visibility-level-4
  "Expand all sections and files." :label "Visibility Level 4" []
  (set-visibility-level 4))

# --- Staging operations ---

(defn- hunk-region-lines
  "If the buffer has an active line selection within the current hunk section,
  return [sel-start sel-end] as 0-indexed offsets into hunk :lines.
  Otherwise return nil (meaning stage the whole hunk)."
  [b section]
  (def sel-range (line-select-range b))
  (unless sel-range (break nil))
  (def [sel-first sel-last] sel-range)
  (def hunk-data (get-in section [:data :hunk]))
  (unless hunk-data (break nil))
  # The hunk section spans lines section:start to section:end.
  # Line section:start is the hunk header, actual diff lines start at +1.
  (def first-content-line (+ (section :start) 1))
  # Convert to hunk-relative offsets (0-indexed into hunk :lines)
  (def rel-start (- sel-first first-content-line))
  (def rel-end (- sel-last first-content-line))
  (def max-idx (- (length (hunk-data :lines)) 1))
  # Only valid if selection is within the hunk content lines
  (when (and (>= rel-start 0) (<= rel-end max-idx))
    [(max 0 rel-start) (min max-idx rel-end)]))

(defn- find-parent-file-section
  "Walk up from a section to find the parent :file section."
  [section]
  (var s (section :parent))
  (while (and s (not= (s :type) :file))
    (set s (s :parent)))
  s)

(defn- file-status-for-hunk
  "Determine the file status for a hunk by finding its parent file section."
  [section]
  (when-let [file-sec (find-parent-file-section section)]
    (get-in file-sec [:data :status])))

(command/defcmd git-stage
  "Stage the file, hunk, or section at point."
  :label "Stage"
  []
  (let [b (buffer)
        line (cursor-line)
        section (sec/section-at-line b line)]
    (unless section (break))
    (let [data (section :data)
          root (get-in b [:locals :git-root])]
      (unless root (break))
      (use-buffer-root)

  (case (section :type)
    # Stage a single file
    :file
    (do
      (case (data :status)
        :untracked (git/run "add" (data :path))
        :unstaged (git/run "add" (data :path))
        nil)
      (when (data :path)
        (editor/message (string "Staged " (data :path)))))

    # Stage a hunk (or region within a hunk)
    :hunk
    (when (= (file-status-for-hunk section) :unstaged)
      (def file-diff (data :file-diff))
      (def hunk (data :hunk))
      (def region (hunk-region-lines b section))
      (def patch
        (if region
          (git/make-region-patch file-diff hunk (region 0) (region 1))
          (git/make-hunk-patch file-diff hunk)))
      (def result (git/run-with-input patch "apply" "--cached"))
      (if (= (result :exit) 0)
        (editor/message (if region "Staged region." "Staged hunk."))
        (editor/message (string "Stage hunk failed: " (result :stderr)))))

    # Stage all files in a section header
    :section-header
    (case (data :status)
      :untracked
      (each child (section :children)
        (git/run "add" (get-in child [:data :path])))
      :unstaged
      (each child (section :children)
        (git/run "add" (get-in child [:data :path])))
      nil))

  # Clear selection after staging
  (line-select-clear b)
  (do-status-refresh b)
  (hook/fire :git-post-operation :stage [] 0))))

(command/defcmd git-unstage
  "Unstage the file, hunk, or section at point."
  :label "Unstage"
  []
  (def b (buffer))
  (def line (cursor-line))
  (def section (sec/section-at-line b line))
  (unless section (break))
  (def data (section :data))
  (use-buffer-root)

  (case (section :type)
    :file
    (when (= (data :status) :staged)
      (git/run "restore" "--staged" (data :path)))

    :hunk
    (when (= (file-status-for-hunk section) :staged)
      (def file-diff (data :file-diff))
      (def hunk (data :hunk))
      (def region (hunk-region-lines b section))
      (def patch
        (if region
          (git/make-region-patch file-diff hunk (region 0) (region 1) true)
          (git/make-hunk-patch file-diff hunk)))
      (def result (git/run-with-input patch "apply" "--cached" "--reverse"))
      (if (= (result :exit) 0)
        (editor/message (if region "Unstaged region." "Unstaged hunk."))
        (editor/message (string "Unstage hunk failed: " (result :stderr)))))

    :section-header
    (when (= (data :status) :staged)
      (each child (section :children)
        (git/run "restore" "--staged" (get-in child [:data :path])))))

  (line-select-clear b)
  (do-status-refresh b)
  (hook/fire :git-post-operation :unstage [] 0))

(command/defcmd git-discard
  "Discard changes for the file, hunk, or section at point."
  :label "Discard"
  []
  (def b (buffer))
  (def line (cursor-line))
  (def section (sec/section-at-line b line))
  (unless section (break))
  (def data (section :data))
  (use-buffer-root)

  (defn discard-file [path status]
    (case status
      :untracked (os/rm (string (get-in b [:locals :git-root]) "/" path))
      :unstaged (git/run "checkout" "--" path)
      nil))

  (defn discard-hunk []
    (def file-diff (data :file-diff))
    (def hunk (data :hunk))
    (def region (hunk-region-lines b section))
    (def patch
      (if region
        (git/make-region-patch file-diff hunk (region 0) (region 1))
        (git/make-hunk-patch file-diff hunk)))
    (git/run-with-input patch "apply" "--reverse"))

  (prompt/activate
    {:prompt "Discard changes? (y/n) "
     :on-submit
     (fn [input]
       (when (= (string/ascii-lower (string/trim input)) "y")
         (case (section :type)
           :file (discard-file (data :path) (data :status))
           :hunk (discard-hunk)
           :section-header
           (each child (section :children)
             (discard-file (get-in child [:data :path])
                           (get-in child [:data :status]))))
         (line-select-clear b)
         (do-status-refresh b)
         (hook/fire :git-post-operation :discard [] 0)))}))

# --- Visit thing at point ---

(defn- visit-hunk-line
  "Visit the file at the diff line under the cursor within a hunk section.
  Context and added lines open the working copy at the corresponding line.
  Removed lines open a temporary buffer with the old file version."
  [b line section]
  (def root (get-in b [:locals :git-root]))
  (unless root (break))
  (use-buffer-root)

  (def hunk (get-in section [:data :hunk]))
  (def file-diff (get-in section [:data :file-diff]))
  (unless (and hunk file-diff) (break))

  # Determine which hunk line the cursor is on
  (def hunk-content-start (+ (section :start) 1))
  (def line-idx (- line hunk-content-start))
  (def hunk-lines (hunk :lines))
  (when (or (< line-idx 0) (>= line-idx (length hunk-lines)))
    # Cursor is on the hunk header — visit the file at hunk start
    (when-let [file-path (file-diff :file)]
      (def full-path (string root "/" file-path))
      (def target-line (- (hunk :new-start) 1))
      (def opened (editor/open-file full-path (editor/get-state)))
      (when (and opened (> target-line 0))
        (when-let [pos (buf/line-byte-offset opened target-line)]
          (set-cursor pos))))
    (break))

  (def diff-line (get hunk-lines line-idx))
  (def prefix (string/slice diff-line 0 1))

  # Walk hunk lines up to line-idx to compute file line numbers
  (var old-line (hunk :old-start))
  (var new-line (hunk :new-start))
  (for i 0 line-idx
    (def l (get hunk-lines i))
    (def p (string/slice l 0 1))
    (cond
      (= p " ") (do (++ old-line) (++ new-line))
      (= p "+") (++ new-line)
      (= p "-") (++ old-line)))

  (def file-path (file-diff :file))
  (def old-file-path (or (file-diff :old-file) file-path))

  (cond
    # Removed line — show old version from git
    (= prefix "-")
    (let [file-status (file-status-for-hunk section)
          # Unstaged diffs compare index vs worktree — old version is index.
          # Staged diffs compare HEAD vs index — old version is HEAD.
          ref-path (if (= file-status :staged)
                     (string "HEAD:" old-file-path)
                     (string ":" old-file-path))
          ref-label (if (= file-status :staged) "HEAD" "index")
          result (git/run "show" ref-path)]
      (when (= (result :exit) 0)
        (def buf-name (string "*" old-file-path " (" ref-label ")*"))
        (def view-buf (editor/make-view-buffer buf-name (result :stdout)))
        (put-in view-buf [:locals :git-root] root)
        (db/pop-to-buffer view-buf (editor/get-state)
                          :actions [:reuse :split-right])
        # Jump to the target line (0-indexed)
        (def target-line (- old-line 1))
        (when (> target-line 0)
          (when-let [pos (buf/line-byte-offset view-buf target-line)]
            (set-cursor pos)))))

    # Context or added line — open working copy
    (let [full-path (string root "/" file-path)
          target-line (- new-line 1)]
      (def opened (editor/open-file full-path (editor/get-state)))
      (when (and opened (> target-line 0))
        (when-let [pos (buf/line-byte-offset opened target-line)]
          (set-cursor pos))))))

(command/defcmd git-visit
  "Visit the thing at point (file, hunk line, or commit)."
  :label "Visit"
  []
  (def b (buffer))
  (def line (cursor-line))
  (def section (sec/section-at-line b line))
  (unless section (break))

  (case (section :type)
    :file
    (let [path (get-in section [:data :path])
          root (get-in b [:locals :git-root])]
      (when (and path root)
        (editor/open-file (string root "/" path) (editor/get-state))))

    :hunk
    (visit-hunk-line b line section)

    :commit
    (let [hash (get-in section [:data :hash])]
      (when hash (show-commit-diff hash)))

    nil))

(command/defcmd git-refresh
  "Refresh the current git buffer."
  :label "Refresh"
  []
  (def b (buffer))
  (do-status-refresh b))

# --- Status buffer creation ---

(defn- get-or-create-status-buffer [root]
  (or (status-bufs root)
      (let [basename (last (string/split "/" root))
            name (string "*git-status: " basename "*")
            b (buf/new name)]
        (mode/activate-major b status-mode)
        (put b :readonly true)
        (put b :hide-gutter true)
        (undo/init b)
        (put-in b [:locals :git-root] root)
        (put-in b [:locals :project-root] root)
        (put-in b [:locals :default-directory] root)
        (put status-bufs root b)
        b)))

# ════════════════════════════════════════════════════════════════════
# Shared buffer commands (defined early so commit/log code can use them)
# ════════════════════════════════════════════════════════════════════

(command/defcmd git-quit
  "Quit the current git buffer."
  :label "Quit"
  []
  (def state (editor/get-state))
  (def p (pane))
  (when p
    (db/quit-window p state)))

(command/defcmd git-show-process-buffer
  "Show the git process output buffer."
  :label "Show Process Buffer"
  []
  (def b (git/get-process-buffer))
  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :split-below]))

# ════════════════════════════════════════════════════════════════════
# Commit
# ════════════════════════════════════════════════════════════════════

(var- commit-buf nil)

(def commit-keymap (keymap/new))

(def commit-mode
  @{:name "git-commit"
    :display-name "Git Commit"
    :keymap commit-keymap})

(mode/register commit-mode)

(command/defcmd git-finish-commit
  "Finalize the commit with the current buffer content."
  :label "Finish Commit" []
  (use-buffer-root)
  (def b (buffer))
  (def text (buf/slice b 0 (buf/length b)))
  # Strip comment lines
  (def msg-lines
    (filter |(not (string/has-prefix? "#" $))
            (string/split "\n" text)))
  (def msg (string/trim (string/join msg-lines "\n")))
  (when (= msg "")
    (editor-message "Aborting commit due to empty message.")
    (break))

  # Get transient args for commit flags
  (def args @["commit" "-m" msg])
  (def targs (or (dyn :transient-args) @{}))
  (when (or (targs "--amend") (get-in b [:locals :amend]))
    (array/push args "--amend"))
  (when (targs "--all") (array/push args "--all"))
  (when (targs "--no-verify") (array/push args "--no-verify"))
  (when (targs "--signoff") (array/push args "--signoff"))
  (when (targs "--allow-empty") (array/push args "--allow-empty"))

  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do
      (editor-message "Committed.")
      (set commit-buf nil)
      (git-quit)
      (hook/fire :git-commit-finished msg)
      (hook/fire :git-post-operation :commit args 0)
      (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Commit failed: " (result :stderr)))))

(command/defcmd git-cancel-commit
  "Cancel the current commit."
  :label "Cancel Commit" []
  (editor-message "Commit cancelled.")
  (set commit-buf nil)
  (git-quit))

(keymap/bind commit-keymap "C-c C-c" git-finish-commit)
(keymap/bind commit-keymap "C-c C-k" git-cancel-commit)

(defn- open-commit-buffer [&opt amend]
  (def root (dyn :git-root))
  (def b (buf/new "*git-commit*"))
  (set commit-buf b)
  (mode/activate-major b commit-mode)
  (put-in b [:locals :git-root] root)
  (put-in b [:locals :project-root] root)
  (put-in b [:locals :default-directory] root)

  # Pre-populate with amend message if amending
  (when amend
    (put-in b [:locals :amend] true)
    (def result (git/run "log" "-1" "--format=%B"))
    (when (= (result :exit) 0)
      (buf/insert b 0 (string/trim (result :stdout)))))

  # Add comment block with status info
  (def status-result (git/run "status"))
  (def comment-text
    (string "\n\n"
            "# Please enter the commit message. Lines starting with '#' are ignored.\n"
            "#\n"
            (string/join
              (map |(string "# " $)
                   (string/split "\n" (string/trim (or (status-result :stdout) ""))))
              "\n")
            "\n"))
  (buf/insert b (buf/length b) comment-text)

  # Place cursor at start
  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :same]))

# ════════════════════════════════════════════════════════════════════
# Log mode
# ════════════════════════════════════════════════════════════════════

(var- log-buf nil)

(def log-keymap (keymap/new))

# --- PEG highlight helper (same pattern as core peg-highlight/hl) ---

(defn- hl
  "Create a PEG pattern that captures a face region."
  [face-name pattern]
  ~(cmt (* ($) ,pattern ($))
        ,(fn [start end]
           {:name face-name :start start :end end})))

# --- Git log PEG grammar ---
#
# Log lines look like:
#   * abc1234 2 hours ago  Fix buffer overflow (HEAD -> main, origin/main)
# Or with graph:
#   | * abc1234 2 hours ago  Subject
#   |/
#   * def5678 ...

(def git-log-peg
  (peg/compile
    ~{:main (any (+ :commit-line :graph-only-line (thru "\n")))
      # A commit line: optional graph, hash, date, subject, optional refs
      :commit-line (* (? ,(hl "git-graph" ~(some (+ "* " "| " "|/" "|\\" "/|"
                                                     "| " "\\ " "/ " "|" "/" "\\"))))
                      ,(hl "git-hash" ~(some (range "09" "af")))
                      " "
                      ,(hl "git-date" ~(to "  "))
                      "  "
                      ,(hl "git-subject" ~(to (+ " (" "\n")))
                      (? (* " " ,(hl "git-refs" ~(* "(" (to ")") ")"))))
                      "\n")
      # Graph-only lines (no commit on this line)
      :graph-only-line (* ,(hl "git-graph" ~(some (+ "| " "|/" "|\\" "/|"
                                                      "\\ " "/ " "|" "/" "\\")))
                          "\n")}))

# --- Git diff PEG grammar ---

(def git-diff-peg
  (peg/compile
    ~{:main (any (+ :file-header :hunk-header :add-line :remove-line
                    :meta-line (thru "\n")))
      :file-header (* ,(hl "git-diff-file-header"
                           ~(+ (* "diff --git " (to "\n"))
                               (* "--- " (to "\n"))
                               (* "+++ " (to "\n"))))
                      "\n")
      :hunk-header (* ,(hl "git-diff-hunk-header" ~(* "@@ " (to " @@") " @@" (to "\n")))
                      "\n")
      :add-line (* ,(hl "git-diff-add" ~(* "+" (to "\n"))) "\n")
      :remove-line (* ,(hl "git-diff-remove" ~(* "-" (to "\n"))) "\n")
      :meta-line (* ,(hl "git-hash"
                         ~(+ (* "commit " (to "\n"))
                             (* "Author: " (to "\n"))
                             (* "Date: " (to "\n"))
                             (* "index " (to "\n"))))
                    "\n")}))

(def log-mode
  @{:name "git-log"
    :display-name "Git Log"
    :keymap log-keymap
    :grammar {:type :peg :highlights git-log-peg}})

(mode/register log-mode)

(set show-commit-diff
  (fn [hash]
  (use-buffer-root)
  (def root (dyn :git-root))
  (def result (git/run "show" "--format=commit %H%nAuthor: %an <%ae>%nDate:   %ad%n%n%s%n%n%b" hash))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer
             (string "*git-diff " hash "*")
             (result :stdout)))
    (mode/activate-major b diff-mode)
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] root)
    (put-in b [:locals :project-root] root)
    (put-in b [:locals :default-directory] root)
    (put-in b [:locals :git-revision] hash)
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :split-right])
    (apply-diff-overlays b))))

(command/defcmd git-log-visit
  "Show the commit at point in a diff buffer."
  :label "Visit Commit"
  []
  (def b (buffer))
  (def line (cursor-line))
  (def section (sec/section-at-line b line))
  (when (and section (= (section :type) :commit))
    (show-commit-diff (get-in section [:data :hash]))))

(def- hash-extract-peg
  "PEG to extract the first hex hash from a log line (skipping graph chars)."
  (peg/compile ~(* (any (+ "* " "| " "|/" "|\\" "/|" "\\ " "/ " "|" "/" "\\"))
                   (capture (some (range "09" "af"))))))

(defn- open-log-buffer [&opt args-table]
  # Use a display-friendly format: hash date  subject (refs)
  # Always include --graph for visual branch structure
  (def targs (or args-table (dyn :transient-args) @{}))
  (def use-graph (not (targs "--no-graph")))
  (def args @["log"
              (string "--format=%h %ar  %s%d")
              (string "-" (string log-max-count))])
  (when use-graph (array/push args "--graph"))

  # Apply transient args and options
  (when (targs "--all") (array/push args "--all"))
  (when (targs "--author=")
    (array/push args (string "--author=" (targs "--author="))))
  (when (targs "--since=")
    (array/push args (string "--since=" (targs "--since="))))
  (when (targs "--grep=")
    (array/push args (string "--grep=" (targs "--grep="))))
  (when (targs "--max-count=")
    (array/push args (string "-" (targs "--max-count="))))
  # Branch or file filtering
  (when (targs :branch)
    (array/push args (targs :branch)))
  (when (targs :file)
    (array/push args "--" (targs :file)))

  (def result (git/run ;args))
  (unless (= (result :exit) 0)
    (editor-message (string "git log failed: " (result :stderr)))
    (break))

  (def output (string/trim (result :stdout)))
  (when (= output "")
    (editor-message "No commits found.")
    (break))

  (def b (buf/new "*git-log*"))
  (set log-buf b)
  (mode/activate-major b log-mode)
  (put b :hide-gutter true)
  (put-in b [:locals :git-root] (dyn :git-root))
  (put-in b [:locals :project-root] (dyn :git-root))
  (put-in b [:locals :default-directory] (dyn :git-root))

  # Insert raw git log output — PEG grammar handles highlighting
  (put b :readonly false)
  (buf/insert b 0 output)
  (put b :readonly true)
  (put b :modified false)

  # Build sections: one per line that contains a commit hash
  (def sections @[])
  (def lines (string/split "\n" output))
  (for i 0 (length lines)
    (def line (get lines i))
    (def m (peg/match hash-extract-peg line))
    (when m
      (array/push sections
        (sec/make-section :commit i i
                          :data @{:hash (get m 0)}
                          :face :git-hash))))
  (sec/build-section-tree b sections)

  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :same]))

# ════════════════════════════════════════════════════════════════════
# Diff mode
# ════════════════════════════════════════════════════════════════════

(def diff-keymap (keymap/new))


(set diff-mode
  @{:name "git-diff"
    :display-name "Git Diff"
    :keymap diff-keymap
    :grammar {:type :peg :highlights git-diff-peg}})

(mode/register diff-mode)

# Git buffers are special-purpose views, not text editing buffers.
# Exclude them from vim-mode so git keybindings (like the y prefix)
# aren't shadowed by vim operators.
(package/after-load "jax/vim-mode"
  (def vim (require "jax/vim-mode"))
  (def exclude (get-in vim ['exclude-mode :value]))
  (when exclude
    (exclude "git-status")))


(set apply-diff-overlays
  (fn [b]
    # Diff mode uses PEG grammar for highlighting now.
    # This function is kept for buffers that set diff-mode after content
    # is already inserted — just ensure the mode is set so PEG kicks in.
    nil))

(defn- open-diff-buffer [&opt diff-args]
  (def args @["diff"])
  (def targs (or diff-args (dyn :transient-args) @{}))
  (when (targs "--cached") (array/push args "--cached"))
  (when (targs "--ignore-whitespace") (array/push args "-w"))
  (when (targs "--stat") (array/push args "--stat"))

  (def result (git/run ;args))
  (unless (= (result :exit) 0)
    (editor-message (string "git diff failed: " (result :stderr)))
    (break))

  (when (= (string/trim (result :stdout)) "")
    (editor-message "No differences.")
    (break))

  (def b (editor/make-view-buffer "*git-diff*" (result :stdout)))
  (mode/activate-major b diff-mode)
  (put b :hide-gutter true)
  (put-in b [:locals :git-root] (dyn :git-root))
  (put-in b [:locals :project-root] (dyn :git-root))
  (put-in b [:locals :default-directory] (dyn :git-root))
  (apply-diff-overlays b)
  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :same]))

# ════════════════════════════════════════════════════════════════════
# Transient menus
# ════════════════════════════════════════════════════════════════════

# --- Commit transient ---

(command/defcmd git-commit-create
  "Create a new commit."
  :label "Commit"
  []
  (use-buffer-root) (open-commit-buffer))

(command/defcmd git-commit-amend
  "Amend the last commit."
  :label "Amend"
  []
  (use-buffer-root) (open-commit-buffer true))

(command/defcmd git-commit-extend
  "Extend the last commit with staged changes (no message edit)."
  :label "Extend"
  []
  (use-buffer-root)
  (def result (git/run "commit" "--amend" "--no-edit"))
  (if (= (result :exit) 0)
    (do (editor-message "Commit extended.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Extend failed: " (result :stderr)))))

(command/defcmd git-commit-reword
  "Reword the last commit message."
  :label "Reword"
  []
  (use-buffer-root)
  (def root (dyn :git-root))
  (def result (git/run "log" "-1" "--format=%B"))
  (when (= (result :exit) 0)
    (def b (buf/new "*git-commit*"))
    (set commit-buf b)
    (mode/activate-major b commit-mode)
    (put-in b [:locals :git-root] root)
    (put-in b [:locals :project-root] root)
    (put-in b [:locals :default-directory] root)
    (put-in b [:locals :amend] true)
    (buf/insert b 0 (string/trim (result :stdout)))
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :same])))

(command/defcmd git-commit-fixup
  "Create a fixup commit for a given commit."
  :label "Fixup"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Fixup commit: "
     :on-submit
     (fn [hash]
       (def result (git/run "commit" "--fixup" hash))
       (if (= (result :exit) 0)
         (do (editor-message "Fixup commit created.")
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Fixup failed: " (result :stderr)))))}))

(command/defcmd git-commit-instant-fixup
  "Create a fixup commit and immediately rebase."
  :label "Instant Fixup"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Instant fixup commit: "
     :on-submit
     (fn [hash]
       (def root (dyn :git-root))
       (def result (git/run "commit" "--fixup" hash))
       (when (= (result :exit) 0)
         (ev/spawn
           (setdyn :git-root root)
           (setdyn :git-no-editor true)
           (def rb (git/run "rebase" "-i" "--autosquash" (string hash "~1")))
           (if (= (rb :exit) 0)
             (do (editor-message "Fixup applied.")
                 (when status-buf (do-status-refresh status-buf)))
             (editor-message (string "Rebase failed: " (rb :stderr)))))))}))

(command/defcmd git-commit-squash
  "Create a squash commit for a given commit."
  :label "Squash"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Squash commit: "
     :on-submit
     (fn [hash]
       (def result (git/run "commit" "--squash" hash))
       (if (= (result :exit) 0)
         (do (editor-message "Squash commit created.")
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Squash failed: " (result :stderr)))))}))

(transient/define :git-commit
  :description "Commit"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-a" :switch "--all" :description "Stage all modified"}
      @{:key "-e" :switch "--allow-empty" :description "Allow empty commit" :level 5}
      @{:key "-v" :switch "--verbose" :description "Show diff in message buffer" :level 5}
      @{:key "-n" :switch "--no-verify" :description "Skip hooks"}
      @{:key "-s" :switch "--signoff" :description "Add Signed-off-by" :level 5}
      @{:key "-R" :switch "--reset-author" :description "Reset author" :level 5}]}
   @{:name "Create"
     :suffixes
     [@{:key "c" :command git-commit-create :description "Commit"}
      @{:key "e" :command git-commit-extend :description "Extend"}
      @{:key "w" :command git-commit-reword :description "Reword"}
      @{:key "a" :command git-commit-amend :description "Amend"}
      @{:key "f" :command git-commit-fixup :description "Fixup"}
      @{:key "F" :command git-commit-instant-fixup :description "Instant fixup" :level 5}
      @{:key "s" :command git-commit-squash :description "Squash" :level 5}]}])

# --- Branch transient ---

(command/defcmd git-branch-checkout
  "Checkout a branch."
  :label "Checkout Branch"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "-a" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Checkout branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (def result (git/run "checkout" (candidate :text)))
       (if (= (result :exit) 0)
         (do (editor-message (string "Switched to " (candidate :text)))
             (hook/fire :git-post-operation :checkout [(candidate :text)] 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Checkout failed: " (result :stderr)))))}))

(command/defcmd git-branch-create-and-checkout
  "Create a new branch and check it out."
  :label "Create & Checkout"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Create and checkout branch: "
     :on-submit
     (fn [name]
       (def result (git/run "checkout" "-b" name))
       (if (= (result :exit) 0)
         (do (editor-message (string "Created and switched to " name))
             (hook/fire :git-post-operation :checkout [name] 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Failed: " (result :stderr)))))}))

(command/defcmd git-branch-create
  "Create a new branch."
  :label "Create Branch"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Create branch: "
     :on-submit
     (fn [name]
       (def result (git/run "branch" name))
       (if (= (result :exit) 0)
         (editor-message (string "Created branch " name))
         (editor-message (string "Failed: " (result :stderr)))))}))

(command/defcmd git-branch-rename
  "Rename a branch."
  :label "Rename Branch"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Rename branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (prompt/activate
         {:prompt (string "Rename " (candidate :text) " to: ")
          :on-submit
          (fn [new-name]
            (def result (git/run "branch" "-m" (candidate :text) new-name))
            (if (= (result :exit) 0)
              (do (editor-message (string "Renamed to " new-name))
                  (when status-buf (do-status-refresh status-buf)))
              (editor-message (string "Failed: " (result :stderr)))))}))}))

(command/defcmd git-branch-delete
  "Delete a branch."
  :label "Delete Branch"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Delete branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (prompt/activate
         {:prompt (string "Delete " (candidate :text) "? (y/n) ")
          :on-submit
          (fn [input]
            (when (= (string/ascii-lower (string/trim input)) "y")
              (def result (git/run "branch" "-d" (candidate :text)))
              (if (= (result :exit) 0)
                (do (editor-message (string "Deleted " (candidate :text)))
                    (when status-buf (do-status-refresh status-buf)))
                (editor-message (string "Failed: " (result :stderr)
                                       " (use -D to force)")))))}))}))

(command/defcmd git-branch-spinoff
  "Create a new branch from the current branch and reset current."
  :label "Spinoff"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Spinoff branch name: "
     :on-submit
     (fn [name]
       (def branch (git/current-branch))
       (unless branch
         (editor-message "Cannot spinoff: not on a branch.")
         (break))
       (def upstream (git/upstream-ref))
       (def result (git/run "checkout" "-b" name))
       (when (= (result :exit) 0)
         (when upstream
           (def parts (string/split "/" upstream))
           (when (>= (length parts) 2)
             (def remote (first parts))
             (def remote-branch (string/join (slice parts 1) "/"))
             (git/run "branch" "--force" branch (string remote "/" remote-branch))))
         (editor-message (string "Spun off " name " from " branch))
         (when status-buf (do-status-refresh status-buf))))}))

(command/defcmd git-branch-reset
  "Reset a branch to a revision."
  :label "Reset Branch"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Reset branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (prompt/activate
         {:prompt (string "Reset " (candidate :text) " to: ")
          :on-submit
          (fn [rev]
            (def result (git/run "branch" "--force" (candidate :text) rev))
            (if (= (result :exit) 0)
              (do (editor-message (string "Reset " (candidate :text) " to " rev))
                  (when status-buf (do-status-refresh status-buf)))
              (editor-message (string "Failed: " (result :stderr)))))}))}))

(transient/define :git-branch
  :description "Branch"
  :groups
  [@{:name "Checkout"
     :suffixes
     [@{:key "b" :command git-branch-checkout :description "Checkout"}
      @{:key "c" :command git-branch-create-and-checkout :description "Create & checkout"}
      @{:key "n" :command git-branch-create :description "Create"}
      @{:key "x" :command git-branch-spinoff :description "Spinoff"}]}
   @{:name "Do"
     :suffixes
     [@{:key "r" :command git-branch-rename :description "Rename"}
      @{:key "k" :command git-branch-delete :description "Delete"}
      @{:key "X" :command git-branch-reset :description "Reset" :level 5}]}])

# --- Push transient ---

(defn- push-build-args
  "Build git push args from transient infix state."
  [targs]
  (def args @["push"])
  (when (targs "--force-with-lease") (array/push args "--force-with-lease"))
  (when (targs "--force") (array/push args "--force"))
  (when (targs "--no-verify") (array/push args "--no-verify"))
  (when (targs "--set-upstream") (array/push args "--set-upstream"))
  (when (targs "--dry-run") (array/push args "--dry-run"))
  args)

(defn- push-run-async
  "Run a push command asynchronously, refresh status on success."
  [args desc]
  (def root (dyn :git-root))
  (editor-message (string desc "..."))
  (ev/spawn
    (setdyn :git-root root)
    (def result (git/run ;args))
    (if (= (result :exit) 0)
      (do (editor-message (string desc " done."))
          (hook/fire :git-post-operation :push args 0)
          (when status-buf (do-status-refresh status-buf)))
      (editor-message (string desc " failed: " (result :stderr))))))

(command/defcmd git-push-pushremote
  "Push to the push remote."
  :label "Push to pushremote"
  []
  (use-buffer-root)
  (def args (push-build-args (or (dyn :transient-args) @{})))
  (def ref (git/push-remote-ref))
  (when ref
    (def parts (string/split "/" ref))
    (when (>= (length parts) 2)
      (array/push args (first parts))))
  (push-run-async args "Pushing to pushremote"))

(command/defcmd git-push-upstream
  "Push to the upstream remote."
  :label "Push to upstream"
  []
  (use-buffer-root)
  (def args (push-build-args (or (dyn :transient-args) @{})))
  (def ref (git/upstream-ref))
  (when ref
    (def parts (string/split "/" ref))
    (when (>= (length parts) 2)
      (array/push args (first parts))))
  (push-run-async args "Pushing to upstream"))

(command/defcmd git-push-elsewhere
  "Push to elsewhere (prompt for remote)."
  :label "Push elsewhere"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Push to remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (use-buffer-root)
       (def args (push-build-args (or (dyn :transient-args) @{})))
       (array/push args (candidate :text))
       (push-run-async args (string "Pushing to " (candidate :text))))}))

(command/defcmd git-push-tag
  "Push a tag to a remote."
  :label "Push tag"
  []
  (use-buffer-root)
  (def tags (git/run-lines "tag" "--sort=-creatordate"))
  (prompt/pick
    {:prompt "Push tag: "
     :candidates (map |(do @{:text $}) tags)
     :on-accept
     (fn [tag-candidate]
       (def remotes (git/run-lines "remote"))
       (prompt/pick
         {:prompt "Push tag to remote: "
          :candidates (map |(do @{:text $}) remotes)
          :on-accept
          (fn [remote-candidate]
            (use-buffer-root)
            (def args (push-build-args (or (dyn :transient-args) @{})))
            (array/push args (remote-candidate :text))
            (array/push args (tag-candidate :text))
            (push-run-async args
              (string "Pushing tag " (tag-candidate :text))))}))}))

(command/defcmd git-push-all-tags
  "Push all tags to a remote."
  :label "Push all tags"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Push all tags to remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (use-buffer-root)
       (def args (push-build-args (or (dyn :transient-args) @{})))
       (array/push args (candidate :text))
       (array/push args "--tags")
       (push-run-async args
         (string "Pushing all tags to " (candidate :text))))}))

(transient/define :git-push
  :description "Push"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-f" :switch "--force-with-lease" :description "Force with lease"}
      @{:key "-F" :switch "--force" :description "Force" :level 5}
      @{:key "-n" :switch "--no-verify" :description "Skip hooks" :level 5}
      @{:key "-u" :switch "--set-upstream" :description "Set upstream"}
      @{:key "-h" :switch "--dry-run" :description "Dry run"}]}
   @{:name (fn []
             (def branch (git/current-branch))
             (if branch
               (string "Push " branch " to")
               "Push to"))
     :suffixes
     [@{:key "p" :command git-push-pushremote
        :description (fn []
                       (or (git/push-remote-ref) "pushremote (unset)"))}
      @{:key "u" :command git-push-upstream
        :description (fn []
                       (or (git/upstream-ref) "upstream (unset)"))}
      @{:key "e" :command git-push-elsewhere
        :description "elsewhere"}]}
   @{:name "Push"
     :suffixes
     [@{:key "T" :command git-push-tag :description "Push a tag"}
      @{:key "t" :command git-push-all-tags :description "Push all tags"}]}])

# --- Pull transient ---

(defn- pull-build-args
  "Build git pull args from transient infix state."
  [targs]
  (def args @["pull"])
  (when (targs "--rebase") (array/push args "--rebase"))
  (when (targs "--no-rebase") (array/push args "--no-rebase"))
  (when (targs "--autostash") (array/push args "--autostash"))
  (when (targs "--ff-only") (array/push args "--ff-only"))
  (when (targs "--no-ff") (array/push args "--no-ff"))
  args)

(defn- pull-run-async
  "Run a pull command asynchronously, refresh status on success."
  [args desc]
  (def root (dyn :git-root))
  (editor-message (string desc "..."))
  (ev/spawn
    (setdyn :git-root root)
    (setdyn :git-no-editor true)
    (def result (git/run ;args))
    (if (= (result :exit) 0)
      (do (editor-message (string desc " done."))
          (hook/fire :git-post-operation :pull args 0)
          (when status-buf (do-status-refresh status-buf)))
      (editor-message (string desc " failed: " (result :stderr))))))

(command/defcmd git-pull-pushremote
  "Pull from the push remote."
  :label "Pull from pushremote"
  []
  (use-buffer-root)
  (def args (pull-build-args (or (dyn :transient-args) @{})))
  (def ref (git/push-remote-ref))
  (when ref
    (def parts (string/split "/" ref))
    (when (>= (length parts) 2)
      (array/push args (first parts))
      (array/push args (string/join (slice parts 1) "/"))))
  (pull-run-async args "Pulling from pushremote"))

(command/defcmd git-pull-upstream
  "Pull from upstream."
  :label "Pull from upstream"
  []
  (use-buffer-root)
  (def args (pull-build-args (or (dyn :transient-args) @{})))
  (pull-run-async args "Pulling from upstream"))

(command/defcmd git-pull-elsewhere
  "Pull from elsewhere (prompt for remote and branch)."
  :label "Pull from elsewhere"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Pull from remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (prompt/activate
         {:prompt (string "Branch on " (candidate :text) ": ")
          :on-submit
          (fn [branch]
            (use-buffer-root)
            (def args (pull-build-args (or (dyn :transient-args) @{})))
            (array/push args (candidate :text))
            (array/push args branch)
            (pull-run-async args
              (string "Pulling from " (candidate :text) "/" branch)))}))}))

(transient/define :git-pull
  :description "Pull"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-f" :switch "--ff-only" :description "Fast-forward only"}
      @{:key "-r" :switch "--rebase" :description "Rebase"}
      @{:key "-A" :switch "--autostash" :description "Autostash"}
      @{:key "-n" :switch "--no-ff" :description "No fast-forward" :level 5}
      @{:key "-N" :switch "--no-rebase" :description "No rebase" :level 5}]}
   @{:name (fn []
             (def branch (git/current-branch))
             (if branch
               (string "Pull into " branch " from")
               "Pull from"))
     :suffixes
     [@{:key "p" :command git-pull-pushremote
        :description (fn []
                       (or (git/push-remote-ref) "pushremote (unset)"))}
      @{:key "u" :command git-pull-upstream
        :description (fn []
                       (or (git/upstream-ref) "upstream (unset)"))}
      @{:key "e" :command git-pull-elsewhere
        :description "elsewhere"}]}])

# --- Fetch transient ---

(defn- fetch-build-args
  "Build git fetch args from transient infix state."
  [targs]
  (def args @["fetch"])
  (when (targs "--prune") (array/push args "--prune"))
  (when (targs "--tags") (array/push args "--tags"))
  (when (targs "--verbose") (array/push args "--verbose"))
  args)

(defn- fetch-run-async
  "Run a fetch command asynchronously, refresh status on success."
  [args desc]
  (def root (dyn :git-root))
  (editor-message (string desc "..."))
  (ev/spawn
    (setdyn :git-root root)
    (def result (git/run ;args))
    (if (= (result :exit) 0)
      (do (editor-message (string desc " done."))
          (hook/fire :git-post-operation :fetch args 0)
          (when status-buf (do-status-refresh status-buf)))
      (editor-message (string desc " failed: " (result :stderr))))))

(command/defcmd git-fetch-pushremote
  "Fetch from the push remote."
  :label "Fetch from pushremote"
  []
  (use-buffer-root)
  (def args (fetch-build-args (or (dyn :transient-args) @{})))
  (def ref (git/push-remote-ref))
  (when ref
    (def parts (string/split "/" ref))
    (when (>= (length parts) 2)
      (array/push args (first parts))))
  (fetch-run-async args "Fetching from pushremote"))

(command/defcmd git-fetch-upstream
  "Fetch from upstream."
  :label "Fetch from upstream"
  []
  (use-buffer-root)
  (def args (fetch-build-args (or (dyn :transient-args) @{})))
  (def ref (git/upstream-ref))
  (when ref
    (def parts (string/split "/" ref))
    (when (>= (length parts) 2)
      (array/push args (first parts))))
  (fetch-run-async args "Fetching from upstream"))

(command/defcmd git-fetch-elsewhere
  "Fetch from elsewhere (prompt for remote)."
  :label "Fetch from elsewhere"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Fetch from remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (use-buffer-root)
       (def args (fetch-build-args (or (dyn :transient-args) @{})))
       (array/push args (candidate :text))
       (fetch-run-async args (string "Fetching from " (candidate :text))))}))

(command/defcmd git-fetch-all
  "Fetch from all remotes."
  :label "Fetch all remotes"
  []
  (use-buffer-root)
  (def args (fetch-build-args (or (dyn :transient-args) @{})))
  (array/push args "--all")
  (fetch-run-async args "Fetching all remotes"))

(transient/define :git-fetch
  :description "Fetch"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-p" :switch "--prune" :description "Prune deleted branches"}
      @{:key "-t" :switch "--tags" :description "Fetch tags"}
      @{:key "-v" :switch "--verbose" :description "Verbose" :level 5}]}
   @{:name "Fetch from"
     :suffixes
     [@{:key "p" :command git-fetch-pushremote
        :description (fn []
                       (or (git/push-remote-ref) "pushremote (unset)"))}
      @{:key "u" :command git-fetch-upstream
        :description (fn []
                       (or (git/upstream-ref) "upstream (unset)"))}
      @{:key "e" :command git-fetch-elsewhere
        :description "elsewhere"}
      @{:key "a" :command git-fetch-all
        :description "all remotes"}]}])

# --- Stash transient ---

(defn- stash-build-args
  "Build common stash push args from transient infix state."
  [targs]
  (def args @["stash" "push"])
  (when (targs "--include-untracked") (array/push args "--include-untracked"))
  (when (targs "--all") (array/push args "--all"))
  (when (targs "--keep-index") (array/push args "--keep-index"))
  args)

(command/defcmd git-stash-both
  "Stash both worktree and index changes."
  :label "Stash"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Stash message (optional): "
     :on-submit
     (fn [msg]
       (def args (stash-build-args (or (dyn :transient-args) @{})))
       (when (> (length (string/trim msg)) 0)
         (array/push args "-m" msg))
       (def result (git/run ;args))
       (if (= (result :exit) 0)
         (do (editor-message "Stashed.")
             (hook/fire :git-post-operation :stash args 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Stash failed: " (result :stderr)))))}))

(command/defcmd git-stash-index
  "Stash only index (staged) changes."
  :label "Stash index"
  []
  (use-buffer-root)
  (def result (git/run "stash" "push" "--staged"))
  (if (= (result :exit) 0)
    (do (editor-message "Stashed index.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Stash failed: " (result :stderr)))))

(command/defcmd git-stash-worktree
  "Stash only worktree changes (keep index)."
  :label "Stash worktree"
  []
  (use-buffer-root)
  (def args (stash-build-args (or (dyn :transient-args) @{})))
  (array/push args "--keep-index")
  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do (editor-message "Stashed worktree.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Stash failed: " (result :stderr)))))

(command/defcmd git-stash-pop
  "Pop the top stash."
  :label "Stash Pop"
  []
  (use-buffer-root)
  (def result (git/run "stash" "pop"))
  (if (= (result :exit) 0)
    (do (editor-message "Stash popped.")
        (hook/fire :git-post-operation :stash-pop [] 0)
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Stash pop failed: " (result :stderr)))))

(command/defcmd git-stash-apply
  "Apply the top stash without removing it."
  :label "Stash Apply"
  []
  (use-buffer-root)
  (def result (git/run "stash" "apply"))
  (if (= (result :exit) 0)
    (do (editor-message "Stash applied.")
        (hook/fire :git-post-operation :stash-apply [] 0)
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Stash apply failed: " (result :stderr)))))

(command/defcmd git-stash-drop
  "Drop the top stash."
  :label "Stash Drop"
  []
  (use-buffer-root)
  (def result (git/run "stash" "drop"))
  (if (= (result :exit) 0)
    (do (editor-message "Stash dropped.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Stash drop failed: " (result :stderr)))))

(command/defcmd git-stash-show
  "Show the top stash diff."
  :label "Show stash"
  []
  (use-buffer-root)
  (def result (git/run "stash" "show" "-p"))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer "*git-stash*" (result :stdout)))
    (mode/activate-major b diff-mode)
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] (dyn :git-root))
    (put-in b [:locals :project-root] (dyn :git-root))
    (put-in b [:locals :default-directory] (dyn :git-root))
    (apply-diff-overlays b)
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :same])))

(command/defcmd git-stash-list-cmd
  "List all stashes."
  :label "List Stashes"
  []
  (use-buffer-root)
  (def result (git/run "stash" "list"))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer "*git-stash-list*" (result :stdout)))
    (put b :hide-gutter true)
    (put-in b [:locals :project-root] (dyn :git-root))
    (put-in b [:locals :default-directory] (dyn :git-root))
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :split-below])))

(transient/define :git-stash
  :description "Stash"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-u" :switch "--include-untracked" :description "Include untracked"}
      @{:key "-a" :switch "--all" :description "Include all (ignored too)" :level 5}
      @{:key "-k" :switch "--keep-index" :description "Keep index"}]}
   @{:name "Stash"
     :suffixes
     [@{:key "z" :command git-stash-both :description "Both"}
      @{:key "i" :command git-stash-index :description "Index"}
      @{:key "w" :command git-stash-worktree :description "Worktree"}]}
   @{:name "Use"
     :suffixes
     [@{:key "a" :command git-stash-apply :description "Apply"}
      @{:key "p" :command git-stash-pop :description "Pop"}
      @{:key "k" :command git-stash-drop :description "Drop"}
      @{:key "v" :command git-stash-show :description "Show"}
      @{:key "l" :command git-stash-list-cmd :description "List"}]}])

# --- Log transient ---

(command/defcmd git-log-current
  "Show log for current branch."
  :label "Log Current"
  []
  (use-buffer-root) (open-log-buffer))

(command/defcmd git-log-other
  "Show log for another branch."
  :label "Log Other"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "-a" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Log for branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept (fn [candidate]
                  (open-log-buffer @{:branch (candidate :text)}))}))

(command/defcmd git-log-file
  "Show log for current file."
  :label "Log File"
  []
  (use-buffer-root)
  (def b (buffer))
  (when (b :file)
    (open-log-buffer @{:file (b :file)})))

(command/defcmd git-log-reflog
  "Show reflog."
  :label "Reflog"
  []
  (use-buffer-root)
  (def result (git/run "reflog" "--format=%h%x00%gs%x00%ar%x00%an%x00"))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer "*git-reflog*" (result :stdout)))
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] (dyn :git-root))
    (put-in b [:locals :project-root] (dyn :git-root))
    (put-in b [:locals :default-directory] (dyn :git-root))
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :same])))

(transient/define :git-log
  :description "Log"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-n" :option "--max-count=" :description "Max count" :reader :string}
      @{:key "-a" :switch "--all" :description "All branches"}
      @{:key "-d" :switch "--decorate" :description "Decorate"}
      @{:key "-g" :switch "--graph" :description "Show graph" :level 5}
      @{:key "--author" :option "--author=" :description "Author" :reader :string :level 5}
      @{:key "--since" :option "--since=" :description "Since" :reader :string :level 5}
      @{:key "--grep" :option "--grep=" :description "Grep" :reader :string :level 5}]}
   @{:name "Log"
     :suffixes
     [@{:key "l" :command git-log-current :description "Current"}
      @{:key "o" :command git-log-other :description "Other branch"}
      @{:key "f" :command git-log-file :description "File"}
      @{:key "r" :command git-log-reflog :description "Reflog"}]}])

# --- Diff transient ---

(command/defcmd git-diff-dwim
  "Show diff (unstaged changes)."
  :label "Diff DWIM"
  []
  (use-buffer-root) (open-diff-buffer))

(command/defcmd git-diff-staged
  "Show staged diff."
  :label "Diff Staged"
  []
  (use-buffer-root) (open-diff-buffer @{"--cached" true}))

(command/defcmd git-diff-commit
  "Show diff for a specific commit."
  :label "Diff Commit"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Diff commit: "
     :on-submit
     (fn [hash]
       (def result (git/run "diff" hash))
       (when (= (result :exit) 0)
         (def b (editor/make-view-buffer
                  (string "*git-diff " hash "*")
                  (result :stdout)))
         (mode/activate-major b diff-mode)
         (put b :hide-gutter true)
         (put-in b [:locals :git-root] (dyn :git-root))
         (put-in b [:locals :project-root] (dyn :git-root))
         (put-in b [:locals :default-directory] (dyn :git-root))
         (apply-diff-overlays b)
         (db/pop-to-buffer b (editor/get-state)
                           :actions [:reuse :same])))}))

(command/defcmd git-diff-range
  "Show diff for a revision range."
  :label "Diff Range"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Diff range (e.g. main..HEAD): "
     :on-submit
     (fn [range]
       (def result (git/run "diff" range))
       (when (= (result :exit) 0)
         (def b (editor/make-view-buffer
                  (string "*git-diff " range "*")
                  (result :stdout)))
         (mode/activate-major b diff-mode)
         (put b :hide-gutter true)
         (put-in b [:locals :git-root] (dyn :git-root))
         (put-in b [:locals :project-root] (dyn :git-root))
         (put-in b [:locals :default-directory] (dyn :git-root))
         (apply-diff-overlays b)
         (db/pop-to-buffer b (editor/get-state)
                           :actions [:reuse :same])))}))

(command/defcmd git-diff-worktree
  "Show diff between HEAD and worktree."
  :label "Diff worktree"
  []
  (use-buffer-root)
  (def result (git/run "diff" "HEAD"))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer "*git-diff HEAD*" (result :stdout)))
    (mode/activate-major b diff-mode)
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] (dyn :git-root))
    (put-in b [:locals :project-root] (dyn :git-root))
    (put-in b [:locals :default-directory] (dyn :git-root))
    (apply-diff-overlays b)
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :same])))

(transient/define :git-diff
  :description "Diff"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-w" :switch "--ignore-whitespace" :description "Ignore whitespace"}
      @{:key "-s" :switch "--stat" :description "Show stat only" :level 5}]}
   @{:name "Diff"
     :suffixes
     [@{:key "d" :command git-diff-dwim :description "Unstaged"}
      @{:key "s" :command git-diff-staged :description "Staged"}
      @{:key "w" :command git-diff-worktree :description "Worktree (HEAD)"}
      @{:key "c" :command git-diff-commit :description "Show commit"}
      @{:key "r" :command git-diff-range :description "Range"}]}])

# --- Merge transient ---

(command/defcmd git-merge-commit
  "Merge a branch."
  :label "Merge"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "-a" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Merge branch: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (use-buffer-root)
       (def targs (or (dyn :transient-args) @{}))
       (def args @["merge"])
       (when (targs "--ff-only") (array/push args "--ff-only"))
       (when (targs "--no-ff") (array/push args "--no-ff"))
       (when (targs "--squash") (array/push args "--squash"))
       (when (targs "--no-commit") (array/push args "--no-commit"))
       (array/push args (candidate :text))
       (setdyn :git-no-editor true)
       (def result (git/run ;args))
       (if (= (result :exit) 0)
         (do (editor-message (string "Merged " (candidate :text)))
             (hook/fire :git-post-operation :merge args 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Merge failed: " (result :stderr)))))}))

(command/defcmd git-merge-abort
  "Abort the current merge."
  :label "Merge abort"
  []
  (use-buffer-root)
  (def result (git/run "merge" "--abort"))
  (if (= (result :exit) 0)
    (do (editor-message "Merge aborted.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Abort failed: " (result :stderr)))))

(transient/define :git-merge
  :description "Merge"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-f" :switch "--ff-only" :description "Fast-forward only"}
      @{:key "-n" :switch "--no-ff" :description "No fast-forward"}
      @{:key "-s" :switch "--squash" :description "Squash" :level 5}
      @{:key "-c" :switch "--no-commit" :description "No commit" :level 5}]}
   @{:name "Merge"
     :suffixes
     [@{:key "m" :command git-merge-commit :description "Merge"}
      @{:key "a" :command git-merge-abort :description "Abort"}]}])

# --- Rebase transient ---

(command/defcmd git-rebase-upstream
  "Rebase onto upstream."
  :label "Rebase onto upstream"
  []
  (use-buffer-root)
  (def targs (or (dyn :transient-args) @{}))
  (def args @["rebase"])
  (when (targs "--autostash") (array/push args "--autostash"))
  (when (targs "--interactive") (array/push args "--interactive"))
  (when (targs "--autosquash") (array/push args "--autosquash"))
  (def root (dyn :git-root))
  (editor/message "Rebasing...")
  (ev/spawn
    (try
      (do
        (setdyn :git-root root)
        (setdyn :git-no-editor true)
        (def result (git/run ;args))
        (if (= (result :exit) 0)
          (do (editor/message "Rebase done.")
              (hook/fire :git-post-operation :rebase args 0)
              (when status-buf (do-status-refresh status-buf)))
          (editor/message (string "Rebase failed: " (result :stderr)))))
      ([err]
        (editor/message (string "Rebase error: " err))))))

(command/defcmd git-rebase-onto
  "Rebase onto a specific branch or commit."
  :label "Rebase onto"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "-a" "--format=%(refname:short)"))
  (prompt/pick
    {:prompt "Rebase onto: "
     :candidates (map |(do @{:text $}) branches)
     :on-accept
     (fn [candidate]
       (use-buffer-root)
       (def targs (or (dyn :transient-args) @{}))
       (def args @["rebase"])
       (when (targs "--autostash") (array/push args "--autostash"))
       (when (targs "--interactive") (array/push args "--interactive"))
       (when (targs "--autosquash") (array/push args "--autosquash"))
       (array/push args (candidate :text))
       (def root (dyn :git-root))
       (editor/message (string "Rebasing onto " (candidate :text) "..."))
       (ev/spawn
         (try
           (do
             (setdyn :git-root root)
             (setdyn :git-no-editor true)
             (def result (git/run ;args))
             (if (= (result :exit) 0)
               (do (editor/message "Rebase done.")
                   (hook/fire :git-post-operation :rebase args 0)
                   (when status-buf (do-status-refresh status-buf)))
               (editor/message (string "Rebase failed: " (result :stderr)))))
           ([err]
             (editor/message (string "Rebase error: " err))))))}))

(command/defcmd git-rebase-interactive
  "Interactive rebase from a commit."
  :label "Interactive rebase"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Rebase interactively from: "
     :on-submit
     (fn [rev]
       (def root (dyn :git-root))
       (def targs (or (dyn :transient-args) @{}))
       (def args @["rebase" "--interactive"])
       (when (targs "--autostash") (array/push args "--autostash"))
       (when (targs "--autosquash") (array/push args "--autosquash"))
       (array/push args rev)
       (editor/message "Starting interactive rebase...")
       (ev/spawn
         (try
           (do
             (setdyn :git-root root)
             (setdyn :git-no-editor true)
             (def result (git/run ;args))
             (if (= (result :exit) 0)
               (do (editor/message "Rebase done.")
                   (when status-buf (do-status-refresh status-buf)))
               (editor/message (string "Rebase failed: " (result :stderr)))))
           ([err]
             (editor/message (string "Rebase error: " err))))))}))

(command/defcmd git-rebase-continue
  "Continue an in-progress rebase."
  :label "Rebase continue"
  []
  (use-buffer-root)
  (def root (or (dyn :git-root)
                (get-in (buffer) [:locals :git-root])
                (git/repo-root)))
  (unless root
    (editor/message "Cannot continue rebase: not in a git repository.")
    (break))
  (editor/message "Continuing rebase...")
  (ev/spawn
    (try
      (do
        (setdyn :git-root root)
        (setdyn :git-no-editor true)
        (def result (git/run "rebase" "--continue"))
        (if (= (result :exit) 0)
          (do (editor/message "Rebase continued.")
              (when status-buf (do-status-refresh status-buf)))
          (editor/message (string "Continue failed: " (result :stderr)))))
      ([err]
        (editor/message (string "Rebase error: " err))))))

(command/defcmd git-rebase-skip
  "Skip the current rebase step."
  :label "Rebase skip"
  []
  (use-buffer-root)
  (def root (or (dyn :git-root)
                (get-in (buffer) [:locals :git-root])
                (git/repo-root)))
  (unless root
    (editor/message "Cannot skip: not in a git repository.")
    (break))
  (editor/message "Skipping rebase step...")
  (ev/spawn
    (try
      (do
        (setdyn :git-root root)
        (setdyn :git-no-editor true)
        (def result (git/run "rebase" "--skip"))
        (if (= (result :exit) 0)
          (do (editor/message "Skipped.")
              (when status-buf (do-status-refresh status-buf)))
          (editor/message (string "Skip failed: " (result :stderr)))))
      ([err]
        (editor/message (string "Skip error: " err))))))

(command/defcmd git-rebase-abort
  "Abort the in-progress rebase."
  :label "Rebase abort"
  []
  (use-buffer-root)
  (def result (git/run "rebase" "--abort"))
  (if (= (result :exit) 0)
    (do (editor-message "Rebase aborted.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Abort failed: " (result :stderr)))))

(transient/define :git-rebase
  :description "Rebase"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-A" :switch "--autostash" :description "Autostash"}
      @{:key "-i" :switch "--interactive" :description "Interactive"}
      @{:key "-a" :switch "--autosquash" :description "Autosquash"}]}
   @{:name "Rebase"
     :suffixes
     [@{:key "u" :command git-rebase-upstream
        :description (fn []
                       (string "onto " (or (git/upstream-ref) "upstream")))}
      @{:key "o" :command git-rebase-onto :description "onto elsewhere"}
      @{:key "i" :command git-rebase-interactive :description "interactively"}]}
   @{:name "Actions"
     :suffixes
     [@{:key "r" :command git-rebase-continue :description "Continue"}
      @{:key "s" :command git-rebase-skip :description "Skip"}
      @{:key "a" :command git-rebase-abort :description "Abort"}]}])

# --- Cherry-pick transient ---

(command/defcmd git-cherry-pick-commit
  "Cherry-pick a commit."
  :label "Cherry-pick"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Cherry-pick commit: "
     :on-submit
     (fn [rev]
       (def targs (or (dyn :transient-args) @{}))
       (def args @["cherry-pick"])
       (when (targs "--no-commit") (array/push args "--no-commit"))
       (when (targs "--edit") (array/push args "--edit"))
       (array/push args rev)
       (def result (git/run ;args))
       (if (= (result :exit) 0)
         (do (editor-message (string "Cherry-picked " rev))
             (hook/fire :git-post-operation :cherry-pick args 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Cherry-pick failed: " (result :stderr)))))}))

(command/defcmd git-cherry-pick-continue
  "Continue an in-progress cherry-pick."
  :label "Cherry-pick continue"
  []
  (use-buffer-root)
  (setdyn :git-no-editor true)
  (def result (git/run "cherry-pick" "--continue"))
  (if (= (result :exit) 0)
    (do (editor-message "Cherry-pick continued.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Continue failed: " (result :stderr)))))

(command/defcmd git-cherry-pick-abort
  "Abort the in-progress cherry-pick."
  :label "Cherry-pick abort"
  []
  (use-buffer-root)
  (def result (git/run "cherry-pick" "--abort"))
  (if (= (result :exit) 0)
    (do (editor-message "Cherry-pick aborted.")
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Abort failed: " (result :stderr)))))

(transient/define :git-cherry-pick
  :description "Cherry-pick"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-n" :switch "--no-commit" :description "No commit"}
      @{:key "-e" :switch "--edit" :description "Edit message"}]}
   @{:name "Cherry-pick"
     :suffixes
     [@{:key "A" :command git-cherry-pick-commit :description "Pick"}]}
   @{:name "Actions"
     :suffixes
     [@{:key "r" :command git-cherry-pick-continue :description "Continue"}
      @{:key "a" :command git-cherry-pick-abort :description "Abort"}]}])

# --- Reset transient ---

(command/defcmd git-reset-soft
  "Soft reset (keep staging and worktree)."
  :label "Reset soft"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Soft reset to: "
     :on-submit
     (fn [rev]
       (def result (git/run "reset" "--soft" rev))
       (if (= (result :exit) 0)
         (do (editor-message (string "Soft reset to " rev))
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Reset failed: " (result :stderr)))))}))

(command/defcmd git-reset-mixed
  "Mixed reset (keep worktree, unstage)."
  :label "Reset mixed"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Mixed reset to: "
     :on-submit
     (fn [rev]
       (def result (git/run "reset" "--mixed" rev))
       (if (= (result :exit) 0)
         (do (editor-message (string "Mixed reset to " rev))
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Reset failed: " (result :stderr)))))}))

(command/defcmd git-reset-hard
  "Hard reset (discard staging and worktree)."
  :label "Reset hard"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Hard reset to (THIS DISCARDS CHANGES): "
     :on-submit
     (fn [rev]
       (def result (git/run "reset" "--hard" rev))
       (if (= (result :exit) 0)
         (do (editor-message (string "Hard reset to " rev))
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Reset failed: " (result :stderr)))))}))

(transient/define :git-reset
  :description "Reset"
  :groups
  [@{:name (fn []
             (def branch (git/current-branch))
             (if branch (string "Reset " branch " to") "Reset to"))
     :suffixes
     [@{:key "s" :command git-reset-soft :description "Soft (keep all changes staged)"}
      @{:key "m" :command git-reset-mixed :description "Mixed (keep worktree, unstage)"}
      @{:key "h" :command git-reset-hard :description "Hard (discard everything)"}]}])

# --- Tag transient ---

(command/defcmd git-tag-create
  "Create a tag."
  :label "Create tag"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Tag name: "
     :on-submit
     (fn [name]
       (prompt/activate
         {:prompt "Tag message (empty for lightweight): "
          :on-submit
          (fn [msg]
            (def args
              (if (> (length (string/trim msg)) 0)
                ["tag" "-a" name "-m" msg]
                ["tag" name]))
            (def result (git/run ;args))
            (if (= (result :exit) 0)
              (do (editor-message (string "Created tag " name))
                  (when status-buf (do-status-refresh status-buf)))
              (editor-message (string "Tag failed: " (result :stderr)))))}))}))

(command/defcmd git-tag-delete
  "Delete a tag."
  :label "Delete tag"
  []
  (use-buffer-root)
  (def tags (git/run-lines "tag" "--sort=-creatordate"))
  (prompt/pick
    {:prompt "Delete tag: "
     :candidates (map |(do @{:text $}) tags)
     :on-accept
     (fn [candidate]
       (def result (git/run "tag" "-d" (candidate :text)))
       (if (= (result :exit) 0)
         (do (editor-message (string "Deleted tag " (candidate :text)))
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Delete failed: " (result :stderr)))))}))

(command/defcmd git-tag-list
  "List tags."
  :label "List tags"
  []
  (use-buffer-root)
  (def result (git/run "tag" "-n1" "--sort=-creatordate"))
  (when (= (result :exit) 0)
    (def b (editor/make-view-buffer "*git-tags*" (result :stdout)))
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] (dyn :git-root))
    (put-in b [:locals :project-root] (dyn :git-root))
    (put-in b [:locals :default-directory] (dyn :git-root))
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :split-below])))

(transient/define :git-tag
  :description "Tag"
  :groups
  [@{:name "Tag"
     :suffixes
     [@{:key "t" :command git-tag-create :description "Create"}
      @{:key "k" :command git-tag-delete :description "Delete"}
      @{:key "l" :command git-tag-list :description "List"}]}])

# --- Git dispatch (top-level transient) ---

(command/defcmd git-commit-transient
  "Open commit transient."
  :label "Commit"
  [] (transient/activate :git-commit))

(command/defcmd git-branch-transient
  "Open branch transient."
  :label "Branch"
  [] (transient/activate :git-branch))

(command/defcmd git-push-transient
  "Open push transient."
  :label "Push"
  [] (transient/activate :git-push))

(command/defcmd git-pull-transient
  "Open pull transient."
  :label "Pull"
  [] (transient/activate :git-pull))

(command/defcmd git-fetch-transient
  "Open fetch transient."
  :label "Fetch"
  [] (transient/activate :git-fetch))

(command/defcmd git-log-transient
  "Open log transient."
  :label "Log"
  [] (transient/activate :git-log))

(command/defcmd git-diff-transient
  "Open diff transient."
  :label "Diff"
  [] (transient/activate :git-diff))

(command/defcmd git-stash-transient
  "Open stash transient."
  :label "Stash"
  [] (transient/activate :git-stash))

(command/defcmd git-merge-transient
  "Open merge transient."
  :label "Merge"
  [] (transient/activate :git-merge))

(command/defcmd git-rebase-transient
  "Open rebase transient."
  :label "Rebase"
  [] (transient/activate :git-rebase))

(command/defcmd git-cherry-pick-transient
  "Open cherry-pick transient."
  :label "Cherry-pick"
  [] (transient/activate :git-cherry-pick))

(command/defcmd git-reset-transient
  "Open reset transient."
  :label "Reset"
  [] (transient/activate :git-reset))

(command/defcmd git-tag-transient
  "Open tag transient."
  :label "Tag"
  [] (transient/activate :git-tag))

(transient/define :git-dispatch
  :description "Git"
  :groups
  [@{:name "Transient commands"
     :suffixes
     [@{:key "c" :command git-commit-transient :description "Commit..."
        :transient :git-commit}
      @{:key "b" :command git-branch-transient :description "Branch..."
        :transient :git-branch}
      @{:key "P" :command git-push-transient :description "Push..."
        :transient :git-push}
      @{:key "F" :command git-pull-transient :description "Pull..."
        :transient :git-pull}
      @{:key "f" :command git-fetch-transient :description "Fetch..."
        :transient :git-fetch}
      @{:key "m" :command git-merge-transient :description "Merge..."
        :transient :git-merge}
      @{:key "r" :command git-rebase-transient :description "Rebase..."
        :transient :git-rebase}
      @{:key "A" :command git-cherry-pick-transient :description "Cherry-pick..."
        :transient :git-cherry-pick}
      @{:key "X" :command git-reset-transient :description "Reset..."
        :transient :git-reset}
      @{:key "t" :command git-tag-transient :description "Tag..."
        :transient :git-tag}
      @{:key "l" :command git-log-transient :description "Log..."
        :transient :git-log}
      @{:key "d" :command git-diff-transient :description "Diff..."
        :transient :git-diff}
      @{:key "z" :command git-stash-transient :description "Stash..."
        :transient :git-stash}]}])

# --- Bind transients to status keymap ---

(keymap/bind status-keymap "c" git-commit-transient)
(keymap/bind status-keymap "b" git-branch-transient)
(keymap/bind status-keymap "P" git-push-transient)
(keymap/bind status-keymap "F" git-pull-transient)
(keymap/bind status-keymap "f" git-fetch-transient)
(keymap/bind status-keymap "m" git-merge-transient)
(keymap/bind status-keymap "r" git-rebase-transient)
(keymap/bind status-keymap "A" git-cherry-pick-transient)
(keymap/bind status-keymap "X" git-reset-transient)
(keymap/bind status-keymap "t" git-tag-transient)
(keymap/bind status-keymap "l" git-log-transient)
(keymap/bind status-keymap "d" git-diff-transient)
(keymap/bind status-keymap "z" git-stash-transient)

# --- Status keymap: navigation and operations ---

(def status-g-map (keymap/new))
(keymap/bind status-g-map "r" git-refresh)
(keymap/bind status-g-map "g" move/beginning-of-buffer)
(keymap/bind status-keymap "g" status-g-map)
(keymap/bind status-keymap "G" move/end-of-buffer)
(keymap/bind status-keymap "n" git-next-section)
(keymap/bind status-keymap "p" git-prev-section)
(keymap/bind status-keymap "M-n" git-next-sibling)
(keymap/bind status-keymap "M-p" git-prev-sibling)
(keymap/bind status-keymap "^" git-section-parent)
(keymap/bind status-keymap "tab" git-toggle-section)
(keymap/bind status-keymap "s" git-stage)
(keymap/bind status-keymap "u" git-unstage)
(keymap/bind status-keymap "x" git-discard)
(keymap/bind status-keymap "enter" git-visit)
(keymap/bind status-keymap "q" git-quit)
(keymap/bind status-keymap "$" git-show-process-buffer)
(keymap/bind status-keymap "`" git-show-process-buffer)
(keymap/bind status-keymap "v" git-toggle-line-select)
(keymap/bind status-keymap "V" git-toggle-line-select)
(keymap/bind status-keymap "escape" git-clear-selection)
(keymap/bind status-keymap "C-g" git-clear-selection)
(keymap/bind status-keymap "j" git-next-line)
(keymap/bind status-keymap "k" git-prev-line)
(keymap/bind status-keymap "1" git-visibility-level-1)
(keymap/bind status-keymap "2" git-visibility-level-2)
(keymap/bind status-keymap "3" git-visibility-level-3)
(keymap/bind status-keymap "4" git-visibility-level-4)

# --- Log keymap bindings ---

(keymap/bind log-keymap "enter" git-log-visit)
(keymap/bind log-keymap "q" git-quit)
(keymap/bind log-keymap "n" git-next-section)
(keymap/bind log-keymap "p" git-prev-section)
(keymap/bind log-keymap "j" git-next-line)
(keymap/bind log-keymap "k" git-prev-line)
(keymap/bind log-keymap "G" move/end-of-buffer)
(def log-g-map (keymap/new))
(keymap/bind log-g-map "g" move/beginning-of-buffer)
(keymap/bind log-keymap "g" log-g-map)
(keymap/bind log-keymap "$" git-show-process-buffer)

# --- Diff keymap bindings ---

(keymap/bind diff-keymap "q" git-quit)
(keymap/bind diff-keymap "n" git-next-section)
(keymap/bind diff-keymap "p" git-prev-section)
(keymap/bind diff-keymap "j" git-next-line)
(keymap/bind diff-keymap "k" git-prev-line)
(keymap/bind diff-keymap "G" move/end-of-buffer)
(def diff-g-map (keymap/new))
(keymap/bind diff-g-map "g" move/beginning-of-buffer)
(keymap/bind diff-keymap "g" diff-g-map)
(keymap/bind diff-keymap "$" git-show-process-buffer)

# ════════════════════════════════════════════════════════════════════
# Copy / yank commands
# ════════════════════════════════════════════════════════════════════

(defn- section-copy-value
  "Return the copyable string value for the section at the current line.
  For commits returns the full SHA (resolved via rev-parse), for files
  the path, for stashes the stash ref."
  []
  (def b (buffer))
  (def section (sec/section-at-line b (cursor-line)))
  (unless section (break nil))
  (def data (section :data))
  (case (section :type)
    :commit
    (when-let [hash (data :hash)]
      (use-buffer-root)
      (def result (git/run "rev-parse" hash))
      (if (= (result :exit) 0)
        (string/trim (result :stdout))
        hash))

    :file
    (data :path)

    :stash
    (data :ref)

    # Section headers — return status key name
    :section-header
    (when-let [status (data :status)]
      (string status))

    nil))

(command/defcmd git-copy-section-value
  "Copy the value of the section at point to the kill ring.
  For commits copies the full SHA, for files the path, for stashes
  the stash ref."
  :label "Copy Section Value"
  []
  (if-let [value (section-copy-value)]
    (do
      (kill-ring/push value)
      (editor/message (string "Copied: " value)))
    (editor/message "Nothing to copy at point.")))

(command/defcmd git-copy-buffer-revision
  "Copy the revision displayed in the current buffer to the kill ring.
  In a diff buffer showing a commit, copies the full SHA. In a log
  buffer, copies the revision of the commit at point."
  :label "Copy Buffer Revision"
  []
  (def b (buffer))
  (use-buffer-root)
  # Try buffer-level revision first (diff buffers store this)
  (def rev (get-in b [:locals :git-revision]))
  (if rev
    (do
      (def result (git/run "rev-parse" rev))
      (def full-rev (if (= (result :exit) 0)
                      (string/trim (result :stdout))
                      rev))
      (kill-ring/push full-rev)
      (editor/message (string "Copied: " full-rev)))
    # Fall back to HEAD
    (do
      (def result (git/run "rev-parse" "HEAD"))
      (if (= (result :exit) 0)
        (let [full-rev (string/trim (result :stdout))]
          (kill-ring/push full-rev)
          (editor/message (string "Copied: " full-rev)))
        (editor/message "No revision to copy.")))))

(command/defcmd git-show-refs
  "List branches and tags in a dedicated buffer."
  :label "Show Refs"
  []
  (use-buffer-root)
  (def root (dyn :git-root))
  (def branch-result (git/run "branch" "-a" "--format=%(refname:short) %(objectname:short) %(subject)"))
  (def tag-result (git/run "tag" "-n1" "--sort=-creatordate"))
  (unless (= (branch-result :exit) 0)
    (editor/message "Failed to list refs.")
    (break))
  (def lines @[])
  (array/push lines "Branches:")
  (each line (string/split "\n" (string/trim (branch-result :stdout)))
    (when (> (length line) 0)
      (array/push lines (string "  " line))))
  (when (and (= (tag-result :exit) 0)
             (> (length (string/trim (tag-result :stdout))) 0))
    (array/push lines "")
    (array/push lines "Tags:")
    (each line (string/split "\n" (string/trim (tag-result :stdout)))
      (when (> (length line) 0)
        (array/push lines (string "  " line)))))
  (def content (string/join lines "\n"))
  (def b (editor/make-view-buffer "*git-refs*" content))
  (put b :hide-gutter true)
  (put-in b [:locals :git-root] root)
  (put-in b [:locals :project-root] root)
  (put-in b [:locals :default-directory] root)
  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :same]))

(command/defcmd git-yank-whole-line
  "Copy the current line to the kill ring."
  :label "Yank Whole Line"
  []
  (def b (buffer))
  (def [line _] (buf/line-col b (cursor)))
  (def start (buf/line-byte-offset b (- line 1)))
  (def end (buf/line-end-byte-offset b (- line 1)))
  (when (and start end)
    (def text (buf/slice b start end))
    (kill-ring/push text)
    (editor/message "Copied line")))

# --- Copy keybindings (Emacs/Magit convention: C-w and M-w) ---
(each km [status-keymap log-keymap diff-keymap]
  (keymap/bind km "C-w" git-copy-section-value)
  (keymap/bind km "M-w" git-copy-buffer-revision))

# ════════════════════════════════════════════════════════════════════
# Commands & global keybindings
# ════════════════════════════════════════════════════════════════════

(defn- detect-git-root []
  "Detect the git root for the current context.
  Checks: 1) current buffer's :git-root local, 2) current buffer's
  file directory, 3) CWD. Sets (dyn :git-root) and returns the root."
  (def b (buffer))
  (when b
    # If we're already in a git buffer, use its stored root
    (def stored-root (get-in b [:locals :git-root]))
    (when stored-root
      (setdyn :git-root stored-root)
      (break stored-root))
    # Detect from current buffer's file
    (when-let [file (b :file)]
      (def dir (string/join (slice (string/split "/" file) 0 -2) "/"))
      (when (> (length dir) 0)
        (setdyn :git-root dir)
        (def root (git/repo-root))
        (when root
          (setdyn :git-root root)
          (break root)))))
  # Fall back to CWD
  (setdyn :git-root nil)
  (git/repo-root))

(command/defcmd git-status
  "Open the git status buffer."
  :label "Git Status"
  []
  (unless (git/git-available?)
    (editor-message "git is not installed or not in PATH.")
    (break))
  (def root (detect-git-root))
  (unless root
    (editor-message "Not in a git repository.")
    (break))
  (setdyn :git-root root)
  (def b (get-or-create-status-buffer root))
  (set status-buf b)
  (def state (editor/get-state))
  (db/pop-to-buffer b state :actions [:reuse :same])
  (do-status-refresh b))

(command/defcmd git-dispatch
  "Open the top-level git transient menu."
  :label "Git Dispatch"
  []
  (unless (git/git-available?)
    (editor-message "git is not installed or not in PATH.")
    (break))
  (def root (detect-git-root))
  (unless root
    (editor-message "Not in a git repository.")
    (break))
  (setdyn :git-root root)
  (set status-buf (get-or-create-status-buffer root))
  (transient/activate :git-dispatch))

(command/defcmd git-process-buffer
  "Show the git process log buffer."
  :label "Git Process Buffer"
  []
  (git-show-process-buffer))

# --- Global keybindings ---

(def global-km (editor/global-keymap))
(keymap/bind global-km "C-x g" git-status)
(keymap/bind global-km "C-x G" git-dispatch)

# --- Debounced refresh on save ---

(hook/add :after-save
  (fn [b]
    (when (and refresh-on-save status-buf)
      # Generation-based debounce: only the latest trigger fires
      (def root (get-in status-buf [:locals :git-root]))
      (def gen (++ refresh-generation))
      (ev/spawn
        (ev/sleep refresh-debounce-delay)
        (when (= refresh-generation gen)
          (setdyn :git-root root)
          (do-status-refresh status-buf))))))

# --- Auto-revert after git operations ---

(hook/add :git-post-operation
  (fn [op args exit]
    # After tree-changing operations, revert unmodified file buffers
    (when (find |(= op $) [:checkout :pull :stash-pop :merge :rebase])
      (each b (editor/buffers)
        (when (and (b :file)
                   (not (b :modified))
                   (os/stat (b :file)))
          (git/revert-buffer b))))))
