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

# Forward declarations for keymaps that reference commands
(var do-refresh nil)
(var do-stage nil)
(var do-unstage nil)
(var do-discard nil)
(var do-visit nil)
(var do-toggle-section nil)
(var do-next-section nil)
(var do-prev-section nil)
(var do-next-sibling nil)
(var do-prev-sibling nil)
(var do-section-parent nil)
(var quit-git-buffer nil)
(var show-process-buffer nil)
(var set-visibility-level nil)
(var show-commit-diff nil)
(var apply-diff-overlays nil)
(var diff-mode nil)

# --- Status mode keymap ---

(def status-keymap (keymap/new))

# Section navigation
(keymap/bind status-keymap "n" (fn [] (do-next-section)))
(keymap/bind status-keymap "p" (fn [] (do-prev-section)))
(keymap/bind status-keymap "M-n" (fn [] (do-next-sibling)))
(keymap/bind status-keymap "M-p" (fn [] (do-prev-sibling)))
(keymap/bind status-keymap "^" (fn [] (do-section-parent)))
(keymap/bind status-keymap "tab" (fn [] (do-toggle-section)))

# Operations
(keymap/bind status-keymap "g" (fn [] (do-refresh)))
(keymap/bind status-keymap "s" (fn [] (do-stage)))
(keymap/bind status-keymap "u" (fn [] (do-unstage)))
(keymap/bind status-keymap "x" (fn [] (do-discard)))
(keymap/bind status-keymap "enter" (fn [] (do-visit)))
(keymap/bind status-keymap "q" (fn [] (quit-git-buffer)))

# Line selection for region staging
(keymap/bind status-keymap "v"
  (fn []
    (def p (pane))
    (if (p :mark)
      (do (put p :mark nil)
          (put p :selection-type nil))
      (do (put p :mark (cursor))
          (put p :selection-type :line)))))

(keymap/bind status-keymap "escape"
  (fn []
    (def p (pane))
    (put p :mark nil)
    (put p :selection-type nil)))

# Transients — bound after transient definitions below
(keymap/bind status-keymap "$"
  (fn [] (show-process-buffer)))

# Level visibility
(keymap/bind status-keymap "1"
  (fn [] (set-visibility-level 1)))
(keymap/bind status-keymap "2"
  (fn [] (set-visibility-level 2)))
(keymap/bind status-keymap "3"
  (fn [] (set-visibility-level 3)))
(keymap/bind status-keymap "4"
  (fn [] (set-visibility-level 4)))

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
  (def status-data (results :status))
  (def branch (status-data :branch))
  (def entries (status-data :entries))
  (def log-data (results :log))
  (def stash-data (results :stash))
  (def diff-unstaged (results :diff-unstaged))
  (def diff-staged (results :diff-staged))

  # Read view state from buffer locals
  (def expanded-files (or (get-in b [:locals :expanded-files]) @{}))
  (def collapsed-sections (or (get-in b [:locals :collapsed-sections]) @{}))

  # Save cursor position for restoration
  (def saved-cursor (or (get-in b [:locals :saved-cursor]) 0))

  # Clear buffer
  (put b :readonly false)
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

  # --- Recent commits ---
  (when (and log-data (> (length log-data) 0))
    (def section-start (length lines))
    (add-line (string "Recent commits (" (length log-data) ")")
              :git-section-heading)
    (def children @[])
    (unless (collapsed-sections :log)
      (each commit log-data
        (def refs-str (if (commit :refs) (string " (" (commit :refs) ")") ""))
        (def commit-line
          (add-line (string "  " (commit :hash) " "
                           (commit :date) "  "
                           (commit :subject) refs-str)
                    :git-hash))
        (array/push children
          (sec/make-section :commit commit-line commit-line
                            :data commit
                            :face :git-hash))))
    (def section-end (- (length lines) 1))
    (array/push sections
      (sec/make-section :section-header section-start section-end
                        :data @{:status :log}
                        :children children
                        :collapsed (truthy? (collapsed-sections :log))
                        :face :git-section-heading))
    (add-line ""))

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
  (hook/fire :git-status-refreshed b))

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

  (each [key & args]
    [[:status "status" "--porcelain=v2" "--branch"]
     [:diff-unstaged-raw "diff"]
     [:diff-staged-raw "diff" "--cached"]
     [:log-raw "log" (string "--format=%h%x00%s%x00%ar%x00%an%x00%D")
      (string "-" (string log-max-count))]
     [:stash-raw "stash" "list"]]
    (ev/go (fn []
             (def result (git/run ;args))
             (ev/give ch [key result]))))

  # Collect results
  (repeat 5
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

  # Render
  (render-status-buffer b parsed)

  (set refresh-pending false))

# --- Section navigation commands ---

(defn- cursor-line []
  (def b (buffer))
  (- (first (buf/line-col b (cursor))) 1))

(defn- goto-line [b line]
  (when line
    (def pos (buf/line-byte-offset b line))
    (when pos (set-cursor pos))))

(set do-next-section
  (fn []
    (def b (buffer))
    (def line (sec/next-section-line b (cursor-line)))
    (goto-line b line)))

(set do-prev-section
  (fn []
    (def b (buffer))
    (def line (sec/prev-section-line b (cursor-line)))
    (goto-line b line)))

(set do-next-sibling
  (fn []
    (def b (buffer))
    (def line (sec/next-sibling-line b (cursor-line)))
    (goto-line b line)))

(set do-prev-sibling
  (fn []
    (def b (buffer))
    (def line (sec/prev-sibling-line b (cursor-line)))
    (goto-line b line)))

(set do-section-parent
  (fn []
    (def b (buffer))
    (def line (sec/parent-section-line b (cursor-line)))
    (goto-line b line)))

(set do-toggle-section
  (fn []
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

        # Hunk sections — toggle parent file
        :hunk
        (when-let [parent (section :parent)]
          (when-let [expand-key (get-in parent [:data :expand-key])]
            (put expanded-files expand-key nil)
            (put-in b [:locals :expanded-files] expanded-files)
            (re-render-status b)))))))

(set set-visibility-level
  (fn [level]
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
    (re-render-status b)))

# --- Staging operations ---

(defn- hunk-region-lines
  "If the pane has an active mark within the current hunk section,
  return [sel-start sel-end] as 0-indexed offsets into hunk :lines.
  Otherwise return nil (meaning stage the whole hunk)."
  [b section]
  (def p (pane))
  (def mark (p :mark))
  (unless mark (break nil))
  (def cur (p :cursor))
  (def hunk-data (get-in section [:data :hunk]))
  (unless hunk-data (break nil))
  # The hunk section spans lines section:start to section:end.
  # Line section:start is the hunk header, actual diff lines start at +1.
  (def first-content-line (+ (section :start) 1))
  (def mark-line (- (first (buf/line-col b mark)) 1))
  (def cur-line (- (first (buf/line-col b cur)) 1))
  (def sel-first (min mark-line cur-line))
  (def sel-last (max mark-line cur-line))
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

(set do-stage
  (fn []
    (def b (buffer))
    (def line (cursor-line))
    (def section (sec/section-at-line b line))
    (unless section (break))
    (def data (section :data))
    (def root (get-in b [:locals :git-root]))
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
    (put (pane) :mark nil)
    (put (pane) :selection-type nil)
    (do-status-refresh b)
    (hook/fire :git-post-operation :stage [] 0)))

(set do-unstage
  (fn []
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

    (put (pane) :mark nil)
    (put (pane) :selection-type nil)
    (do-status-refresh b)
    (hook/fire :git-post-operation :unstage [] 0)))

(set do-discard
  (fn []
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
           (put (pane) :mark nil)
           (put (pane) :selection-type nil)
           (do-status-refresh b)
           (hook/fire :git-post-operation :discard [] 0)))})))

# --- Visit thing at point ---

(set do-visit
  (fn []
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

      :commit
      (let [hash (get-in section [:data :hash])]
        (when hash (show-commit-diff hash)))

      nil)))

(set do-refresh
  (fn []
    (def b (buffer))
    (do-status-refresh b)))

# --- Status buffer creation ---

(defn- get-or-create-status-buffer [root]
  (or (status-bufs root)
      (let [basename (last (string/split "/" root))
            name (string "*git-status: " basename "*")
            b (buf/new name)]
        (put b :major-mode status-mode)
        (put b :readonly true)
        (put b :hide-gutter true)
        (put-in b [:locals :git-root] root)
        (put-in b [:locals :project-root] root)
        (put-in b [:locals :default-directory] root)
        (put status-bufs root b)
        b)))

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

(defn- finish-commit []
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
  (when (targs "--amend") (array/push args "--amend"))
  (when (targs "--all") (array/push args "--all"))
  (when (targs "--no-verify") (array/push args "--no-verify"))
  (when (targs "--signoff") (array/push args "--signoff"))
  (when (targs "--allow-empty") (array/push args "--allow-empty"))

  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do
      (editor-message "Committed.")
      (hook/fire :git-commit-finished msg)
      (hook/fire :git-post-operation :commit args 0)
      # Kill commit buffer and refresh status
      (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Commit failed: " (result :stderr)))))

(defn- cancel-commit []
  (editor-message "Commit cancelled.")
  (set commit-buf nil))

(keymap/bind commit-keymap "C-c C-c" finish-commit)
(keymap/bind commit-keymap "C-c C-k" cancel-commit)

(defn- open-commit-buffer [&opt amend]
  (def root (dyn :git-root))
  (def b (buf/new "*git-commit*"))
  (set commit-buf b)
  (put b :major-mode commit-mode)
  (put-in b [:locals :git-root] root)
  (put-in b [:locals :project-root] root)
  (put-in b [:locals :default-directory] root)

  # Pre-populate with amend message if amending
  (when amend
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
    (put b :major-mode diff-mode)
    (put b :hide-gutter true)
    (put-in b [:locals :git-root] root)
    (put-in b [:locals :project-root] root)
    (put-in b [:locals :default-directory] root)
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :split-below])
    (apply-diff-overlays b))))

(keymap/bind log-keymap "enter"
  (fn []
    (def b (buffer))
    (def line (cursor-line))
    (def section (sec/section-at-line b line))
    (when (and section (= (section :type) :commit))
      (show-commit-diff (get-in section [:data :hash])))))

(keymap/bind log-keymap "q" (fn [] (quit-git-buffer)))
(keymap/bind log-keymap "n" (fn [] (do-next-section)))
(keymap/bind log-keymap "p" (fn [] (do-prev-section)))

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
  (put b :major-mode log-mode)
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
(keymap/bind diff-keymap "q" (fn [] (quit-git-buffer)))
(keymap/bind diff-keymap "n" (fn [] (do-next-section)))
(keymap/bind diff-keymap "p" (fn [] (do-prev-section)))

(set diff-mode
  @{:name "git-diff"
    :display-name "Git Diff"
    :keymap diff-keymap
    :grammar {:type :peg :highlights git-diff-peg}})

(mode/register diff-mode)

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
  (put b :major-mode diff-mode)
  (put b :hide-gutter true)
  (put-in b [:locals :git-root] (dyn :git-root))
  (put-in b [:locals :project-root] (dyn :git-root))
  (put-in b [:locals :default-directory] (dyn :git-root))
  (apply-diff-overlays b)
  (db/pop-to-buffer b (editor/get-state)
                    :actions [:reuse :same]))

# ════════════════════════════════════════════════════════════════════
# Shared helpers
# ════════════════════════════════════════════════════════════════════

(set quit-git-buffer
  (fn []
    (def state (editor/get-state))
    (def p (pane))
    (def prev (db/pane-previous-buffer p state))
    (if prev
      (put p :buffer prev)
      (editor-message "No previous buffer"))))

(set show-process-buffer
  (fn []
    (def b (git/get-process-buffer))
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :split-below])))

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

(command/defcmd git-commit-fixup
  "Create a fixup commit."
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
    (put b :major-mode commit-mode)
    (put-in b [:locals :git-root] root)
    (put-in b [:locals :project-root] root)
    (put-in b [:locals :default-directory] root)
    (buf/insert b 0 (string/trim (result :stdout)))
    # Override finish to use --amend
    (db/pop-to-buffer b (editor/get-state)
                      :actions [:reuse :same])))

(transient/define :git-commit
  :description "Commit"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-a" :switch "--all" :description "Stage all modified"}
      @{:key "-e" :switch "--allow-empty" :description "Allow empty commit"}
      @{:key "-n" :switch "--no-verify" :description "Skip hooks"}
      @{:key "-s" :switch "--signoff" :description "Add Signed-off-by"}]}
   @{:name "Create"
     :suffixes
     [@{:key "c" :command git-commit-create :description "Commit"}
      @{:key "a" :command git-commit-amend :description "Amend"}
      @{:key "f" :command git-commit-fixup :description "Fixup"}
      @{:key "w" :command git-commit-reword :description "Reword"}]}])

# --- Branch transient ---

(command/defcmd git-branch-checkout
  "Checkout a branch."
  :label "Checkout Branch"
  []
  (use-buffer-root)
  (def branches (git/run-lines "branch" "--format=%(refname:short)"))
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

(transient/define :git-branch
  :description "Branch"
  :groups
  [@{:name "Checkout"
     :suffixes
     [@{:key "b" :command git-branch-checkout :description "Checkout"}
      @{:key "c" :command git-branch-create-and-checkout :description "Create & checkout"}
      @{:key "n" :command git-branch-create :description "Create"}]}
   @{:name "Do"
     :suffixes
     [@{:key "r" :command git-branch-rename :description "Rename"}
      @{:key "k" :command git-branch-delete :description "Delete"}]}])

# --- Push transient ---

(command/defcmd git-push-pushremote
  "Push to push remote."
  :label "Push"
  []
  (use-buffer-root)
  (def targs (or (dyn :transient-args) @{}))
  (def args @["push"])
  (when (targs "--force-with-lease") (array/push args "--force-with-lease"))
  (when (targs "--set-upstream") (array/push args "--set-upstream"))
  (when (targs "--dry-run") (array/push args "--dry-run"))
  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do (editor-message "Pushed successfully.")
        (hook/fire :git-post-operation :push args 0)
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Push failed: " (result :stderr)))))

(command/defcmd git-push-other
  "Push to another remote."
  :label "Push to Other"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Push to remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (def targs (or (dyn :transient-args) @{}))
       (def args @["push" (candidate :text)])
       (when (targs "--force-with-lease") (array/push args "--force-with-lease"))
       (when (targs "--set-upstream") (array/push args "--set-upstream"))
       (def result (git/run ;args))
       (if (= (result :exit) 0)
         (do (editor-message (string "Pushed to " (candidate :text)))
             (hook/fire :git-post-operation :push args 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Push failed: " (result :stderr)))))}))

(transient/define :git-push
  :description "Push"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-f" :switch "--force-with-lease" :description "Force with lease"}
      @{:key "-u" :switch "--set-upstream" :description "Set upstream"}
      @{:key "-n" :switch "--dry-run" :description "Dry run"}]}
   @{:name "Push to"
     :suffixes
     [@{:key "p" :command git-push-pushremote :description "Push to pushremote"}
      @{:key "o" :command git-push-other :description "Push to other"}]}])

# --- Pull transient ---

(command/defcmd git-pull-default
  "Pull from upstream."
  :label "Pull"
  []
  (use-buffer-root)
  (def targs (or (dyn :transient-args) @{}))
  (def args @["pull"])
  (when (targs "--rebase") (array/push args "--rebase"))
  (when (targs "--no-rebase") (array/push args "--no-rebase"))
  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do (editor-message "Pulled successfully.")
        (hook/fire :git-post-operation :pull args 0)
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Pull failed: " (result :stderr)))))

(command/defcmd git-pull-other
  "Pull from another remote."
  :label "Pull from Other"
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
            (def result (git/run "pull" (candidate :text) branch))
            (if (= (result :exit) 0)
              (do (editor-message "Pulled successfully.")
                  (hook/fire :git-post-operation :pull
                             ["pull" (candidate :text) branch] 0)
                  (when status-buf (do-status-refresh status-buf)))
              (editor-message (string "Pull failed: " (result :stderr)))))}))}))

(transient/define :git-pull
  :description "Pull"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-r" :switch "--rebase" :description "Rebase"}
      @{:key "-n" :switch "--no-rebase" :description "No rebase"}]}
   @{:name "Pull from"
     :suffixes
     [@{:key "p" :command git-pull-default :description "Pull from upstream"}
      @{:key "o" :command git-pull-other :description "Pull from other"}]}])

# --- Fetch transient ---

(command/defcmd git-fetch-default
  "Fetch from upstream."
  :label "Fetch"
  []
  (use-buffer-root)
  (def targs (or (dyn :transient-args) @{}))
  (def args @["fetch"])
  (when (targs "--prune") (array/push args "--prune"))
  (when (targs "--all") (array/push args "--all"))
  (def result (git/run ;args))
  (if (= (result :exit) 0)
    (do (editor-message "Fetched successfully.")
        (hook/fire :git-post-operation :fetch args 0)
        (when status-buf (do-status-refresh status-buf)))
    (editor-message (string "Fetch failed: " (result :stderr)))))

(command/defcmd git-fetch-other
  "Fetch from another remote."
  :label "Fetch from Other"
  []
  (use-buffer-root)
  (def remotes (git/run-lines "remote"))
  (prompt/pick
    {:prompt "Fetch from remote: "
     :candidates (map |(do @{:text $}) remotes)
     :on-accept
     (fn [candidate]
       (def result (git/run "fetch" (candidate :text)))
       (if (= (result :exit) 0)
         (do (editor-message (string "Fetched from " (candidate :text)))
             (hook/fire :git-post-operation :fetch ["fetch" (candidate :text)] 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Fetch failed: " (result :stderr)))))}))

(transient/define :git-fetch
  :description "Fetch"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-p" :switch "--prune" :description "Prune deleted branches"}
      @{:key "-a" :switch "--all" :description "Fetch all remotes"}]}
   @{:name "Fetch from"
     :suffixes
     [@{:key "f" :command git-fetch-default :description "Fetch from upstream"}
      @{:key "o" :command git-fetch-other :description "Fetch from other"}]}])

# --- Stash transient ---

(command/defcmd git-stash-save
  "Save changes to stash."
  :label "Stash Save"
  []
  (use-buffer-root)
  (prompt/activate
    {:prompt "Stash message (optional): "
     :on-submit
     (fn [msg]
       (def targs (or (dyn :transient-args) @{}))
       (def args @["stash" "push"])
       (when (targs "--include-untracked") (array/push args "--include-untracked"))
       (when (targs "--all") (array/push args "--all"))
       (when (targs "--keep-index") (array/push args "--keep-index"))
       (when (> (length (string/trim msg)) 0)
         (array/push args "-m" msg))
       (def result (git/run ;args))
       (if (= (result :exit) 0)
         (do (editor-message "Stashed.")
             (hook/fire :git-post-operation :stash args 0)
             (when status-buf (do-status-refresh status-buf)))
         (editor-message (string "Stash failed: " (result :stderr)))))}))

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
  (prompt/activate
    {:prompt "Drop top stash? (y/n) "
     :on-submit
     (fn [input]
       (when (= (string/ascii-lower (string/trim input)) "y")
         (def result (git/run "stash" "drop"))
         (if (= (result :exit) 0)
           (do (editor-message "Stash dropped.")
               (when status-buf (do-status-refresh status-buf)))
           (editor-message (string "Stash drop failed: " (result :stderr))))))}))

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
      @{:key "-a" :switch "--all" :description "Include all (ignored too)"}
      @{:key "-k" :switch "--keep-index" :description "Keep index"}]}
   @{:name "Stash"
     :suffixes
     [@{:key "z" :command git-stash-save :description "Save"}
      @{:key "p" :command git-stash-pop :description "Pop"}
      @{:key "a" :command git-stash-apply :description "Apply"}
      @{:key "d" :command git-stash-drop :description "Drop"}
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

(transient/define :git-log
  :description "Log"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-n" :option "--max-count=" :description "Max count" :reader :string}
      @{:key "-a" :switch "--all" :description "All branches"}
      @{:key "-G" :switch "--no-graph" :description "Hide graph"}
      @{:key "--author" :option "--author=" :description "Author" :reader :string}
      @{:key "--since" :option "--since=" :description "Since" :reader :string}
      @{:key "--grep" :option "--grep=" :description "Grep" :reader :string}]}
   @{:name "Log"
     :suffixes
     [@{:key "l" :command git-log-current :description "Current"}
      @{:key "o" :command git-log-other :description "Other branch"}
      @{:key "f" :command git-log-file :description "File"}]}])

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
         (put b :major-mode diff-mode)
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
         (put b :major-mode diff-mode)
         (put b :hide-gutter true)
         (put-in b [:locals :git-root] (dyn :git-root))
         (put-in b [:locals :project-root] (dyn :git-root))
         (put-in b [:locals :default-directory] (dyn :git-root))
         (apply-diff-overlays b)
         (db/pop-to-buffer b (editor/get-state)
                           :actions [:reuse :same])))}))

(transient/define :git-diff
  :description "Diff"
  :groups
  [@{:name "Arguments"
     :infixes
     [@{:key "-w" :switch "--ignore-whitespace" :description "Ignore whitespace"}
      @{:key "--stat" :switch "--stat" :description "Show stat only"}]}
   @{:name "Diff"
     :suffixes
     [@{:key "d" :command git-diff-dwim :description "Dwim (unstaged)"}
      @{:key "s" :command git-diff-staged :description "Staged"}
      @{:key "c" :command git-diff-commit :description "Commit"}
      @{:key "r" :command git-diff-range :description "Range"}]}])

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

(transient/define :git-dispatch
  :description "Git"
  :groups
  [@{:name "Git"
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
      @{:key "l" :command git-log-transient :description "Log..."
        :transient :git-log}
      @{:key "d" :command git-diff-transient :description "Diff..."
        :transient :git-diff}
      @{:key "z" :command git-stash-transient :description "Stash..."
        :transient :git-stash}]}])

# --- Bind transients to status keymap ---

(keymap/bind status-keymap "c" (fn [] (transient/activate :git-commit)))
(keymap/bind status-keymap "b" (fn [] (transient/activate :git-branch)))
(keymap/bind status-keymap "P" (fn [] (transient/activate :git-push)))
(keymap/bind status-keymap "F" (fn [] (transient/activate :git-pull)))
(keymap/bind status-keymap "f" (fn [] (transient/activate :git-fetch)))
(keymap/bind status-keymap "l" (fn [] (transient/activate :git-log)))
(keymap/bind status-keymap "d" (fn [] (transient/activate :git-diff)))
(keymap/bind status-keymap "z" (fn [] (transient/activate :git-stash)))

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
  (show-process-buffer))

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
