# git-mode/plumbing.janet — Git subprocess execution and output parsing
#
# All git interaction goes through git/run, which logs commands to the
# process buffer. Parsers convert git output into structured Janet data.

(import jax/buffer :as buf)
(import jax/editor :as editor)
(import jax/mode)
(import jax/keymap)
(import jax/command)
(import jax/faces)
(import jax/hook)
(import jax/core :prefix "")

# --- Faces ---

(faces/defface :git-process-command
  "Git command in process buffer."
  @{:bold true :fg "#7cafc2"})

(faces/defface :git-process-ok
  "Successful exit in process buffer."
  @{:fg "#a1b56c"})

(faces/defface :git-process-error
  "Failed exit in process buffer."
  @{:fg "#ab4642"})

# --- Process buffer ---

(var- process-buf nil)
(var- max-lines 5000)

(defn set-max-lines
  "Set the maximum number of lines to keep in the process buffer."
  [n]
  (set max-lines n))

(def process-keymap (keymap/new))
(command/defcmd git-process-quit
  "Show message about switching buffers."
  :label "Process Quit" []
  (editor/message "Use C-x b to switch buffers"))
(keymap/bind process-keymap "q" git-process-quit)

(def process-mode
  @{:name "git-process"
   :display-name "Git Process"
   :keymap process-keymap})

(mode/register process-mode)

(defn get-process-buffer
  "Get or create the git process buffer."
  []
  (unless process-buf
    (set process-buf (buf/new "*git-process*"))
    (put process-buf :readonly true)
    (put process-buf :hide-gutter true)
    (put process-buf :major-mode process-mode)
    (editor/add-buffer process-buf))
  process-buf)

(defn- append-to-process-buffer
  "Append text to the process buffer."
  [text]
  (def b (get-process-buffer))
  (put b :readonly false)
  (def len (buf/length b))
  (buf/insert b len text)
  # Trim if over max lines
  (def line-count (buf/line-count b))
  (when (> line-count max-lines)
    (def trim-to (- line-count max-lines))
    (def trim-end (buf/line-byte-offset b trim-to))
    (buf/delete-forward b 0 trim-end))
  (put b :readonly true)
  (put b :modified false))

# --- Git execution ---

(var- git-available nil)

(defn git-available?
  "Check if the git binary is available. Caches the result."
  []
  (when (nil? git-available)
    (set git-available
      (try
        (do
          (def p (os/spawn ["git" "--version"] :p {:out :pipe :err :pipe}))
          (:read (p :out) :all)
          (:read (p :err) :all)
          (= (:wait p) 0))
        ([_] false))))
  git-available)

(defn- git-cmd
  "Build a git command array, prepending -C <root> if :git-root is set."
  [args]
  (def root (dyn :git-root))
  (if root
    ["git" "-C" root ;args]
    ["git" ;args]))

(defn repo-root
  "Return the git repository root, or nil.
  Uses (dyn :git-root) if set, otherwise detects from CWD."
  []
  (unless (git-available?) (break nil))
  (try
    (do
      (def p (os/spawn (git-cmd ["rev-parse" "--show-toplevel"]) :p
                       {:out :pipe :err :pipe}))
      (def out (string/trim (or (:read (p :out) :all) "")))
      (:read (p :err) :all)
      (if (= (:wait p) 0) out nil))
    ([_] nil)))

(defn in-repo?
  "Return true if we're inside a git repository.
  Uses (dyn :git-root) if set, otherwise detects from CWD."
  []
  (truthy? (repo-root)))

# --- Branch / remote helpers ---

(defn current-branch
  "Return the current branch name, or nil if detached HEAD."
  []
  (try
    (do
      (def p (os/spawn (git-cmd ["symbolic-ref" "--short" "HEAD"]) :p
                       {:out :pipe :err :pipe}))
      (def out (string/trim (or (:read (p :out) :all) "")))
      (:read (p :err) :all)
      (if (= (:wait p) 0) out nil))
    ([_] nil)))

(defn upstream-ref
  "Return the upstream tracking ref for a branch (e.g. 'origin/master'),
  or nil if none configured. Defaults to current branch."
  [&opt branch]
  (def br (or branch (current-branch)))
  (unless br (break nil))
  (try
    (do
      (def p (os/spawn (git-cmd ["config" "--get"
                                 (string "branch." br ".remote")]) :p
                       {:out :pipe :err :pipe}))
      (def remote (string/trim (or (:read (p :out) :all) "")))
      (:read (p :err) :all)
      (unless (= (:wait p) 0) (break nil))
      (def p2 (os/spawn (git-cmd ["config" "--get"
                                  (string "branch." br ".merge")]) :p
                        {:out :pipe :err :pipe}))
      (def merge-ref (string/trim (or (:read (p2 :out) :all) "")))
      (:read (p2 :err) :all)
      (unless (= (:wait p2) 0) (break nil))
      # Convert refs/heads/main -> main
      (def short-ref
        (if (string/has-prefix? "refs/heads/" merge-ref)
          (string/slice merge-ref 11)
          merge-ref))
      (string remote "/" short-ref))
    ([_] nil)))

(defn push-remote-ref
  "Return the push remote ref for a branch, or nil.
  Checks pushRemote config, falls back to upstream remote.
  Defaults to current branch."
  [&opt branch]
  (def br (or branch (current-branch)))
  (unless br (break nil))
  (try
    (do
      # Check branch.<name>.pushRemote first
      (def p (os/spawn (git-cmd ["config" "--get"
                                 (string "branch." br ".pushRemote")]) :p
                       {:out :pipe :err :pipe}))
      (def push-remote (string/trim (or (:read (p :out) :all) "")))
      (:read (p :err) :all)
      (def remote
        (if (and (= (:wait p) 0) (> (length push-remote) 0))
          push-remote
          # Fall back to remote.pushDefault
          (do
            (def p2 (os/spawn (git-cmd ["config" "--get"
                                        "remote.pushDefault"]) :p
                              {:out :pipe :err :pipe}))
            (def pd (string/trim (or (:read (p2 :out) :all) "")))
            (:read (p2 :err) :all)
            (if (and (= (:wait p2) 0) (> (length pd) 0))
              pd
              # Fall back to upstream remote
              (do
                (def p3 (os/spawn (git-cmd ["config" "--get"
                                            (string "branch." br ".remote")]) :p
                                  {:out :pipe :err :pipe}))
                (def r (string/trim (or (:read (p3 :out) :all) "")))
                (:read (p3 :err) :all)
                (when (= (:wait p3) 0) r))))))
      (when (and remote (> (length remote) 0))
        (string remote "/" br)))
    ([_] nil)))

(defn run
  "Run a git command, log to the process buffer, return
  {:exit exit-code :stdout string :stderr string :elapsed seconds}.
  Respects (dyn :git-root) for the working directory."
  [& args]
  (def start-time (os/clock))
  (def result @{:exit -1 :stdout "" :stderr ""})
  (try
    (do
      (def p (os/spawn (git-cmd args) :p {:out :pipe :err :pipe}))
      (def stdout (or (:read (p :out) :all) ""))
      (def stderr (or (:read (p :err) :all) ""))
      (def exit (:wait p))
      (def elapsed (- (os/clock) start-time))
      (put result :exit exit)
      (put result :stdout (string stdout))
      (put result :stderr (string stderr))
      (put result :elapsed elapsed)
      # Log to process buffer
      (def status-str (if (= exit 0) "ok" (string "exit " exit)))
      (def elapsed-str (string/format "%.3fs" elapsed))
      (append-to-process-buffer
        (string "$ git " (string/join (map string args) " ")
                "  [" elapsed-str ", " status-str "]\n"
                (when (> (length (string stdout)) 0) (string stdout))
                (when (and (> (length (string stderr)) 0) (not= exit 0))
                  (string stderr))
                "\n")))
    ([err]
      (put result :stderr (string err))
      (append-to-process-buffer
        (string "$ git " (string/join (map string args) " ")
                "  [ERROR: " (string err) "]\n\n"))))
  result)

(defn run-with-input
  "Run a git command with data piped to stdin.
  Logs to process buffer, returns {:exit :stdout :stderr :elapsed}.
  Respects (dyn :git-root) for the working directory."
  [input & args]
  (def start-time (os/clock))
  (def result @{:exit -1 :stdout "" :stderr ""})
  (try
    (do
      (def p (os/spawn (git-cmd args) :p {:in :pipe :out :pipe :err :pipe}))
      (:write (p :in) input)
      (:close (p :in))
      (def stdout (or (:read (p :out) :all) ""))
      (def stderr (or (:read (p :err) :all) ""))
      (def exit (:wait p))
      (def elapsed (- (os/clock) start-time))
      (put result :exit exit)
      (put result :stdout (string stdout))
      (put result :stderr (string stderr))
      (put result :elapsed elapsed)
      (def status-str (if (= exit 0) "ok" (string "exit " exit)))
      (def elapsed-str (string/format "%.3fs" elapsed))
      (append-to-process-buffer
        (string "$ git " (string/join (map string args) " ")
                " <<stdin  [" elapsed-str ", " status-str "]\n"
                (when (and (> (length (string stderr)) 0) (not= exit 0))
                  (string stderr))
                "\n")))
    ([err]
      (put result :stderr (string err))
      (append-to-process-buffer
        (string "$ git " (string/join (map string args) " ")
                " <<stdin  [ERROR: " (string err) "]\n\n"))))
  result)

(defn run-ok
  "Run a git command and return stdout if exit 0, nil otherwise."
  [& args]
  (def result (run ;args))
  (when (= (result :exit) 0)
    (result :stdout)))

(defn run-lines
  "Run a git command and return stdout lines if exit 0, empty array otherwise."
  [& args]
  (def result (run ;args))
  (if (= (result :exit) 0)
    (let [out (string/trim (result :stdout))]
      (if (= out "") @[]
        (map string/trim (string/split "\n" out))))
    @[]))

# --- Buffer revert ---

(defn revert-buffer
  "Reload a buffer's contents from disk. Returns true on success."
  [buffer]
  (when-let [path (buffer :file)]
    (try
      (do
        (def raw (slurp path))
        (def old-len (buf/length buffer))
        (when (> old-len 0)
          (buf/delete-forward buffer 0 old-len))
        (when (> (length raw) 0)
          (buf/insert buffer 0 raw))
        (put buffer :modified false)
        true)
      ([_] nil))))

# --- Parsers ---

(defn parse-status
  "Parse `git status --porcelain=v2 --branch` into structured data.
  Returns {:branch {:oid :head :upstream :ab} :entries [...]}"
  [output]
  (def result @{:branch @{} :entries @[]})
  (each line (string/split "\n" output)
    (when (> (length line) 0)
      (cond
        # Branch headers
        (string/has-prefix? "# branch.oid " line)
        (put-in result [:branch :oid] (string/slice line 14))

        (string/has-prefix? "# branch.head " line)
        (put-in result [:branch :head] (string/slice line 14))

        (string/has-prefix? "# branch.upstream " line)
        (put-in result [:branch :upstream] (string/slice line 18))

        (string/has-prefix? "# branch.ab " line)
        (let [ab (string/slice line 13)
              parts (string/split " " ab)]
          (put-in result [:branch :ahead]
                  (scan-number (string/slice (get parts 0) 1)))
          (put-in result [:branch :behind]
                  (scan-number (string/slice (get parts 1) 1))))

        # Changed entries (ordinary)
        (string/has-prefix? "1 " line)
        (let [parts (string/split " " line)
              xy (get parts 1)
              path (string/join (slice parts 8) " ")]
          (array/push (result :entries)
            @{:type :changed :xy xy :path path
              :staged (not= (string/slice xy 0 1) ".")
              :unstaged (not= (string/slice xy 1 2) ".")}))

        # Renamed/copied
        (string/has-prefix? "2 " line)
        (let [parts (string/split " " line)
              xy (get parts 1)
              rest-str (string/join (slice parts 8) " ")
              path-parts (string/split "\t" rest-str)
              path (get path-parts 1)
              orig-path (get path-parts 0)]
          (array/push (result :entries)
            @{:type :renamed :xy xy :path path :orig-path orig-path
              :staged (not= (string/slice xy 0 1) ".")
              :unstaged (not= (string/slice xy 1 2) ".")}))

        # Unmerged
        (string/has-prefix? "u " line)
        (let [parts (string/split " " line)
              xy (get parts 1)
              path (string/join (slice parts 10) " ")]
          (array/push (result :entries)
            @{:type :unmerged :xy xy :path path}))

        # Untracked
        (string/has-prefix? "? " line)
        (let [path (string/slice line 2)]
          (array/push (result :entries)
            @{:type :untracked :path path})))))
  result)

(defn parse-diff
  "Parse unified diff output into structured data.
  Returns array of {:file :old-file :hunks [{:header :old-start :old-count
  :new-start :new-count :lines [...]}]}."
  [output]
  (def files @[])
  (var current-file nil)
  (var current-hunk nil)

  (each line (string/split "\n" output)
    (cond
      # New file diff
      (string/has-prefix? "diff --git " line)
      (do
        (when (and current-file current-hunk)
          (array/push (current-file :hunks) current-hunk))
        (when current-file
          (array/push files current-file))
        (set current-file @{:hunks @[] :header line})
        (set current-hunk nil))

      # File names
      (string/has-prefix? "--- " line)
      (when current-file
        (def path (string/slice line 4))
        (put current-file :old-file
             (if (= path "/dev/null") nil
               (if (string/has-prefix? "a/" path)
                 (string/slice path 2) path))))

      (string/has-prefix? "+++ " line)
      (when current-file
        (def path (string/slice line 4))
        (put current-file :file
             (if (= path "/dev/null") nil
               (if (string/has-prefix? "b/" path)
                 (string/slice path 2) path))))

      # Index line
      (string/has-prefix? "index " line)
      nil

      # Binary file
      (string/has-prefix? "Binary files " line)
      (when current-file
        (put current-file :binary true))

      # New file mode / deleted file mode
      (or (string/has-prefix? "new file mode " line)
          (string/has-prefix? "deleted file mode " line)
          (string/has-prefix? "old mode " line)
          (string/has-prefix? "new mode " line)
          (string/has-prefix? "similarity index " line)
          (string/has-prefix? "rename from " line)
          (string/has-prefix? "rename to " line)
          (string/has-prefix? "copy from " line)
          (string/has-prefix? "copy to " line))
      nil

      # Hunk header
      (string/has-prefix? "@@" line)
      (do
        (when (and current-file current-hunk)
          (array/push (current-file :hunks) current-hunk))
        # Parse @@ -old,count +new,count @@
        (def m (peg/match
                 ~{:main (* "@@ -" :range " +" :range " @@" (? (* " " (capture (any 1)))))
                   :range (* (capture :d+) (? (* "," (capture :d+))))
                   :d+ (some (range "09"))}
                 line))
        (def old-start (if m (scan-number (get m 0)) 0))
        (def old-count (if (and m (get m 1)) (scan-number (get m 1)) 1))
        (def new-start (if m (scan-number (get m 2)) 0))
        (def new-count (if (and m (get m 3)) (scan-number (get m 3)) 1))
        (def context (if m (get m 4) nil))
        (set current-hunk @{:header line
                            :old-start old-start :old-count old-count
                            :new-start new-start :new-count new-count
                            :context context
                            :lines @[]}))

      # Diff content lines
      (and current-hunk
           (or (string/has-prefix? "+" line)
               (string/has-prefix? "-" line)
               (string/has-prefix? " " line)
               (= line "\\ No newline at end of file")))
      (array/push (current-hunk :lines) line)))

  # Flush last hunk and file
  (when (and current-file current-hunk)
    (array/push (current-file :hunks) current-hunk))
  (when current-file
    (array/push files current-file))
  files)

(defn parse-log
  "Parse `git log --format=...` output into structured data.
  Expected format: %h%x00%s%x00%ar%x00%an%x00%D
  Returns array of {:hash :subject :date :author :refs}."
  [output]
  (def commits @[])
  (each line (string/split "\n" (string/trim output))
    (when (> (length line) 0)
      (def parts (string/split "\x00" line))
      (when (>= (length parts) 4)
        (array/push commits
          @{:hash (get parts 0)
            :subject (get parts 1)
            :date (get parts 2)
            :author (get parts 3)
            :refs (if (and (get parts 4) (> (length (get parts 4)) 0))
                    (get parts 4) nil)}))))
  commits)

(defn parse-stash-list
  "Parse `git stash list` output into structured data.
  Returns array of {:ref :message}."
  [output]
  (def stashes @[])
  (each line (string/split "\n" (string/trim output))
    (when (> (length line) 0)
      (def m (peg/match
               ~{:main (* (capture (to ": ")) ": " (capture (any 1)))}
               line))
      (when m
        (array/push stashes
          @{:ref (get m 0) :message (get m 1)}))))
  stashes)

(defn parse-branch-list
  "Parse `git branch -a --format=...` output into structured data.
  Expected format: %(refname:short)%00%(objectname:short)%00%(HEAD)
  Returns array of {:name :hash :current :remote}."
  [output]
  (def branches @[])
  (each line (string/split "\n" (string/trim output))
    (when (> (length line) 0)
      (def parts (string/split "\x00" line))
      (when (>= (length parts) 3)
        (def name (get parts 0))
        (array/push branches
          @{:name name
            :hash (get parts 1)
            :current (= (get parts 2) "*")
            :remote (string/has-prefix? "remotes/" name)}))))
  branches)

# --- Diff patch generation ---

(defn make-hunk-patch
  "Generate a patch for a single hunk that can be applied with git apply.
  `file-diff` is a file entry from parse-diff, `hunk` is one of its hunks."
  [file-diff hunk]
  (def lines @[])
  (array/push lines (string "diff --git a/" (or (file-diff :old-file) (file-diff :file))
                            " b/" (file-diff :file)))
  (array/push lines (string "--- a/" (or (file-diff :old-file) (file-diff :file))))
  (array/push lines (string "+++ b/" (file-diff :file)))
  (array/push lines (hunk :header))
  (each l (hunk :lines)
    (array/push lines l))
  (string (string/join lines "\n") "\n"))

(defn make-region-patch
  "Generate a partial hunk patch for selected lines within a hunk.
  `file-diff` is the file entry, `hunk` is the hunk,
  `sel-start` and `sel-end` are 0-indexed line numbers within the hunk
  (relative to hunk :lines).

  For staging: keep selected +/- lines, convert unselected +/- to context.
  For unstaging: reverse the sense (+ becomes -, - becomes +)."
  [file-diff hunk sel-start sel-end &opt reverse]
  (def lines @[])
  (array/push lines (string "diff --git a/" (or (file-diff :old-file) (file-diff :file))
                            " b/" (file-diff :file)))
  (array/push lines (string "--- a/" (or (file-diff :old-file) (file-diff :file))))
  (array/push lines (string "+++ b/" (file-diff :file)))

  # Build modified hunk lines
  (def hunk-lines @[])
  (var old-count 0)
  (var new-count 0)
  (for i 0 (length (hunk :lines))
    (def l ((hunk :lines) i))
    (def in-sel (and (>= i sel-start) (<= i sel-end)))
    (cond
      (string/has-prefix? " " l)
      (do (++ old-count) (++ new-count)
          (array/push hunk-lines l))

      (string/has-prefix? "+" l)
      (if in-sel
        (do (++ new-count) (array/push hunk-lines l))
        (do nil)) # skip unselected additions

      (string/has-prefix? "-" l)
      (if in-sel
        (do (++ old-count) (array/push hunk-lines l))
        (do (++ old-count) (++ new-count)
            (array/push hunk-lines (string " " (string/slice l 1)))))))

  (array/push lines
    (string/format "@@ -%d,%d +%d,%d @@"
                   (hunk :old-start) old-count
                   (hunk :new-start) new-count))
  (each l hunk-lines (array/push lines l))

  (when reverse
    # Swap + and - for unstaging
    (def result @[])
    (each l lines
      (array/push result
        (cond
          (string/has-prefix? "+++ " l) l
          (string/has-prefix? "--- " l) l
          (string/has-prefix? "+" l) (string "-" (string/slice l 1))
          (string/has-prefix? "-" l) (string "+" (string/slice l 1))
          l)))
    (break (string (string/join result "\n") "\n")))

  (string (string/join lines "\n") "\n"))
