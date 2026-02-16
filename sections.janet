# git-mode/sections.janet — Section data model and navigation
#
# Sections are the core structural unit of git buffers. Each section
# represents a region of the buffer (file, hunk, commit, header) with
# metadata for navigation, collapse/expand, and operations.

(import jax/buffer :as buf)
(import jax/overlay)
(import jax/faces)
(import jax/core :prefix "")

# --- Section data model ---
#
# A section tree is stored in buffer locals as :git-sections.
# Each section is a table:
#   :type      — :header, :file, :hunk, :commit, :stash, :section-header
#   :start     — start line (0-indexed)
#   :end       — end line (0-indexed, inclusive)
#   :collapsed — whether children are hidden
#   :data      — section-specific data (file path, hunk info, etc.)
#   :children  — array of child sections
#   :parent    — reference to parent section (set during tree build)
#   :face      — face keyword for the section header line

(defn make-section
  "Create a new section."
  [type start end &keys opts]
  @{:type type
    :start start
    :end end
    :collapsed (or (opts :collapsed) false)
    :data (or (opts :data) @{})
    :children (or (opts :children) @[])
    :parent nil
    :face (opts :face)})

(defn set-sections
  "Store a section tree on a buffer."
  [b sections]
  (put-in b [:locals :git-sections] sections))

(defn get-sections
  "Get the section tree from a buffer."
  [b]
  (get-in b [:locals :git-sections] @[]))

(defn- link-parents
  "Set :parent references on all children recursively."
  [sections &opt parent]
  (each s sections
    (put s :parent parent)
    (link-parents (s :children) s)))

(defn build-section-tree
  "Store sections on a buffer and link parent references."
  [b sections]
  (link-parents sections)
  (set-sections b sections))

# --- Section lookup ---

(defn section-at-line
  "Find the deepest section containing the given line."
  [b line]
  (var best nil)
  (defn walk [sections]
    (each s sections
      (when (and (<= (s :start) line) (>= (s :end) line))
        (set best s)
        (walk (s :children)))))
  (walk (get-sections b))
  best)

(defn section-at-cursor
  "Find the section at the current cursor position."
  [b cursor]
  (def line (- (first (buf/line-col b cursor)) 1))
  (section-at-line b line))

# --- Navigation ---

(defn all-sections-flat
  "Flatten the section tree into a sorted array of all sections."
  [b]
  (def result @[])
  (defn walk [sections]
    (each s sections
      (array/push result s)
      (unless (s :collapsed)
        (walk (s :children)))))
  (walk (get-sections b))
  (sort-by |($ :start) result))

(defn next-section-line
  "Find the start line of the next section after the given line."
  [b line]
  (var found nil)
  (each s (all-sections-flat b)
    (when (and (nil? found) (> (s :start) line))
      (set found (s :start))))
  found)

(defn prev-section-line
  "Find the start line of the previous section before the given line."
  [b line]
  (var found nil)
  (each s (all-sections-flat b)
    (when (< (s :start) line)
      (set found (s :start))))
  found)

(defn next-sibling-line
  "Find the start line of the next sibling section."
  [b line]
  (def sec (section-at-line b line))
  (unless sec (break nil))
  (def parent (sec :parent))
  (def siblings (if parent (parent :children) (get-sections b)))
  (var found-current false)
  (var result nil)
  (each s siblings
    (if found-current
      (when (nil? result) (set result (s :start)))
      (when (= s sec) (set found-current true))))
  result)

(defn prev-sibling-line
  "Find the start line of the previous sibling section."
  [b line]
  (def sec (section-at-line b line))
  (unless sec (break nil))
  (def parent (sec :parent))
  (def siblings (if parent (parent :children) (get-sections b)))
  (var result nil)
  (each s siblings
    (when (= s sec) (break))
    (set result (s :start)))
  result)

(defn parent-section-line
  "Find the start line of the parent section."
  [b line]
  (def sec (section-at-line b line))
  (when (and sec (sec :parent))
    ((sec :parent) :start)))

# --- Collapse / Expand ---

(defn toggle-section
  "Toggle collapsed state of the section at the given line.
  Returns true if toggled."
  [b line]
  (def sec (section-at-line b line))
  (when (and sec (> (length (sec :children)) 0))
    (put sec :collapsed (not (sec :collapsed)))
    true))

(defn set-section-level
  "Set visibility to the given level (1-4). Level 1 collapses all
  top-level sections, level 4 shows everything."
  [b level]
  (defn walk [sections depth]
    (each s sections
      (when (> (length (s :children)) 0)
        (put s :collapsed (>= depth level)))
      (walk (s :children) (+ depth 1))))
  (walk (get-sections b) 1))

# --- Rendering helpers ---

(defn visible-lines
  "Return an array of {:line :text :face :indent} for visible lines,
  respecting collapsed sections. This is used by the status buffer
  renderer to know which lines are logically visible."
  [b]
  (def sections (get-sections b))
  (def hidden @{})
  # Mark hidden line ranges from collapsed sections
  (defn mark-hidden [secs]
    (each s secs
      (when (s :collapsed)
        (for i (+ (s :start) 1) (+ (s :end) 1)
          (put hidden i true)))
      (unless (s :collapsed)
        (mark-hidden (s :children)))))
  (mark-hidden sections)
  # Return visible lines
  (def result @[])
  (for i 0 (buf/line-count b)
    (unless (hidden i)
      (array/push result i)))
  result)
