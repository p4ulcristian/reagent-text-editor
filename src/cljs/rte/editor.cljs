(ns rte.editor
  (:require
    [reagent.core :as r :refer [atom]]))

;;;;;;;;;;;;;
;;;;Utils;;;;
;;;;;;;;;;;;;
(def nbsp "\u00A0")

(def log (.-log js/console))

(defn add-event-listener [el type callback]
  (.addEventListener el type callback true))

(defn remove-event-listener [el type callback]
  (.removeEventListener el type callback true))

(defn get-element-by-id [id & [parent-element]]
  (.getElementById (or parent-element js/document) id))

(defn select-el-with-attribute [attr value]
  (.querySelector js/document (str "[" attr "='" value "']")))

(defn first-child-node [el]
  (first (.-childNodes el)))

(defn get-selection-object []
  (.getSelection js/window))

(defn add-time-out [func timeout]
  (.setTimeout js/window func timeout))

(defn insert-in-vector [coll index value]
  (if-not (empty? coll)
    (vec (concat (subvec coll 0 index)
           [value]
           (subvec coll index)))))

(defn remove-from-vector [coll index]
  (if-not (empty? coll)
    (vec (concat
           (subvec coll 0 index)
           (subvec coll (inc index))))))

(defn insert-in-string [string position value]
  (apply str (insert-in-vector (mapv #(str %) string) position (str value))))

(defn remove-from-string [string position]
  (let [new-str (apply str (remove-from-vector (mapv #(str %) string) position))]
    (if (= "" new-str)
      nbsp
      new-str)))


;;;;;;;;;;;;;;
;;;;Config;;;;
;;;;;;;;;;;;;;

(def text-editor-id "text-editor")

(defn text-editor-dom [] (get-element-by-id text-editor-id))

(def start-id 0)

(def cursor-state
  (atom {:start-block 0
         :end-block   0
         :start       0
         :end         0}))

(def text-editor-state
  (atom [{:content "hello there"
          :type    :div}]))

;;;;;;;;;;;;;;
;;;;Events;;;;
;;;;;;;;;;;;;;

(defn set-cursor-state! [new]
  (reset! cursor-state (merge @cursor-state new)))

(defn set-editor-state [new]
  (reset! text-editor-state (merge @text-editor-state new)))

(defn update-editor-state [path new]
  (reset! text-editor-state (assoc-in @text-editor-state path new)))

(def empty-block {:content ""
                  :type    :div})

(defn get-block-content [index]
  (get-in @text-editor-state [index :content]))

(defn split-string
  ([string start] (apply str (subvec (mapv identity string) start)))
  ([string start end] (apply str (subvec (mapv identity string) start end))))

(defn trim-string [string]
  (case (count string)
    0 " "
    1 string
    string))

(defn add-block-to-editor [index block sub-index]
  (let [this-block (get-block-content index)
        next-block (get-block-content (inc index))]
    (reset! text-editor-state
      (assoc-in @text-editor-state [index :content] (trim-string (split-string this-block 0 sub-index))))
    (reset! text-editor-state
      (insert-in-vector @text-editor-state (inc index) (assoc block :content (trim-string (split-string this-block sub-index)))))))

(defn collapse-block-to-left [index]
  (let [prev-block  (clojure.string/trimr (get-block-content (max 0 (dec index))))
        this-block (get-block-content index)]
    (log "torolnem a: " index "-et")
    (update-editor-state [(dec index) :content] (str prev-block this-block))
    (reset! text-editor-state (remove-from-vector @text-editor-state index))))

(defn collapse-block-to-right [index]
  (let [this-block (get-block-content index)
        next-block  (get-block-content (min
                                         (count @text-editor-state)
                                         (inc index)))]

    (update-editor-state [index :content] (str this-block next-block))
    (log "torolni jobbra: " (inc index) " - " (clj->js (remove-from-vector @text-editor-state (inc index))))
    ;(update-editor-state [index :content] (str this-block next-block))
    (set-cursor-state! {:start 0 :start-block index})
    (reset! text-editor-state (remove-from-vector @text-editor-state (inc index)))))

;;;;;;;;;;;;;;;;;;;;
;;;;Editor utils;;;;
;;;;;;;;;;;;;;;;;;;;

;We need the node so we can collapse at the start of it
(defn get-block-node [block-id]
  (first-child-node (select-el-with-attribute "data-block" block-id)))

;We collapse the selection to the start of a node with the respective id
(defn set-cursor-to-block-start [id]
  (.collapse (get-selection-object)
    (get-block-node id) 0))

;We need the block-id of the text-node which id edited.
(defn get-block-id [container]
  (-> container .-parentNode .-dataset .-block))

;When making a selection we need the start and end block of the selections
(defn get-block-boundaries [range]
  (let [start-container (-> range .-startContainer)
        end-container   (-> range .-endContainer)]
    [(get-block-id start-container)
     (get-block-id end-container)]))

;Deciding if the container is a text-node
(defn text-node? [container]
  (if (= "#text" (.-nodeName container))
    true false))

;Deciding if the selection both ends are text-nodes
(defn selection-text-node? [range]
  (let [start-container (-> range .-startContainer)
        end-container   (-> range .-endContainer)]
    (if (and
          (text-node? start-container)
          (text-node? end-container))
      true false)))


;Updating the cursor state
(defn set-range []
  ;We need to get the selection, and check if it has ranges.

  (let [selection  (.getSelection js/document)
        has-range? (not= 0 (.-rangeCount selection))]
    ;If it has range(s) we set the cursor-state.
    (if has-range?
      (let [the-range     (.getRangeAt selection 0)
            start         (.-startOffset the-range)
            end           (.-endOffset the-range)
            is-text-node? (selection-text-node? the-range)
            [start-block end-block] (get-block-boundaries the-range)]
        ;We check if it is a text-node, we are interested in only those.
        (if is-text-node?
          (set-cursor-state!
            {:start       start
             :end         end
             :start-block (int start-block)
             :end-block   (int end-block)}))))))

(defn set-cursor-position [row column]
  (let [selection (.getSelection js/window)]
    (.collapse selection (get-block-node row) column)))


;;;;;;;;;;;;;
;;Listeners;;
;;;;;;;;;;;;;

(defn enter-listener [e]
  (if (= (.-which e) 13)
    (let [block-index (:start-block @cursor-state)
          start (:start @cursor-state)]
      (do
        (.preventDefault e)
        (set-cursor-state! {:start-block (inc block-index) :start 0})
        (add-block-to-editor block-index empty-block start)))))
        ;(add-time-out #(set-cursor-to-block-start id) 0)))))

(defn add-enter-listener []
  (add-event-listener (text-editor-dom) "keydown" enter-listener))

(defn remove-enter-listener []
  (remove-event-listener (text-editor-dom) "keydown" enter-listener))

(defn get-block-content-length [index]
  (let [text (get-block-content index)
        only-space? (boolean (= text " "))]
    (if only-space?
      0
      (count text))))

(defn delete-content-backward []
  (let [the-keys [(:start-block @cursor-state) :content]]
    (cond
      ;If the cursor in not at start of the line
      (> (:start @cursor-state) 0)
      (do
        (update-editor-state the-keys (remove-from-string
                                        (get-in @text-editor-state the-keys)
                                        (dec (:start @cursor-state))))
        (set-cursor-state! {:start (max 0 (dec (:start @cursor-state)))}))
      ;If the cursor is at start [0] of first [0] line
      (and (= (:start @cursor-state) 0) (= (:start-block @cursor-state) 0))
      (do
        (update-editor-state the-keys (remove-from-string
                                        (get-in @text-editor-state the-keys)
                                        (dec (:start @cursor-state))))
        nil)
      ;If the cursor is at the start of some line
      (= (:start @cursor-state) 0) (do
                                     (let [block-index  (dec (:start-block @cursor-state))
                                           block-length (get-block-content-length block-index)]
                                       (collapse-block-to-left (:start-block @cursor-state))
                                       (set-cursor-state! {:start-block (dec (:start-block @cursor-state))
                                                           :start       block-length})))

      :else (do (log "exception delete")
                {}))))

(defn delete-to-right-from-text-editor []
  (let [the-keys [(:start-block @cursor-state) :content]]
    (update-editor-state the-keys
      (remove-from-string
        (get-in @text-editor-state the-keys)
        (:start @cursor-state)))))

(defn delete-content-forward []
  (cond
    ;If the cursor in not at start of the line
    (< (:start @cursor-state) (get-block-content-length (:start-block @cursor-state)))
    (do
      (delete-to-right-from-text-editor)
      (set-cursor-state! {:start (min
                                   (get-block-content-length (:start-block @cursor-state))
                                   (:start @cursor-state))}))

    ;If the cursor is at the ends on  some line
    (= (:start @cursor-state) (get-block-content-length (:start-block @cursor-state)))
    (let [this-block (:start-block @cursor-state)
          block-length-before-collapse (get-block-content-length this-block)]

      (collapse-block-to-right (:start-block @cursor-state))
      (log "mi folyik itt: ")
      (set-cursor-state! {:start-block this-block
                          :start       block-length-before-collapse}))

    :else (do (log "exception delete")
              {})))

(defn insert-text [event]
  (let [the-keys [(:start-block @cursor-state) :content]]
    (update-editor-state the-keys (insert-in-string
                                    (get-in @text-editor-state the-keys)
                                    (:start @cursor-state)
                                    (.-data event)))
    (set-cursor-state! {:start (inc (:start @cursor-state))})))

(defn input-listener [event]
  (.preventDefault event)
  (.removeAllRanges (get-selection-object))
  (case (.-inputType event)
    "insertText" (insert-text event)
    "deleteContentBackward" (delete-content-backward)
    "deleteContentForward" (delete-content-forward)
    (log (.-inputType event))))



(defn add-input-listener []
  (add-event-listener (text-editor-dom) "beforeinput" input-listener))

(defn remove-input-listener []
  (remove-event-listener (text-editor-dom) "beforeinput" input-listener))

(defn selection-listener [e]
  (.preventDefault e)
  (set-range))

(defn add-selection-listener []
  (add-event-listener js/document "selectionchange" selection-listener))

(defn remove-selection-listener []
  (remove-event-listener js/document "selectionchange" selection-listener))

(defn add-all-editor-listeners []
  (do (add-selection-listener)
      (add-input-listener)
      (add-enter-listener)))

(defn remove-all-editor-listeners []
  (do (remove-selection-listener)
      (remove-input-listener)
      (remove-enter-listener)))

;;;;;;;;;;
;;;;UI;;;;
;;;;;;;;;;

;Display cursor state for debugging purposes.
(defn display-cursor-state [state]
  [:div
   [:div [:strong "start: " (:start state)]]
   [:div [:strong "end: " (:end state)]]
   [:div [:strong "start-block: " (:start-block state)]]
   [:div [:strong "end-block: " (:end-block state)]]])

;Display editor state for debugging purposes.
(defn display-editor-state [state]
  [:div {:style {:height "200px" :overflow-y "scroll"}}
   (for [one state]
     ^{:key (str (random-uuid))}
     [:div (str one)])])

;For non existing modules
(defn non-existing []
  [:div {:style {:color "red"}} "non existing type added"])

;This is the modular part, here we can style how each block looks
(defn edn->rich-text [state]
  [:<>
   (for [[id {:keys [type content]}] (map-indexed #(vector %1 %2) state)]
     (case type
       :div ^{:key id} [:div {:style      {:min-height  "1em"
                                           :white-space "pre-wrap"}
                              :data-block id}
                        content]
       [non-existing]))])

;The content editable, which renders the edn structure in hiccup.

(defn all-listeners []
  (do
    (add-enter-listener)
    (add-input-listener)
    (add-selection-listener)
    (.focus (get-element-by-id text-editor-id))))


(defn content-editable []
  (let [last-block (atom 0)
        style      {:background "#222" :overflow-y "auto"
                    :display    "flex"}]
    (r/create-class
      {:component-did-update
       (fn []
         (set-cursor-position (:start-block @cursor-state) (:start @cursor-state))
         (reset! last-block (:start-block @cursor-state)))
       :reagent-render
       (fn []
         @cursor-state
         [:div {:style style}
          [:div {:content-editable                  true
                 :id                                text-editor-id
                 :on-focus                          #(add-all-editor-listeners)
                 :on-blur                           #(remove-all-editor-listeners)
                 :style                             {:padding "10px" :color "white" :height "500px"
                                                     :width   "100%"}
                 :suppress-content-editable-warning true}
           [edn->rich-text @text-editor-state]]])})))

;The text editor, with some debugging tools
(defn text-editor []
  [:div
   [display-cursor-state @cursor-state]
   [display-editor-state @text-editor-state]
   [content-editable]])