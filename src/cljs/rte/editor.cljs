(ns rte.editor
  (:require
    [reagent.core :as r :refer [atom]]))

;;;;;;;;;;;;;
;;;;Utils;;;;
;;;;;;;;;;;;;

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
    (if (not= 0 index)
      (vec (concat
             (subvec coll 0 (dec index))
             (subvec coll index)))
      coll)))

(defn insert-in-string [string position value]
  (apply str (insert-in-vector (mapv #(str %) string) position (str value))))

(defn remove-from-string [string position]
  (log position " - "(clj->js (remove-from-vector (mapv #(str %) string) position)))
  (apply str (remove-from-vector (mapv #(str %) string) position)))

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

(def nbsp "\u00A0")

(def empty-block {:content ""
                  :type    :div})

(defn get-block-content [index]
  (get-in @text-editor-state [index :content]))

(defn split-string
  ([string start] (apply str (subvec (mapv identity string) start)))
  ([string start end] (apply str (subvec (mapv identity string) start end))))

(defn add-block-to-editor [index block sub-index]
  (let [this-block (get-block-content index)
        next-block (get-block-content (inc index))]
    (reset! text-editor-state (assoc-in @text-editor-state [index :content] (str (split-string this-block 0 sub-index) nbsp)))
    (reset! text-editor-state
      (insert-in-vector @text-editor-state
        (inc index)
        (assoc block :content (str (split-string this-block sub-index) nbsp))))))


(defn remove-block-from-editor [index]
  (let [prev-block (get-block-content (max 0 (dec index)))
        this-block (get-block-content index)]
    (log (str prev-block " - " this-block index))
    (update-editor-state [(dec index) :content] (str prev-block this-block))
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
    (add-time-out #(.collapse selection (get-block-node row) column) 0)))


;;;;;;;;;;;;;
;;Listeners;;
;;;;;;;;;;;;;

(defn enter-listener [e]
  (if (= (.-which e) 13)
    (let [id (:start-block @cursor-state)]
      (do
        (.preventDefault e)
        (set-cursor-state! {:start-block (inc id)})
        (add-block-to-editor id empty-block (:start @cursor-state))
        (add-time-out #(set-cursor-to-block-start id) 0)))))

(defn add-enter-listener []
  (add-event-listener (text-editor-dom) "keydown" enter-listener))

(defn remove-enter-listener []
  (remove-event-listener (text-editor-dom) "keydown" enter-listener))

(defn on-delete-backwards []
  (cond
    (> (:start @cursor-state) 0) (set-cursor-state! {:start (max 0 (dec (:start @cursor-state)))})
    (= (:start @cursor-state) 0) (do
                                   (remove-block-from-editor (:start-block @cursor-state))
                                   (set-cursor-state! {:start-block (max 0 (dec (:start-block @cursor-state)))
                                                       :start       0}))

    :else (do (log "exception delete")
              {})))

(defn input-listener [event]
  (let [the-keys [(:start-block @cursor-state) :content]]
    (.removeAllRanges (get-selection-object))
    (log "input: " (.-inputType event))
    (case (.-inputType event)
      "insertText" (do
                     (update-editor-state the-keys (insert-in-string
                                                     (get-in @text-editor-state the-keys)
                                                     (:start @cursor-state)
                                                     (.-data event)))
                     (set-cursor-state! {:start (inc (:start @cursor-state))}))
      "deleteContentBackward" (do
                                (update-editor-state the-keys (remove-from-string
                                                                (get-in @text-editor-state the-keys)
                                                                (:start @cursor-state)))
                                (on-delete-backwards))

      (log (.-inputType event)))))



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
       :div ^{:key id} [:div {:style      {:white-space "pre"}
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
        style      {:background "#222" :overflow-y "auto"}]
    (r/create-class
      {:component-did-update
       (fn []
         (if (= @last-block (:start-block @cursor-state))
           (set-cursor-position (:start-block @cursor-state) (:start @cursor-state))
           (set-cursor-position (:start-block @cursor-state) 0))
         (reset! last-block (:start-block @cursor-state)))
       :reagent-render
       (fn []
         [:div {:style style}
          [:div {:content-editable                  true
                 :id                                text-editor-id
                 :on-focus                          #(add-all-editor-listeners)
                 :on-blur                           #(remove-all-editor-listeners)
                 :style                             {:padding "10px" :color "white" :height "500px"}
                 :suppress-content-editable-warning true}
           [edn->rich-text @text-editor-state]]])})))

;The text editor, with some debugging tools
(defn text-editor []
  [:div
   [display-cursor-state @cursor-state]
   [display-editor-state @text-editor-state]
   [content-editable]])