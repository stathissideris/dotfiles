(ns emacs
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            clojure.pprint)
  (:use [clojure.java.browse])
  (:import [java.awt Toolkit]
           [java.awt.datatransfer StringSelection]
           [java.net URLClassLoader URL]))

(defn bye [] (System/exit 0))
(defn pwd [] (System/getProperty "user.dir"))

(def CLOJUREDOCS-VERSION "1.3.0")

(set! *print-length* 300)
(alter-var-root #'*print-length* (constantly 300))

(defn sanitise-symbol-name [name]
    (-> name
        (str/replace #"\?" "_q")))

(defn clojuredocs
  "Look up symbol in clojuredocs.org"
  [symbol-name]
  (let [sym (-> symbol-name symbol resolve)
        ns (str (:ns (meta sym)))
        sym-name (:name (meta sym))
        url
        (cond
          (nil? sym)
          (str "http://clojuredocs.org/search?q="
               (sanitise-symbol-name symbol-name))
          
          (= "clojure.core" ns)
          (str "http://clojuredocs.org/clojure_core/"
               CLOJUREDOCS-VERSION "/" ns
               "/" (sanitise-symbol-name sym-name) "#examples")
          
          :else
          (str "http://clojuredocs.org/search?q="
               (sanitise-symbol-name sym-name)))]
    (browse-url url)))

(defmacro without-print-limit [& body]
  `(with-bindings {#'clojure.core/*print-length* nil}
     ~@body))

(defmethod print-dup java.util.Date [o w]
           (print-ctor o (fn [o w] (print-dup (.getTime o) w)) w))

(defn to-file
  "Save a clojure form to a file"
  [file form & pretty?]
  (without-print-limit
    (binding [*print-dup* true]
     (with-open [w (java.io.FileWriter. (java.io.File. file))]
       (if pretty?
         (clojure.pprint/pprint form w)
         (print-dup form w))))))

(defn from-file
  "Load a clojure form from file."
  [file]
  (with-open [r (java.io.PushbackReader. (java.io.FileReader. (java.io.File. file)))]
     (read r)))

(defn to-clipboard [text]
  "Send a string to the system clipboard."
  (let [text (StringSelection. text)]
    (-> (Toolkit/getDefaultToolkit)
        (.getSystemClipboard)
        (.setContents text text))))

;;;;;;;;;;;;; time

(defn days->millis [x] (* x 1000 60 60 24))
(defn hours->millis [x] (* x 1000 60 60))
(defn minutes->millis [x] (* x 1000 60))
(defn seconds->millis [x] (* x 1000))

(defn millis->days [m] (/ m 1000 60 60 24))
(defn millis->hours [m] (/ m 1000 60 60))
(defn millis->minutes [m] (/ m 1000 60))
(defn millis->seconds [m] (/ m 1000))

(defn now-millis [] (.getTime (java.util.Date.)))

(defn millis->duration [m]
  (let [days (int (millis->days m))

        m (- m (days->millis days))
        hours (int (millis->hours m))

        m (- m (hours->millis hours))
        minutes (int (millis->minutes m))

        m (- m (minutes->millis minutes))
        seconds (int (millis->seconds m))]
    {:days days
     :hours hours
     :minutes minutes
     :seconds seconds}))

(defn duration-str
  "Converts a time duration from millis to a human-readable duration (down to
   seconds).

   e.g.
   > (duration-str (* 1000 60 20))
   \"20 minutes\""
  [m]
  (if (< m 1000)
    "0 seconds"
    (let [dur (->> (millis->duration m)
                   (remove (comp zero? second))
                   (map (fn [[k v]] [k (str v " " (name k))]))
                   (into {}))]
      (clojure.string/join
       ", "
       (remove nil? ((juxt :days :hours :minutes :seconds) dur))))))

;;; csv ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn load-csv [filename & options]
 (with-open [in-file (io/reader filename)]
   (doall
    (apply csv/read-csv in-file options))))

(defn save-csv [filename data & options]
 (with-open [out-file (io/writer filename)]
   (apply csv/write-csv out-file data options)))

(defn transform-csv
  "Load CSV, transform by applying a function, save elsewhere as CSV."
  [fun in-filename out-filename]
  (with-open [in-file (io/reader in-filename)
              out-file (io/writer out-filename)]
    (doall
     (csv/write-csv out-file
                    (fun
                     (csv/read-csv in-file))))))

;;; notifications ;;;;;;;;;;;;;;;;;

(defn icon->image [icon]
  (let [w (.getIconWidth icon)
        h (.getIconHeight icon)
        img (-> (java.awt.GraphicsEnvironment/getLocalGraphicsEnvironment)
                (.getDefaultScreenDevice)
                (.getDefaultConfiguration)
                (.createCompatibleImage w h))
        gfx (.createGraphics img)
        icon (.paintIcon icon nil gfx 0 0)]
    (.dispose gfx)
    img))

(defn remove-all-tray-icons []
  (let [tray (java.awt.SystemTray/getSystemTray)]
   (doseq [i (seq (.getTrayIcons tray))]
     (.remove tray i))))

(try
  (remove-all-tray-icons)
  (catch Exception e))

(def tray-icon
  (try
    (let [icon (java.awt.TrayIcon.
                (icon->image (.getIcon (javax.swing.UIManager/getDefaults) "FileView.computerIcon"))
                #_(.getImage (java.awt.Toolkit/getDefaultToolkit) "tray.gif")
                "REPL notifications" nil)]
      (.add (java.awt.SystemTray/getSystemTray) icon)
      icon)
    (catch Exception e)))

(def tray-msg-type-map
  {:none java.awt.TrayIcon$MessageType/NONE
   :info java.awt.TrayIcon$MessageType/INFO
   :warning java.awt.TrayIcon$MessageType/WARNING
   :error java.awt.TrayIcon$MessageType/ERROR})

(defn notify
  "Send a notification to the system tray."
  [msg & [title type]]
  (let [type (or type :none)
        type (or (tray-msg-type-map type) (:none tray-msg-type-map))]
   (.displayMessage tray-icon title msg type))
  msg)

(defmacro count-time
  "Like (time), but returns the millis instead of printing."
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     {:time (/ (double (- (. System (nanoTime)) start#)) 1000000.0)
      :value ret#}))

(defmacro with-notify
  "Execute the passed code, report completion or failure in the system
  tray, including the total time of execution."
  [& body]
  `(try (do (let [~'res (count-time (do ~@body))
                  ~'value (:value ~'res)
                  ~'time (duration-str (:time ~'res))]
              (notify (str ~(apply pr-str body) "\n\nTime: " ~'time)
                      "REPL job success" :info)
              ~'value))
        (catch Exception ~'e
          (do
            (notify (or (.getMessage ~'e) "No message.") "REPL job error" :error)
            (throw ~'e)))))
(try
  (notify "clojure-dev.clj loaded" "emacs" :info)
  (catch Exception e))

;;;; parsing

(defn parse-int [s] (Integer/parseInt s))
(defn parse-long [s] (Long/parseLong s))

(defn truncate-string [s limit]
  (if (<= (.length s) limit)
    s
    (str (.substring s 0 limit) "...")))

(defn limit-strings
  ([t] (limit-strings t 100))
  ([t limit]
     (walk/postwalk (fn [x]
                      (cond (not (string? x)) x
                            :else (truncate-string x limit))) t)))

;;;; reverse engineering

(defn hierarchy-seq [cl]
  (if-not (nil? cl)
    (cons cl (lazy-seq (hierarchy-seq (.getSuperclass cl))))
    []))

(defn inspect-class [cl]
  (map
   first
   (partition-by
    identity
    (map (fn [x]         
           (str
            (java.lang.reflect.Modifier/toString (.getModifiers x))
            " "
            (.getName (.getReturnType x))
            " "
            (.getName x)
            "("
            (apply str (interpose ", " (map #(.getName %) (.getParameterTypes x))))
            ")"))
         (sort-by #(.getName %)
                  (mapcat #(.getDeclaredMethods %) (hierarchy-seq cl)))))))

;;;; testing

(defn random-vec
  ([] (random-vec 10))
  ([size] (vec (take size (map int (repeatedly #(rand 100)))))))

(defn random-list
  ([] (random-list 10))
  ([size] (take size (map int (repeatedly #(rand 100))))))

(defn random-str
  ([] (random-str 4))
  ([size]
     (apply
      str
      (take size
            (repeatedly #(char (int (+ 97 (rand 26)))))))))

(defn random-map
  ([] (random-map 10 4))
  ([size key-size]
     (let [rand-key #(keyword (random-str key-size))]
      (zipmap
       (repeatedly rand-key)
       (random-vec size)))))

#_(defn classes-in-jar [filename]
    (filter #(.endsWith % ".class")
            (map #(.getName %)
                 (enumeration-seq
                  (.entries (JarFile. filename))))))

;;refactoring

(defn to-date-time [s]
  (let [d (map parse-int (rest (re-find #"(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d).(\d\d\d)" s)))]
    `(time/date-time ~@d)))

(defn lists->vectors [r]
  (walk/walk #(if (list? %) (vec %) %) vec r))

(defn grab-line [filename line-number]
  (-> filename
      io/file
      io/reader
      line-seq
      (->> (into []))
      (nth line-number)))

(defn who-called-me
  ([] (who-called-me 25))
  ([depth]
     (try (throw (ex-info "probe" {}))
          (catch Exception e (clojure.repl/pst e depth)))))

(defn trace-fn [tag fun]
  (fn [& args]
    (println (format "(%s %s)" (name tag) (apply pr-str args)))
    (let [res (apply fun args)]
      (println (format "%s=> %s" (name tag) (pr-str res)))
      res)))
