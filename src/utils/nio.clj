(in-ns 'utils.package)
(import
  '(java.nio.channels Channels FileLock ServerSocketChannel Selector)
  '(java.nio ByteBuffer CharBuffer)
  '(java.nio.charset CharsetEncoder CharsetDecoder Charset)
  '(java.io File InputStream OutputStream
     FileInputStream FileOutputStream RandomAccessFile)
  '(java.lang Byte))

(defmacro with-duplicate-buffer [buf & body]
  (let [[buf-new-name buf-val] (if (coll? buf)
                                 [(first buf) (second buf)]
                                 [buf buf])]
    `(let [~buf-new-name (.duplicate ~buf-val)]
       ~@body)))

(defmacro with-file-lock [[channel start end & shared] & body]
  `(let [lock# #^FileLock (.lock ~channel ~start ~end ~@shared)]
     (do  ~@body)
     (finally (.release lock#))))

(defn allocate-byte-buffer [size]
  (ByteBuffer/allocate size))
(defn allocate-dbyte-buffer [size]
  (ByteBuffer/allocateDirect size))
(defn clear [buffer]
  (.clear buffer))
(defn flip [buffer]
  (.flip buffer))

;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;
(defn- file-specifier? [item]
  (or (instance? File item)
    (instance? String item)))

(defn parse-nio [item & other]
  (cond
    (empty? other) (class item)
    true (let [dir (first other)]
           (cond
             (and (or (= dir :in) (= dir :out))
               (file-specifier? item)) :file
             (and
               (= dir :io)
               (file-specifier? item)) :random-access-file))))
(defmulti nio parse-nio)
(defmethod nio :file [file dir]
  (condp = dir
    :out (.getChannel (FileOutputStream. file))
    :in (.getChannel (FileInputStream. file))))
(defmethod nio :random-access-file [file dir]
  (.getChannel (RandomAccessFile. file "rw")))
(defmethod nio InputStream [stream]
  (Channels/newChannel stream))
(defmethod nio OutputStream [stream]
  (Channels/newChannel stream))

(defmacro with-nio [[nm arg direction] & body]
  `(let [~nm (nio ~arg ~direction)]
     (try
       (do ~@body)
       (finally (.close ~nm)))))

(defmacro with-nios [args & body]
  (if (single? args)
    `(with-nio ~@args ~@body)
    `(with-nio ~(first args)
       (with-nios ~(rest args)
         ~@body))))

(defmulti put-into-buffer
  (fn [v buf]
    (cond
      (integer? v) :integer
      (float? v) :float
      (string? v) :string
      (char? v) :char)))

(defmethod put-into-buffer :integer [v buf]
  (.putInt buf v))

(defmethod put-into-buffer :string [v buf]
  (dorun (map #(put-into-buffer % buf) v)))

(defmethod put-into-buffer :float [v buf]
  (.putDouble buf v))

(defmethod put-into-buffer :char [v buf]
  (.put buf (byte (int v))))

(defn into-buffer [buf s]
  (dorun
    (map (fn [i arg]
           (put-into-buffer arg buf))
      (take (.limit buf) (iterate inc 0)) s))
  buf)
;;;DECODING THE BYTE BUFFER
(defn byte-buffer-decoder [char-name]
  "Get a decoder. This is used to convert CharBuffer into ByteBuffer"
  (.newDecoder (Charset/forName char-name)))
(defn byte-buffer-encoder [char-name]
  "Get a encoder for the ByteBuffer. This is used to convert ByteBuffer into CharBuffer"
  (.newEncoder (Charset/forName char-name)))

(defn decode-byte-buffer [char-name byte-buffer]
  "Converts ByteBuffer into CharBuffer"
  (.decode (byte-buffer-decoder char-name) byte-buffer))
(defn encode-byte-buffer [char-name char-buffer]
  "Returns Charbuffer into ByteBuffer"
  (.encode (byte-buffer-encoder char-name) char-buffer))

;;CONVERT BUFFER TO STRING!
(defmulti to-string (fn [buf & _] (class buf)))
(defmethodl to-string ByteBuffer [buf &op char-name "ISO-8859-1"]
  (with-duplicate-buffer buf
    (.toString (decode-byte-buffer  char-name (.flip buf)))))

(comment
  "I was going to also do the SocketChannel stuff but I don't need that right now. So i haven't completed it"

  (defn open-selector [] (Selector/open))

  (defn register-channel [channel selector typ]
    (.register channel selector typ))

  (defmacro selector-key-case [k & cases]
    (let [constants
          (map (fn [item]
                 (condp = (first cases)
                   :accept SelectionKey/OP_ACCEPT
                   :read SelectionKey/OP_READ
                   :write SelectionKey/OP_WRITE
                   :connect Selectionkey/OP_CONNECT)))
          key-gen (gensym)
          bit-ands
          (map (fn [con case]
                 `((bit-and ~con ~key-gen) (do ~@(second case))))
            constants cases)]
      `(let [~key-gen (.readyOps ~k)]
         (cond ~@bit-ands))))

  (defn server-socket-channel [k]
    (cast ServerSocketChannel (.getChannel k)))
  (defnl accepted-socket-channel [k &op block? true]
    (doto (.accept (server-socket-channel k))
      (.configureBlocking false)))
  )

;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;














