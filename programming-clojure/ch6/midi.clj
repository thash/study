(ns examples.datatypes.midi
  (:import [javax.sound.midi MidiSystem]))

(defprotocol MidiNote
  (to-msec [this tempo])
  (key-number [this])
  (play [this tempo midi-channel]))

(defn perform [notes & {:keys [tempo] :or {tempo 88}}]
  (with-open [synth (doto (MidiSystem/getSynthesizer).open)]
    (let [channel (aget (.getChannels synth) 0)]
      (doseq [note notes]
        (play note tempo channel)))))

(defrecord Note [pitch octave duration]
  MidiNote
  (to-msec [this tempo]
    (let [duration-to-bmp {1 240, 1/2 120, 1/4 60, 1/8 30, 1/16 15}]
      (* 1000 (/ (duration-to-bmp (:duration this))
                 tempo))))
  (key-number [this]
    (let [scale {:C 0, :C# 1, :Bb 10, :B 11}] ;; 略
      (+ (* 12 (inc (:octave this)))
         (scale (:pitch this)))))
  (play [this tempo midi-channel]
    (let [velocity (or :velocity this) 64)]
      (.noteOn midi-channel (key-number this) velocity)
      (Thread/sleep (to-msec this tempo)))))
