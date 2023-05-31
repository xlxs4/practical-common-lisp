(in-package :cl-user)

(defpackage :xlxs4.id3v2
  (:use :common-lisp
        :xlxs4.binary
        :xlxs4.pathnames)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :song
   :year
   :size
   :translated-genre))
