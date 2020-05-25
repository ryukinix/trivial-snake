;;;; trivial-snake.asd

(asdf:defsystem #:trivial-snake
  :description "trivial-snake"
  :author "Manoel Vilela"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "trivial-snake")))
