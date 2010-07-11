#lang racket

(require srfi/13
         srfi/14
         scheme/cmdline
         "modes/latex-mode.rkt"
         "modes/composition-mode.rkt"
         "modes/esmc-mode.rkt"
         "params.rkt")

;; EBOC by Paulo J. Matos <pocmatos@gmail.com>
(define version-str "0.1")

(working-directory (current-directory))

;(current-command-line-arguments (vector "-v" "-m" "latex" "casestudies/counter.mch"))
;(current-command-line-arguments (vector "-v" "-m" "latex" "casestudies/john/regs/regs.mch"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/counter.mch"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/ndetinit.mch"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/consts.mch"
;                                        "-o" "bound=10"))

;(current-command-line-arguments (vector "-v" "-m" "esmc" 
;                                        "-o" "file=../casestudies/eboc/mbutler/ctsctrl/ctsctrl.mch"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/john/regs/regs.mch"
;                                        "-o" "setbound=5"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/infinite.mch"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=../casestudies/eboc/jenny/has-db/has_db.mch"
;                                        "-o" "bound=100"
;                                        "-o" "setbound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/jukebox.mch"
;                                        "-o" "setbound=2"
;                                        "-o" "bound=100"))

;(current-command-line-arguments (vector "-v" "-d" "-m" "esmc" 
;                                        "-o" "file=./casestudies/eboc/john/huffman/huffman.mch"
;                                        "-o" "bound=10"))

(command-line
 #:program "eboc" ;; Should be name of executable
 #:once-each
 [("-v" "--verbose") "Verbose messages"
                     (verbose-mode #t)]
 [("-n" "--no-header") "Doesn't print header information."
                       (no-header? #t)]
 [("-d" "--debug") "Prints debug information"
                   (print-debug? #t)]
 [("-m" "--mode") mode  "Mode to run on (esmc, latex, composition)"
                  (execution-mode (string->symbol mode))]
 [("-s" "--spaces-tab") st "Number of spaces per tab in output"
                        (spaces/tab (string->number st))]
 #:multi 
 [("-o" "--mode-option") opt ; flag takes one argument
                         "Add an option to current mode."
                         (let* ([str-tokens (string-tokenize opt (char-set-difference char-set:graphic (char-set #\=)))]
                                [key (string->symbol (first str-tokens))]
                                [val (apply string-append (add-between (rest str-tokens) "="))])
                           (mode-options (cons (cons key val) (mode-options))))])

;; This the main function
;(: main (-> Any))
(define (main)
  (unless (no-header?)
    (printf "EBoC - Event-B Model Checker~n")
    (printf "---- Version: ~a ----~n" (string-pad version-str 9))
    (printf "---- by: Paulo J. Matos ----~n~n"))
  
  (when (verbose-mode)
    (printf "Running in mode ~a.~n" (execution-mode)))
  
  (case (execution-mode) 
    [(latex) (latex-mode (mode-options))]
    [(composition) (composition-mode (mode-options))]
    [(esmc) (esmc-mode (mode-options))]
    [else (fprintf (current-error-port) "Unsupported mode: ~a. Exiting...~n" (execution-mode))]))

;; Run main
(main)
