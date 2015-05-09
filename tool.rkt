#lang racket/gui

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         browser/external
         mrlib/switchable-button
         whalesong/whalesong-helpers)

(provide tool@)

(define ERROR "Error!")
(define COMPILE-RUN-LABEL "Whalesong Run")
(define MSG-COMPILE-UNSAVED-FILE "Please save the file before compiling")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define compile-run-button-mixin
      (mixin (drracket:unit:frame<%>) ()
             (super-new)
             (inherit get-button-panel
                      get-editor)
             (inherit register-toolbar-button)

             (let ((btn
                     (new switchable-button%
                          (label COMPILE-RUN-LABEL)
                          (callback
                            (λ (button)
                              (compile-run
                                (send (get-editor) get-filename))))
                          (parent (get-button-panel))
                          (bitmap compile-bitmap))))
               (register-toolbar-button btn)
               (send (get-button-panel) change-children
                     (λ (l)
                       (cons btn (remq btn l)))))))

    ; get-file-url : string -> url-string
    ; Converts a given path string to a valid url string
    (define (get-file-url file-str) (string-append "file:///" file-str))

    ; get-output-filename : path -> string
    ; Returns the filename of generated html file
    (define (get-output-filename fpath)
      (regexp-replace #rx"[.](rkt|ss)$" (path->string fpath) ".html"))

    ; get-output-directory : path -> path
    ; Return base directory of fpath
    (define (get-output-directory fpath)
      (match-let-values (((output-dir _ _) (split-path fpath)))
                        output-dir))

    ; compile-run : path -> Void
    ; Sets output directory to fpath's basedir, compile the file and open
    ; on browser
    (define (compile-run fpath)
      (if (false? fpath)
        (message-box ERROR MSG-COMPILE-UNSAVED-FILE)
        (begin
          (current-output-dir (get-output-directory fpath))
          (build-html-and-javascript fpath)
          (send-url (get-file-url (get-output-filename fpath))))))

    ; Bitmap for compile/run button icon
    (define compile-bitmap
      ; TODO: Make compile icon
      (let* ((bmp (make-bitmap 16 16))
             (bdc (make-object bitmap-dc% bmp)))
        (send bdc erase)
        (send bdc set-smoothing 'smoothed)
        (send bdc set-pen "black" 1 'transparent)
        (send bdc set-brush "blue" 'solid)
        (send bdc draw-ellipse 2 2 8 8)
        (send bdc set-brush "red" 'solid)
        (send bdc draw-ellipse 6 6 8 8)
        (send bdc set-bitmap #f)
        bmp))

    (define (phase1) (void))
    (define (phase2) (void))

    (drracket:get/extend:extend-unit-frame compile-run-button-mixin)))
