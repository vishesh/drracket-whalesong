#lang racket/gui

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         browser/external
         mrlib/switchable-button
         whalesong/whalesong-helpers)

(provide tool@)

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
                          (label "Whalesong Run")
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


    (define (get-output-filename fpath)
      (regexp-replace #rx"[.](rkt|ss)$" (path->string fpath) ".html"))

    (define (get-output-directory fpath)
      (match-let-values (((output-dir _ _) (split-path fpath)))
                        output-dir))

    (define (compile-run fpath)
      (if (false? fpath)
        (message-box "Error!" "Plase save the file before compiling!")
        (begin
          (current-output-dir (get-output-directory fpath))
          (build-html-and-javascript fpath)
          (send-url (get-output-filename fpath)))))

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
