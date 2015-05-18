#lang racket/base

(require drracket/tool
         racket/class
         racket/gui/base
         racket/unit
         racket/path
         racket/port
         net/url
         browser/external
         mrlib/switchable-button
         whalesong/whalesong-helpers)

(provide tool@)

(define COMPILE-RUN-LABEL "Whalesong Run")
(define PUBLISH-LABEL "Whalesong Publish")
(define PUBLISH-ENDPOINT-URL
  (string->url "http://bigbang.ccs.neu.edu/api/upload"))
(define MSG-COMPILE-UNSAVED-FILE "Please save the file before compiling")
(define MSG-COMPILE-WHALESONG-LANG "Language must be #lang whalesong")

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)

    (define compile-run-button-mixin
      (mixin (drracket:unit:frame<%>) ()
        (super-new)
        (inherit get-button-panel
                 get-editor
                 register-toolbar-button
                 get-definitions-text)

        ; checks if the current language is #lang whalesong
        (define (whalesong-lang?)
          (define defs-txt-in (open-input-text-editor (get-definitions-text)))
          ; TODO: do more precise check, probably dont need entire defs text
          (regexp-match "#lang whalesong" defs-txt-in))
          
        ; compile and run button
        (define compile-btn
          (new switchable-button%
               [label COMPILE-RUN-LABEL]
               [callback
                (λ (button)
                  (unless (whalesong-lang?)
                    (raise-user-error MSG-COMPILE-WHALESONG-LANG))
                  (compile-run
                   (send (get-editor) get-filename)))]
               [parent (get-button-panel)]
               [bitmap compile-bitmap]))

        (register-toolbar-button compile-btn)
        
        (send (get-button-panel)
              change-children
              (λ (l) (cons compile-btn (remq compile-btn l))))

        ; publish to web
        (define publish-btn
          (new switchable-button%
               [label PUBLISH-LABEL]
               [callback
                (λ (button)
                  (unless (whalesong-lang?)
                    (raise-user-error MSG-COMPILE-WHALESONG-LANG))
                  (publish-to-web
                    (port->string
                      (open-input-text-editor (get-definitions-text)))))]
               [parent (get-button-panel)]
               [bitmap compile-bitmap])) ; todo: publish bitmap

        (register-toolbar-button publish-btn)

        (send (get-button-panel)
              change-children
              (λ (l) (cons publish-btn (remq publish-btn l))))))


    ; get-file-url : path -> url-string
    ; Converts a given racket file path to a compiled html file url
    (define (get-file-url fpath)
      (string-append
       "file:///"
       (path->string (path-replace-suffix fpath ".html"))))

    ; compile-run : Maybe<path> -> Void
    ; Set output dir to fpath's basedir, compile file and open in the browser
    (define (compile-run fpath)
      (unless fpath (raise-user-error MSG-COMPILE-UNSAVED-FILE))
      (parameterize ([current-output-dir (path-only fpath)])
        (build-html-and-javascript fpath)
        (send-url (get-file-url fpath))))

    ; publish-to-web : string -> Void
    ; Publishes whalesong program to web and opens it in the browser
    (define (publish-to-web source)
      (send-url
        (port->string (post-pure-port PUBLISH-ENDPOINT-URL
                                      (string->bytes/utf-8 source)))))


    ; Bitmap for compile/run button icon
    (define compile-bitmap
      ; TODO: Make compile icon
      (let* ([bmp (make-bitmap 16 16)]
             [bdc (make-object bitmap-dc% bmp)])
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
