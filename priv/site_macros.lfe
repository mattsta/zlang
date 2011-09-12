(defsyntax cxn-arg
 ([] '__zog_cxn))
(defsyntax append-cxn-arg-to
 ([current-args] (++ current-args (list (cxn-arg)))))

; vars-type: form or cookie
; vars-local: local names of these variables
; vars-web: names from the web form
; vars-lc: a comprehension to run over resolved vars-web before returning
; when free time => auto-generate var values by concatenating atoms... THEN
;  we can reduce this back to a non-match macro and use lookup-fun for cookies
;  too.  just needs a dumb compile time atom to list, concatenate, list to atom.
(defmacro with-http-vars
  (['form method vars-local vars-web vars-lc body-after-vars]
   (let* ((lookup-fun (cond ((== 'GET method) 'form_queryvars)
                            ((== 'POST method) 'form_postvars))))
    `(let* ((form-vars-vals
             (lc ((<- tvar
                  (: zog_page ,lookup-fun ,(cxn-arg) ,vars-web))) ,vars-lc))
            (,vars-local form-vars-vals)
            (form-vars-names ',vars-local))
      ,@body-after-vars)))
  (['cookie method vars-local vars-web vars-lc body-after-vars]
   `(let* ((cookie-vars-vals
            (lc ((<- tvar (: zog_page cookie ,(cxn-arg) ',vars-web))) ,vars-lc))
           (,vars-local cookie-vars-vals)
           (cookie-vars-names ',vars-local))
     ,@body-after-vars)))

(defmacro create-http-function (base-path method-subpath-bodies)
; (let* ((outer-lc (lc ((<- (list abc none) '((def bob) (hij meme)))) abc)))
 `(defun ,base-path
   ,@(lc ((<- (list method sub-path body) method-subpath-bodies))
      `([,method ,sub-path ,(cxn-arg)] ,body))))

; Add param to auto-make COOKIE or FORM vars for here
(defmacro remove-then-zip (remove-vars)
 `(let* ((cleanup (lambda (del acc)
                   (: lists keydelete del 1 acc)))
         (zipped  (: lists zip form-vars-names form-vars-vals))
         (removed (: lists foldl cleanup zipped ',remove-vars)))
   removed))

(defmacro safe-external-call
 ([mod func]      `(: ,mod ,func))
 ([mod func args] `(: ,mod ,func ,@args)))

(defmacro safe-call
 ([func]      `(,func ,(cxn-arg)))
 ([func args] `(,func ,@(append-cxn-arg-to args))))

(defmacro local-function (name args body)
 `(defun ,name ,(append-cxn-arg-to args) ,body))

(defmacro local-bind (local-name value bound-body)
 `(let ((,local-name ,value))
   ,@bound-body))

(defmacro math
 (['+ vals] `(+ ,@vals))
 (['- vals] `(- ,@vals))
 (['/ vals] `(/ ,@vals))
 (['* vals] `(* ,@vals)))

(defmacro output
 (['template args] `(: zog_page ok ,(cxn-arg) (: zog_template rn ,@args)))
 (['json arg]      `(: zog_page ok ,(cxn-arg) (: mochijson2 encode ,arg)))
 (['plain arg ]    `(: zog_page ok ,(cxn-arg) ,args))
 ([arg]            `(: zog_page ok ,(cxn-arg) ,arg)))
