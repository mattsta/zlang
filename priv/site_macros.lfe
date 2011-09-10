(defsyntax cxn-arg
 ([] '__zog_cxn))
(defsyntax append-cxn-arg-to
 ([current-args] (++ current-args (list (cxn-arg)))))

(defmacro with-http-vars (method vars-type vars vars-lc body-after-vars)
 (let* ((lookup-fun (cond ((== 'GET method) 'form_queryvars)
                           ((== 'POST method) 'form_postvars))))
  `(let* ((vars-vals (lc ((<- tvar (: zog_page ,lookup-fun ',vars))) ,vars-lc))
          (,vars vars-vals)
          (vars-names ',vars))
    ,@body-after-vars)))

(defmacro create-http-function (base-path method-subpath-bodies)
; (let* ((outer-lc (lc ((<- (list abc none) '((def bob) (hij meme)))) abc)))
 `(defun ,base-path
   ,@(lc ((<- (list method sub-path body) method-subpath-bodies))
      `([,method ,sub-path ,(cxn-arg)] ,body))))

(defmacro remove-from-vars-then (remove-vars action)
 `(let* ((cleanup (lambda (del acc)
                   (: lists keydelete del 1 acc)))
         ([zipped] (: lists zip vars-names vars-vals))
         ([removed] (: lists foldl cleanup zipped ',remove-vars)))
   (case ,action
    (['zip-name-values] removed)
    ([_] '()))))

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
 (['json args] `(: zog_page ok (cxn-arg) (: mochijson2 encode ,args))))
