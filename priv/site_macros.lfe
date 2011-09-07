; macros for creating http functions given methods, paths, and bodies.
(defsyntax http-function
 ([method path body] (create-http-function method path body))
 ([path body]        (create-http-function 'GET path body)))

(defmacro with-http-vars (method vars vars-lc body-after-vars)
 (let* ((lookup-fun (cond ((== 'GET method) 'form_queryvars)
                           ((== 'POST method) 'form_postvars))))
  `(let* ((vars-vals (lc ((<- tvar (: zog_page ,lookup-fun ',vars))) ,vars-lc))
          (,vars vars-vals)
          (vars-names ',vars))
    ,@body-after-vars)))

(defmacro create-http-function (method path body)
 `(defun ,(car path)
   ([,method ',path cxn] ,body)))

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
 ([func]      `(,func))
 ([func args] `(,func ,@args)))
