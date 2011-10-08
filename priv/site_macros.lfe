;;;------------------------------------------------------------------+
;;; static value management
;;;------------------------------------------------------------------+
(defsyntax cxn-arg
 ([] '__zog_cxn))
(defsyntax append-cxn-arg-to
 ([current-args] (++ current-args (list (cxn-arg)))))

(defsyntax object-hash-key
 ([] #b("__zfh")))  ; zfh = zog_fetched_hash

(defsyntax type-key
 ([] #b("__zog_type"))
 ([type] (tuple (type-key) type)))

(defsyntax idx-prefix
 ; this gets io-list-to-binary'd in idx-key
 ([type] (list '"zidx:_:" (namespace site) '":_:" type '":_:")))

(defsyntax idx-key
 ([type idx-name] (iolist_to_binary (list (idx-prefix type) idx-name))))

;(defun idx-key
; ([type idx-name] (iolist_to_binary (list (idx-prefix type) idx-name))))

;;;------------------------------------------------------------------+
;;; local binding of http vars (form and cookie)
;;;------------------------------------------------------------------+
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
            (lc ((<- tvar (: zog_page cookie ,(cxn-arg) ,vars-web))) ,vars-lc))
           (,vars-local cookie-vars-vals)
           (cookie-vars-names ',vars-local))
     ,@body-after-vars)))

;;;------------------------------------------------------------------+
;;; Create a multi-clause http function (lc to merge all cases to one fun)
;;;------------------------------------------------------------------+
(defmacro create-http-function (base-path method-subpath-bodies)
;;; (let* ((outer-lc (lc ((<- (list abc none) '((def bob) (hij meme)))) abc)))
 `(defun ,base-path
   ,@(lc ((<- (list method sub-path body) method-subpath-bodies))
      `([,method ,sub-path ,(cxn-arg)] ,body))))

;;; Add param to auto-make COOKIE or FORM vars for here
(defmacro remove-then-zip (remove-vars)
 `(let* ((cleanup (lambda (del acc)
                   (: lists keydelete del 1 acc)))
         (zipped  (: lists zip form-vars-names form-vars-vals))
         (removed (: lists foldl cleanup zipped ',remove-vars)))
   removed))

;;;------------------------------------------------------------------+
;;; Local (non-HTTP) functions and bindings
;;;------------------------------------------------------------------+
(defmacro local-function-noargs
  ([name args body] `(defun ,name ,(append-cxn-arg-to args) ,body))
  ([name body] `(defun ,name ,(append-cxn-arg-to '()) ,body)))

(defmacro local-function (name args-bodies)
 `(defun ,name
   ,@(lc ((<- (list args body) args-bodies))
      `(,(append-cxn-arg-to args) ,body))))

(defmacro local-bind
 ([local-name value bound-body]
 `(let ((,local-name ,value))
   ,@bound-body)))

(defmacro local-matcher (name initial-arg body)
 `(fletrec ((,name ,@body))
   (,name ,initial-arg)))

;;;------------------------------------------------------------------+
;;; Create a multi-clause http function (lc to merge all cases to one fun)
;;;------------------------------------------------------------------+
(defmacro safe-external-call
 ([mod func]      `(: ,mod ,func))
 ([mod func args] `(: ,mod ,func ,@args)))

(defmacro safe-call
 ([func]      `(,func ,(cxn-arg)))
 ([func args] `(,func ,@(append-cxn-arg-to args))))

(defmacro safe-redo
 ([func]      `(,func))
 ([func args] `(,func ,args)))

;;;------------------------------------------------------------------+
;;; Math (potentially pre-computable if no vars...)
;;;------------------------------------------------------------------+
(defmacro math
 (['% vals] `(rem ,@vals))
 (['+ vals] `(+ ,@vals))
 (['- vals] `(- ,@vals))
 (['/ vals] `(/ ,@vals))
 (['* vals] `(* ,@vals)))

;;;------------------------------------------------------------------+
;;; Misc. data structure helpers
;;;------------------------------------------------------------------+
(defmacro append-tups (original-var append-tups)
 `(: lists append ,original-var ,append-tups))

(defun atom-first
 ([((tuple something something-else) . xs)]
  (cons (tuple (to-atom something) something-else) (atom-first xs)))
 ([()] ()))

(defun to-atom
 ([x] (when (is_atom x)) x)
 ([x] (when (is_list x)) (list_to_atom x))
 ([x] (when (is_binary x)) (to-atom (binary_to_list x))))

;;;------------------------------------------------------------------+
;;; Built-in template rendering (NB: add i18n and riak-ification)
;;;------------------------------------------------------------------+
(defmacro output
 (['template (site . (template-name . bindings))] `(: zog_page ok ,(cxn-arg)
                     (: zog_template rn ,site ,template-name
                      (atom-first ,@bindings))))
 (['json arg]      `(: zog_page ok ,(cxn-arg) (: mochijson2 encode ,arg)))
 (['plain arg]     `(: zog_page ok ,(cxn-arg) ,args))
 ([arg]            `(: zog_page ok ,(cxn-arg) ,arg)))

;;;------------------------------------------------------------------+
;;; Manipulate the omni-present client connection parameter
;;;------------------------------------------------------------------+
(defmacro cxn
 ([function arg] `(call ,(cxn-arg) ,function ,arg))
 ([function] `(call ,(cxn-arg) ,function)))

(defmacro cxn-property
 ([#b("path")] `(cxn 'get 'path)))
;;; ([_] `'"invalid property requested"))

(defmacro user-property
 ([#b("name")] `(cxn 'userId)))

;;;------------------------------------------------------------------+
;;; Write, Find, Get, Update
;;;------------------------------------------------------------------+
(defmacro val (key proplist)
 ; keyfind retuns the entire found tuple, not just a value.
 ; use `element` to extract the value from the proplist
 `(element 2 (: lists keyfind ,key 1 ,proplist)))

(defmacro meta-idx-fields (type)
 `(list
   (tuple ,(iolist_to_binary (list '"zobj-type-" (namespace site))) ,type)
   (tuple #b("zobj-owner") (namespace site))))

(defun create-car-idx
 ; idx needs to store index vals for each key in indexes
 ; AND siteid for this object
 ; AND siteid-type for this object
 ([(idx . idxs) type pairs all-idx]
  ; get key from indexes, get value of it from pairs,
  ; make [(tuple siteid-type-index (lowercase value))]
  (create-car-idx idxs type pairs
   ; add zidx-SiteId-ObjType-Idx: Val to all-idx
   (cons (tuple (idx-key type idx) (val idx pairs)) all-idx)))
 ([() type _ all-idx]
  (: car index_bind (++ (meta-idx-fields type) all-idx))))

(defun binary-join-delim
 ([(bin . bins)]
  ; This is: iolist_to_binary([H, [<<":_:", B/binary>> || B <- T]]).
  (iolist_to_binary
   (list bin
    (lc ((<- b bins)) (binary (#b(":_:") binary) (b binary))))))) 

(defun de-hash-finds (objs)
 ; covert {Hash, Obj} to Hash as a member of the orddict Obj.
 (lc ((<- (tuple hash dict) objs))
  (: orddict store (object-hash-key) hash dict)))

(defmacro writer (type pairs indexes)
 ; car:write/2 -> take proplist and indexes, create statebox, store statebox
 `(: car write
   (cons ,(type-key type) ,pairs) (create-car-idx ,indexes ,type ,pairs '())))

(defmacro finder-all (type pairs)
 `(let* (((tuple keys values) (: lists unzip ,pairs)))
   (de-hash-finds
    (: car objects_by_index
     (idx-key ,type (binary-join-delim keys))
      (binary-join-delim values)))))

(defmacro finder-all-idx (type pairs index-spec)
 `(let* (((tuple _ values) (: lists unzip ,pairs)))
   (de-hash-finds
    (: car objects_by_index
     (idx-key ,type (binary-join-delim ,index-spec))
      (binary-join-delim values)))))

(defmacro finder-one (type pairs) 'ok)
 ; pull finder-all, but if result list > 1, whisper error "multiple returns."
 ; - OR - don't allow finder-one.  everything is a list, after all.

; NB: mutation as defsyntax means all mutations must be ready at compile time
(defsyntax convert-mutation
 (['list] list)  ; bug or feature?  the lc for mutations passes 'list too.

 ([(tuple 'set (tuple key val))]
  (tuple key (tuple 'write_value val)))

 ([(tuple 'add (tuple key val))]
  (tuple key (tuple 'add_to_set val)))

 ([(tuple 'remove (tuple key val))]
  (tuple key (tuple 'remove_from_set val)))

 ([(tuple 'clear key)]
  (tuple key 'clear_value))

 ([(tuple 'delete key)]
  (tuple key (tuple 'delete_with_value val))))

(defmacro extract-obj-hash (type-object)
 `(: orddict fetch (object-hash-key) ,type-object))

(defmacro updater-obj (update-obj mutations)
 `(: car update_object (extract-obj-hash ,update-obj)
   ,(lc ((<- m mutations)) `(convert-mutation ,m))))

(defmacro updater-find (find-spec mutations)
 'ok)

(defun dict-find (key dict)
 (case (: orddict find key dict)
  ((tuple 'ok value) value)
  ('error (binary))))

; getter makes no sense stand-alone.  it needs to be the recipe to equality

(defun getter
 ([extract-keys ()] ())
 ([extract-keys dict-or-dicts] (when (is_list (car dict-or-dicts)))
  (cons
   (keys-from-dict extract-keys (car dict-or-dicts))
    (getter extract-keys (cdr dict-or-dicts))))
 ([extract-keys dict] (when (is_tuple (car dict)))
  (keys-from-dict extract-keys dict)))

(defun keys-from-dict
 ([key dict] (when (is_binary key)) (dict-find key dict))
 ([keys dict] (when (is_list keys))
  (lc ((<- k keys)) (dict-find k dict))))

;;;------------------------------------------------------------------+
;;; Logging
;;;------------------------------------------------------------------+
(defmacro whisper-logger (args)
 `(: whisper say ,(namespace site) 'poopie 'poopin (list ,@args)))

;;;------------------------------------------------------------------+
;;; Uniquers
;;;------------------------------------------------------------------+
(defmacro binhex (what)
 `(list_to_binary (: mochihex to_hex ,what)))

(defmacro unique-big ()
 `(binhex (: crypto sha (term_to_binary (tuple (node) (now))))))

(defmacro unique-small ()
 `(let (((binary (first-half binary (size 10)) (_ binary)) ; only use 1/2 len
          (term_to_binary (tuple (node) (now)))))
   (binhex first-half)))

