
;;;
;;; This file defines object types for use in our simulation
;;; world.  

;;--------------------
;; named-object
;; 
;; Named objects are the basic underlying object type in our
;; system. For example, persons, places, and things will all 
;; be kinds of (inherit from) named objects.
;;
;; Behavior (messages) supported by all named objects:
;;  - Answers #T to the question NAMED-OBJECT?
;;  - Has a NAME that it can return
;;  - Handles an INSTALL message
;;  - Handles a  DESTROY message

(define (make-named-object name . characteristics) ; symbol, string ->  named-object
  (let ((root-part (make-root-object)))
    (lambda (message)
      (case message
	((NAMED-OBJECT?) (lambda (self) #T))
	((NAME) (lambda (self) name))
	((CHARACTERISTICS) (lambda (self) characteristics))
	((INSTALL) (lambda (self) 'INSTALLED))
	((DESTROY) (lambda (self) 'DESTROYED))
	(else (find-method message root-part))))))

(define (create-named-object name characteristics)
  (create make-named-object name characteristics))

(define (names-of objects)
  ;; Given a list of objects, returns a list of their names.
  (map (lambda (x) (ask x 'NAME)) objects))


;;--------------------
;; container
;;
;; A container holds THINGS.  
;; 
;; This class is not really meant as a "stand-alone" object class;
;; rather, it is expected that other classes will inherit from the
;; container class in order to be able to contain things.
     
(define (make-container)		; void -> container
  (let ((root-part (make-root-object))
	(things '()))			; a list of THING objects in container
    (lambda (message)
      (case message
	((CONTAINER?) (lambda (self) #T))
	((THINGS) (lambda (self) things))
	((HAVE-THING?)
	 (lambda (self thing)		; container, thing -> boolean
	   (not (equal? #f (memq thing things)))))
	((ADD-THING)
	 (lambda (self new-thing)
	   (if (not (ask self 'HAVE-THING? new-thing))
	       (set! things (cons new-thing things)))
	   'DONE))
	((DEL-THING)
	 (lambda (self thing)
	   (set! things (delq thing things))
	   'DONE))
	(else (find-method message root-part))))))


;;--------------------
;; thing
;;
;; A thing is a named-object that has a LOCATION
;;
;; Note that there is a non-trivial installer here.  What does it do?

(define (make-thing name location characteristics)  
  ;; symbol, location, characteristics -> thing
  (let ((named-object-part (make-named-object name characteristics)))
    (lambda (message)
      (case message
	((THING?) (lambda (self) #T))
	((LOCATION) (lambda (self) location))
	((INSTALL)
	 (lambda (self)			; Install: synchronize thing and place
	   (ask (ask self 'LOCATION) 'ADD-THING self)
	   (delegate named-object-part self 'INSTALL)))
	((DESTROY)
	 (lambda (self)			; Destroy: remove from place
	   (ask (ask self 'LOCATION) 'DEL-THING self)
	   (delegate named-object-part self 'DESTROY)))
	((EMIT)
	 (lambda (self text)		; Output some text
	   (ask screen 'TELL-ROOM (ask self 'LOCATION)
		(append (list "At" (ask (ask self 'LOCATION) 'NAME))
			text))))
	(else (get-method message named-object-part))))))

(define (create-thing name location characteristics)
  (create make-thing name location characteristics))

;;--------------------
;; mobile-thing
;;
;; A mobile thing is a thing that has a LOCATION that can change.

(define (make-mobile-thing name location characteristics)
  ;; symbol, location, string -> mobile-thing
  (let ((thing-part (make-thing name location characteristics)))
    (lambda (message)
      (case message
	((MOBILE-THING?) (lambda (self) #T))
	((LOCATION)			; This shadows message to thing-part!
	 (lambda (self) location))
	((CHANGE-LOCATION)
	 (lambda (self new-location)
           (ask location 'DEL-THING self)
           (ask new-location 'ADD-THING self)
           (set! location new-location)))
	((ENTER-ROOM)
	 (lambda (self exit) #t))
	((LEAVE-ROOM)
	 (lambda (self exit) #t))
	((CREATION-SITE)
	 (lambda (self)
	   (delegate thing-part self 'LOCATION)))
	(else (get-method message thing-part))))))

(define (create-mobile-thing name location characteristics)
  (create make-mobile-thing name location characteristics))

;;--------------------
;; place
;;
;; A place is a container (so things may be in the place).
;;
;; A place has EXITS, which are passages from one place
;; to another.  One can retrieve all of the exits of a 
;; place, or an exit in a given direction from place. 
     
(define (make-place name characteristics) ; symbol, string -> place
  (let ((named-obj-part (make-named-object name characteristics))
	(container-part (make-container))
	(exits '()))			; a list of exits
    (lambda (message)
      (case message
	((PLACE?) (lambda (self) #T))
	((EXITS) (lambda (self) exits))
	((EXIT-TOWARDS)
	 (lambda (self direction)	   
	   (let ((ex (find-exit-in-direction exits direction)))
	     (if (and ex (ask ex 'HIDDEN?))
		 #F
		 ex))))
	((ADD-EXIT)
	 (lambda (self exit)		; place, symbol -> exit | #f
	   (let ((direction (ask exit 'DIRECTION)))
	     (cond ((ask self 'EXIT-TOWARDS direction)
		    (error (list name "already has exit" direction)))
		   (else 
		    (set! exits (cons exit exits))
		    'DONE)))))
	(else
	 (find-method message container-part named-obj-part))))))

(define (create-place name characteristics)
  (create make-place name characteristics))

;;------------------------------------------------------------
;; exit
;;
;; An exit leads FROM one place TO another in some DIRECTION.

(define (make-exit from direction to lock-proc)
  ;; idea behind lock-proc is that if false, then exit
  ;; always open, if a procedure, try it to see if it opens lock
  (let ((named-object-part (make-named-object direction))
	(partner '())
	(hidden? (if (eq? lock-proc #f)
		     #f
		     #t))
	(locked? (if (eq? lock-proc #f)
		     #f
		     #t)))
    (lambda (message)
      (case message
	((EXIT?)        (lambda (self) #T))
	((FROM)         (lambda (self) from))
	((TO)           (lambda (self) to))
	((DIRECTION)    (lambda (self) direction))
	((HIDDEN?)      (lambda (self) hidden?))
	((PARTNER)      (lambda (self) partner))
	((SET-PARTNER)  (lambda (self p) (set! partner p)))
	((USE)
	 (lambda (self whom)
	   (if locked?
	       ;; check to see if can unlock
	       ;;;;;;;; NOTE THE CHANGES!!!!!
	       ;;;; old code:  (begin (lock-proc self whom) (if locked?  .... ))
	       ;;;; new code: (if (lock-proc self whom) ...)
	       ;;;; this allows us to keep a door locked by default, 
	       ;;;; or lock-proc can change the state of self to get it to 
	       ;;;; unlock the door permanently
	       (if (lock-proc self whom)
		   ;; if lock-proc returns true, you may pass
		   (begin
		     (ask whom 'LEAVE-ROOM self)
		     (ask screen 'TELL-ROOM (ask whom 'LOCATION)
			  (list (ask whom 'NAME)
				"moves from" 
				(ask (ask whom 'LOCATION) 'NAME)
				"to"
				(ask to 'NAME)))
		     (ask whom 'CHANGE-LOCATION to)
                     (ask whom 'ENTER-ROOM (ask self 'PARTNER)))
		   ;; if lock-proc returns false, you may not pass
		   (ask screen 'TELL-ROOM (ask whom 'LOCATION)
			(list (ask whom 'NAME)
			      "tries to move from"
			      (ask (ask whom 'LOCATION) 'NAME)
			      "to"
			      (ask to 'NAME)
			      "but"
			      (ask (ask whom 'LOCATION) 'NAME)
			      "is locked in direction"
			      direction)))
	       ;; locked? is false... the door is not locked
	       (begin
		 (ask whom 'LEAVE-ROOM self)
		 (ask screen 'TELL-ROOM (ask whom 'LOCATION)
		      (list (ask whom 'NAME)
			    "moves from" 
			    (ask (ask whom 'LOCATION) 'NAME)
			    "to"
			    (ask to 'NAME)))
		 (ask whom 'CHANGE-LOCATION to)
                 (ask whom 'ENTER-ROOM (ask self 'PARTNER))
                 ))))
        ((CHANGE-STATE)
         (lambda (self)
           (if (eq? lock-proc #f)
               (begin (set! locked? #t) (set! lock-proc (lambda (self whom) #f)))
               (begin (set! locked? #f) (set! lock-proc #f)))))
	((CHANGE-HIDDEN)
	 (lambda (self)
	   (set! hidden? #f)))
	((INSTALL)
	 (lambda (self)
	   (ask (ask self 'FROM) 'ADD-EXIT self)
	   (delegate named-object-part self 'INSTALL)))
	(else (get-method message named-object-part))))))

(define (create-exit from direction to backdir locked-to? locked-back?)
  (let ((out (create make-exit from direction to locked-to?))
	(back (create make-exit to backdir from locked-back?)))
    (ask out 'SET-PARTNER back)
    (ask back 'SET-PARTNER out)
    (cons out back)))

(define (find-exit-in-direction exits dir)
  ;; Given a list of exits, find one in the desired direction.
  (cond ((null? exits) #f)
	((eq? dir (ask (car exits) 'DIRECTION))
	 (car exits))
	(else (find-exit-in-direction (cdr exits) dir))))

(define (random-exit place)
  (pick-random (ask place 'EXITS)))

