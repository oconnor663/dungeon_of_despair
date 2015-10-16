;;The Dungeon of Despair

;; A two-person project by John O'Connor and Will Wilson


;;;;;;;--------------------------------------DOCUMENTATION--------------------------------------------------------
;;This is a MUD-like game in which the player's avatar wanders around the world slaying his enemies.
;;The first thing that happens is that the player chooses his character, either Klaus the Strong, or Toshi
;;the wise. Then the player is inserted into the world and given a command line to interact with it.
;;Commands are as follows:
;;NB: AN INCORRECT NUMBER OF ARGUMENTS MAY CRASH THE GAME. YOU HAVE BEEN WARNED.
;;(move [exit])
;;(attack [target])
;;(grab [item])
;;(drop [item])
;;(items)
;;(equipment)
;;(wield [weapon in inventory])
;;(wear [armor in inventory])
;;(abilities)
;;(ability [name] {[target]})  
;;(inspect [target])            <---Enjoy the game! Inspect everything!
;;(use [item in inventory])   
;;()    <- to pass
;;(quit)

;;;;;;;--------------------------------Specification--------------------------------------------------
;;This game uses amost the exact same object system as problem set 3. Rooms, exits, and mobile objects are all employed.
;;The combatants are simply subtypes of mobile object that know how to keep track of health and attack the player.
;;The command line is parsed in almost exactly the same way as in problem set 4. A triage function (turn) sends commands
;;off to the appropriate function for execution.

;;Turns are done with the clock object, which sends a callback to each combatant and the player with every tick.
;;The clock will keep ticking until the final boss is dead.



(load "Resources/classdefs.scm")
(load "Resources/objects.scm")

;----------------------Objects-----------------------
;;First a little function for finding objects in a container. Ignores players.
(define (find-in items name)
  ;; Given a list of exits, find one in the desired direction.
  (cond ((null? items) #f)
	((and (eq? name (ask (car items) 'NAME))
              (not (is-a (car items) 'PLAYER?)))
	 (car items))
	(else (find-in (cdr items) name))))

;---------Corpses
;;A corpse is little more than an object which will be created
;;when a combatant dies. It has no special methods.
(define (make-corpse name location characteristics)
  (let ((mobile-part (make-mobile-thing name location characteristics))) ;Zombies!
    (lambda (message)
      (case message
        ((CORPSE?) (lambda (self) #t))
        (else (get-method message mobile-part))))))

(define (create-corpse name location characteristics)
  (create make-corpse name location characteristics))

(define (create-corpse-of thing)
  (let ((name (if (symbol? (ask thing 'NAME))
                  (symbol->string (ask thing 'NAME))
                  (ask thing 'NAME))))  ;puts "corpse-of-" on the name
    (create-corpse (string->symbol(string-append "corpse-of-"
                                  name))
                   (ask thing 'LOCATION)
                   (string-append " " name " is no more. "
                                  name " has ceased to be."))))
                 
;---------Weapon/Armor/Ability/Usable-item Objects
(define (make-weapon name location characteristics damage)
  (let ((mobile-part (make-mobile-thing name location characteristics)))
    (lambda (message)
      (case message
        ((WEAPON?) (lambda (self) #t))
        ((DAMAGE) (lambda self damage))
        (else (get-method message mobile-part))))))

(define (create-weapon name location characteristics damage)
  (create make-weapon name location characteristics damage))

(define (make-armor name location characteristics rating)
  (let ((mobile-part (make-mobile-thing name location characteristics)))
    (lambda (message)
      (case message
        ((ARMOR?) (lambda (self) #t))
        ((ARMOR) (lambda (self) rating))
        (else (get-method message mobile-part))))))

(define (create-armor name location characteristics rating)
  (create make-armor name location characteristics rating))

(define (make-ability name characteristics function) ;the function should always take the player as
  (let ((named-part (make-named-object name characteristics)))                  ;the first argument
    (lambda (message)
      (case message
        ((ABILITY?) (lambda (self) #t))
        ((EFFECT) (lambda (self) function))
        (else (get-method message named-part))))))

(define (make-usable name location characteristics function) ;the function should always take the player as
  (let ((mobile-part (make-mobile-thing name location characteristics)))                ;the first argument
    (lambda (message)
      (case message
        ((USABLE?) (lambda (self) #t))
        ((EFFECT) (lambda (self) function))
        (else (get-method message mobile-part))))))

(define (create-usable name location characteristics function)
  (create make-usable name location characteristics function))

;---------Fighting Objects
;;The player object will be the user's avatar. It will keep track of things like
;;what he is wearing and whether he is in combat.
(define (make-player name location characteristics)
  (let ((mobile-part (make-mobile-thing name location characteristics))
        (container-part (make-container))
        (health 100)
        (combat? #f)
        (enemies ())
        (armor 'none)
        (weapon 'none)
        (abilities ()))
    (lambda (message)
      (case message
        ((PLAYER?) (lambda (self) #t))
        ((INSTALL)
	 (lambda (self)
           (ask clock 'ADD-CALLBACK
                (make-clock-callback 'turn self 'TURN))
           (ask clock 'ADD-CALLBACK
                (make-clock-callback 'provoke self 'PROVOKE))
	   (delegate mobile-part self 'INSTALL)))
        ((TURN) (lambda (self) 
                  (if (not (or (ask self 'LOSE?) (ask self 'WIN?)))
                      (turn self (read)))))
        ((HEALTH) (lambda (self) health))
        ((DAMAGE) (lambda (self amount) (set! health (- health amount))))
        ((HEAL) (lambda (self amount) (if (< health 100)
                                           (if (< (+ health amount) 100)
                                               (set! health (+ health amount))
                                               (set! health 100)))))
        ((LOSE?) (lambda (self) (if (<= health 0)
                                    (begin (set! *quit* 1)
                                           (ask screen 'TELL-WORLD
                                                `("YOU LOSE THE GAME")))
                                    #f)))
        ((WIN?) (lambda (self) (< (ask magroth-ul-lord-of-chaos 'HEALTH) 0)))
        ((VICTORY) (lambda (self) (if (ask self 'WIN?)
                                      (begin (set! *quit* 1)
                                             (ask screen 'TELL-WORLD
                                                  `("YOU WIN THE GAME"))))))
        ((COMBAT?) (lambda (self) combat?))
        ((ENEMIES) (lambda (self) enemies))
        ((ENTER-COMBAT) (lambda (self enemy)
                          (set! combat? #t)
                          (if (null? (filter (lambda (thing) (eq? thing enemy))
                                             enemies))
                              (set! enemies (cons enemy enemies)))))
        ((EXIT-COMBAT) (lambda (self enemy)
                         (set! enemies (filter (lambda (thing) (not (eq? thing enemy)))
                                               enemies))
                         (if (null? enemies) (begin (set! combat? #f)
                                                    (print-room (ask self 'LOCATION))
                                                    (set! health 100)))))
        ((PROVOKE) (lambda (self)
                     (map (lambda (x) (ask x 'ATTACK self))
                          enemies))) ;always make sure enemies list is cleared when combat ends
        ((ARMOR) (lambda (self) armor))
        ((WEAPON) (lambda (self) weapon))
        ((INVENTORY) (lambda (self) container-part))
        ((ITEMS) (lambda (self) (ask container-part 'THINGS)))
        ((WEAR) (lambda (self thing) (set! armor thing)))
        ((WIELD) (lambda (self thing) (set! weapon thing)))
        ((ABILITIES) (lambda (self) abilities))
        ((LEARN) (lambda (self skill) (set! abilities (cons skill abilities))))
        (else (find-method message mobile-part))))))

(define (create-player name location characteristics)
  (create make-player name location characteristics))

;;A combatant is a mobile thing that keeps track of its own health and dies
;;when its health reaches zero.
(define (make-combatant name location characteristics health strength)
  (let ((mobile-part (make-mobile-thing name location characteristics))
        (alive? #t)
        (toughness 0)
        (enemies ()))
    (lambda (message)
      (case message
        ((COMBATANT?) (lambda (self) #t))
        ((INSTALL)
	 (lambda (self)			; We add a death callback
           (ask clock 'ADD-CALLBACK
                (make-clock-callback 'death self 'DIE?))
	   (delegate mobile-part self 'INSTALL)))
        ((HEALTH) (lambda (self) health))
        ((ALIVE?) (lambda (self) alive?))
        ((DAMAGE) (lambda (self amount) (set! health (- health amount))))
        ((DIE?) (lambda (self)  ;the death callback will activate this method with each tick
                 (if (<= (ask self 'HEALTH) 0)
                     (begin (set! alive? #f)
                            (ask screen 'TELL-WORLD
                                 `(,name "dies"))
                            (ask clock 'REMOVE-CALLBACK self 'death)
                            (create-corpse-of self)
                            (ask self 'DESTROY)
                            (map (lambda (thing) (if (is-a thing 'PLAYER?)
                                                     (ask thing 'EXIT-COMBAT self)))
                                 (ask location 'THINGS)))
                     'still-kickin)))
        ((SET-STRENGTH) (lambda (self val) (set! strength val)))
        ((SET-TOUGHNESS) (lambda (self) (set! toughness val)))
        ((ADD-ENEMY) (lambda (self enemy)
                       (if (null? (filter (lambda (thing) (eq? thing enemy))
                                          enemies))
                           (set! enemies (cons enemy enemies)))))
        ((REMOVE-ENEMY) (lambda (self enemy)
                         (set! enemies (filter (lambda (thing) (not (eq? thing enemy)))
                                               enemies))))
        ((BE-ATTACKED) (lambda (self whom)
                         (let ((damage (ask (ask whom 'WEAPON) 'DAMAGE)))
                           (if (> damage toughness)
                               (ask self 'DAMAGE (- damage toughness))))))
        ((ATTACK) (lambda (self player)
                    (let ((armor (ask (ask player 'ARMOR) 'ARMOR))) ;ARMOR gets both the object
                      (if (> (- strength armor) 0)                ;and the value
                          (ask player 'DAMAGE (- strength armor))))
                    (ask screen 'TELL-WORLD `(,(ask self 'NAME)
                                               ,(string-append "(" 
                                                         (number->string
                                                          (ask self 'HEALTH))
                                                         ")")
                                               "attacks"
                                               ,(ask player 'NAME)
                                               ,(string-append "(" 
                                                         (number->string
                                                          (ask player 'HEALTH))
                                                         ")")))))
        (else (get-method message mobile-part))))))

(define (create-combatant name location characteristics health strength)
  (create make-combatant name location characteristics health strength))
  
;;An aggressor is a combatant who will put the player into combat mode if
;;the player is in the room.
(define (make-aggressor name location characteristics health strength)
  (let ((combatant-part (make-combatant name location characteristics health strength)))
    (lambda (message)
      (case message
        ((AGGRESSOR?) (lambda (self) #t))
        ((INSTALL)
	 (lambda (self)			;adds an aggressive callback when created
           (ask clock 'ADD-CALLBACK
                (make-clock-callback 'auto-engage self 'AUTO-ENGAGE))
	   (delegate combatant-part self 'INSTALL)))
        ((AUTO-ENGAGE) (lambda (self) 
                    (if (ask self 'ALIVE?)
                        (map (lambda (thing) (if (is-a thing 'PLAYER?)
                                                 (ask thing 'ENTER-COMBAT self)))
                             (ask location 'THINGS)))))
        (else (get-method message combatant-part))))))

(define (create-aggressor name location characteristics health strength)
  (create make-aggressor name location characteristics health strength))

;-----------------------Executing a Player's Turn---------
;;First we define all the functions needed to do a turn.

;This little function takes care of errors in the player's input.
(define (again player msg)
  (ask screen 'TELL-WORLD `(,msg "\n"))
  (turn player (read)))

(define (tag? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (quit? exp) (tag? exp 'quit))

(define (move? exp) (tag? exp 'move))
(define (move-dir exp) (cadr exp))
(define (do-move player exp)
  (let ((exit (find-exit-in-direction
               (ask (ask player 'LOCATION) 'EXITS)
               (move-dir exp))))
    (cond ((not exit) (again player "No such exit."))
          ((ask player 'COMBAT?) (again player "You are in combat."))
          (else (ask exit 'USE player)
                (print-room (ask player 'LOCATION))))))

(define (attack? exp) (tag? exp 'attack))
(define (attack-target exp) (cadr exp))
(define (do-attack player exp)
  (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                (attack-target exp))))
    (if target
        (begin 
          (ask target 'BE-ATTACKED player)
          (ask player 'ENTER-COMBAT target)
          (ask screen 'TELL-WORLD 
               `(,(ask player 'NAME)
                  ,(string-append "(" (number->string
                                       (ask player 'HEALTH)) ")")
                  "attacks"
                  ,(ask target 'NAME)
                  ,(string-append "(" (number->string
                                       (ask target 'HEALTH)) ")"))))
        (again player "No such target."))))

(define (abilities? exp) (tag? exp 'abilities))

(define (ability? exp) (tag? exp 'ability))
(define ability-name cadr)
(define ability-args cddr)
(define (do-ability player exp)
  (let ((ability (find-in (ask player 'ABILITIES) (ability-name exp))))
    (if ability
        (apply (ask ability 'EFFECT)
               player
               (ability-args exp))
        (again player "No such ability."))))

(define (grab? exp) (tag? exp 'grab))
(define (grab-target player exp)
  (find-in (ask (ask player 'LOCATION) 'THINGS) (cadr exp)))
(define (do-grab player exp)
  (let ((target (grab-target player exp)))
    (if target
        (begin (ask target 'CHANGE-LOCATION (ask player 'INVENTORY))
               (ask screen 'TELL-WORLD `(,(ask player 'NAME)
                                          "grabs"
                                          ,(ask target 'NAME))))
        (again player "No such item."))))

(define (drop? exp) (tag? exp 'drop))
(define (drop-target player exp)
  (find-in (ask player 'ITEMS) (cadr exp)))
(define (do-drop player exp)
  (let ((target (drop-target player exp)))
    (if target
        (cond ((eq? target (ask player 'WEAPON)) (again player "Stop wielding this weapon first."))
              ((eq? target (ask player 'ARMOR)) (again player "Stop wearing this armor first."))
              (else (begin (ask target 'CHANGE-LOCATION (ask player 'LOCATION))
                           (ask screen 'TELL-WORLD `(,(ask player 'NAME)
                                                      "drops"
                                                      ,(ask target 'NAME))))))
        (again player "No such item in inventory."))))

(define (wield? exp) (tag? exp 'wield))
(define (wield-target player exp)
  (find-in (ask player 'ITEMS) (cadr exp)))
(define (do-wield player exp)
  (let ((target (wield-target player exp)))
    (if target
        (if (is-a target 'WEAPON?)
            (begin (ask player 'WIELD target)
                   (ask screen 'TELL-WORLD `(,(ask player 'NAME)
                                              "wields"
                                              ,(ask target 'NAME))))
            (again player "You can't wield that."))
        (again player "No such item in inventory."))))

(define (wear? exp) (tag? exp 'wear))
(define (wear-target player exp)
  (find-in (ask player 'ITEMS) (cadr exp)))
(define (do-wear player exp)
  (let ((target (wear-target player exp)))
    (if target
        (if (is-a target 'ARMOR?)
            (begin (ask player 'WEAR target)
                   (ask screen 'TELL-WORLD `(,(ask player 'NAME)
                                              "wears"
                                              ,(ask target 'NAME))))
            (again player "You can't wear that."))
        (again player "No such item in inventory."))))

(define (items? exp) (tag? exp 'items))

(define (look? exp) (tag? exp 'look))

(define (equipment? exp) (tag? exp 'equipment))

(define (inspect? exp) (tag? exp 'inspect))
(define (inspect-target player exp)
  (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS) (cadr exp))))
    (if target
        target
        (let ((target (find-in (ask player 'ITEMS) (cadr exp))))
          (if target
              target
              (let ((target (find-in (ask player 'ABILITIES) (cadr exp))))
                (if target
                    target
                    #f)))))))
(define (do-inspect player exp)
  (let ((target (inspect-target player exp)))
    (if target 
        (ask screen 'TELL-WORLD `(,@(ask target 'CHARACTERISTICS)
                                    "\n"))
        (again player "No such target."))))

(define (use? exp) (tag? exp 'use))
(define use-obj cadr)
(define use-args cddr)
(define (do-use player exp)
  (let ((object (find-in (ask player 'ITEMS) (use-obj exp))))
    (if object
        (begin (apply (ask object 'EFFECT)
                      player
                      (use-args exp))
               (ask screen 'TELL-WORLD 
                    `(,(ask player 'NAME)
                       ,(string-append "(" (number->string
                                            (ask player 'HEALTH)) ")")
                       "uses"
                       ,(ask object 'NAME))))
        (again player "No such item."))))

;;This function will be called by the callback that is created
;;when every player object is created.

(define (turn player exp)
  (with-handlers
      ([(lambda (exn) #t) (lambda (exn) (printf "ERROR! Did you use the wrong number of arguments, Michaelson?"))])
      (turn-inner player exp)))

(define (turn-inner player exp)
  (cond ((quit? exp) (ask clock 'RESET) (set! *quit* 1))
        ((move? exp) (do-move player exp))
        ((attack? exp) (do-attack player exp))
        ((abilities? exp) (print-list (map (lambda (x) (ask x 'NAME))
                                                (ask player 'ABILITIES)))
                          (turn player (read)))
        ((ability? exp) (do-ability player exp))
        ((grab? exp) (do-grab player exp))
        ((drop? exp) (do-drop player exp))
        ((wield? exp) (do-wield player exp))
        ((wear? exp) (do-wear player exp))
        ((items? exp) (print-list (map (lambda (x) (ask x 'NAME))
                                       (ask player 'ITEMS)))
                      (turn player (read)))
        ((look? exp) (print-room (ask player 'LOCATION))
                     (turn player (read)))
        ((equipment? exp) (print-equipment player)
                          (turn player (read)))
        ((inspect? exp) (do-inspect player exp)
                        (turn player (read)))
        ((use? exp) (do-use player exp))
        ((null? exp) 'pass)
        (else (again player "I do not understand."))))
  


;-----------------------Display Functions--------------------------
;--------------Displaying a room
(define (room-name room) (ask room 'NAME)) ;returns name
(define (room-desc room) (car (ask room 'CHARACTERISTICS))) ;returns characteristics
(define (room-contents room) (map (lambda (thing) (ask thing 'NAME))
                                  (filter (lambda (x) (not (is-a x 'PLAYER?)))
                                          (ask room 'THINGS)))) ;returns list of nonplayers
(define (room-exits room) (map (lambda (exit) (ask exit 'DIRECTION))
                                       (filter (lambda (exit) (not (ask exit 'HIDDEN?)))
                                               (ask room 'EXITS)))) ;returns list of not hidden exits

(define (insert-newlines list)
  (if (null? list)
      ()
      (cons (car list)
            (cons "\n" (insert-newlines (cdr list))))))

(define (print-room room)
  (ask screen 'TELL-WORLD `("\n--------------------------------------------------\n"
                            ,(room-name room)
                            "\n\n"
                            ,(room-desc room)
                            "\n\nPresent:\n"
                            ,@(insert-newlines
                               (room-contents room))
                            "\nExits:\n"
                            ,@(room-exits room)
                            "\n--------------------------------------------------\n")))

;-------------displaying character traits

(define (print-list the-list) ;this prints lists of things, like abilities or items
  (ask screen 'TELL-WORLD `("" ,@(insert-newlines the-list))))

(define (print-equipment player)
  (ask screen 'TELL-WORLD `("Armor: "
                            ,(ask (ask player 'ARMOR) 'NAME)
                             ,(string-append "(" 
                                             (number->string
                                              (ask (ask player 'ARMOR) 'ARMOR))
                                             ")")
                             "\nWeapon: "
                             ,(ask (ask player 'WEAPON) 'NAME)
                             ,(string-append "(" 
                                             (number->string
                                              (ask (ask player 'WEAPON) 'DAMAGE))
                                             ")"))))

;------------------displaying spell effects

(define (display-attack player target verb)
  (ask screen 'TELL-WORLD 
       `(,(ask player 'NAME)
          ,(string-append "(" (number->string
                               (ask player 'HEALTH)) ")")
          ,verb
          ,(ask target 'NAME)
          ,(string-append "(" (number->string
                               (ask target 'HEALTH)) ")"))))

(define (display-passive player verb)
  (ask screen 'TELL-WORLD 
       `(,(ask player 'NAME)
          ,verb
          ,(string-append "(" (number->string
                               (ask player 'HEALTH)) ")"))))

;--------Clock modification------------------------------------------
;;The message "THE-CLOCK Tick ..." is very annoying.
;;The following section disables it.
(define (make-quiet-clock . args)
  (let ((clock-part (apply make-clock args)))
    (lambda (message)
      (case message
        ((QUIET-CLOCK?) (lambda (self) #t))
        ((PRINT-TICK) (lambda (self) 'in-space-no-one-can-hear-you-scream))
        (else (find-method message clock-part))))))

;;This object replaces the clock that is defined in ps3-objects.scm.
(define clock (make-quiet-clock))

;-----------------------Starting the Game-------------
;;This variable will determine much of the way the game
;;is rendered. *quit* is created here for later use in quiting the game.
;;*avatar* will be replaced by the chosen character
(define *choice* 0)
(define *quit* 0)
(define *avatar* 'dummy)

(begin (ask screen 'TELL-WORLD '("Select your character:"
                                 "1) Klaus the Strong"
                                 "2) Toshi the Wise"))
       (if #f 0));avoids a nasty output

(define (start)
  (let ((x (read)))
    (cond ((or (equal? x 1) (equal? x 2)) (set! *choice* x))
          (else (ask screen 'TELL-WORLD '("Pick 1 or 2"))
                (start)))))

(start)


;------------------------------Abilities-----------------------------
;----------------tomes

(define (create-tome ability location characteristics)
  (let ((name (string->symbol (string-append (symbol->string (ask ability 'NAME)) "-tome"))))
    (create-usable name location characteristics (lambda (player)
                                                    (ask player 'LEARN ability)
                                                    (ask screen 'TELL-WORLD `(,(ask player 'NAME)
                                                                               "learns"
                                                                               ,(ask ability 'NAME)))))))

;-----------------healing
;(define first-aid (make-ability 'first-aid "Fully heals the player outside of combat"
;                           (lambda (player)
;                             (if (not (ask player 'COMBAT?))
;                                 (begin (ask player 'HEAL 100)
;                                        (display-passive player "patches himself up"))
;                                 (again player "You are in combat")))))

(define minor-heal (make-ability 'minor-heal "(ability minor-heal)"
                           (lambda (player)
                             (ask player 'HEAL 20)
                             (display-passive player "is healed a bit"))))

(define heal (make-ability 'heal "(ability heal)"
                           (lambda (player)
                             (ask player 'HEAL 40)
                             (display-passive player "sees his woulds close up"))))

(define greater-heal (make-ability 'greater-heal "(ability greater-heal)"
                           (lambda (player)
                             (ask player 'HEAL 60)
                             (display-passive player "sees even massive injuries vanish in an instant"))))

(define full-heal (make-ability 'full-heal "(ability full-heal)"
                           (lambda (player)
                             (ask player 'HEAL 100)
                             (display-passive player "is touched by the gods"))))

;-----------------------damage

(define shock (make-ability 'shock "(ability shock [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 6)
                                              (display-attack player target "shocks"))
                                       (again player "No such target."))))))

(define freeze (make-ability 'freeze "(ability freeze [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 12)
                                              (display-attack player target "freezes"))
                                       (again player "No such target."))))))

(define fireball (make-ability 'fireball "(ability fireball [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 20)
                                              (display-attack player target "fireballs"))
                                       (again player "No such target."))))))

(define immolate (make-ability 'immolate "(ability immolate [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 35)
                                              (display-attack player target "immolates in flame"))
                                       (again player "No such target."))))))

(define implode (make-ability 'implode "(ability implode [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 60)
                                              (display-attack player target "warps the very fabric of"))
                                       (again player "No such target."))))))

(define dissolve (make-ability 'dissolve "(ability dissolve [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 90)
                                              (display-attack player target "liquifies the flesh of"))
                                       (again player "No such target."))))))

(define word-of-death (make-ability 'word-of-death "(ability word-of-death [target])"
                               (lambda (player targ)
                                 (let ((target (find-in (ask (ask player 'LOCATION) 'THINGS)
                                                        targ)))
                                   (if target
                                       (begin (ask target 'DAMAGE 120)
                                              (display-attack player target "claims the soul of"))
                                       (again player "No such target."))))))

;-----------------------area damage
(define flail (make-ability 'flail "(ability flail)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) 
                                          (ask x 'DAMAGE (/ (ask (ask player 'WEAPON) 'DAMAGE) 4))) 
                                        target-list)
                                   (display-passive player "swirls about, striking out randomly")))))

(define whirlwind (make-ability 'whirlwind "(ability whirlwind)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) 
                                          (ask x 'DAMAGE (/ (ask (ask player 'WEAPON) 'DAMAGE) 3))) 
                                        target-list)
                                   (display-passive player "swarms about the battlefield in a chaos of one")))))

(define merge (make-ability 'merge "(ability merge)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) 
                                          (ask x 'DAMAGE (/ (ask (ask player 'WEAPON) 'DAMAGE) 2))) 
                                        target-list)
                                   (display-passive player "is one with combat, his blade an extension of his eye")))))

(define chain-lightning (make-ability 'chain-lightning "(ability chain-lightning)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) (ask x 'DAMAGE 9)) target-list)
                                   (display-passive player "forks lightning from his fingers")))))

(define detonate (make-ability 'detonate "(ability detonate)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) (ask x 'DAMAGE 16)) target-list)
                                   (display-passive player "projects an explosion")))))

(define rain-of-fire (make-ability 'rain-of-fire "(ability rain-of-fire)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) (ask x 'DAMAGE 30)) target-list)
                                   (display-passive player "summons a rain of fire and brimstone")))))

(define cone-of-disintegrate (make-ability 'cone-of-disintegrate "(ability cone-of-disintegrate)"
                               (lambda (player)
                                 (let ((target-list (filter (lambda (x) (is-a x 'COMBATANT?))
                                                            (ask (ask player 'LOCATION) 'THINGS))))
                                   (map (lambda (x) (ask x 'DAMAGE 50)) target-list)
                                   (display-passive 
                                    player 
                                    "wills asunder the flesh of those who would oppose him")))))


;-----------------------The World-----------------------

;----------rooms
(define dungeon-start (make-place 'Dungeon-Entrance "You stand at the entrance to the Dungeon of Despair."))
(define caved-passage (make-place 'Blocked-Passage "This passage has been blocked by a recent cave-in."))
(define abandoned-atrium (make-place 'Abandoned-Atrium "A beautiful moonlit atrium, this place has long since been abandoned."))
(define quiet-passage (make-place 'Quiet-Passage "A passageway, cramped but peaceful. The moonlight casts shadows on the walls."))
(define pool-of-mystery (make-place 'Pool-of-Mystery "A mystic pool which channels some indefinable energy. You can feel the walls radiating around you."))
(define dim-cavern (make-place 'Dim-Cavern "This dim cavern is littered with stalagmites and stalagtites. Bats flutter in the shadows. You feel uncomfortable."))
(define spooky-prison (make-place 'Spooky-Prison "An abandoned collection of jail cells. This place is very spooky."))
(define torture-chamber (make-place 'Torture-Chamber "An old torture chamber adjoined to the prison. The floor is bloodstained and the instruments of torture are still iying around."))
(define silent-hallway (make-place 'Silent-Hallway "This echoing hallway must have been how guards entered and left the prison."))
(define diabolical-laboratory (make-place 'Diabolical-Laboratory "The ground is littered with shattered test tubes, and the air reeks with the stench of those creatures who died here... and were born here."))
(define hole-in-the-wall (make-place 'Hole-in-the-Wall "This appears to be where goblins broke through the walls of the dungeon and invaded its inhabitants. The floor is strewn with rubble."))
(define goblin-tunnel (make-place 'Goblin-Tunnel "The stones here have been smoothed by the passage of many feet. You hear gutteral voices ahead..."))
(define goblin-lair-entrance (make-place 'Goblin-Lair-Entrance "This is the entrance to the goblin lair. There is a small guard post here."))
(define goblin-warrens (make-place 'Goblin-Warrens "You are in the teeming warrens under the goblin city."))
(define goblin-kitchen (make-place 'Goblin-Kitchen "The chefs in this kitchen are busy preparing a meal of human flesh."))
(define storage-cupboard (make-place 'Storage-Cupboard "It's dark in here. You see dog biscuits."))
(define goblin-cafeteria (make-place 'Goblin-Cafeteria "You see many goblins munching away on human flesh."))
(define goblin-dormitory (make-place 'Goblin-Dormitory "You see many sleeping goblins. They wake up when you begin killing them."))
(define goblin-city (make-place 'Goblin-City "You are in the capitol city of the Grand Empire of Goblinania. You see the palace to the South and a smaller building East."))
(define goblin-treasure-room (make-place 'Goblin-Treasure-Room "This is where the King of Goblinania keeps his treasure. You fill your pockets with as much as you can carry."))
(define goblin-palace (make-place 'Goblin-Palace "You are in a dirty palace. The walls are covered with tapestries depicting goblins eating human flesh."))
(define palace-corridor (make-place 'Palace-Corridor "You stride purposefully through the corridor. The King's throne room is to the South, and there is a small antechamber to the East."))
(define antechamber (make-place 'Antechamber "This is where the King of Goblinania keeps his guests while he decides whether or not to eat them."))
(define throne-room (make-place 'Throne-Room "This is the throne room of Gaum'tharp Wazzal, King of Goblinania. There is a curious ladder behind the throne."))

(define dungeon-start-caved-passage (create-exit dungeon-start 'west caved-passage 'east #f #f))
(define dungeon-start-abandoned-atrium (create-exit dungeon-start 'east abandoned-atrium 'west #f #f))
(define abandoned-atrium-quiet-passage (create-exit abandoned-atrium 'east quiet-passage 'west #f #f))
(define quiet-passage-pool-of-mystery (create-exit quiet-passage 'east pool-of-mystery 'west #f #f))
(define abandoned-atrium-dim-cavern (create-exit abandoned-atrium 'south dim-cavern 'north #f #f))
(define dim-cavern-spooky-prison (create-exit dim-cavern 'south spooky-prison 'north #f #f))
(define spooky-prison-torture-chamber (create-exit spooky-prison 'east torture-chamber 'west #f #f))
(define spooky-prison-silent-hallway (create-exit spooky-prison 'west silent-hallway 'east #f #f))
(define spooky-prison-diabolical-laboratory (create-exit spooky-prison 'south diabolical-laboratory 'north #f #f))
(define silent-hallway-hole-in-the-wall (create-exit silent-hallway 'south hole-in-the-wall 'north #f #f))
(define diabolical-laboratory-hole-in-the-wall (create-exit diabolical-laboratory 'west hole-in-the-wall 'east #f #f))
(define hole-in-the-wall-goblin-tunnel (create-exit hole-in-the-wall 'south goblin-tunnel 'north #f #f))
(define goblin-tunnel-goblin-lair-entrance (create-exit goblin-tunnel 'south goblin-lair-entrance 'north #f #f))
(define goblin-lair-entrance-goblin-warrens (create-exit goblin-lair-entrance 'south goblin-warrens 'north #f #f))
(define goblin-warrens-goblin-kitchen (create-exit goblin-warrens 'west goblin-kitchen 'east #f #f))
(define goblin-kitchen-storage-cupboard (create-exit goblin-kitchen 'cupboard storage-cupboard 'out #f #f))
(define goblin-kitchen-goblin-cafeteria (create-exit goblin-kitchen 'south goblin-cafeteria 'north #f #f))
(define goblin-warrens-goblin-goblin-dormitory (create-exit goblin-warrens 'south goblin-dormitory 'north #f #f))
(define goblin-dormitory-goblin-cafeteria (create-exit goblin-dormitory 'west goblin-cafeteria 'east #f #f))
(define goblin-warrens-goblin-city (create-exit goblin-warrens 'east goblin-city 'west #f #f))
(define goblin-city-goblin-treasure-room (create-exit goblin-city 'east goblin-treasure-room 'west #f #f))
(define goblin-city-goblin-palace (create-exit goblin-city 'south goblin-palace 'north #f #f))
(define goblin-palace-palace-corridor (create-exit goblin-palace 'south palace-corridor 'north #f #f))
(define palace-corridor-antechamber (create-exit palace-corridor 'east antechamber 'west #f #f))
(define palace-corridor-throne-room (create-exit palace-corridor 'south throne-room 'north #f #f))

(define swamp-entrance (make-place 'Center-of-Swamp "Welcome to the Swamp of the Damned! You see a small hatch in the ground leading pack to Goblinania. Paths lead in all directions."))
(define throne-room-swamp-entrance (create-exit throne-room 'ladder swamp-entrance 'hatch #f #f))

(define swamp-a (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-b (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-c (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-d (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-e (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-f (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to feel lost..."))
(define swamp-g (make-place 'The-Swamp-of-the-Damned "You are in a disgusting swamp, beginning to get hungry..."))

(define swamp-path (make-place 'Path-through-Swamp "You have found a path through the swamp. Things are starting to look up..."))

(define swamp-entrance-swamp-a (create-exit swamp-entrance 'brambles swamp-a 'wrong-turn #f #f))
(define swamp-entrance-swamp-b (create-exit swamp-entrance 'wrong-turn swamp-b 'false-path #f #f))
(define swamp-entrance-swamp-c (create-exit swamp-entrance 'false-path swamp-c 'brambles #f #f))
(define swamp-entrance-swamp-d (create-exit swamp-entrance 'bushes swamp-d 'brambles #f #f))

(define swamp-a-swamp-c (create-exit swamp-a 'bushes swamp-c 'false-path #f #f))
(define swamp-a-swamp-e (create-exit swamp-a 'false-path swamp-e 'brambles #f #f))
(define swamp-b-swamp-d (create-exit swamp-b 'brambles swamp-d 'false-path #f #f))
(define swamp-b-swamp-e (create-exit swamp-b 'bushes swamp-e 'false-path #f #f))
(define swamp-c-swamp-f (create-exit swamp-c 'bushes swamp-f 'false-path #f #f))
(define swamp-d-swamp-f (create-exit swamp-d 'bushes swamp-f 'brambles #f #f))

(define swamp-e-swamp-f (create-exit swamp-e 'wrong-turn swamp-f 'bushes #f #f))
(define swamp-e-swamp-g (create-exit swamp-e 'bushes swamp-g 'false-path #f #f))
(define swamp-f-swamp-g (create-exit swamp-f 'wrong-turn swamp-g 'brambles #f #f))

(define swamp-g-swamp-path (create-exit swamp-g 'bushes swamp-path 'west #f #f))

(define path-fork (make-place 'Fork-in-Path "You have come to a fork in the path. To the south lies a cannibal village, to the west the path continues."))
(define village-entrance (make-place 'Cannibal-Village-Entrance "You are at the entrance of the cannibal village. You hear war drums. There are many heads on spikes."))
(define village-center (make-place 'Cannibal-Village-Center "You are in the middle of the cannibal village. To the south is an ornate hut. To the west, you see the path."))
(define witch-doctor-hut (make-place 'Hut-of-the-Witch-Doctor "You are in the hut of the village witch doctor. There are many shrunken heads."))
(define path-end (make-place 'End-of-Path "You have reached the end of the path. There is a graveyard to the west."))

(define swamp-path-path-fork (create-exit swamp-path 'south path-fork 'north #f #f))
(define path-fork-village-entrance (create-exit path-fork 'south village-entrance 'north #f #f))
(define path-fork-path-end (create-exit path-fork 'west path-end 'east #f #f))
(define village-entrance-village-center (create-exit village-entrance 'south village-center 'north #f #f))
(define village-center-path-end (create-exit village-center 'west path-end 'south #f #f))
(define village-center-witch-doctor-hut (create-exit village-center 'south witch-doctor-hut 'north #f #f))

(define graveyard (make-place 'Graveyard "A graveyard with many unearthed graves. You hear death's rattle in the shadows..."))
(define crypt (make-place 'Crypt "An ornate crypt of an ancient king. The shadows of the dead are everywhere."))
(define mausoleum (make-place 'Mausoleum "This mausoleum is the center of the Necromancer's dark power. You see a wall of black mist in front of you."))
(define pool-of-darkness (make-place 'Pool-of-Darkness "A wretched and filthy pool, filled with the bile of some creature. You are assailed by the creature's tentacles."))
(define pit-of-evil (make-place 'Pit-of-Evil "The walls are covered in arcane runes, so evil it hurts to look at them. There is a portal in the center of the pit."))
(define tower-entrance (make-place 'Right-Outside-Tower "Welcome to the Tower of the Ancients! There is a drawbridge going over a moat in front of you."))

(define path-end-graveyard (create-exit path-end 'west graveyard 'east #f #f))
(define graveyard-crypt (create-exit graveyard 'crypt crypt 'out #f #f))
(define crypt-mausoleum (create-exit crypt 'deeper mausoleum 'out #f #f))
(define mausoleum-pool (create-exit mausoleum 'blackness pool-of-darkness 'back #f #f))
(define pool-pit (create-exit pool-of-darkness 'into-pit pit-of-evil 'back #f #f))
(define pit-tower (create-exit pit-of-evil 'glowing-portal tower-entrance 'back-through-portal #f #f))

(define drawbridge (make-place 'Drawbridge "A rickety drawbridge over the moat."))
(define drawbridge-middle (make-place 'Middle-of-Drawbridge "The drawbridge is very rickety. You see gargoyles. The moat is full of unspeakable horrors."))
(define gate-north (make-place 'Outside-of-Gate "You come to a locked gate at the end of the drawbridge. You see some vines on the wall. They look eminently climbable."))
(define turret (make-place 'Guard-Turret "You are in a small guard turret above the gate."))
(define mechanism-room (make-place 'Gate-Mechanism-Room "This appears to be the room that controls the gate. From here, you can go back down."))
(define gate-south (make-place 'Inside-of-Gate "You are now inside the locked gate. The doors to the Tower of the Ancients gape in front of you."))
(define grand-stairway (make-place 'Grand-Staircase "You stand on the grand staircase of the tower. This was once a beautiful place."))
(define tower-hallway (make-place 'Main-Hallway "This is the main hallway of the tower. It is lined with oil paintings and fine artifacts."))
(define tower-1-a (make-place 'Tower-Passage "The Tower of Ancients awes you with its majesty."))

(define tower-entrance-drawbridge (create-exit tower-entrance 'bridge drawbridge 'back #f #f))
(define drawbridge-drawbridge (create-exit drawbridge 'forward drawbridge-middle 'back #f #f))
(define drawbridge-gate (create-exit drawbridge-middle 'south gate-north 'north #f #f))
(define gate-turret (create-exit gate-north 'up turret 'down #f #f))
(define turret-mechanism-room (create-exit turret 'south mechanism-room 'north #f #f))
(define mechanism-room-gate-south (create-exit mechanism-room 'down gate-south 'up #f #f))
(define gate-south-grand-stairway (create-exit gate-south 'tower grand-stairway 'outside #f #f))
(define grand-stairway-tower-hallway (create-exit grand-stairway 'up tower-hallway 'down #f #f))
(define tower-hallway-tower-1-a (create-exit tower-hallway 'south tower-1-a 'north #f #f))

(define tower-1-b (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-c (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-d (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-e (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-f (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-g (make-place 'Tower-West-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-i (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-j (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-k (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-l (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-m (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-1-n (make-place 'Tower-East-Passage "The Tower of Ancients awes you with its majesty."))
(define tower-great-hall (make-place 'Great-Hall "This lofty chamber seems to stretch towards heaven. You see a door behind the banquet table."))
(define tower-mini-stairway (make-place 'Staircase "A winding spiral staircase which takes you ever higher into the tower."))

(define tower-1-a-tower-1-b (create-exit tower-1-a 'west tower-1-b 'east #f #f))
(define tower-1-b-tower-1-c (create-exit tower-1-b 'west tower-1-c 'east #f #f))
(define tower-1-c-tower-1-d (create-exit tower-1-c 'south tower-1-d 'north #f #f))
(define tower-1-d-tower-1-e (create-exit tower-1-d 'south tower-1-e 'north #f #f))
(define tower-1-e-tower-1-f (create-exit tower-1-e 'south tower-1-f 'north #f #f))
(define tower-1-f-tower-1-g (create-exit tower-1-f 'east tower-1-g 'west #f #f))
(define tower-1-g-tower-great-hall (create-exit tower-1-g 'east tower-great-hall 'west #f #f))
(define tower-1-a-tower-1-i (create-exit tower-1-a 'east tower-1-i 'west #f #f))
(define tower-1-i-tower-1-j (create-exit tower-1-i 'east tower-1-j 'west #f #f))
(define tower-1-j-tower-1-k (create-exit tower-1-j 'south tower-1-k 'north #f #f))
(define tower-1-k-tower-1-l (create-exit tower-1-k 'south tower-1-l 'north #f #f))
(define tower-1-l-tower-1-m (create-exit tower-1-l 'south tower-1-m 'north #f #f))
(define tower-1-m-tower-1-n (create-exit tower-1-m 'west tower-1-n 'east #f #f))
(define tower-1-n-tower-great-hall (create-exit tower-1-n 'west tower-great-hall 'east #f #f))

(define tower-great-hall-tower-mini-stairway (create-exit tower-great-hall 'doorway tower-mini-stairway 'hall #f #f))

(define tower-observatory (make-place 'Ancient-Observatory "You stand in a grand observatory full of arcane equipment."))
(define universe-room (make-place 'Model-Room "This room is dominated by an enormous model of the known universe."))
(define crystal-room (make-place 'Crystal-Room "This room is full of crystals which appear to power the machinery in the tower. They hum menacingly."))
(define maze-entrance (make-place 'Light-Maze-Entrance "This is the entrance to a maze whose walls are made of beams of light."))

(define tower-mini-stairway-tower-observatory (create-exit tower-mini-stairway 'up tower-observatory 'down #f #f))
(define tower-observatory-universe-room (create-exit tower-observatory 'south universe-room 'north #f #f))
(define universe-room-crystal-room (create-exit universe-room 'east crystal-room 'west #f #f))
(define universe-room-maze-entrance (create-exit universe-room 'south maze-entrance 'north #f #f))

(define lm-1 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-2 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-3 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-4 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-5 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-6 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-7 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-8 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-9 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))
(define lm-10 (make-place 'Inside-the-Light-Maze "You are dazzled by beams of light from all sides."))

(define lm-out (make-place 'End-of-Light-Maze "You are finally out of the horrible light maze. You look up and the stars beckon."))

(define maze-entrance-lm-1 (create-exit maze-entrance 'in lm-1 'huh? #f #f))

(define lm-1-lm-2 (create-exit lm-1 'wha? lm-2 'chwat! #f #f))
(define lm-1-lm-4 (create-exit lm-1 'whfa? lm-4 'hjy! #f #f))
(define lm-1-lm-5 (create-exit lm-1 'hangh? lm-5 'ow! #f #f))
(define lm-1-lm-8 (create-exit lm-1 'whao? lm-8 'grgh! #f #f))
(define lm-1-lm-10 (create-exit lm-1 'ouch! lm-10 'kly! #f #f))
(define lm-2-lm-3 (create-exit lm-2 'enh? lm-3 'trh! #f #f))
(define lm-2-lm-7 (create-exit lm-2 'weah? lm-7 'hwa! #f #f))
(define lm-2-lm-10 (create-exit lm-2 'chwa? lm-10 'ugh! #f #f))
(define lm-3-lm-4 (create-exit lm-3 'hwa? lm-4 'grms! #f #f))
(define lm-4-lm-5 (create-exit lm-4 'knh? lm-5 'fah! #f #f))
(define lm-5-lm-6 (create-exit lm-5 'thg? lm-6 'aww! #f #f))
(define lm-5-lm-7 (create-exit lm-5 'fnf? lm-7 'aaah! #f #f))
(define lm-5-lm-8 (create-exit lm-5 'yeuch? lm-8 'yeouch! #f #f))
(define lm-5-lm-9 (create-exit lm-5 'klr? lm-9 'plgh! #f #f))
(define lm-6-lm-7 (create-exit lm-6 'grgl? lm-7 'fwt! #f #f))
(define lm-7-lm-8 (create-exit lm-7 'flt? lm-8 'trn! #f #f))
(define lm-8-lm-9 (create-exit lm-8 'kgh? lm-9 'ghj! #f #f))
(define lm-9-lm-10 (create-exit lm-9 'erh? lm-10 'gahjhk! #f #f))
(define lm-10-lm-out (create-exit lm-10 'gnh? lm-out 'fwd! #f #f))

(define astral-passage (make-place 'Astral-Passageway "You seem to be walking on pure energy. The void swirls around you."))
(define stellar-vortex (make-place 'Stellar-Vortex "Cataclysmic energies are released around you. Terrible forces are gathering here."))
(define twisted-portal (make-place 'Twisted-Portal "This is an evil place. Hatred and despair flow like a dark river from a rift in the universe. You know you must go forward."))
(define plane-entrance (make-place 'Border-of-the-Plane-of-Agony "Welcome to the Plane of Agony! Enjoy your stay..."))

(define lm-out-astral-passage (create-exit lm-out 'ascend astral-passage 'descend #f #f))
(define astral-passage-stellar-vortex (create-exit astral-passage 'onwards stellar-vortex 'retreat #f #f))
(define stellar-vortex-twisted-portal (create-exit stellar-vortex 'onwards twisted-portal 'retreat #f #f))
(define twisted-portal-plane-entrance (create-exit twisted-portal 'rift plane-entrance 'rift #f #f))

(define agony-1 (make-place 'The-Plane-of-Agony "The howls of the damned fill you senses and your mind."))
(define agony-2 (make-place 'The-Place-of-Choosing "Before you yawn four great temples. In the distance lies a seething ocean of pure darkness."))
(define agony-a (make-place 'The-Temple-of-Despair "The howls of the damned fill you senses and your mind."))
(define agony-b (make-place 'The-Temple-of-Lamentation "The howls of the damned fill you senses and your mind."))
(define agony-c (make-place 'The-Temple-of-Hate "The howls of the damned fill you senses and your mind."))
(define agony-d (make-place 'The-Temple-of-Loathing "The howls of the damned fill you senses and your mind."))
(define agony-3 (make-place 'Ocean-of-Darkness "Pure darkness engulfs this place. Turn back now if you are not ready."))
(define agony-4 (make-place 'Chaos "The source of all the evil power which has beseiged your world. Magroth-Ul meets your eyes and smiles. It is time to end this. Now."))

(define plane-entrance-agony-1 (create-exit plane-entrance 'onwards agony-1 'retreat #f #f))
(define agony-1-agony-2 (create-exit agony-1 'onwards agony-2 'retreat #f #f))
(define agony-2-agony-a (create-exit agony-2 'despair agony-a 'choosing #f #f))
(define agony-2-agony-b (create-exit agony-2 'lamentation agony-b 'choosing #f #f))
(define agony-2-agony-c (create-exit agony-2 'hate agony-c 'choosing #f #f))
(define agony-2-agony-d (create-exit agony-2 'loathing agony-d 'choosing #f #f))
(define agony-2-agony-3 (create-exit agony-2 'darkness agony-3 'choosing #f #f))
(define agony-3-agony-4 (create-exit agony-3 'fate agony-4 'fate #f #f))

(define infinity (make-place 'Infinity "You stare into the void."))

;----------items

(define heavy-cudgel (if (= *choice* 1) (create-weapon 'heavy-cudgel pool-of-mystery "A little bit sturdier, maybe." 8)))
(define poisoned-dagger (if (= *choice* 1) (create-weapon 'poisoned-dagger torture-chamber "Has a bite to it." 12)))
(define short-sword (if (= *choice* 1) (create-weapon 'short-sword storage-cupboard "A gladiator's finest friend." 17)))
(define war-hammer (if (= *choice* 1) (create-weapon 'war-hammer throne-room "Finally, some heft." 22)))
(define bastard-sword (if (= *choice* 1) (create-weapon 'bastard-sword swamp-g "Bears the markings of some ancient lord." 28)))
(define two-handed-axe (if (= *choice* 1) (create-weapon 'two-handed-axe village-center "The heavy black iron stinks of Draenor." 34)))
(define trident-of-might (if (= *choice* 1) (create-weapon 'trident-of-might pool-of-darkness "Disconcertingly pointy, and the shaft has an unusual resonance." 41)))
(define mace-of-slaughter (if (= *choice* 1) (create-weapon 'mace-of-slaughter turret "Stained with blood, it appears to draw itself toward its victim's skull." 48)))
(define arcane-tanto (if (= *choice* 1) (create-weapon 'arcane-tanto tower-hallway "Its edge was forged with an art long lost to combat foes soon dead." 60)))
(define halberd-of-justice (if (= *choice* 1) (create-weapon 'halberd-of-justice crystal-room "The blade humms through shield and armor as though it were wheat." 80)))
(define mailed-fist-of-heavens (if (= *choice* 1) (create-weapon 'mailed-fist-of-heavens stellar-vortex "The mithril links repulse blade and bone alike, and too often find themselves clean through an opponents chest." 110)))
(define battleaxe-of-kharnei-dunn (if (= *choice* 1) (create-weapon 'battleaxe-of-kharnei-dunn agony-b "The creation of this monstrosity is recounted in engravings on the weapon itself. It has slain gods." 150)))
(define sword-of-the-apocalypse (if (= *choice* 1) (create-weapon 'sword-of-the-apocalypse infinity "The maker of this blade spent his very soul in its creation. Good trade." 1000)))

(define short-staff (if (= *choice* 2) (create-weapon 'short-staff pool-of-mystery "A good walking stick." 2)))
(define oak-staff (if (= *choice* 2) (create-weapon 'oak-staff antechamber "Might really give your head a good knobbing." 5)))
(define staff-of-power (if (= *choice* 2) (create-weapon 'staff-of-power witch-doctor-hut "Topped with a shining ruby and tingling with static." 10)))
(define gnarled-staff-of-smiting (if (= *choice* 2) (create-weapon 'gnarled-staff-of-smiting mechanism-room "Shatters the stones below your feet when you drop it." 20)))
(define cobalt-wand-of-incineration (if (= *choice* 2) (create-weapon 'cobalt-wand-of-incineration astral-passage "One end is very cold. Do not touch the other end." 30)))
(define grand-scepter-of-annihilation (if (= *choice* 2) (create-weapon 'grand-scepter-of-annihilation agony-b "Mischievous gods used to toy with warring nations by giving them weapons of limitless power. Sometimes they forgot to clean up the mess." 50)))

(define tattered-hide-armor (if (= *choice* 1) (create-armor 'tattered-hide-armor caved-passage "Sewn from goblin leather." 2)))
(define leather-armor (if (= *choice* 1) (create-armor 'leather-armor goblin-dormitory "Layers of tough buffalo hide." 3)))
(define studded-leather-armor (if (= *choice* 1) (create-armor 'studded-leather-armor antechamber "A bit of steel prevents some broken bones." 4)))
(define chain-mail (if (= *choice* 1) (create-armor 'chain-mail swamp-path "An intricate job, strong and light." 6)))
(define full-plate-mail (if (= *choice* 1) (create-armor 'full-plate-mail mausoleum "Much heavier than chain, and nigh impervious to any mortal man" 9)))
(define dragonscale-plate (if (= *choice* 1) (create-armor 'dragonscale-plate gate-south "Dragons are weak at the belly, but nowhere else." 12)))
(define breastplate-of-heartiness (if (= *choice* 1) (create-armor 'breastplate-of-heartiness tower-great-hall "Your wounds don't bleed as badly, and light scratches no longer pierce the surface" 16)))
(define greaves-of-glory (if (= *choice* 1) (create-armor 'greaves-of-glory lm-out "You become so lithe and agile that blows simply dodge themselves." 20)))
(define shield-of-splendor (if (= *choice* 1) (create-armor 'shield-of-splendor twisted-portal "At the center of the shield is a jewel so bright that arrows cannot bear to fly toward it." 25)))
(define armor-of-karziebarth (if (= *choice* 1) (create-armor 'armor-of-karziebarth agony-a "When Satan first descended into Hell, his armor melted into the earth. The ore has been reforged." 32)))

(define white-robe (if (= *choice* 2) (create-armor 'white-robe torture-chamber "Made from fine elven silks." 1)))
(define padded-robe (if (= *choice* 2) (create-armor 'padded-robe goblin-kitchen "A few extra layers help deflect the smaller cuts." 3)))
(define gryphon-hair-cloak (if (= *choice* 2) (create-armor 'gryphon-hair-cloak crypt "The mane-hairs of a bull gryphon are tempered by the frigid Northern winds." 6)))
(define cowl-of-wisdom (if (= *choice* 2) (create-armor 'cowl-of-wisdom tower-hallway "The headpiece moves your perceptions a split second forward. That is all the advantage a skilled fighter needs." 10)))
(define robe-of-shimmering (if (= *choice* 2) (create-armor 'robe-of-shimmering agony-a "The robe understands that its master is above petty sticks and stones." 15)))

(define freeze-tome (if (= *choice* 2) (create-tome freeze diabolical-laboratory "The binding cracks loudly and the pages quiver stiffly.")))
(define fireball-tome (if (= *choice* 2) (create-tome fireball goblin-treasure-room "This musty volume bursts open the instant you begin to leaf through its pages.")))
(define immolate-tome (if (= *choice* 2) (create-tome immolate witch-doctor-hut "As you pass your hands over the ancient leather, you feel warmed head to toe.")))
(define implode-tome (if (= *choice* 2) (create-tome implode crystal-room "The tome feels as though it has to endure great strain merely to remain in this world.")))
(define dissolve-tome (if (= *choice* 2) (create-tome dissolve stellar-vortex "Your eyes can only bear the writings of this tome for a few seconds at a time, and your hands begin to blister before you finish the first page.")))
(define word-of-death-tome (if (= *choice* 2) (create-tome word-of-death agony-c "The cover is jet black and cold as ice. The only adornment is a tortured, violent drawing on the first page--an apple.")))

(define chain-lightning-tome (if (= *choice* 2) (create-tome chain-lightning antechamber "The entire room crackles.")))
(define detonate-tome (if (= *choice* 2) (create-tome detonate mausoleum "You feel everything you carry being repulsed from this book.")))
(define rain-of-fire-tome (if (= *choice* 2) (create-tome rain-of-fire universe-room "The room is covered in a thin film of ash.")))
(define cone-of-disintegrate-tome (if (= *choice* 2) (create-tome cone-of-disintegrate agony-d "You cannot read this book without a mirror, and the mirrors melt after a few minutes.")))

(define flail-tome (if (= *choice* 1) (create-tome flail diabolical-laboratory "Reading this book seems to drive you into a rage.")))
(define whirlwind-tome (if (= *choice* 1) (create-tome whirlwind witch-doctor-hut "This is a volume of battles so violent that the earth itself was smashed underfoot.")))
(define merge-tome (if (= *choice* 1) (create-tome merge agony-c "Legends tell of warriors who simply disappeared into the fray, invisible until the last drop of their enemies blood sank into the dirt.")))

(define heal-tome (if (= *choice* 2) 
                      (create-tome heal goblin-treasure-room "This tome seems to attract small, cute woodland creatures.")
                      (create-tome heal tower-observatory "This tome seems to attract small, cute woodland creatures.")))
(define full-heal-tome (if (= *choice* 2) (create-tome full-heal tower-observatory "This volume is bound in a pure, white cover. It is so bright that you feel you should be blinded by it, but you are drawn to it.")))
(define minor-heal-tome (if (= *choice* 1) (create-tome minor-heal goblin-treasure-room "The cover of this book is a soft leather, soothing to the touch.")))
(define greater-heal-tome (if (= *choice* 1) (create-tome greater-heal agony-d "This book is covered with images of great warriors wading into battle wearing nothing but peasants' cloth.")))


;----------------creatures
(define rat (create-aggressor 'rabid-rat caved-passage "Foaming, swarming vermin." 1 1))
(define rat (create-aggressor 'rabid-rat caved-passage "Foaming, swarming vermin." 1 1))
(define rat (create-aggressor 'rabid-rat abandoned-atrium "Foaming, swarming vermin." 1 1))
(define rat (create-aggressor 'rabid-rat abandoned-atrium "Foaming, swarming vermin." 1 1))
(define rat (create-aggressor 'rabid-rat abandoned-atrium "Foaming, swarming vermin." 1 1))
(define rat (create-aggressor 'rabid-rat dim-cavern "Foaming, swarming vermin." 1 1))

(define lichen (create-aggressor 'vicious-self-aware-lichen pool-of-mystery "Secretes digestive juices as it glides." 3 1))
(define lichen (create-aggressor 'vicious-self-aware-lichen pool-of-mystery "Secretes digestive juices as it glides." 3 1))
(define lichen (create-aggressor 'vicious-self-aware-lichen pool-of-mystery "Secretes digestive juices as it glides." 3 1))
(define lichen (create-aggressor 'vicious-self-aware-lichen dim-cavern "Secretes digestive juices as it glides." 3 1))

(define ghost (create-aggressor 'ghost silent-hallway "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost spooky-prison "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost spooky-prison "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost spooky-prison "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost torture-chamber "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost torture-chamber "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost torture-chamber "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost torture-chamber "The only sign of its presence is the intense cold of its touch." 2 3))
(define ghost (create-aggressor 'ghost torture-chamber "The only sign of its presence is the intense cold of its touch." 2 3))

(define mutant (create-aggressor 'mutant diabolical-laboratory "Half-goblin, half-rat, this thing knows only hunger and fear." 2 3))

(define goblin (create-aggressor 'goblin goblin-lair-entrance "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-lair-entrance "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-warrens "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-warrens "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-warrens "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-cafeteria "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-cafeteria "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-cafeteria "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-cafeteria "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-dormitory "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-dormitory "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-dormitory "A small, shrieking creature." 3 2))
(define goblin (create-aggressor 'goblin goblin-dormitory "A small, shrieking creature." 3 2))


(define goblin-soldier (create-aggressor 'goblin-soldier goblin-palace "A rather larger, more deeply voiced creature." 5 3))
(define goblin-soldier (create-aggressor 'goblin-soldier goblin-palace "A rather larger, more deeply voiced creature." 5 3))
(define goblin-soldier (create-aggressor 'goblin-soldier antechamber "A rather larger, more deeply voiced creature." 5 3))
(define goblin-soldier (create-aggressor 'goblin-soldier antechamber "A rather larger, more deeply voiced creature." 5 3))

(define goblin-treasure-room-guards (create-aggressor 'goblin-treasure-room-guard goblin-treasure-room "These goblins are unusually armored. Apparently their lives are valuable." 7 3))
(define goblin-treasure-room-guards (create-aggressor 'goblin-treasure-room-guard goblin-treasure-room "These goblins are unusually armored. Apparently their lives are valuable." 7 3))

(define goblin-grand-vizier (create-aggressor 'grand-vizier-of-goblinania palace-corridor "This goblin wears a bishop's miter at least a foot taller than himself. He also wears the bishop's head." 12 8))

(define goblin-king (create-aggressor 'goblin-king throne-room "Nearly human sized, and well beyond human girth, he wears an enormous, bloody crown." 20 10))

(define hellhound (create-aggressor 'hellhound swamp-e "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-e "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-f "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-f "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-g "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-g "Faster than lightning, and barks fire." 10 5))
(define hellhound (create-aggressor 'hellhound swamp-g "Faster than lightning, and barks fire." 10 5))

(define swamp-creature (create-aggressor 'swamp-creature swamp-a "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-a "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-b "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-b "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-c "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-c "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-d "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-d "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-e "It is difficult to make out what is underneath this shroud of decay and death." 7 4))
(define swamp-creature (create-aggressor 'swamp-creature swamp-f "It is difficult to make out what is underneath this shroud of decay and death." 7 4))


(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-entrance "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-entrance "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-entrance "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-center "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-center "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-center "Marked in gruesome red paint. At least you hope it is paint." 6 6))
(define cannibal-headhunters (create-aggressor 'cannibal-head-hunter village-center "Marked in gruesome red paint. At least you hope it is paint." 6 6))

(define witch-doctor (create-aggressor 'witch-doctor witch-doctor-hut "The only thing more disconcerting than the number of skulls on this man's headdress is the number of human skulls in his throne." 30 15))

(define voodoo-spirit (create-aggressor 'voodoo-spirit witch-doctor-hut "This apparition hovers about its master as though bound by incorporeal chains." 10 5))
(define voodoo-spirit (create-aggressor 'voodoo-spirit witch-doctor-hut "This apparition hovers about its master as though bound by incorporeal chains." 10 5))

(define skeleton-warrior (create-aggressor 'skeleton-warrior graveyard "Moves with a fearsome rattle and swings a rusty scimitar." 5 7))
(define skeleton-warrior (create-aggressor 'skeleton-warrior graveyard "Moves with a fearsome rattle and swings a rusty scimitar." 5 7))
(define skeleton-warrior (create-aggressor 'skeleton-warrior mausoleum "Moves with a fearsome rattle and swings a rusty scimitar." 5 7))
(define skeleton-warrior (create-aggressor 'skeleton-warrior mausoleum "Moves with a fearsome rattle and swings a rusty scimitar." 5 7))

(define living-dead (create-aggressor 'living-dead crypt "These monstrosities lack a rattle to warn of their approach, and moan only just before they strike." 8 8))
(define living-dead (create-aggressor 'living-dead crypt "These monstrosities lack a rattle to warn of their approach, and moan only just before they strike." 8 8))
(define living-dead (create-aggressor 'living-dead crypt "These monstrosities lack a rattle to warn of their approach, and moan only just before they strike." 8 8))
(define living-dead (create-aggressor 'living-dead mausoleum "These monstrosities lack a rattle to warn of their approach, and moan only just before they strike." 8 8))
(define living-dead (create-aggressor 'living-dead mausoleum "These monstrosities lack a rattle to warn of their approach, and moan only just before they strike." 8 8))

(define necromancer (create-aggressor 'necromancer mausoleum "The only thing you can see under the shroud is the glaring, red eyes of this soulless mage. His voice has long since decayed, and his attacks are utterly mute." 20 20))

(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))
(define tentacle-of-the-depths (create-aggressor 'tentacle-of-the-depths pool-of-darkness "These long, green arms lunge out as one writing whole, pulling with all their strength to drown you in their waters." 10 3))

(define creature-of-the-depths (create-aggressor 'creature-of-the-depth pit-of-evil "This once unstoppable creature now lies mutilated, and the agony spewing forth from its enormous beak almost makes you sorry for what you now must finish." 30 20))

(define gargoyle (create-aggressor 'gargoyle drawbridge-middle "The sound of stone clacking on stone heralds the assault of these creatures, who dive from high in the air to ram soft mortals." 10 10))
(define gargoyle (create-aggressor 'gargoyle drawbridge-middle "The sound of stone clacking on stone heralds the assault of these creatures, who dive from high in the air to ram soft mortals." 10 10))
(define gargoyle (create-aggressor 'gargoyle grand-stairway "The sound of stone clacking on stone heralds the assault of these creatures, who dive from high in the air to ram soft mortals." 10 10))
(define gargoyle (create-aggressor 'gargoyle grand-stairway "The sound of stone clacking on stone heralds the assault of these creatures, who dive from high in the air to ram soft mortals." 10 10))

(define fiend (create-aggressor 'fiend gate-north "These creatures envelop themselves in their great, black wings. As they attack, the air itself whimpers under the weight of the creature it is damned to hold aloft." 15 12))
(define fiend (create-aggressor 'fiend gate-south "These creatures envelop themselves in their great, black wings. As they attack, the air itself whimpers under the weight of the creature it is damned to hold aloft." 15 12))

(define vampire (create-aggressor 'vampire tower-1-a "These creatures fill you with an inexplicable urge to eat delicious, chocolatey cereal, and you feel compelled to count aloud as you hack them down." 10 15))
(define vampire (create-aggressor 'vampire tower-1-f "These creatures fill you with an inexplicable urge to eat delicious, chocolatey cereal, and you feel compelled to count aloud as you hack them down." 10 15))
(define vampire (create-aggressor 'vampire tower-1-m "These creatures fill you with an inexplicable urge to eat delicious, chocolatey cereal, and you feel compelled to count aloud as you hack them down." 10 15))

(define clanking-automaton (create-aggressor 'clanking-automaton tower-1-c "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))
(define clanking-automaton (create-aggressor 'clanking-automaton tower-1-c "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))
(define clanking-automaton (create-aggressor 'clanking-automaton tower-1-j "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))
(define clanking-automaton (create-aggressor 'clanking-automaton tower-1-j "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))
(define clanking-automaton (create-aggressor 'clanking-automaton tower-great-hall "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))
(define clanking-automaton (create-aggressor 'clanking-automaton tower-great-hall "These machines have long since slain their makers, and now they roam these empty halls in search of further mortals to punish." 15 15))

(define mechanical-monstrosity (create-aggressor 'mechanical-monstrosity tower-great-hall "You cannot fathom why anyone would build a weapon so large that it could never be moved from the room in which it was made." 50 25))

(define abaddon-the-despoiler (create-aggressor 'abaddon-the-despoiler twisted-portal "This hulking monstrosity has leveled entire civilizations. The souls of its victims are damned to circle the portal here for all eternity." 75 50))

(define phantasm-of-hate (create-aggressor 'phantasm-of-hate agony-c "This demigod takes the form of a woman wielding a great mace nearly twice her size. She is naked but for the shroud dust kept swirling by the ceaseless violence she wreaks on everything about her. The bodies of her victims are strewn about for all to see, and she refuses their souls exit from their shattered remains." 100 40))
(define spectre-of-despair (create-aggressor 'spectre-of-despair agony-a "When you first see this pitiful man, he is huddled to the ground, weeping over a child in his arms. At the fall of your footsteps, the long dead child evaporates into the air, and the father lets out a scream so agonized that for a moment God himself flees the room. You owe this man release from his eternal torment." 100 40))
(define shade-of-loathing (create-aggressor 'shade-of-loathing agony-d "This demigod appears as a woman wearing the purest white. Her eyes are more beautiful than the stars themselves, and yet the deepest abyss shows through from behind them. This creature knows nothing of joy or love, but she has long lost the ability to hide her pain from those she would ravage to abate it. A bit of steel glints among the fabric of her dress as she approaches." 100 40))
(define wraith-of-lamentation (create-aggressor 'wraith-of-lamentation agony-b "This soul was once a great king of men. Blind with zeal, he ruined his people for the sake of conquest, and returned victorious to face his execution. Now he roams this place, tortured by the gods and by himself for his failure, forced to act as executor of the punishments of the damned. He has long since paid his debt." 100 40))

(define magroth-ul-lord-of-chaos (create-aggressor 'magroth-ul-lord-of-chaos agony-4 "Before you stands he who would end the world. He is but a man. A mortal. And yet he is the lord of this realm." 300 50))

(define killjoy (create-aggressor 'Killjoy storage-cupboard "Killjoy is slaying a biscuit." 1 1))
(define fluffy (create-aggressor 'Fluffy storage-cupboard "Fluffy is slaying a biscuit." 1 1))



;-----------------------Making the player-------------
(if (= *choice* 1)
    (let* ((klaus (create-player 'Klaus dungeon-start "Klaus is that is."))
           (wooden-plank (create-weapon 'wooden-plank (ask klaus 'INVENTORY) 
                                         "Not very sturdy." 5))
           (clothes (create-armor 'old-clothes (ask klaus 'INVENTORY) "Rather worn." 0)))
      (ask klaus 'WEAR clothes)
      (ask klaus 'WIELD wooden-plank)
      ;(ask klaus 'LEARN first-aid)
      (set! *avatar* klaus)))

(if (= *choice* 2)
  (let* ((toshi (create-player 'Toshi dungeon-start "hankou wa mueki."))
        (short-staff (create-weapon 'short-staff (ask toshi 'INVENTORY)
                   "Hasn't seen much combat." 1))
        (clothes (create-armor 'old-clothes (ask toshi 'INVENTORY) "Rather worn." 0)))
    (ask toshi 'WEAR clothes)
    (ask toshi 'WIELD short-staff)
    (ask toshi 'LEARN shock)
    (ask toshi 'LEARN minor-heal)
    (set! *avatar* toshi)))


;---------------------------Executing the Game------------------

(define (proceed)
  (if (not (= *quit* 1))
      (begin (run-clock 1)
             (proceed))))

(begin (print-room dungeon-start) (if #f 0)) ;avoid nasty MESSAGE-DISPLAYED
(proceed)