#lang racket/gui

(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(require "abilities.rkt")
(require "random.rkt")
(require "constants.rkt")
;---------------------------------------checker_exports------------------------------------------------
(provide next-state)
(provide next-state-bird)
(provide next-state-bird-onspace)
(provide change)

(provide get-pipes)
(provide get-pipe-x)
(provide next-state-pipes)
(provide add-more-pipes)
(provide clean-pipes)
(provide move-pipes)

(provide invalid-state?)
(provide check-ground-collision)
(provide check-pipe-collisions)

(provide draw-frame)

(provide get-initial-state)
(provide get-bird)
(provide get-bird-y)
(provide get-bird-v-y)

; pipe
(provide get-pipes)
(provide get-pipe-x)

; score25
(provide get-score)

(provide get-abilities)
(provide get-abilities-visible)
(provide get-abilities-active)
; variables
(provide get-variables)
(provide get-variables-gravity)
(provide get-variables-momentum)
(provide get-variables-scroll-speed)

; bird struct
(struct bird-struct (y v-y))
; pipe struct
(struct pipe-struct (x gap-y))
; variables struct
(struct variables-struct (gravity momentum scroll-speed))
; state struct
(struct state-struct (bird pipes score variables abilities))

(define (get-initial-state)
  (let ([init-bird (bird-struct bird-initial-y 0)]
        [init-pipes (list (pipe-struct scene-width (+ added-number (random random-threshold))))]
        [init-score 0]
        [init-variables (variables-struct initial-gravity initial-momentum initial-scroll-speed)]
        [init-abilities (list (fill-abilities null DISPLAYED_ABILITIES ABILITIES) null)]
        )
  (state-struct init-bird init-pipes init-score init-variables init-abilities)
    )
)

(define (get-bird state)
  (state-struct-bird state))
(define (get-bird-y bird)
  (bird-struct-y bird))

(define (next-state-bird bird gravity)
  (bird-struct (+ (get-bird-y bird) (get-bird-v-y bird)) (+ (get-bird-v-y bird) gravity)))

(define (get-bird-v-y bird)
  (bird-struct-v-y bird))

(define (next-state-bird-onspace bird momentum)
  (bird-struct (get-bird-y bird) (- momentum)))

(define (change current-state pressed-key)
  (if (equal? pressed-key " ")
      (struct-copy state-struct current-state [bird (next-state-bird-onspace (get-bird current-state) initial-momentum)])
      current-state
      )
  )

(define (get-pipes state)
  (state-struct-pipes state))

(define(get-pipe-x pipe)
  (pipe-struct-x pipe))

(define (move-pipes pipes scroll-speed)
  (cond [(not (empty? pipes)) (map (λ(pipe) (struct-copy pipe-struct pipe [x (- (pipe-struct-x pipe) scroll-speed)]))  pipes)]
        [else pipes])
  )

(define (clean-pipes pipes)
  (filter (λ(pipe) (> (+ (pipe-struct-x pipe) pipe-width) 0)) pipes))


(define (add-more-pipes pipes)
  (cond
    [(< (length pipes) no-pipes)
     (append pipes (list (pipe-struct (+ (+ pipe-width pipe-gap) (if (empty? pipes) scene-width (pipe-struct-x (last pipes))))
                                      (+ added-number (random random-threshold)))))]
    [else pipes]
    )
  )

(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

(define (get-score state)
  (state-struct-score state))

(define (check-ground-collision bird)
  (>= (+ (get-bird-y bird) bird-height) (+ ground-y (quotient ground-height 2)))
  )

(define (invalid-state? state)
  (or (check-ground-collision (get-bird state)) (check-pipe-collisions (get-bird state) (get-pipes state)))
  )

(define (check-pipe-collisions bird pipes)
  (letrec
      ([A1 (make-posn bird-x (get-bird-y bird))]
       [A2 (make-posn (+ bird-x bird-width) (+ (get-bird-y bird) bird-height))]
       [get-B1 (λ(pipe) (make-posn (get-pipe-x pipe) (- (pipe-struct-gap-y pipe) pipe-height)))]
       [get-B2 (λ(pipe) (make-posn (+ (get-pipe-x pipe) pipe-width) (pipe-struct-gap-y pipe)))]
       [get-C1 (λ(pipe) (make-posn (get-pipe-x pipe) (+ (pipe-struct-gap-y pipe) pipe-self-gap)))]
       [get-C2 (λ(pipe) (make-posn (+ (get-pipe-x pipe) pipe-width) pipe-height))]
       )
      (ormap (λ(pipe)
                (cond [(check-collision-rectangles A1 A2 (get-B1 pipe) (get-B2 pipe)) #t]
                      [(check-collision-rectangles A1 A2 (get-C1 pipe) (get-C2 pipe)) #t]
                      [else #f]
                      ) ) pipes)
    )
  )

(define (check-collision-rectangles A1 A2 B1 B2)
  (match-let ([(posn AX1 AY1) A1]
              [(posn AX2 AY2) A2]
              [(posn BX1 BY1) B1]
              [(posn BX2 BY2) B2])
    (and (< AX1 BX2) (> AX2 BX1) (< AY1 BY2) (> AY2 BY1))))

(define (next-state state)
  (state-struct (next-state-bird (get-bird state) initial-gravity)
                (next-state-pipes (get-pipes state) (variables-struct-scroll-speed (state-struct-variables state)))
                (+ (get-score state) 0.1)
                (next-variables get-variables (get-abilities-active (get-abilities state)))
                (next-abilities (get-abilities state) (get-bird state) (variables-struct-scroll-speed (state-struct-variables state)))
                )
  )


(define bird-image (rectangle bird-width bird-height  "solid" "yellow"))
(define ground-image (rectangle scene-width ground-height "solid" "brown"))
(define initial-scene (rectangle scene-width scene-height "solid" "white"))

(define full-pipe (rectangle pipe-width pipe-height "solid" "green"))

(define text-family (list "Gill Sans" 'swiss 'normal 'bold #f))
(define (score-to-image x)
(if SHOW_SCORE
	(apply text/font (~v (round x)) 24 "indigo" text-family)
	empty-image))

(define (draw-frame state)
  (let* ([bird-level-x (+ bird-x (quotient bird-width 2))]
        [bird-level-y (+ (get-bird-y (get-bird state)) (quotient bird-height 2))]
        [ground-level-x (quotient scene-width 2)]
        [ground-level-y (+ ground-y (quotient ground-height 2))]
        [score-level (score-to-image (get-score state))]
        [ability-level (place-visible-abilities (get-abilities-visible (get-abilities state)) initial-scene)]
        [pipe-level (place-pipes (get-pipes state) ability-level)]
        )
    (place-image bird-image bird-level-x bird-level-y
                 (place-image ground-image ground-level-x ground-level-y
                              (place-active-abilities (get-abilities-active (get-abilities state))
                                                      (place-image score-level text-x text-y
                                                                   pipe-level))))
    )
  )

(define (place-pipes pipes scene)
  (letrec (
           [get-x (λ(pipe) (+ (get-pipe-x pipe) (quotient pipe-width 2)))]
           [get-y-up (λ(pipe) (- (pipe-struct-gap-y pipe) (quotient pipe-height 2)))]
           [get-y-down (λ(pipe) (+ (+ (pipe-struct-gap-y pipe) pipe-self-gap) (quotient pipe-height 2)))]
           [image-list (foldl (λ(pipe acc)
                                (append acc (list full-pipe full-pipe))) null pipes)]
           [pos-list (foldl (λ(pipe acc) (append acc (list (make-posn (get-x pipe) (get-y-up pipe)) (make-posn (get-x pipe) (get-y-down pipe))))) null pipes)]
           )
    (place-images image-list pos-list scene)
    )
  )

(define slow-ability (abilities-struct (hourglass "mediumseagreen") 10 null (λ(scroll-speed) (max 5 (sub1 scroll-speed)))))

(define fast-ability (abilities-struct (hourglass "tomato") 30 null (λ(scroll-speed) (add1 scroll-speed))))

(define ABILITIES (list fast-ability slow-ability))


(define get-variables (state-struct-variables (get-initial-state)))
(define get-variables-gravity (variables-struct-gravity get-variables))
(define get-variables-momentum (variables-struct-momentum get-variables))
(define get-variables-scroll-speed (variables-struct-scroll-speed get-variables))

(define (get-abilities state) (state-struct-abilities state))

(define (get-abilities-visible abilities)
  (cond [(empty? abilities) null]
        [else (first abilities)]
        )
  )

(define (get-abilities-active abilities)
  (cond [(empty? abilities) null]
        [(= 1 (length abilities)) null]
        [else (last abilities)]
        )
  )

(define (clean-abilities abilities)
  (cond [(empty? abilities) abilities]
        [else (filter (λ(ability) (> (+ (posn-x (get-ability-pos ability)) (image-width (get-ability-image ability))) 0)) abilities)])
  )


(define (move-abilities abilities scroll-speed)
  (letrec ([get-x (λ(ability) (posn-x (get-ability-pos ability)))]
           [get-y (λ(ability) (posn-y (get-ability-pos ability)))]
           )
    (cond
      [(not (empty? abilities))
       (map (λ(ability) (struct-copy abilities-struct ability [pos (make-posn (- (get-x ability) scroll-speed) (get-y ability))])) abilities)]
      [else null])
    )
  )

(define (time-counter abilities)
  (let ([filtered-abilities (filter (λ(ability) (and (not (null? ability)) (> (get-ability-time ability) 0))) abilities)])
  (cond [(not (null? filtered-abilities))
         (map (λ(ability) (struct-copy abilities-struct ability [time (- (get-ability-time ability) (/ 1 fps))])) filtered-abilities)]
        [else filtered-abilities]
        )
    )
  )

(define (next-abilities-visible visible scroll-speed)
  (fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES)
  )

(define (next-abilities abilities bird scroll-speed)
  (letrec ([active? (λ(ability) (let* ([ability-height (image-height (get-ability-image ability))]
                                      [ability-width (image-width (get-ability-image ability))]
                                      [get-x (- (posn-x (get-ability-pos ability)) (quotient ability-width 2))]
                                      [get-y (- (posn-y (get-ability-pos ability)) (quotient ability-height 2))]
                                      [A1 (make-posn bird-x (get-bird-y bird))]
                                      [A2 (make-posn (+ bird-x bird-width) (+ (get-bird-y bird) bird-height))]
                                      [B1 (make-posn get-x get-y)]
                                      [B2 (make-posn (+ get-x ability-width) (+ get-y ability-height))]
                                      )
                                  (check-collision-rectangles A1 A2 B1 B2)
                                  ))]
           [visibles (foldl (λ(ability acc) (cond [(not (active? ability)) (append acc (list ability))] [else (append acc null)]))
                            null (next-abilities-visible (get-abilities-visible abilities) scroll-speed))]
           [new-actives (foldl (λ(ability acc) (cond [(active? ability) (append acc (list ability))] [else (append acc null)]))
                               null (get-abilities-visible abilities))]
           [actives (append (get-abilities-active abilities) new-actives)]
           )
    (list visibles (time-counter actives))
      )
  )

(define (next-variables variables abilities)
  (struct-copy variables-struct variables [scroll-speed (cond [(null? abilities) initial-scroll-speed]
                                                              [else ((compose-abilities abilities) get-variables-scroll-speed)])])
  )


(define (place-visible-abilities abilities scene)
  (letrec (
           [ability-height (λ(ability) (image-height (get-ability-image ability)))]
           [ability-width (λ(ability) (image-width (get-ability-image ability)))]
           [get-x (λ(ability) (posn-x (get-ability-pos ability)) )]
           [get-y (λ(ability) (posn-y (get-ability-pos ability)) )]
           [image-list (foldl (λ(ability acc)
                                (append acc (list (get-ability-image ability)))) null abilities)]
           [pos-list (foldl (λ(ability acc) (append acc (list (make-posn (get-x ability) (get-y ability))))) null abilities)]
           )
    (place-images image-list pos-list scene)
    )
  )

(define (place-active-abilities abilities scene)
  (letrec ([get-x (λ(i) (- (posn-x abilities-posn) (* 50 i)))]
           [image-list (foldl (λ(ability acc)
                                (append acc (list (scale 0.75 (get-ability-image ability))))) null abilities)]
           [pos-list (foldl (λ(ability acc) (append acc (list (make-posn (get-x (index-of abilities ability)) (posn-y abilities-posn))))) null abilities)]
           )
    (place-images image-list pos-list scene)
    )
  )

(module+ main
	(big-bang (get-initial-state)
	 [on-tick next-state (/ 1.0 fps)]
	 [to-draw draw-frame]
	 [on-key change]
	 [stop-when invalid-state?]
	 [close-on-stop #t]
	 [record? #f]))
