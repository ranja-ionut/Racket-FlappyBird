#lang racket/gui
;Ignorați următoarele linii de cod. Conțin import-uri și export-uri necesare checker-ului.

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

;---------------------------------------checker_exports------------------------------------------------
; Checker-ul contine un numar de teste, fiecare cu numele sau. In acest fisier veti gasi comentarii
; care incep cu TODO %nume_test, unde trebuie sa modificati sau sa implementati o functie, pentru
; a trece testul %nume_test.
;
;Initial state
; Primul pas pe care trebuie sa il facem este sa cream starea initiala a jocului.
; Aceasta va fi salvata in (get-initial-state), si trebuie sa incapsuleze toate informatiile
; necesare jocului, si anume: informatii despre pasare, despre pipes si despre powerups.
; Recomandam ca in pasare, sa retineti, printre altele, informatii despre y-ul curent
; si viteza pe y
; Pe parcursul temei, in state, salvati coordonatele colturilor din stanga sus ale obiectelor.
; Aceasta va face mai usoara atat logica miscarii obiectelor, cat si testarea cerintelor.
; Toate coordonatele oferite in comentarii sau in fisierul constants.rkt, se refera la
; coltul din stanga sus ale obiectelor!
;Inițial state
; Primul pas pe care trebuie să îl facem este să creăm starea inițială a jocului.
; Aceasta va fi salvată în (get-initial-state), și trebuie să incapsuleze toate informațiile
; necesare jocului, și anume: informații despre pasăre, despre pipes și, pentru bonus,
; despre powerups și despre variabilele de mediu.
; Recomandăm ca în pasăre, să rețineți, printre altele, informații despre y-ul curent
; și viteză pe y.
; Pe parcursul temei, în state, salvați coordonatele colțurilor din stânga sus ale obiectelor.
; Aceasta va face mai ușoară atât logică mișcării obiectelor, cât și testarea cerințelor.
; Toate coordonatele oferite în comentarii sau în fișierul variables.rkt se referă la
; colțul din stânga sus ale obiectelor!

; bird struct
(struct bird-struct (y v-y))
; pipe struct
(struct pipe-struct (x gap-y))
; variables struct
(struct variables-struct (gravity momentum scroll-speed))
; state struct
(struct state-struct (bird pipes score variables abilities))

;TODO 1
; După ce definiți structurile lui (get-initial-state) și a păsării, introduceți în prima
; pe cea din urmă. Colțul din stânga sus a păsării se va află inițial la:
;    y = bird-inițial-y
; și x = bird-x.
; (get-initial-state) va fi o funcție care va returna starea inițială a jocului

;TODO 8
; În starea jocului, trebuie să păstrăm informații despre pipes. Pe parcursul jocului,
; pipe-urile se vor schimba, unele vor fi șterse și vor fi adăugate altele.
; După ce definiți structura pentru pipe și pentru mulțimea de pipes din stare,
; adăugați primul pipe în starea jocului. Acesta se va află inițial în afară ecranului.
; Celelalte pipe-uri vor fi adăugate ulterior, poziționându-le după acest prim pipe.
; Atenție! Fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap.
; Colțul din stânga sus al gap-ului dintre componentele primului pipe se va afla inițial la:
;    y = (+ added-number (random random-threshold)), pentru a da un element de noroc jocului,
; și x = scene-width,
; pentru a-l forța să nu fie inițial pe ecran.
; Atenție! Recomandăm să păstrați în stare colțul din stânga sus al chenarului lipsa
; dintre cele 2 pipe-uri!

;TODO 16
; Vrem o modalitate de a păstra scorul jocului. După ce definiți structura
; acestuia, adăugați scorul inițial, adică 0, în starea inițială a jocului.
; Atenție get-initial-state trebuie sa fie o funcție
; și trebuie apelată în restul codului.
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
;TODO 2
; După aceasta, implementați un getter care extrage din structura voastră
; pasărea, și un al doilea getter care extrage din structura pasăre
; y-ul curent pe care se află această.
(define (get-bird state)
  (state-struct-bird state))
(define (get-bird-y bird)
  (bird-struct-y bird))

;TODO 3
; Trebuie să implementăm logică gravitației. next-state-bird va primi drept
; parametri o structură de tip pasăre, și gravitația(un număr real). Aceasta va adaugă
; pozitiei pe y a păsării viteza acesteia pe y, si va adaugă vitezei pe y a păsării,
; gravitația.
(define (next-state-bird bird gravity)
  (bird-struct (+ (get-bird-y bird) (get-bird-v-y bird)) (+ (get-bird-v-y bird) gravity)))

;TODO 4
; După aceasta, implementati un getter care extrage din structura voastră
; viteza pe y a păsării.
(define (get-bird-v-y bird)
  (bird-struct-v-y bird))

;TODO 6
; Dorim să existe un mod prin care să imprimăm păsării un impuls.
; Definiți funcția next-state-bird-onspace care va primi drept parametri
; o structură de tip pasăre, momentum(un număr real), și va schimba viteza
; pe y a păsării cu -momentum.
(define (next-state-bird-onspace bird momentum)
  (bird-struct (get-bird-y bird) (- momentum)))

; Change
; Change va fi responsabil de input-ul de la tastatură al jocului.
;TODO 7
; Acesta va primi drept parametri o structură de tip stare, și tasta pe
; care am apăsat-o. Aceasta va imprimă păsării momentum-ul, apelând
; funcția next-state-bird-onspace. Pentru orice altă tasta, starea rămâne aceeași.
(define (change current-state pressed-key)
  (if (equal? pressed-key " ")
      (struct-copy state-struct current-state [bird (next-state-bird-onspace (get-bird current-state) initial-momentum)])
      current-state
      )
  )

;TODO 9
; După ce ați definit structurile pentru mulțimea de pipes și pentru un singur pipe,
; implementați getterul get-pipes, care va extrage din starea jocului mulțimea de pipes,
; sub formă de lista.
(define (get-pipes state)
  (state-struct-pipes state))

;TODO 10
; Implementați get-pipe-x ce va extrage dintr-o singură structura de tip pipe, x-ul acesteia.
(define(get-pipe-x pipe)
  (pipe-struct-x pipe))

;TODO 11
; Trebuie să implementăm logica prin care se mișcă pipes.
; Funcția move-pipes va primi drept parametri mulțimea pipe-urilor din stare
; și scroll-speed(un număr real). Aceasta va scădea din x-ul fiecărui pipe
; scroll-speed-ul dat.
(define (move-pipes pipes scroll-speed)
  (cond [(not (empty? pipes)) (map (λ(pipe) (struct-copy pipe-struct pipe [x (- (pipe-struct-x pipe) scroll-speed)]))  pipes)]
        [else pipes])
  )

;TODO 12
; Vom implementa logica prin care pipe-urile vor fi șterse din stare. În momentul
; în care colțul din DREAPTA sus al unui pipe nu se mai află pe ecran, acesta trebuie
; șters.
; Funcția va primi drept parametru mulțimea pipe-urilor din stare.
;
; Hint: cunoaștem lățimea unui pipe, pipe-width
(define (clean-pipes pipes)
  (filter (λ(pipe) (> (+ (pipe-struct-x pipe) pipe-width) 0)) pipes))


;TODO 13
; Vrem să avem un sursa continuă de pipe-uri.
; Implementati funcția add-more-pipes, care va primi drept parametru mulțimea pipe-urilor
; din stare și, dacă avem mai puțin de no-pipes pipe-uri, mai adăugăm una la mulțime,
; având x-ul egal cu pipe-width + pipe-gap + x-ul celui mai îndepărtat pipe, în raport
; cu pasărea.
(define (add-more-pipes pipes)
  (cond
    [(< (length pipes) no-pipes)
     (append pipes (list (pipe-struct (+ (+ pipe-width pipe-gap) (if (empty? pipes) scene-width (pipe-struct-x (last pipes))))
                                      (+ added-number (random random-threshold)))))]
    [else pipes]
    )
  )

;TODO 14
; Vrem ca toate funcțiile implementate anterior legate de pipes să fie apelate
; de către next-state-pipes.
; Aceasta va primi drept parametri mulțimea pipe-urilor și scroll-speed-ul,
; și va apela cele trei funcții implementate anterior, în această ordine:
; move-pipes, urmat de clean-pipes, urmat de add-more pipes.
(define (next-state-pipes pipes scroll-speed)
  (add-more-pipes (clean-pipes (move-pipes pipes scroll-speed))))

;TODO 17
; Creați un getter ce va extrage scorul din starea jocului.
(define (get-score state)
  (state-struct-score state))

;TODO 19
; Vrem să creăm logica coliziunii cu pământul.
; Implementati check-ground-collision, care va primi drept parametru
; o structura de tip pasăre, și returnează true dacă aceasta are coliziune
; cu pământul.
;
; Hint: știm înălțimea păsării, bird-height, și y-ul pământului, ground-y.
; Coliziunea ar presupune ca un colț inferior al păsării să aibă y-ul
; mai mare sau egal cu cel al pământului.
(define (check-ground-collision bird)
  (>= (+ (get-bird-y bird) bird-height) (+ ground-y (quotient ground-height 2)))
  )

; invalid-state?
; invalid-state? îi va spune lui big-bang dacă starea curentă mai este valida,
; sau nu. Aceasta va fi validă atât timp cât nu avem coliziuni cu pământul
; sau cu pipes.
; Aceasta va primi ca parametru starea jocului.

;TODO 20
; Vrem să integrăm verificarea coliziunii cu pământul în invalid-state?.

;TODO 22
; Odată creată logică coliziunilor dintre pasăre și pipes, vrem să integrăm
; funcția nou implementată în invalid-state?.
(define (invalid-state? state)
  (or (check-ground-collision (get-bird state)) (check-pipe-collisions (get-bird state) (get-pipes state)))
  )

;TODO 21
; Odată ce am creat pasărea, pipe-urile, scor-ul și coliziunea cu pământul,
; următorul pas este verificarea coliziunii dintre pasăre și pipes.
; Implementati funcția check-pipe-collisions care va primi drept parametri
; o structură de tip pasăre, mulțimea de pipes din stare, și va returna
; true dacă există coliziuni, și false în caz contrar. Reiterând,
; fiecare pipe este format din 2 părți, cea superioară și cea inferioară,
; acestea fiind despărțite de un gap de înălțime pipe-self-gap. Pot există
; coliziuni doar între pasăre și cele două părți. Dacă pasărea se află în
; chenarul lipsă, nu există coliziune.
;
; Hint: Vă puteți folosi de check-collision-rectangle, care va primi drept parametri
; colțul din stânga sus și cel din dreapta jos ale celor două dreptunghiuri
; pe care vrem să verificăm coliziunea.
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

;Next-state
; Next-state va fi apelat de big-bang la fiecare cadru, pentru a crea efectul de
; animație. Acesta va primi ca parametru o structură de tip stare, și va întoarce
; starea corespunzătoare următorului cadru.

;TODO 5
; Trebuie să integrăm funcția implementată anterior, și anume next-state-bird,
; în next-state.

;TODO 15
; Vrem să implementăm logică legată de mișcarea, ștergerea și adăugarea pipe-urilor
; în next-state. Acesta va apela next-state-pipes pe pipe-urile din starea curentă.

;TODO 18
; Vrem ca next-state să incrementeze scorul cu 0.1 la fiecare cadru.
(define (next-state state)
  (state-struct (next-state-bird (get-bird state) initial-gravity)
                (next-state-pipes (get-pipes state) (variables-struct-scroll-speed (state-struct-variables state)))
                (+ (get-score state) 0.1)
                (next-variables get-variables (get-abilities-active (get-abilities state)))
                (next-abilities (get-abilities state) (get-bird state) (variables-struct-scroll-speed (state-struct-variables state)))
                )
  )

; draw-frame
; draw-frame va fi apelat de big-bang dupa fiecare apel la next-state, pentru a afisa cadrul curent.
;TODO 23
; Fiecare cadru va fi desenat in urmatorul mod:
; bird peste ground, peste scor, peste pipes, peste empty-scene.
;
; Hint: score-to-image primeste un numar real si intoarce scor-ul sub forma de imagine;
; Scor-ul îl puteți plasa direct la coordonatele date, fără a mai face translatiile menționate mai jos.
; Noi tinem minte coltul din stanga sus al imaginii, insa, la suprapunerea unei imagini A peste o alta imagine,
; coordonatele unde plasam imaginea A reprezinta centrul acesteia. Trebuie facute translatiile de la coltul din stanga
; sus la centrul imaginilor.
; Variabile folosite in aceasta functie:
; bird -> bird-width si bird-height
; ground -> ground-y si ground-height, acesta va acoperi intreaga latime a ecranului
; scor -> text-x si text-y
; pipes -> pipe-width si pipe-height
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

; Folosind `place-image/place-images` va poziționa pipe-urile pe scenă.
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

; Bonus
; Completați abilities.rkt mai întâi, aceste funcții căt, apoi legați
; această funcționalitate la jocul inițial.


; Abilitatea care va încetini timpul va dura 10 de secunde, va avea imaginea (hourglass "mediumseagreen")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = max(5, scroll-speed - 1)
(define slow-ability (abilities-struct (hourglass "mediumseagreen") 10 null (λ(scroll-speed) (max 5 (sub1 scroll-speed)))))

; Abilitatea care va accelera timpul va dura 30 de secunde, va avea imaginea (hourglass "tomato")
; va avea inițial poziția null si va modifica scrolls-speed dupa formulă
; scroll-speed = scroll-speed + 1
(define fast-ability (abilities-struct (hourglass "tomato") 30 null (λ(scroll-speed) (add1 scroll-speed))))

; lista cu toate abilităţile posibile în joc
(define ABILITIES (list fast-ability slow-ability))


(define get-variables (state-struct-variables (get-initial-state)))
(define get-variables-gravity (variables-struct-gravity get-variables))
(define get-variables-momentum (variables-struct-momentum get-variables))
(define get-variables-scroll-speed (variables-struct-scroll-speed get-variables))

; Întoarce abilităţile din stare, cu o reprezentare
; intermediară care trebuie să conțină două liste:
;  - lista abilităţilor vizibile (încarcate în scenă dar nu neaparat vizibile pe ecran).
;  - lista abilităţilor activate (cu care pasărea a avut o coloziune).
(define (get-abilities state) (state-struct-abilities state))

; Întoarce abilităţile vizibile din reprezentarea intermediară.
(define (get-abilities-visible abilities)
  (cond [(empty? abilities) null]
        [else (first abilities)]
        )
  )

; Întoarce abilităţile active din reprezentarea intermediară.
(define (get-abilities-active abilities)
  (cond [(empty? abilities) null]
        [(= 1 (length abilities)) null]
        [else (last abilities)]
        )
  )

; Șterge din reprezentarea abilităţilor vizibile pe cele care nu mai sunt vizibile.
; echivalent cu clean-pipes.
(define (clean-abilities abilities)
  (cond [(empty? abilities) abilities]
        [else (filter (λ(ability) (> (+ (posn-x (get-ability-pos ability)) (image-width (get-ability-image ability))) 0)) abilities)])
  )


; Muta abilităţile vizibile spre stanga.
; echivalent cu move-pipes.
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


; Scurge timpul pentru abilităţile activate și le sterge pe cele care au expirat.
; Puteți să va folosiți de variabila globală fps.
(define (time-counter abilities)
  (let ([filtered-abilities (filter (λ(ability) (and (not (null? ability)) (> (get-ability-time ability) 0))) abilities)])
  (cond [(not (null? filtered-abilities))
         (map (λ(ability) (struct-copy abilities-struct ability [time (- (get-ability-time ability) (/ 1 fps))])) filtered-abilities)]
        [else filtered-abilities]
        )
    )
  )

; Generează următoarele abilitați vizibile.
; *Atentie* La orice moment pe scena trebuie să fie exact DISPLAYED_ABILITIES
; abilităţi vizibile
; Folosiți funcția fill-abilities din abilities.rkt cât si cele scrise mai sus:
; move-abilities, clean-abilities, time-counter, etc..
(define (next-abilities-visible visible scroll-speed)
  (fill-abilities (clean-abilities (move-abilities visible scroll-speed)) DISPLAYED_ABILITIES ABILITIES)
  )

; Generează structura intermediară cu abilități.
; Observați ca nu există next-abilities-active aceastea sunt acele abilităti
; întoarse next-abilities-visible care au o coliziune cu pasărea.
; Puteti folosi `filer`/`filter-not` ca sa verificați ce abilităti au și abilitați
; nu au coliziuni cu pasărea sau puteti folosi `partition`
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

; Dând-use variabilele actuale și abilitațile calculați care vor
; variabile finale folosite în joc
; Folositi compose-abilities
; Atenție când apelați `next-variables` în next-state dați ca paremetru
; initial-variables și nu variabilele aflate deja în stare
; In felul acesta atunci când
(define (next-variables variables abilities)
  (struct-copy variables-struct variables [scroll-speed (cond [(null? abilities) initial-scroll-speed]
                                                              [else ((compose-abilities abilities) get-variables-scroll-speed)])])
  )


; Folosind `place-image/place-images` va poziționa abilităţile vizibile la ability pos.
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

; Folosind `place-image/place-images` va poziționa abilităţile active
; în partea de sus a ecranului lângă scor.
; Imaginiile vor scalate cu un factor de 0.75 și așezate plecând
; de la ability-posn (constantă globală) cu spații de 50 de px.
; Imaginea cu indexul i va fi așezată la (ability-posn.x - 50*i, ability-posn.y)
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
