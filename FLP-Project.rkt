#lang eopl

; ==========================================================
; Interpretador - ProyectoFinal
; Integrantes:
; Sebastian
; Jairo
; Luis Alberto Carabali Rivera 202410006-3743
; URL repositorio GitHub: https://github.com/sebaspapu/FLP-ProyectoFinal
; ==========================================================

;; ============================================================
;; MATHFLOW - INTÉRPRETE
;; Paso 1: Especificación Léxica
;; ============================================================
;; La especificación léxica le dice a SLLGEN cómo dividir
;; el texto en "tokens" (piezas básicas del lenguaje).
;; Cada regla tiene la forma:
;;   (nombre-token  expresion-regular  tipo)
;; ============================================================

(define lexica
  '(
    ;; ----------------------------------------------------------
    ;; 1. ESPACIOS EN BLANCO Y COMENTARIOS (se ignoran)
    ;; ----------------------------------------------------------
    ;; Cualquier espacio, tab o salto de línea se descarta
    (espacio-blanco  (whitespace)  skip)

    ;; Comentarios con # hasta el final de la línea
    (comentario ("#" (arbno (not #\newline))) skip)

    ;; ----------------------------------------------------------
    ;; 2. NÚMEROS
    ;; ----------------------------------------------------------
    ;; Número flotante: dígitos, punto, más dígitos
    ;; Ej: 3.14  9.5  0.0
    (numero-flotante
      (digit (arbno digit) "." digit (arbno digit))
      number)

    ;; Número entero: uno o más dígitos
    ;; Ej: 0  42  100
    (numero-entero
      (digit (arbno digit))
      number)

    ;; ----------------------------------------------------------
    ;; 3. IDENTIFICADORES
    ;; ----------------------------------------------------------
    ;; Empieza con letra, puede tener letras, dígitos o guión bajo
    ;; Ej: x  miVariable  nombre1
    (identificador
      (letter (arbno (or letter digit)))
      symbol)

    ;; ----------------------------------------------------------
    ;; 4. CADENAS DE TEXTO
    ;; ----------------------------------------------------------
    ;; Texto entre comillas dobles
    ;; Ej: "hola"  "Robinson"  "texto con espacios"
    (cadena
      ("\"" (arbno (not #\")) "\"")
      string)

    ;; ----------------------------------------------------------
    ;; 5. OPERADORES DOBLES (van antes que los simples)
    ;; ----------------------------------------------------------
    ;; Importante: los de 2 caracteres van PRIMERO
    ;; para que no se confundan con los de 1 caracter

    (op-igual-igual    ("==")   symbol)   ;; igualdad
    (op-diferente      ("<>")   symbol)   ;; diferente
    (op-menor-igual    ("<=")   symbol)   ;; menor o igual
    (op-mayor-igual    (">=")   symbol)   ;; mayor o igual

    ;; ----------------------------------------------------------
    ;; 6. OPERADORES SIMPLES
    ;; ----------------------------------------------------------
    (op-suma           ("+")    symbol)
    (op-resta          ("-")    symbol)
    (op-mult           ("*")    symbol)
    (op-div            ("/")    symbol)
    (op-modulo         ("%")    symbol)
    (op-menor          ("<")    symbol)
    (op-mayor          (">")    symbol)
    (op-asignacion     ("=")    symbol)   ;; x = valor

    ;; ----------------------------------------------------------
    ;; 7. PUNTUACIÓN
    ;; ----------------------------------------------------------
    (punto-coma        (";")    symbol)   ;; separador
    (coma              (",")    symbol)   ;; separador de args
    (dos-puntos        (":")    symbol)   ;; en diccionarios
    (punto             (".")    symbol)   ;; acceso a campo

    (par-abre          ("(")    symbol)   ;; paréntesis
    (par-cierra        (")")    symbol)

    (llave-abre        ("{")    symbol)   ;; llaves
    (llave-cierra      ("}")    symbol)

    (corchete-abre     ("[")    symbol)   ;; corchetes
    (corchete-cierra   ("]")    symbol)
  ))

;; ============================================================
;; GRAMATICA
;; ============================================================

(define gramatica
  '(
    ;; El programa es un begin implicito
    (programa
     (expresion)
     un-programa)

    ;; VALORES BÁSICOS
    (expresion (numero-entero)   exp-entero)
    (expresion (numero-flotante) exp-flotante)
    (expresion (cadena)          exp-cadena)
    (expresion ("true")          exp-true)
    (expresion ("false")         exp-false)
    (expresion ("null")          exp-null)
    (expresion (identificador) exp-identificador)

    (expresion
     ("call" identificador "(" (separated-list expresion ",") ")")
     exp-invocacion)

    ;; VARIABLES Y CONSTANTES
    (expresion ("var" (separated-list identificador "=" expresion ",") ";") exp-var)
    (expresion ("const" identificador "=" expresion ";") exp-const)
    (expresion ("set" identificador "=" expresion ";")   exp-asignacion)

    ;; OPERACIONES BINARIAS
    (expresion ("(" expresion op-binario expresion ")") exp-binaria)
    (expresion ("add1" "(" expresion ")")               exp-add1)
    (expresion ("sub1" "(" expresion ")")               exp-sub1)
    (expresion ("not"  "(" expresion ")")               exp-not)

    (op-binario ("+")   op-suma-t)
    (op-binario ("-")   op-resta-t)
    (op-binario ("*")   op-mult-t)
    (op-binario ("/")   op-div-t)
    (op-binario ("%")   op-mod-t)
    (op-binario ("==")  op-igual-t)
    (op-binario ("<>")  op-diferente-t)
    (op-binario ("<")   op-menor-t)
    (op-binario (">")   op-mayor-t)
    (op-binario ("<=")  op-menor-igual-t)
    (op-binario (">=")  op-mayor-igual-t)
    (op-binario ("and") op-and-t)
    (op-binario ("or")  op-or-t)

    ;; IF
    (expresion
     ("if" expresion "then" expresion "else" expresion "end")
     exp-if)

    ;; PRINT
    (expresion ("print" "(" expresion ")") exp-print)

    ;; BEGIN
    (expresion ("{" (arbno expresion) "}") exp-begin)

    ;; WHILE
    (expresion
     ("while" expresion "do" expresion "done")
     exp-while)

    ;; FUNC — definicion de funcion (sin return obligatorio al final)
    (expresion
     ("func" identificador "(" (separated-list identificador ",") ")"
      "{" (arbno expresion) "}")
     exp-func)

    ;; RETURN como expresión — puede usarse en cualquier parte del cuerpo
    (expresion ("return" expresion ";") exp-return)

    ;; SWITCH
    (expresion
     ("switch" expresion "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}")
     exp-switch)

    (expresion ("for" identificador "in" expresion "do" expresion "done") exp-for)

    ;; LISTAS literales: [e1, e2, ...]
    (expresion
     ("[" (separated-list expresion ",") "]")
     exp-lista)
    ))

(sllgen:make-define-datatypes lexica gramatica)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;; ============================================================
;; AMBIENTE
;; ============================================================

(define ambiente-vacio (lambda () '()))

(define buscar-variable
  (lambda (env nombre)
    (cond
      ((null? env)
       (eopl:error 'buscar-variable "Variable no definida: ~s" nombre))
      ((eq? (caar env) nombre) (car env))
      (else (buscar-variable (cdr env) nombre)))))

(define extender-ambiente
  (lambda (env nombre valor constante?)
    (cons (list nombre (vector valor) constante?) env)))

(define actualizar-ambiente
  (lambda (env nombre nuevo-valor)
    (cond
      ((null? env)
       (eopl:error 'actualizar-ambiente "Variable no definida: ~s" nombre))
      ((eq? (caar env) nombre)
       (if (caddr (car env))
           (eopl:error 'actualizar-ambiente
                       "No se puede modificar la constante: ~s" nombre)
           (begin
             (vector-set! (cadr (car env)) 0 nuevo-valor)
             env)))
      (else
       (actualizar-ambiente (cdr env) nombre nuevo-valor)))))

;; ============================================================
;; INTÉRPRETE
;; ============================================================

(define interpretar
  (lambda (codigo)
    (evaluar-programa (scan&parse codigo) (ambiente-vacio))
    (display "")))

;; Continuación de "return" activa durante la ejecución de una función.
;; #f cuando no estamos dentro de ninguna función.
(define current-return-k (make-parameter #f))

(define evaluar-programa
  (lambda (prog env)
    (cases programa prog
      (un-programa (exp)
        (let* ((r (evaluar-exp exp env)))
          (display "")
          (car r))))))

(define evaluar-secuencia
  (lambda (lista-exp env)
    (if (null? lista-exp)
        0
        (let loop ((exps lista-exp) (env-actual env) (ultimo 0))
          (if (null? exps)
              ultimo
              (let* ((resultado  (evaluar-exp (car exps) env-actual))
                     (val        (car resultado))
                     (nuevo-env  (cadr resultado)))
                (loop (cdr exps) nuevo-env val)))))))

(define evaluar-declaraciones
  (lambda (ids exps env constante?)
    (let loop ((ids ids) (exps exps) (env-actual env) (ultimo 0))
      (if (null? ids)
          (list ultimo env-actual)
          (let* ((r   (evaluar-exp (car exps) env-actual))
                 (val (car r))
                 (e2  (cadr r))
                 (e3  (extender-ambiente e2 (car ids) val constante?)))
            (loop (cdr ids) (cdr exps) e3 val))))))

(define evaluar-exp
  (lambda (exp env)
    (cases expresion exp

      (exp-entero   (n) (list n env))
      (exp-flotante (n) (list n env))
      (exp-cadena   (s) (list s env))
      (exp-true     ()  (list #t env))
      (exp-false    ()  (list #f env))
      (exp-null     ()  (list 'null env))

      ;; identificador solo, o invocacion identificador(args)
      (exp-identificador (id)
  (let ((entrada (buscar-variable env id)))
    (list (vector-ref (cadr entrada) 0) env)))

      (exp-invocacion (id args-exps)
  (let* ((entrada  (buscar-variable env id))
         (clausura (vector-ref (cadr entrada) 0)))
    (invocar-funcion clausura args-exps env)))

      (exp-var (ids exps)
        (evaluar-declaraciones ids exps env #f))

      (exp-const (id exp-val)
        (let* ((r   (evaluar-exp exp-val env))
               (val (car r))
               (e2  (cadr r)))
          (list val (extender-ambiente e2 id val #t))))

      (exp-asignacion (id exp-val)
        (let* ((r   (evaluar-exp exp-val env))
               (val (car r)))
          (list val (actualizar-ambiente env id val))))

      (exp-binaria (e1 op e2)
        (let* ((r1 (evaluar-exp e1 env))
               (v1 (car r1))
               (r2 (evaluar-exp e2 env))
               (v2 (car r2)))
          (list
           (cases op-binario op
             (op-suma-t ()
                        (cond
                          ((and (string? v1) (string? v2)) (string-append v1 v2))
                          ((string? v1) (string-append v1 (valor->string v2)))
                          ((string? v2) (string-append (valor->string v1) v2))
                          (else (+ v1 v2))))
             (op-resta-t       () (- v1 v2))
             (op-mult-t        () (* v1 v2))
             (op-div-t         ()
               (if (= v2 0)
                   (eopl:error 'division "Division entre cero")
                   (/ v1 v2)))
             (op-mod-t         () (modulo v1 v2))
             (op-igual-t       () (equal? v1 v2))
             (op-diferente-t   () (not (equal? v1 v2)))
             (op-menor-t       () (< v1 v2))
             (op-mayor-t       () (> v1 v2))
             (op-menor-igual-t () (<= v1 v2))
             (op-mayor-igual-t () (>= v1 v2))
             (op-and-t         () (and (es-verdadero? v1) (es-verdadero? v2)))
             (op-or-t          () (or  (es-verdadero? v1) (es-verdadero? v2))))
           env)))

      (exp-add1 (e)
        (let* ((r (evaluar-exp e env)) (v (car r)))
          (list (+ v 1) env)))

      (exp-sub1 (e)
        (let* ((r (evaluar-exp e env)) (v (car r)))
          (list (- v 1) env)))

      (exp-not (e)
        (let* ((r (evaluar-exp e env)) (v (car r)))
          (list (not (es-verdadero? v)) env)))

      (exp-if (cond-exp then-exp else-exp)
        (let* ((r        (evaluar-exp cond-exp env))
               (cond-val (car r)))
          (if (es-verdadero? cond-val)
              (evaluar-exp then-exp env)
              (evaluar-exp else-exp env))))

      (exp-print (e)
        (let* ((r (evaluar-exp e env)) (v (car r)))
          (display (valor->string v))
          (newline)
          (list v env)))

      (exp-begin (lista-exp)
        (let loop ((exps lista-exp) (env-actual env) (ultimo 0))
          (if (null? exps)
              (list ultimo env-actual)
              (let* ((r        (evaluar-exp (car exps) env-actual))
                     (v        (car r))
                     (nuevo-env (cadr r)))
                (loop (cdr exps) nuevo-env v)))))

      ;; WHILE
      (exp-while (cond-exp cuerpo)
        (let loop ((env-actual env) (ultimo 0))
          (let* ((r        (evaluar-exp cond-exp env-actual))
                 (cond-val (car r)))
            (if (es-verdadero? cond-val)
                (let* ((r2  (evaluar-exp cuerpo env-actual))
                       (v2  (car r2))
                       (e2  (cadr r2)))
                  (loop e2 v2))
                (list ultimo env-actual)))))

     ;; FUNC — guarda la funcion como una clausura en el ambiente
      (exp-func (nombre parametros cuerpo)
                (let* ((env-extendido (extender-ambiente env nombre 'pendiente #f))
                       (celda-funcion (cadr (car env-extendido)))
                       (clausura      (list 'clausura parametros cuerpo env-extendido)))
                  (vector-set! celda-funcion 0 clausura)
                  (list clausura env-extendido)))

      ;; RETURN — corta la ejecución de la función actual devolviendo v
      (exp-return (e)
        (let* ((r (evaluar-exp e env))
               (v (car r))
               (k (current-return-k)))
          (if k
              (k v)
              (eopl:error 'exp-return "return usado fuera de una función"))))

      ;; SWITCH
      (exp-switch (control casos-exps casos-vals default-val)
        (let* ((r (evaluar-exp control env))
               (v (car r)))
          (let buscar ((cs casos-exps) (vs casos-vals))
            (cond
              ((null? cs) (evaluar-exp default-val env))
              (else
               (let* ((rc (evaluar-exp (car cs) env))
                      (vc (car rc)))
                 (if (equal? v vc)
                     (evaluar-exp (car vs) env)
                     (buscar (cdr cs) (cdr vs)))))))))

      ;; FOR id IN exp DO cuerpo DONE
      (exp-for (id exp-iterable cuerpo)
        (let* ((r          (evaluar-exp exp-iterable env))
               (coleccion  (car r))
               (env-base   (cadr r)))
          (if (not (list? coleccion))
              (eopl:error 'exp-for "for espera una lista, se obtuvo: ~s" coleccion)
              (let loop ((elems coleccion) (env-actual env-base) (ultimo 0))
                (if (null? elems)
                    (list ultimo env-actual)
                    (let* ((env-iter (extender-ambiente env-actual id (car elems) #f))
                           (r2       (evaluar-exp cuerpo env-iter))
                           (v2       (car r2))
                           (env-post (cadr r2))
                           (env-siguiente (remover-liga env-post id)))
                      (loop (cdr elems) env-siguiente v2)))))))
      
      ;; LISTA literal: [e1, e2, ...]
      (exp-lista (exps)
        (let loop ((exps exps) (env-actual env) (acc '()))
          (if (null? exps)
              (list (reverse acc) env-actual)
              (let* ((r (evaluar-exp (car exps) env-actual))
                     (v (car r))
                     (e2 (cadr r)))
                (loop (cdr exps) e2 (cons v acc))))))
      )))

;; ============================================================
;; FUNCION AUXILIAR 
;; ============================================================
(define remover-liga
  (lambda (env nombre)
    (cond
      ((null? env) env)
      ((eq? (caar env) nombre) (cdr env))
      (else (cons (car env) (remover-liga (cdr env) nombre))))))

;; ============================================================
;; INVOCACION DE FUNCIONES
;; ============================================================

(define invocar-funcion
  (lambda (clausura args-exps env-llamador)
    (let* ((parametros     (cadr clausura))
           (cuerpo         (caddr clausura))
           (env-definicion (cadddr clausura))
           (env-funcion    (crear-ambiente-funcion parametros args-exps env-llamador env-definicion))
           (valor
            (call-with-current-continuation
             (lambda (k)
               (parameterize ((current-return-k k))
                 (let loop ((exps cuerpo) (env-actual env-funcion))
                   (if (null? exps)
                       'null              ;; sin return en el cuerpo -> null
                       (let* ((r (evaluar-exp (car exps) env-actual))
                              (nuevo-env (cadr r)))
                         (loop (cdr exps) nuevo-env)))))))))
      (list valor env-llamador))))

(define crear-ambiente-funcion
  (lambda (parametros args-exps env-llamador env-definicion)
    (let loop ((params parametros) (args args-exps) (env-nuevo env-definicion))
      (if (null? params)
          env-nuevo
          (let* ((r   (evaluar-exp (car args) env-llamador))
                 (val (car r)))
            (loop (cdr params)
                  (cdr args)
                  (extender-ambiente env-nuevo (car params) val #f)))))))

;; ============================================================
;; UTILIDADES
;; ============================================================

(define es-verdadero?
  (lambda (v)
    (cond
      ((boolean? v)  v)
      ((number? v)   (not (= v 0)))
      ((string? v)   (not (string=? v "")))
      ((eq? v 'null) #f)
      (else #t))))

(define valor->string
  (lambda (v)
    (cond
      ((boolean? v)  (if v "true" "false"))
      ((eq? v 'null) "null")
      ((number? v)   (number->string v))
      ((string? v)   v)
      ((list? v)     (string-append "[" (unir-con-comas (map valor->string v)) "]"))
      ((symbol? v)   (symbol->string v))
      (else          (symbol->string v)))))

;; Une una lista de strings con ", " sin depender de racket/string
(define unir-con-comas
  (lambda (strs)
    (cond
      ((null? strs) "")
      ((null? (cdr strs)) (car strs))
      (else (string-append (car strs) ", " (unir-con-comas (cdr strs)))))))
