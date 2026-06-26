#lang eopl

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
    (espacio-blanco  (" " "\t" "\n")  skip)

    ;; Comentarios con # hasta el final de la línea
    (comentario
     ("#" (arbno (not #\newline)))
     skip)

    ;; ----------------------------------------------------------
    ;; 2. NÚMEROS
    ;; ----------------------------------------------------------
    ;; Número flotante: dígitos, punto, más dígitos
    ;; Ej: 3.14  9.5  0.0
    (numero-flotante
      (digit (arbno digit) "." (arbno digit))
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
      (letter (arbno (or letter digit (one-of "_-"))))
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

