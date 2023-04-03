(define-module (liate utils hashmaps)
  #:use-module (srfi srfi-146 hash)

  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:use-module (liate utils proc-tools)
  #:use-module (liate utils comparators)

  #:re-export ( ;; hashmap procedures
               hashmap-unfold
               hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
               hashmap-ref hashmap-ref/default hashmap-key-comparator
               hashmap-adjoin hashmap-set hashmap-replace
               hashmap-delete hashmap-delete-all hashmap-intern
               hashmap-update hashmap-update/default
               hashmap-pop hashmap-search
               hashmap-size hashmap-find hashmap-count hashmap-any? hashmap-every?
               hashmap-keys hashmap-values hashmap-entries
               hashmap-map hashmap-map->list hashmap-for-each hashmap-fold hashmap-filter
               hashmap-remove hashmap-partition
               hashmap-copy hashmap->alist alist->hashmap
               hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
               hashmap-union hashmap-intersection hashmap-difference hashmap-xor)
  #:export ((hashmap* . hashmap) hashmap->hash-table hash-table->hashmap
            print-hashmap))

(define (hashmap* . args)
  "([#:comparator COMPARATOR] key val …)

Makes a new hashmap where KEY is set to the corresponding VAL,
favoring earlier on duplicates.
If the first argument is #:comparator,
the next argument will be used as the hashmap's comparator;
otherwise, (liate utils comparators):default-comparator will be used."
  (match args
    ((#:comparator (? comparator? comparator) . (? (compose even? length) plist))
     (apply hashmap comparator plist))
    ((? (proc-and list? (compose even? length)) plist)
     (apply hashmap default-comparator plist))))

(define (hashmap->hash-table hashmap)
  "Creates a normal guile hash table with the same key-value associations as HASHMAP.
Returns the new hash table, a hash function suitable for hashx- procedures, and
an assoc function suitable for hashx- procedures."
  (let ((ret (make-hash-table))
        (hash (comparator-sized-hash-function (hashmap-key-comparator hashmap)))
        (assoc (comparator-hashx-assoc (hashmap-key-comparator hashmap))))
    (hashmap-for-each (λ (k v)
                        (hashx-set! hash assoc ret k v))
                      hashmap)
    (values ret hash assoc)))

(define* (hash-table->hashmap hashtable #:optional comparator)
  "Creates a hashmap with the same key-value associations as HASHTABLE,
a normal guile hashtable.
If given, COMPARATOR is used as the new hashmap's comparator;
otherwise the default comparator is used."
  (hash-fold (λ (k v acc) (hashmap-set acc k v))
             (hashmap* #:comparator (or comparator default-comparator))
             hashtable))

(define (print-hashmap hm port)
  "Prints the hashmap HM to port/#t/#f PORT (as format),
in the format {[#:comparator comparator ](key val) …},
only printing #:comparator if the comparator is not the default."
  (format port "{~:[#:comparator ~s ~;~*~]~{~s~^ ~}}"
          (eq? (hashmap-key-comparator hm) default-comparator) (hashmap-key-comparator hm)
          (hashmap-map->list list hm)))
