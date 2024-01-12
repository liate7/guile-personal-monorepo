(define-module (liate weather)
  #:use-module (json)
  #:use-module (pipe)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:use-module (web client)
  #:use-module (ice-9 format)

  #:export (forecast->string
            forecast-url
            show-weather
            ))

(define (forecast->string forecast)
  (format #f "~a°~a, ~a"
          (assoc-ref forecast "temperature")
          (assoc-ref forecast "temperatureUnit")
          (assoc-ref forecast "shortForecast")))

(define* (forecast-url forecast-office gridpt #:optional type)
  (format #f "https://api.weather.gov/gridpoints/~a/~a,~a/forecast~:[~^~;/~a~]"
          forecast-office
          (car gridpt) (cadr gridpt)
          type type))

(define* (show-weather forecast-office gridpt #:optional (out (current-output-port)))
  (let* ((_ port
            (http-request (forecast-url forecast-office gridpt)
                          #:method 'GET
                          #:headers '((accept (application/geo+json))
                                      (user-agent . "guile's (web client)"))
                          #:streaming? #t))
         (json (json->scm port))
         (forecasts (-> json
                        (assoc-ref "properties")
                        (assoc-ref "periods")
                        (vector->list)))
         (names (map (cute assoc-ref <> "name") forecasts))
         (tab-to (+ 2 (apply max (map string-length names)))))
    (for-each (λ (name forecast)
                (format out "~a:~vt~a~%"
                        name
                        tab-to
                        (forecast->string forecast)))
              names
              forecasts)))
