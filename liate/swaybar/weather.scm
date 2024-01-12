(define-module (liate swaybar weather)
  #:use-module (goblins)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (liate goblins)
  #:use-module (liate weather)
  #:use-module (liate utils time)
  #:use-module (liate utils proc-tools)
  #:use-module (json)
  #:use-module (pipe)
  #:export (^weather-block))

(define (^weather-block _bcom cell http forecast-office-id gridpt)
  (define url
    (forecast-url forecast-office-id gridpt 'hourly))
  (define-cell quit? #f)

  (<- cell "Waiting for weather data…")

  (methods
   ((tick)
    (unless ($ quit?)
      (on (<- http 'get url
              #:decoder (λ (_resp port) (json->scm port)))
          (λ (json)
            (let ((periods (-> json
                               (assoc-ref "properties")
                               (assoc-ref "periods")
                               (vector->list))))
              (<- cell
                  (let* ((periods
                          (drop-while (λ (period)
                                        (let* ((end (assoc-ref period "endTime"))
                                               (end (iso-8601-string->date end)))
                                          (time<=? (date->time-utc end)
                                                   (add-duration (current-time time-utc)
                                                                 (make-time time-duration
                                                                            0
                                                                            (minutes 5))))))
                                      periods))
                         (periods (take periods 2))
                         (now (car periods))
                         (next (cadr periods)))
                    (define (forecast-hour forecast)
                      (date->string
                       (iso-8601-string->date (assoc-ref forecast "startTime"))
		               "~H"))
                    (define (forecast-temp-str forecast)
                      (format #f "~a°~a"
                              (assoc-ref forecast "temperature")
                              (assoc-ref forecast "temperatureUnit")))
                    `((full_text
                       . ,(format #f "~a: ~a; ~a: ~a, ~a"
                                  (forecast-hour now)
                                  (forecast->string now)
                                  (forecast-hour next)
                                  (forecast-temp-str next)
                                  (if (equal? (assoc-ref now "shortForecast")
                                              (assoc-ref next "shortForecast"))
                                      "--"
                                      (assoc-ref next "shortForecast"))))
                      (short_text
                       . ,(format #f "~a: ~a; ~a: ~a"
                                  (forecast-hour now)
                                  (forecast-temp-str now)
                                  (forecast-hour next)
                                  (forecast-temp-str next)))))))))))
   ((quit)
    ($ quit? #t))))
