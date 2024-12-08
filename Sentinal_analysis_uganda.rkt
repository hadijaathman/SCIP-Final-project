#lang racket
;Name: Khadija Athman
;REG Number: 2024/HD05/21918U
;Student number: 2400721918
;Course code: MCN 7105

(require csv-reading)
(require csv-writing)
(require data-science)
(require plot)
(require math)
(require threading)
(require racket/date)

; Function to find the index of a column by header name
(define (find-column-index headers column-name)
  (define index (index-of headers column-name))
  (if index
      index
      (error "Column not found" column-name)))

; Function to add a `SentimentText` column if it doesn't exist
(define (ensure-sentiment-column data headers text-columns)
  (if (member "SentimentText" headers)
      data ; If it exists, return the data as-is
      (let ([text-indices (map (lambda (col) (find-column-index headers col)) text-columns)])
        (define new-data
          (map (lambda (row)
                 (let ([text-content 
                        (string-join 
                         (map (lambda (idx) 
                                (if (< idx (length row)) (list-ref row idx) "")) 
                              text-indices))])
                   (append row (list text-content))))
               data))
        (values (append headers (list "SentimentText")) new-data))))

; Function to analyze sentiment
(define (analyse-sentiment data sentiment-column)
  (define text-column
    (map (lambda (row) 
           (if (>= sentiment-column 0) 
               (list-ref row sentiment-column)
               ""))
         data))
  (define join-data
    (string-join (map string-downcase text-column)))
  (define tokens (document->tokens join-data #:sort? #t))
  (define analysed-data (list->sentiment tokens #:lexicon 'nrc))
  analysed-data)

; Function to aggregate sentiment results
(define (aggregate-sentiments sentiment-data)
  (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))

; Function to group sentiments by dates
(define (group-sentiments-by-date data date-column sentiment-column)
  (define grouped-data (make-hash)) ; Create a mutable hash table
  
  ; Ensure valid data
  (for ([row data])
    (cond
      [(and (list? row)
            (>= (length row) (max date-column sentiment-column)))
       (let* ([date (list-ref row date-column)]
              [sentiment-text (list-ref row sentiment-column)]
              [sentiment (string-downcase sentiment-text)]
              [current-group (hash-ref grouped-data date '())])
         (hash-set! grouped-data date (cons sentiment current-group)))]
      [else
       (printf "Skipping invalid row: ~a\n" row)])) ; Log invalid rows
  
  ; Aggregate grouped data
  (hash-map grouped-data
            (lambda (date sentiments)
              (list date (aggregate-sentiments sentiments)))))

; Function to plot sentiments
(define (plot-sentiments analyse-data title)
  (parameterize ([plot-width 800])
    (plot
     (list
      (tick-grid)
      (discrete-histogram
       (sort analyse-data (λ (x y) (> (second x) (second y))))
       #:color "MediumSlateBlue"
       #:line-color "MediumSlateBlue"))
     #:x-label "Affective Label"
     #:y-label "Frequency"
     #:title title)))

; Main function to run sentiment analysis
(define (run-sentiment-analysis filepath date-columns text-columns)
  (define data (read-csv filepath))
  (define headers (first data))
  (define data-rows (rest data))
  (define-values [new-headers new-data]
    (ensure-sentiment-column data-rows headers text-columns))
  (define sentiment-column (find-column-index new-headers "SentimentText"))
  (let ([results (analyse-sentiment new-data sentiment-column)])
    (let ([aggregate (aggregate-sentiments results)])
      (plot-sentiments aggregate "Overall Sentiment Analysis"))))

; Main function to run sentiment analysis grouped by dates
(define (run-sentiment-analysis-by-date filepath date-columns text-columns)
  (define data (read-csv filepath))
  (define headers (first data))
  (define data-rows (rest data))
  
  ; Ensure the sentiment column exists
  (define-values [new-headers new-data]
    (ensure-sentiment-column data-rows headers text-columns))
  
  ; Find the date column index
  (define date-column
    (let ([index (find-column-index new-headers (car date-columns))])
      (if (number? index)
          index
          (error "Invalid date column name" (car date-columns)))))
  
  ; Find the sentiment column index
  (define sentiment-column
    (let ([index (find-column-index new-headers "SentimentText")])
      (if (number? index)
          index
          (error "SentimentText column not found"))))
  
  ; Group data by dates and perform sentiment analysis
  (define grouped-data
    (group-sentiments-by-date new-data date-column sentiment-column))
  
  ; Plot and display results for each date
  (for ([date+data (hash->list grouped-data)])
    (let* ([date (first date+data)]
           [aggregate-data (second date+data)])
      (printf "Date: ~a\n" date)
      (plot-sentiments aggregate-data (format "Sentiment Analysis for ~a" date)))))
; Example usage
; Replace with your CSV file path and the relevant text columns to concatenate
(run-sentiment-analysis "/home/hadija/Documents/uganda (Copy).csv" '("date") '("text"))
;(run-sentiment-analysis-by-date "/home/hadija/Documents/uganda (Copy).csv" '("date") '("text"))
