#!/usr/bin/env racket
#lang racket

(require racket/cmdline)

(define current-program (make-parameter #f))
(define profile-directory (make-parameter #f))
(define current-commit (make-parameter #f))
(define current-type (make-parameter #f))
(define display-ranges (make-parameter #f))
(define commits-to-compare (make-parameter #f))

(define (handle-compare-commits _ . commits)
  (commits-to-compare commits))

(command-line
 #:once-each
 [("-d" "--profile-directory") dir "Profile results directory"
                               (profile-directory dir)]
 [("-r" "--range") "Output [+-(max-min)] ranges"
                   (display-ranges #t)]
 #:once-any
 [("-a" "--analyze-program") path "Reports average compilation time on each commit"
                             (current-program path)]
 [("-f" "--fastest") commit "Lists programs that compiled fastest on a commit"
                     (current-commit commit)
                     (current-type "fastest")]
 [("-s" "--slowest") commit "Lists programs that compiled slowest on a commit"
                     (current-commit commit)
                     (current-type "slowest")]
 [("-c" "--compare-commits") => handle-compare-commits
                             '("Compares the performance of programs between two or more commits")])

(define info
  (for/hash ([commit-tag (in-list (directory-list (profile-directory)))])
    (define commit-directory (build-path (profile-directory) commit-tag))
    (define profiles
      (list (file->list commit-directory))
      #;(for/list ([profile (in-directory commit-directory)])
        (file->list profile)))
    (define combined-profiles
      (for*/fold ([result (hash)])
                 ([profile (in-list profiles)]
                  [data (in-list profile)])
        (match data
          [(list name duration)
           (hash-set result
                     name
                     (cons duration (hash-ref result name '())))])))
    (values (path->string commit-tag) combined-profiles)))

(define average-info
  (for/hash ([(commit-tag data) (in-hash info)])
    (values commit-tag
            (for/hash ([(name durations) (in-hash data)])
              (values name (floor (/ (apply + durations) (length durations))))))))

(define average-info+ranges
  (for/hash ([(commit-tag data) (in-hash info)])
    (values commit-tag
            (for/hash ([(name durations) (in-hash data)])
              (values name
                      (list (floor (/ (apply + durations) (length durations)))
                            (apply max durations)
                            (apply min durations)))))))

(define (sorted-averages name)
  (sort (for/list ([(commit data) (in-hash average-info+ranges)])
          (cons commit (hash-ref data name)))
        <
        #:key cadr))

(define (average-of-test name)
  (printf ":: Average compilation times of \"~a\"~n" name)
  (define averages (sorted-averages name))
  (for ([i (in-naturals)]
        [data (in-list averages)])
    (match data
      [(cons commit-tag (list average max min))
       (cond [(display-ranges)
              (define range (- max min))
              (printf (cond [(= i 0)
                             " => ~a: ~a[+-~a]ms\t(fastest)~n"]
                            [(= i (sub1 (length averages)))
                             " => ~a: ~a[+-~a]ms\t(slowest)~n"]
                            [else
                             " => ~a: ~a[+-~a]ms\t~n"])
                      commit-tag average range)]
             [else
              (printf (cond [(= i 0)
                             " => ~a: ~ams\t(fastest)~n"]
                            [(= i (sub1 (length averages)))
                             " => ~a: ~ams\t(slowest)~n"]
                            [else
                             " => ~a: ~ams\t~n"])
                      commit-tag average)])])))

(define (fastest-commit program)
  (match (first (sorted-averages program))
    [(cons name (list duration _ ...))
     name]))

(define (slowest-commit program)
  (match (last (sorted-averages program))
    [(cons name (list duration _ ...))
     name]))

(define (program->averages+ranges program)
  (for/hash ([(commit data) (in-hash average-info+ranges)])
    (values commit (hash-ref data program))))

(define (fastest-programs commit)
  (unless (member commit (hash-keys info))
    (printf ":: No data available for commit ~.a. Possible commits are:~n" commit)
    (for ([key (in-list (hash-keys info))])
      (printf " => ~a~n" key))
    (exit 1))
  (define programs (hash-keys (hash-ref info (first (hash-keys info)))))
  (define the-fastest-programs
    (sort
     (for/list ([program (in-list programs)]
                #:when (equal? commit (fastest-commit program)))
       (cons program (program->averages+ranges program)))
     <
     #:key (match-lambda
             [(cons name h)
              (first (hash-ref h commit))])))
  (printf ":: ~a programs compiled fastest on \"~a\":~n" (length the-fastest-programs) commit)
  (for ([program-info (in-list the-fastest-programs)])
    (match program-info
      [(cons name h)
       (define data (hash-ref h commit))
       (define average (first data))
       (define max (second data))
       (define min (third data))
       (cond [(display-ranges)
              (printf " => ~a (~a[+-~a]ms)~n" name average (- max min))]
             [else
              (printf " => ~a (~ams)~n" name average)])]))
  (when (empty? the-fastest-programs)
    (printf " => No program compiled fastest on this commit~n")))

(define (slowest-programs commit)
  (unless (member commit (hash-keys info))
    (printf ":: No data available for commit ~.a. Possible commits are:~n" commit)
    (for ([key (in-list (hash-keys info))])
      (printf " => ~a~n" key))
    (exit 1))
  (define programs (hash-keys (hash-ref info (first (hash-keys info)))))
  (define the-slowest-programs
    (sort
     (for/list ([program (in-list programs)]
                #:when (equal? commit (slowest-commit program)))
       (cons program (program->averages+ranges program)))
     >
     #:key (match-lambda
             [(cons name h)
              (first (hash-ref h commit))])))
  (printf ":: ~a programs compiled slowest on \"~a\":~n" (length the-slowest-programs) commit)
  (for ([program-info (in-list the-slowest-programs)])
    (match program-info
      [(cons name h)
       (define data (hash-ref h commit))
       (define average (first data))
       (define max (second data))
       (define min (third data))
       (cond [(display-ranges)
              (printf " => ~a (~a[+-~a]ms)~n" name average (- max min))]
             [else
              (printf " => ~a (~ams)~n" name average)])]))
  (when (empty? the-slowest-programs)
    (printf " => No program compiled slowest on this commit~n")))

(define (compare-commits commits)
  (define programs (hash-keys (hash-ref info (first (hash-keys info)))))
  (printf ":: Performance of ~a as compared to ~a:~n" (rest commits) (first commits))
  (define data
    (sort
     (for/list ([program (in-list programs)])
       (define h (program->averages+ranges program))
       (define first-average (first (hash-ref h (first commits))))
       (cons program
             (for/list ([commit (in-list (rest commits))])
               (match (hash-ref h commit)
                 [(list average max min)
                  (list commit (- average first-average) (- max min))]))))
     <
     #:key (match-lambda
             [(list _ (list _ diff1 _) _ ...)
              diff1])))
  (for ([d (in-list data)])
    (match d
      [(list xs ...)
       (match xs
         [(list program (list commits diffs ranges) ...)
          (printf " => ")
          (for ([diff (in-list diffs)]
                [range (in-list ranges)])
            (cond [(display-ranges)
                   (printf "~a[+-~a]ms\t" (~r #:sign '+ diff) range)]
                  [else
                   (printf "~ams\t" (~r #:sign '+ diff))]))
          (printf "~a" program)
          (newline)
          ])])))

(cond [(current-program) => average-of-test]
      [(and (equal? (current-type)
                    "fastest")
            (current-commit)) => fastest-programs]
      [(and (equal? (current-type)
                    "slowest")
            (current-commit)) => slowest-programs]
      [(commits-to-compare) => compare-commits])
