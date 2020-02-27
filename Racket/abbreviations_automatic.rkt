; Zack Strickland (github.com/trouvant)
; 27 February 2020
; Abbreviations, automatic (partially complete)
; http://rosettacode.org/wiki/Abbreviations,_automatic
; ------------------------------------------------------------------------------
#lang racket

(define (min_abbrev l)
  (define (_clip_prefix l pre restore return)
    (if (null? l)
        return
        (if (string-prefix? (car l) pre)
            (_clip_prefix (cdr l)
                          pre
                          restore
                          (append return
                                  (list (string-trim (car l)
                                                     pre
                                                     #:right? #f))))
            (_clip_prefix restore
                          (substring pre
                                     0
                                     (sub1 (string-length pre)))
                          restore
                          '()))))

  (define (_min_abbrev l pre n restore return)
    (if (null? l)
        return
        (if (string-prefix? (car l) pre)
            (_min_abbrev (cdr restore)
                         (substring (car restore)
                                    0
                                    (add1 n))
                         (add1 n)
                         restore
                         (list (substring (car restore)
                                          0
                                          (add1 n))))
            (_min_abbrev (cdr l)
                         pre
                         n
                         restore
                         (append return
                                 (list (substring (car l)
                                                  0 n)))))))

  (let ([clipped (_clip_prefix l
                               (substring (car l)
                                          0
                                          (sub1 (string-length (car l))))
                               l
                               '())])
    (_min_abbrev (cdr clipped)
                 (substring (car clipped)
                             0
                             1)
                 1
                 clipped
                 '())))
    
    