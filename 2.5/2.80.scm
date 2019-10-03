(put '=zero? 'scheme-number
       zero?)

(put '=zero? 'complex
     (lambda (x) (zero? (magnitude x))))

(put '=zero? 'rational
     (lambda (x) (zero? (numer x))))

