(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens) 
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1)))) ; this gets called every loop of flat-map
 (enumerate-interval 1 board-size))

