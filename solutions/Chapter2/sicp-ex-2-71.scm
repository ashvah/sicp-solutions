#lang sicp 

;       (a,b,c,d,e)
;        /    \
;       a   (b,c,d,e)
;            /  \
;           b  (c,d,e)
;              /    \
;             c     (d,e)
;                    / \
;                   d   e
; 
;       (a,b,c,d,e,f,g,h,i,j)
;        /              \
;       a           (b,c,d,e,f,g,h,i,j)
;                    /             \
;                   b          (c,d,e,f,g,h,i,j)
;                                /           \
;                               c       (d,e,f,g,h,i,j)
;                                          /       \
;                                         d    (e,f,g,h,i,j)
;                                                /     \
;                                               e   (f,g,h,i,j)
;                                                      /   \
;                                                     f  (g,h,i,j)
;                                                           /  \
;                                                          g   (h,i,j)
;                                                               /  \
;                                                              h  (i,j)
;                                                                  /  \
;                                                                 i    j
;
; 1-bit ~ n-bit
