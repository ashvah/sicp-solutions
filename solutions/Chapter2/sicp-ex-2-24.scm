#lang sicp

(list 1 (list 2 (list 3 4)))
(display "\nI'm a painter.\n")
(display "tree structure:\n")
(display "        * (1 (2 (3 4)))\n")
(display "       / \\\n")
(display "    1  *   * (2 (3 4))\n")
(display "          / \\\n")
(display "       2 *   * (3 4)\n")
(display "            / \\\n")
(display "           3   4\n")
(display "\n")
(display "\n")
(display "more detailed:\n") 
(display "  (1 (2 (3 4)))    ((2 (3 4)))\n")
(display "    -------          -------\n")
(display "   | * | * | -----> | * | / |\n")
(display "    -------          -------\n")
(display "     |                |\n")
(display "     V                V\n")
(display "     1              (2 (3 4))         ((3 4))      \n")
(display "                     -------          -------\n")
(display "                    | * | * | -----> | * | / |\n")
(display "                     -------          -------\n")
(display "                      |                |\n")
(display "                      V                V\n")
(display "                      2                (3 4)             (4)\n")
(display "                                      -------          -------\n")
(display "                                     | * | * | -----> | * | / |\n")
(display "                                      -------          -------\n")
(display "                                       |                |\n")
(display "                                       V                V\n")
(display "                                       3                4")

; I'm a painter.
; tree structure:
;        * (1 (2 (3 4)))
;       / \
;   1  *   * (2 (3 4))
;         / \
;      2 *   * (3 4)
;           / \
;          3   4
;
;
; more detailed:
; (1 (2 (3 4)))    ((2 (3 4)))
;   -------          -------
;  | * | * | -----> | * | / |
;   -------          -------
;    |                |
;    V                V
;    1              (2 (3 4))         ((3 4))       
;                    -------          -------
;                   | * | * | -----> | * | / |
;                    -------          -------
;                     |                |
;                     V                V
;                     2                (3 4)             (4)
;                                     -------          -------
;                                    | * | * | -----> | * | / |
;                                     -------          -------
;                                      |                |
;                                      V                V
;                                      3                4
