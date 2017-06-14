%
(Examples from M700V/M70V Series Programming Manual 13.5)
(#100 = 1 means a test succeeded)

(comparison operators)
#100 = 0  EQ -0
#100 = #0 EQ #0
#100 = 0  EQ #0  EQ 0
#100 = 0  EQ 1   EQ 0
#100 = 0  NE #0
#100 = 0  NE 1
#100 = 0  NE 0   EQ 0
#100 = 0  LT 1
#100 = 0  LT 0   EQ 0
#100 = 1  LT 0   EQ 0
#100 = 1  GT 0
#100 = 0  GT 0   EQ 0
#100 = 0  GT 1   EQ 0
#100 = 0  LE 1
#100 = 0  LE 0
#100 = 1  LE 0   EQ 0
#100 = 1  GE 0
#100 = 0  GE 0
#100 = 0  GE 1   EQ 0

(basic math)
#100 = 1000  -   50    EQ 950
#100 = 100   +   1000  EQ 1100
#100 = - 10  -   3     EQ -13
#100 = - 10  +   200   EQ 190
#100 = 100   *   100   EQ 10000
#100 = 100   /   100   EQ 1
(#100 = 1/0 => P283)
#100 = - 10  *   100   EQ -1000
#100 = [-10] *   100   EQ -1000
#100 = - 10  /   200   EQ -.05
#100 = [-10] /   200   EQ -.05
#100 = 48    MOD 9     EQ 3

(associativity)
#100 = [2 + 3 * 4]             EQ [2 + [3 * 4]]          (* before +)
#100 = [[2 + 3] * 4]           NE [2 + 3 * 4]
#100 = [2 - 3 / 4]             EQ [2 - [3 / 4]]          (/ before -)
#100 = [[2 - 3] / 4]           NE [2 - 3 / 4]
#100 = [-1 AND 3]              EQ [-[1 AND 3]]           (AND before -)
#100 = [[-1] AND 3]            NE [-1 AND 3]
#100 = [-1 OR  2]              NE [-[1 OR  2]]           (left to right eval)
#100 = [-1 XOR 3]              NE [-[1 XOR 3]]           (left to right eval)
#100 = [14 EQ 2 + 3 * 4]       EQ [14 EQ [2 + 3 * 4]]    (+ before EQ)
#100 = [14 EQ 14 EQ 1]         EQ [[14 EQ 14] EQ 1]      (left to right eval)
#100 = [14 EQ 14 EQ 1]         NE [14 EQ [14 EQ 1]]      (left to right eval)
#100 = [1 EQ 1 AND 2 EQ 2]     EQ [1 EQ [1 AND 2] EQ 2]  (AND before EQ)
#100 = [[1 EQ 1] AND [2 EQ 2]] NE [1 EQ 1 AND 2 EQ 2]
#100 = [1 EQ 1 OR 2 EQ 2]      EQ [1 EQ [1 OR 2] EQ 2]   (OR before EQ)
#100 = [[1 EQ 1] OR [2 EQ 2]]  NE [1 EQ 1 OR 2 EQ 2]

(bit operators)
#100 = 100 OR  14  EQ 110
#100 = 100 XOR 14  EQ 106
#100 = 100 AND 15  EQ 4

(trigonometric functions)
#100 = ABS[SIN[60]         - 0.866025]  LE 0.0000005
#100 = ABS[COS[45]         - 0.707107]  LE 0.0000005
#100 = ABS[TAN[60]         - 1.732051]  LE 0.0000005
#100 = ABS[ASIN[.5]        - 30]        LE 0.0000005
#100 = ABS[ASIN[-.5]       + 30]        LE 0.0000005
#100 = ABS[ATAN[1.7320508] - 60]        LE 0.0000005
#100 = ABS[ACOS[1/SQRT[2]] - 45]        LE 0.0000005

(exp functions)
#100 = ABS[SQRT[1000]         - 31.622777]  LE 0.0000005
#100 = ABS[SQR[10*10 + 20*20] - 22.360680]  LE 0.0000005
(#100 = SQRT[-4] => P282)
#100 = ABS[LN[5]              - 1.609438]   LE 0.0000005
#100 = ABS[LN[.5]             + 0.693147]   LE 0.0000005
(#100 = LN[-5] => P282)
#100 = ABS[EXP[2]             - 7.389056]   LE 0.0000005
#100 = ABS[EXP[1]             - 2.718282]   LE 0.0000005
#100 = ABS[EXP[-2]            - 0.135335]   LE 0.0000005
#100 = ABS[EXP[LN[2] * .5]    - SQRT[2]]    LE 0.0000005

(bcd functions)
#100 = BIN[100]   EQ 64
#100 = BIN[-100]  EQ 64
#100 = BCD[100]   EQ 256
#100 = BCD[-100]  EQ 256
#100 = BIN[BCD[100]]  EQ 100

(rounding functions)
#100 = ROUND[14 / 3]  EQ 5
#100 = RND[-14 / 3]   EQ -5
#100 = FIX[14 / 3]    EQ 4
#100 = FIX[-14 / 3]   EQ -4
#100 = FUP[14 / 3]    EQ 5
#100 = FUP[-14 / 3]   EQ -5

M30
%
