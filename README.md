# MIPSsinglecycle32bit
Required project to complete advance computer architecture course in BITS Pilani KK Birla Goa Campus
<pre>
Instructions that I've done:- addi, ori, add, sub, beq, sw ,lw, j, sub, mult, div, mfhi, mflo, sll
Instruction That I've not done:- madd, msub, mtlo, mthi, jalr
NOTE: wd0 = low register(Lo)
      wd1 = high register(Hi)
______________________________________________________________________________________________________________

INSTRUCTION              DESCRIPTION                                                         MEMORY ADDRESS  
______________________________________________________________________________________________________________
addi $2, $0, 5          # initialize $2 = 5                                                      0
addi $3, $0, 12         # initialize $3 = 12 (c in hex)                                          4
addi $7, $3, -9         # initialize $7 = 3                                                      8 
ori $7, $4, 5           # $4 = $7 or 5 = 3 or 5 = 7                                              C
add $5, $3, $4          # $5 = $3 + $4 = 7 + 12 = 19 (13 in hex)                                 10
sub $6, $3, $4          # $6 = $3 - $4 = 12 - 7 = 5                                              14
beq $2, $6, 1           # branch to next inst after the following inst (i.e address 20 )         18
sub $8, $3, $2          # not executed bcz of branch                                             1c
sw $5, 68($3)           #  [80] = 13                                                             20
lw $2, 80($0)           # $2 = [80] = 13                                                         24
j 13                    #  jump to address 38                                                    2C
sub $6, $2, $7          # not executed bcz of jump                                               30
mult $3, $4             #  not executed bcz of jump                                              34
div $3, $4              #  Lo = quotient Hi = remainder                                          38
mult $4, $6             # [Hi:Lo] = $4 * $6 = 35 (Hi = 0 in hex, Lo = 23 in hex)                 3c
mfhi $8                 # $8 = content of Hi (0 in hex)                                          40
mflo $8                 # $8 = content of Lo (23 in hex)                                         44
sll $3, $6, 2           # $3 = $6 << 2 = 5 << 2 = 20                                             48 
______________________________________________________________________________________________________________                                                           

All these instructions are feeded into instruction memory(in hex format) using a external file "memfile.dat".
To run the "mips.v" file, insure that "memfile.dat" is kept in same folder. 
Contents of "memfile.dat" are:-

20020005
2003000c
2067fff7
34E40005
00642820
00643022
10C20001 
00624022
AC650044 
8C020050
0800000D
00473022
00640018
0064001A
00860018
00004010
00004012
00061880
</pre>
