as9 ../sbug.asm -l c now s cre bin s19

mv .bin sbug.bin; mv .crf sbug.crf; mv .lst sbug.lst; mv .s19 sbug.s19; mv .sym sbug.sym
objcopy sbug.s19 sbug.bin -Isrec -Obinary --gap-fill=255
