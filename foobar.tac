0:	mov R0 #99;
	mov Rx R0;
1:	mov R1 #0;
	sub R2 Rx R1;
	brnz R2 #3;
	mov R2 #0;
	jmp #4;
3:	mov R2 #1;
4:	brz R2 #2;
	mov R3 #1;
	sub R4 Rx R3;
	mov Rx R4;
	jmp #1;
2:	exit;
