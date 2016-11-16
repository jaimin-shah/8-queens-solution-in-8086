mydata segment
board DB 64 DUP(0) ; 0 shows enpty 1 shown queen
valid DB 00H ;0 shows valid 1 shows invalid
correct DB 00H; correct sol found 1 , not found 0
row DB ? ;row no stat from 0
col DB ? ;col no stat from 0
mydata ends

mystack segment stack
	dw 500 dup(0)
	tos label word
mystack ends

mycode segment
  start:
	assume cs:mycode,ds:mydata,ss:mystack
    mov ax,mydata
	mov ds,ax
	mov ax,mystack
	mov ss,ax
	lea sp,tos
    LEA SI, board              ; set SI=offset address of ARRAY
    MOV BH, 8                   
    MOV BL, 8   
    ;CALL PRINT_2D_ARRAY 
   ; mov row,00H
    ;call checkrow
    ;mov col,00H
    ;call checkcol
    ;call checkdiag
	mov bx, 00H	;call function by passing col# 0 as parameter 
	push bx
	call solver

	
    LEA SI, board              ; set SI=offset address of ARRAY
    MOV BH, 8                   
    MOV BL, 8   
    CALL PRINT_2D_ARRAY 
    mov ah,4CH
	INT 21H
    
    checkrow proc near
    
    push ax
	push bx
    push cx 
	push dx
	push bp
    push si
    push di
    lea dx,board
    mov cx,08H
    mov al,08H
    mov bh,row
    mul bh
    add dx,ax
    mov bl,00H
    loop1:
        mov di,dx
        mov bh,00H
        add di,bx
        mov bh,board[di]
        inc bl
        cmp bh,00H
        jnz out1
    loop loop1
    jmp continue
    out1:
        mov valid,01H
    continue:
        pop di
        pop si
		pop bp
		pop dx
        pop cx
		pop bx
		pop ax
		
        RET
    checkrow endp
    
    checkcol proc near
    
    push ax
	push bx
    push cx
	push dx
	push bp
    push si
    push di
    lea dx,board
    mov cx,08H
    mov bl,col
    mov bh,00H
    add dx,bx
    mov bh,00H
    loop2:
        mov al,08H
        mul bh
        inc bh
        add ax,dx
        mov si,ax
        mov bl,board[si]
        cmp bl,00H
        jnz out2
    loop loop2
    jmp continue1
    out2:
        mov valid,01H
    continue1:
        pop di
        pop si
		pop bp
		pop dx
        pop cx
		pop bx
		pop ax
		
        RET
    checkcol endp
    
    checkdiag proc near
    
    push ax
	push bx
    push cx
	push dx
	push bp
    push si
    push di
    
    mov dl,row
    mov bl,col
    loop3:
        mov al,08H
        mul dl
        add ax,bx
        mov di,ax
        lea ax,board
        add di,ax
        mov al,board[di]
        cmp al,00H
        jnz out3
        cmp dl,00H
        jz nextloop
        dec dl
        jz nextloop
        dec bl
        cmp bl,00H
        jmp loop3
        
    nextloop:
    mov dl,row
    mov bl,col
    loop4:
        mov al,08H
        mul dl
        add ax,bx
        mov di,ax
        lea ax,board
        add di,ax
        mov al,board[di]
        cmp al,00H
        jnz out3
        cmp dl,07H
        jz continue2
        inc dl
        jz continue2
        dec bl
        cmp bl,00H
        jmp loop4
    jmp continue2
    out3:
        mov valid,01H
    continue2:
        pop di
        pop si
		pop bp
		pop dx
        pop cx
		pop bx
		pop ax
		
        RET
    checkdiag endp
	
	proc solver 
    ;input bx pushed at location [bp+14]
    ;output returned in same location so when we pop bx after call it will be answer 55H value in bx indiacted solution is found
		push ax
        push cx
		push dx
		push bp
        push si
        push di
		mov bp, sp
		mov bx, [bp+14]	
        cmp bx,08H
        JZ found
        mov cx,08H
        mov al,00H
        loop5:
        mov bx, [bp+14]	
        mov valid,00H
        mov col,bl
        call checkcol
        mov row,al
        call checkrow
        call checkdiag
        
        cmp valid,01H
        jz nextrow
        
        mov bh,08H
        mul bh
        mov bh,00H
        add ax,bx
        lea di,board
        add di,ax
        mov byte ptr [di],01H
        inc bl
        mov bh,00H
        push bx
        call solver
        pop bx
        cmp bx,55H
        jz cnt1
        mov byte ptr [di],00H
        nextrow:
        inc al
        loop loop5
        jmp cnt
        
        found:
            mov word ptr [bp+14],55H
            jmp cnt
        
        cnt1:
        mov word ptr [bp+14],55H
        cnt:
            
            pop di
            pop si
			pop bp
			pop dx
            pop cx
			pop ax
			
            
           
			ret
	solver endp
			
      PRINT_2D_ARRAY PROC near
    ; this procedure will print the given 2D array
    ; input : SI=offset address of the 2D array
    ;       : BH=number of rows
    ;       : BL=number of columns 
    ; output : none

    
	push ax
    push cx
	push dx
	push bp
    push di
   MOV CX, BX                     ; set CX=BX

   @OUTER_LOOP:                   ; loop label
     MOV CL, BL                   ; set CL=BL

     @INNER_LOOP:                 ; loop label
       MOV AH, 2                  ; set output function
       MOV DL, 20H                ; set DL=20H
       INT 21H                    ; print a character
       
       mov ah,00H
       MOV Al, [SI]               ; set AX=[SI]
                            
       CALL OUTDEC                ; call the procedure OUTDEC

       INC SI                     ; set SI=SI+2
       DEC CL                     ; set CL=CL-1
     JNZ @INNER_LOOP              ; jump to label @INNER_LOOP if CL!=0
                           
     MOV AH, 2                    ; set output function
     MOV DL, 0DH                  ; set DL=0DH
     INT 21H                      ; print a character

     MOV DL, 0AH                  ; set DL=0AH
     INT 21H                      ; print a character

     DEC CH                       ; set CH=CH-1
   JNZ @OUTER_LOOP                ; jump to label @OUTER_LOOP if CX!=0

    pop di
	pop bp
	pop dx
    pop cx
	pop ax
	
    RET
 PRINT_2D_ARRAY ENDP

 ;**************************************************************************;
 ;--------------------------------  OUTDEC  --------------------------------;
 ;**************************************************************************;

 OUTDEC PROC near
   ; this procedure will display a decimal number
   ; input : AX
   ; output : none

    
	push bx
    push cx
	push dx
	push bp
    push si
    push di
   XOR CX, CX                     ; clear CX
   MOV BX, 10                     ; set BX=10

   @OUTPUT:                       ; loop label
     XOR DX, DX                   ; clear DX
     DIV BX                       ; divide AX by BX
     PUSH DX                      ; push DX onto the STACK
     INC CX                       ; increment CX
     OR AX, AX                    ; take OR of Ax with AX
   JNE @OUTPUT                    ; jump to label @OUTPUT if ZF=0

   MOV AH, 2                      ; set output function

   @DISPLAY:                      ; loop label
     POP DX                       ; pop a value from STACK to DX
     OR DL, 30H                   ; convert decimal to ascii code
     INT 21H                      ; print a character
   LOOP @DISPLAY                  ; jump to label @DISPLAY if CX!=0

    pop di
    pop si
	pop bp
	pop dx
    pop cx
    pop bx
	
    RET                            ; return control to the calling procedure
 OUTDEC ENDP
			
mycode ends
    end start

