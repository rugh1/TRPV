.286
IDEAL
MODEL small
STACK 100h

DATASEG
	NodesData dw 10000 dup (?)
	Mode db 1  ; 1 for a* 2 for Dijkstra's algorithm
	picFilename db 'Hud.bmp', 0 ;ASCIZ (Null-terminated string)
	tmpHeader db 54 dup (0)
	Palette db 1024 dup (0) ; All files should have the same palette, so we apply it once.
	picture db 10 * 320 dup (0)
	Speed db 1 ; 1 to 5
CODESEG
;------------------------------------------------------------------------;
;general equ 
color equ [word ptr NodesData]
FoundPath	equ  [word ptr NodesData + 2]
IndexOfBestNode	equ  [word ptr NodesData + 4]
NumberOfNodesSearcherd	equ  [word ptr NodesData + 6]
XstartPoint	equ		[word ptr NodesData + 8] 
YstartPoint	equ		[word ptr NodesData + 10] 
XendPoint	equ		[word ptr NodesData + 12] 
YendPoint	equ		[word ptr NodesData + 14] 
;------------------------------------------------------------------------;
;------------------------------------------------------------------------;
;delay
;Args:None.
;Action: cause delay depends on speed
;Return: None.
;------------------------------------------------------------------------;
time equ [bp + 4]
proc delay
	push bp
	mov bp, sp 
	pusha 
	xor bx, bx  
	mov bl, time
	mov ax, 2000
	mul bx 
	mov dx, ax 
	mov cx, 0
	xor ax, ax 
	mov ah, 86h
	int 15h
	popa 
	pop bp 
	ret 2
endp delay 
; Open a file. Parameters are:
; 1. reference to filename on dx (ASCIZ format)
; Returns file handle to ax or 0 on error
proc OpenFile
  mov ah, 3Dh
  xor al, al
  int 21h
  jc openerror
  jmp openFinish
openerror:
  mov ax, 0
openFinish:
  ret
endp OpenFile

; Read BMP file header, 54 bytes, into [dx]
; Params:
; 1. BX = file handle
; 2. DX = bmp header buffer
proc ReadBMPHeader
  pusha
  mov ah,3fh
  mov cx,54
  int 21h
  popa
  ret
endp ReadBMPHeader

; Read BMP file color palette, 256 colors * 4 bytes (400h)
; Params:
; 1. BX = file handle
; 2. DX = palette buffer
proc ReadBMPPalette
  pusha
  mov ah, 3fh
  mov cx, 400h
  int 21h
  popa
  ret
endp ReadBMPPalette

; Copy the colors palette to the video memory registers
; The number of the first color should be sent to port 3C8h
; The palette is sent to port 3C9h
; si = palette buffer
proc CopyBMPPalette
  pusha
  mov cx,256
  mov dx,3C8h
  mov al,0
  ; Copy starting color to port 3C8h
  out dx,al
  ; Copy palette itself to port 3C9h
  inc dx
PalLoop:
  ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
  mov al,[si+2] ; Get red value.
  shr al,2 ; Max. is 255, but video palette maximal
   ; value is 63. Therefore dividing by 4.
  out dx,al ; Send it.
  mov al,[si+1] ; Get green value.
  shr al,2
  out dx,al ; Send it.
  mov al,[si] ; Get blue value.
  shr al,2
  out dx,al ; Send it.
  add si,4 ; Point to next color.
   ; (There is a null chr. after every color.)
  loop PalLoop
  popa
  ret
endp CopyBMPPalette

; Copy bitmap to memory
; Params:
; 1. bx = file handle
; 2. dx = buffer
; 3. cx = height
; 4. ax = width
; Note: BMP graphics are saved upside-down.
proc CopyBMPToMemory
  pusha
  push ax ; backup width
  push dx ; backup buffer
  mul cx ; Image size cannot be more than 16bit (max size is 320x200 = 64,000; 16bit register can hold up to 2^16-1 = 65535), so we ignore dx
  pop dx ; dx = buffer
  add dx, ax ; dx = end of buffer
  pop di ; di = width
  sub dx, di ; start of last line
  mov bp, cx ; bp = height
  mov cx, di ; cx = width
ReadLine:
  mov ax, 03F00h
  int 21h; Read from file. BX = file handle, CX = number of bytes to read, DX = buffer
  sub dx, cx
  dec bp
  cmp bp, 0
  jne ReadLine
  popa
  ret
endp CopyBMPToMemory

; Close file. Bx = file handle
proc CloseFile
  pusha
  mov ah,3Eh
  int 21h
  popa
  ret
endp CloseFile

; Draw from memory. Parameters are:
; 1. buffer
; 2. width
; 3. height
; 4. startX
; 5. startY
proc DrawFromMemory
  push bp
  mov bp, sp
  pusha
  mov ax, 0A000h
  mov es, ax
  mov ax, [bp + 4] ; start y
  mov bx, 320
  mul bx ;ax = start of the line
  cld ; for movsb
  mov di, ax ; di = start of the length
  add di, [bp + 6] ; add startX
  mov si, [bp + 12] ; si = buffer
  mov cx, [bp + 8] ;height
CPLoop:
  push cx
  mov cx, [bp + 10]; width
  rep movsb
  pop cx
  sub di, [bp + 10] ; sub the width
  add di, 320 ; go to next row
  loop CPLoop
  popa
  pop bp
  ret 10
endp DrawFromMemory

;------------------------------------------------------------------------;
;DingDong
;Args:None.
;Action: makes a ding and a dong 
;Return: None.
;------------------------------------------------------------------------;

proc DingDong
	pusha 
	mov bl, 1
	mov cx, 60
	loop_DingDong:
	cmp cx, 40 
	jne IfNeedToIncBl
	mov bl, 2
IfNeedToIncBl:
	cmp cx, 10
	jne IfIncNotNeeded
	mov bl, 4
IfIncNotNeeded:
	in al, 61h
	or al, 00000011b
	out 61h, al
	
	mov al, 0B6h
	out 43h, al
	


	mov ax, 1139 ;C 3: 1139  C 2: 2278  C 1: 4556
	mul bl
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte

	
	
	mov ax, 854 ;F 3:  F 2:  F1: 
	mul bl
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte
	
	

	mov ax, 1521 ;G #3 : 718 G# 2: 1436  G# 1: 2872
	mul bl 
	out 42h, al ; Sending lower byte
	mov al, ah
	out 42h, al ; Sending upper byte
	
	push  1
	call delay
	
	in al, 61h
	and al, 11111100b
	out 61h, al
	
	
	loop loop_DingDong
	popa 
	ret 
endp DingDong 

;------------------------------------------------------------------------;
;PaintScreen
;Args:None.
;Action: paint screan Default 
;Return: None.
;------------------------------------------------------------------------;
proc PaintScreen
	mov XstartPoint, 21	;define start node
	mov YstartPoint, 101 
	
	mov XendPoint, 290 ;define end node
	mov YendPoint, 100
	
	mov NumberOfNodesSearcherd, 0 ; reset NumberOfNodesSearcherd
	mov FoundPath, 0 ;reset FoundPath
	paintBoard:
		mov bx, 10
		xor ax, ax 
		xor cx, cx
		xor dx, dx 
		mov al, 0Fh
		mov dx, 9
		loop1_paintboard:
			mov al, 1h
			add dx, 1h
			mov cx, 0h
			cmp dx, 200
			je endPaintScreen
			loop2_paintboard: 
				push dx
				push cx
				mov ax, dx 
				xor dx, dx
				div bx
				push dx
				mov ax, cx 
				xor dx, dx
				div bx
				xor ax, ax
				mov al, 0Fh
				cmp dx, 0
				jne next1_paintboard; if row isnt divided in 10 without remainder
				mov al, 01h
				next1_paintboard:
				pop dx 
				cmp dx, 0
				jne next2_paintboard; if col isnt divided in 10 without remainder
				mov al, 01h
				next2_paintboard:
				pop cx
				pop dx
				mov ah, 0ch
				int 10h
				cmp cx, 320
				je fixpaint_paintboard
				add cx, 1
				jmp loop2_paintboard
				fixpaint_paintboard:
					mov cx, 319
					mov ah, 0ch
					int 10h
					jmp loop1_paintboard
				
	; Wait for key press
	endPaintScreen:
	mov color, 59
	push YstartPoint
	push XstartPoint
	call PaintSquare ; paint startpoint
	
	mov color, 75
	push YendPoint
	push XendPoint
	call PaintSquare ;paint endpoint
	
	mov color, 0
	
	ret
endp PaintScreen

;------------------------------------------------------------------------;
;PaintSquare
;Args: x and y in the square to paint.
;Action: paint square in pos
;Return: None.
;------------------------------------------------------------------------;
x_PaintSquare	equ  [word ptr bp + 4]
y_PaintSquare	equ  [word ptr bp + 6]
xstart_PaintSquare	equ		[word ptr bp - 2] 
ymax_PaintSquare	equ		[word ptr bp - 4] 
proc PaintSquare
	push bp 
	mov bp, sp
	sub sp, 4
	pusha 
	xor ax, ax 
	xor dx, dx
	mov ax, x_PaintSquare
	mov bx, 10
	div bx 
	mul bx
	inc ax
	mov xstart_PaintSquare, ax
	xor dx, dx
	mov ax, y_PaintSquare
	div bx
	mul bx
	mov dx, ax  
	add ax, 10
	mov ymax_PaintSquare, ax
loop1_PaintSquare:
		inc dx
		mov cx, xstart_PaintSquare
		cmp dx, ymax_PaintSquare
		je endPaintSquare
loop2_PaintSquare:
			mov ax, xstart_PaintSquare
			add ax, 9
			cmp cx, ax
			je loop1_PaintSquare
			mov ax, color
			mov ah, 0ch
			int 10h
			inc cx
			jmp loop2_PaintSquare
endPaintSquare:
	popa
	add sp, 4
	pop bp 
	ret 4
endp PaintSquare

;------------------------------------------------------------------------;
;ClearArray 
;Args: None
;Action: reset all of Nodes Array
;Return: ?
;------------------------------------------------------------------------;
proc ClearArray
	pusha 
	mov ax, 10000
	mov cx, 17
loop_ClearArray:
		mov bx, cx 
		mov [word ptr bx], 0
		
		inc cx
		cmp cx, ax 
		jne loop_ClearArray
	popa 
	ret
endp ClearArray

;------------------------------------------------------------------------;
;LoadChildren Note: the pixel of the parent needs to be the top right corner of the block 
;Args: x, y , index in segment of parent
;Action: load into memory the nodes next to the src
;Return: ?
;------------------------------------------------------------------------;
x_LoadChildren	equ  [bp + 4]
y_LoadChildren	equ  [bp + 6]
parentIndex_LoadChildren equ [bp +8] 

proc LoadChildren
	push bp 
	mov bp, sp
	pusha 
	mov cx, -20  ;loop to get all the nodes around a point
loopAdjacentNodesOnXAxis:   
		add cx, 10
		mov dx, -10
		cmp cx, 20
		jne loopAdjacentNodesOnYAxis
		jmp exitLoopAdjacent
loopAdjacentNodesOnYAxis:
			push cx 
			push dx 
			;skip not diagonal
			cmp dx, 0 
			je notdiagonal
			cmp cx, 0
			je notdiagonal
			jmp incYForLoopAdjacent
notdiagonal: 
			
			mov ax,x_LoadChildren
			add ax, cx 
			mov cx, ax 
			mov ax,y_LoadChildren
			add ax, dx 
			mov dx, ax 
			

			cmp cx, x_LoadChildren
			jne notthesame_LoadChildren
			cmp dx, y_LoadChildren
			jne notthesame_LoadChildren
				mov bh, 0h
				mov ah, 0Dh
				int 10h

				cmp al, 59
				jne colorcheckjumpoutofrange_LoadChildren
				jmp incYForLoopAdjacent
				cmp al, 75
				jne colorcheckjumpoutofrange_LoadChildren
				jmp incYForLoopAdjacent
				
colorcheckjumpoutofrange_LoadChildren:
				push ax 
				mov ax, color 
				mov color, 38 
				push dx 
				push cx 
				call PaintSquare
				mov color, ax 
				pop ax 
			jmp incYForLoopAdjacent

notthesame_LoadChildren:
			
			cmp dx, 10						
			jg outofrangecheck2_LoadChildren  
			jmp incYForLoopAdjacent
outofrangecheck2_LoadChildren:
			cmp cx, 0 
			jl incYForLoopAdjacent
			
			cmp cx, 319    ;skip this values if they arent in screen
			jg incYForLoopAdjacent
			cmp dx, 199						
			jg incYForLoopAdjacent  
			
			
			mov bh, 0h
			mov ah, 0Dh
			int 10h
			
			;skip if wall
			cmp al, 0
			je incYForLoopAdjacent
			
			;skip if startpoint
			cmp al, 59
			je incYForLoopAdjacent
			
			;skip if endpoint
			cmp al, 75
			jne nextcolorcheck
			push ax 
			mov ax, parentIndex_LoadChildren 
			mov FoundPath, ax 
			pop ax 
			jmp incYForLoopAdjacent
nextcolorcheck:
			
			push YstartPoint
			push XstartPoint
			push dx 
			push cx 
			call CheckIfNear
			cmp ax, 0 
			jne	NotNearStart
			push dx 
			push cx 
			call CalculateHvalue
			inc ax 
			jmp DoReplaceInMemory
NotNearStart:
			push parentIndex_LoadChildren
			push dx 
			push cx 
			call CalculateFvalue  ;return F value with ax 
DoReplaceInMemory:		
			push ax 
			push parentIndex_LoadChildren
			push dx 
			push cx 
			call ReplaceInMemory
incYForLoopAdjacent:
			pop dx 
			pop cx 
			cmp dx, 10
			jne JumploopAdjacentNodesOnXAxisToLong
			jmp loopAdjacentNodesOnXAxis
JumploopAdjacentNodesOnXAxisToLong:
			add dx, 10
			jmp loopAdjacentNodesOnYAxis
	
exitLoopAdjacent:
	popa 
	pop bp 
	ret 6
endp LoadChildren

;------------------------------------------------------------------------;
;CheckIfNear  
;Args: x1, y1 and x2, y2 
;Action: check if node at x1, y1 is near x2,y2 Note: near is only one up one down one to the side and one to the other side 
;Return: ax = 0 if near
;------------------------------------------------------------------------
x1_CheckIfNear	equ  [bp + 4]
y1_CheckIfNear	equ  [bp + 6]
x2_target equ [bp + 8]
y2_target equ [bp + 10]
proc CheckIfNear
	push bp 
	mov bp, sp 
	push dx 
	push cx 
	push bx 
	
	xor dx, dx 
	mov ax, x1_CheckIfNear
	mov bx, 10 
	div bx 
	mov x1_CheckIfNear, ax 
	
	xor dx, dx 
	mov ax, y1_CheckIfNear
	mov bx, 10 
	div bx 
	mov y1_CheckIfNear, ax 
	
	mov ax, x2_target
	xor dx, dx 
	mov bx, 10 
	div bx 
	sub ax, x1_CheckIfNear
	cmp ax, 1
	jg notNearStartX
	cmp ax, -1 
	jl notNearStartX
	xor dx, dx 
	mov ax, y2_target
	mov bx, 10 
	div bx 
	cmp ax, y1_CheckIfNear
	jne notNearStartX
	mov ax, 0
	jmp endNearStart
	
notNearStartX:
	
	mov ax, y2_target
	xor dx, dx 
	mov bx, 10 
	div bx 
	sub ax, y1_CheckIfNear
	cmp ax, 1
	jg notNearStartY
	cmp ax, -1 
	jl notNearStartY
	xor dx, dx 
	mov ax, x2_target
	mov bx, 10 
	div bx 
	cmp ax, x1_CheckIfNear
	jne notNearStartY
	mov ax, 0
	jmp endNearStart
	
notNearStartY:
	mov ax, 1
	
	endNearStart:
	pop bx 
	pop cx 
	pop dx 
	pop bp
	ret 8
endp CheckIfNear

;------------------------------------------------------------------------;
;ReplaceInMemory  
;Args: x, y ,ParentIndex, f(n)
;Action: if this node exist in memory and this version is better f value replace else just add it 
;Return: Nothing
;------------------------------------------------------------------------
x_ReplaceInMemory	equ  [bp + 4]
y_ReplaceInMemory	equ  [bp + 6]
parentIndex_ReplaceInMemory equ [bp +8] 
fx_ReplaceInMemory equ [bp + 10]
proc ReplaceInMemory
	push bp 
	mov bp, sp 
	pusha 
	mov cx, NumberOfNodesSearcherd
GoOverMemory_ReplaceInMemory:
	push cx 
	mov ax, cx 
	mov bx, 8
	mul bx 
	add ax, 16 
	mov bx, ax 
	mov ax, [word ptr bx + 2]
	call SetXandYProperly
	mov cx, ax 
	mov ax, [word ptr bx + 4]
	call SetXandYProperly
	mov dx, ax 
	cmp cx, x_ReplaceInMemory
	jne notfound_ReplaceInMemory
	cmp dx, y_ReplaceInMemory
	jne notfound_ReplaceInMemory
	mov ax, [word ptr bx + 6]
	cmp ax, fx_ReplaceInMemory
	jg notfound_ReplaceInMemory
	mov [word ptr bx + 6], ax
	mov [word ptr bx + 4], dx 
	mov [word ptr bx + 2], cx 
	pop cx 
	jmp endforReplaceInMemory
notfound_ReplaceInMemory:
	pop cx
	loop GoOverMemory_ReplaceInMemory
	
	mov ax, NumberOfNodesSearcherd ; number of points found 
	inc ax 
	mov NumberOfNodesSearcherd, ax 
	sub ax, 1
	mov bx, 8
	mul bx
	add ax, 16 
	mov bx, ax 
	mov ax, parentIndex_ReplaceInMemory
	mov [word ptr NodesData + bx], ax ;index of parent
	mov cx, x_ReplaceInMemory
	mov [word ptr NodesData + bx + 2], cx  ; x  
	mov dx, y_ReplaceInMemory
	mov [word ptr NodesData + bx + 4], dx ;y
	mov ax, fx_ReplaceInMemory
	mov [word ptr NodesData + bx + 6], ax 
	
	mov color, 14 
	push dx 
	push cx 
	call PaintSquare
	
endforReplaceInMemory:
	popa
	pop bp 
	ret 8
endp ReplaceInMemory

;------------------------------------------------------------------------;
;CalculateFvaluerun
;Args: x and y of node  
;also two local varibles for more easy calculations
;Action: CalculateFvalue of node 
;Return: ax: f value of node
;------------------------------------------------------------------------;
x_CalculateFvalue	equ  [bp + 4]
y_CalculateFvalue	equ  [bp + 6]
index_CalculateFvalue equ  [bp + 8]
hn_CalculateFvalue equ [bp - 4]
gn_CalculateFvalue equ [bp - 2]
proc CalculateFvalue
	push bp 
	mov bp, sp
	sub sp, 4
	push dx 
	push cx 
	push si 
	push bx 
	
	push y_CalculateFvalue
	push x_CalculateFvalue
	call CalculateHvalue
	mov hn_CalculateFvalue, ax 
	
	;get g(n) g(n) = f(n) - h(n) of last node 
	mov bx, index_CalculateFvalue
	mov cx, [word ptr NodesData + bx + 2];make x node square number 
;	
	mov dx, [word ptr NodesData + bx + 4] ;make y node square number 
	
	push dx 
	push cx 
	call CalculateHvalue
	mov bx, index_CalculateFvalue
	mov dx, [word ptr NodesData + bx + 6]
	sub dx, ax 
	inc dx 
	
	mov ax, hn_CalculateFvalue
	add ax, dx ;f(n) = h(n) + g(n)   if i dont add h(n) it will be jefuhdrfvu algo
	
	pop bx 
	pop si 
	pop cx 
	pop dx 
	add sp, 4
	pop bp 
	ret 6
endp CalculateFvalue

;------------------------------------------------------------------------;
;CalculateHvaluerun
;Args: x and y of node 
;Action: CalculateHvalue of node 
;Return: ax : H value of node
;------------------------------------------------------------------------;
x_CalculateHvalue	equ  [bp + 4]
y_CalculateHvalue	equ  [bp + 6]
Proc CalculateHvalue 
	push bp 
	mov bp, sp
	push dx 
	push cx 
	push si 
	push bx 
	
	mov ax, 0
	mov bl,[Mode]
	cmp bl, 2
	je endCalculateHvalue
	
	xor dx, dx 
	mov ax, x_CalculateHvalue ;make x node square number 
	mov bx, 10 
	div bx 
	mov cx, ax 
	
	xor dx, dx 
	mov ax, y_CalculateHvalue ;make y node square number 
	mov bx, 10 
	div bx 
	mov dx, ax 
	
	push dx
	xor dx, dx 
	mov ax, YendPoint ;make end point y square nunmber
	mov bx, 10 
	div bx  
	mov si, ax 
		
	xor dx, dx 
	mov ax, XendPoint ;make end point x square nunmber
	mov bx, 10 
	div bx  
	pop dx 
	
	sub cx, ax
	cmp cx, 0
	jg notnegX_CalculateFvalue
	neg cx 
notnegX_CalculateFvalue:
	mov ax, cx 
	
	sub dx, si
	cmp dx, 0
	jg notnegY_CalculateFvalue
	neg dx 
notnegY_CalculateFvalue:
	add ax, dx ; get h(n)
	
endCalculateHvalue:
	
	pop bx 
	pop si 
	pop cx 
	pop dx 
	pop bp 
	ret 4
endp CalculateHvalue
;------------------------------------------------------------------------;
;findLowestFcost
;Args: None
;Action: finds next node to explore from 
;Return: index of node in the IndexOfBestNode part in the array
;------------------------------------------------------------------------;
proc findLowestFcost
	pusha 
	mov ax, 32768
	mov cx, NumberOfNodesSearcherd
	mov IndexOfBestNode, 0
loopAllNodesInMemory:
	push ax
	mov ax, cx 
	mov bx, 8 
	xor dx, dx 
	mul bx 
	mov bx,16 
	add bx, ax 
	pop ax 
	sub bx, 8
	
	 
	mov dx, [word ptr NodesData + bx + 6]
	cmp dx, ax 
	ja noNeedToSearch_findLowestFcost
	
	push bx
	push cx 
	push dx 
	push ax 
	mov cx, [word ptr NodesData + bx + 2]
	mov dx, [word ptr NodesData + bx + 4]
	
	
	
	mov bh, 0h
	mov ah, 0Dh
	int 10h
	
	cmp al, 38 
	jne notExploared_findLowestFcost
	pop ax 
	pop dx 
	pop cx 
	pop bx 
	jmp noNeedToSearch_findLowestFcost
notExploared_findLowestFcost:
	pop ax 
	pop dx 
	pop cx 
	pop bx 
	mov ax, dx 
	mov IndexOfBestNode, bx
noNeedToSearch_findLowestFcost:
	loop loopAllNodesInMemory 
	popa 
	ret 
endp findLowestFcost

;------------------------------------------------------------------------;
;Visualize
;Args: None
;Action: starts the Visualizetion process
;Return: ?
;------------------------------------------------------------------------;
proc Visualize
	;start 
	xor dx, dx 
	xor cx, cx 
	mov ax, FoundPath
	cmp ax, 0 
	jne endallvizualize
	
	push 0
	push YstartPoint
	push XstartPoint
	call LoadChildren
	

algorithemLoop:
	mov ax, 6
	sub al, [Speed]
	push ax 
	call delay
	mov ax, FoundPath
	cmp ax, 0 
	jne exitloop_Visualize
	call findLowestFcost
	mov bx,IndexOfBestNode
	cmp bx, 0
	je endallvizualize
	mov ax, [word ptr NodesData + bx + 4];get y
	call SetXandYProperly
	mov dx, ax 
	
	mov ax, [word ptr NodesData + bx + 2];get x 
	call SetXandYProperly
	mov cx, ax 
	
	push bx 
	push dx 
	push cx 
	call LoadChildren
	

	jmp algorithemLoop
exitloop_Visualize:
	
	call DingDong
	mov bx, 54
	mov color, 54 
	
paintpathloop:
	mov bx, ax 
	mov dx,[word ptr NodesData + bx + 4]
	mov cx,[word ptr NodesData + bx + 2]
	push dx 
	push cx 
	call PaintSquare
	
	mov ax, [word ptr NodesData + bx]
	cmp ax, 0 
	jne paintpathloop
endallvizualize:
	mov color, 0h
	mov FoundPath, 1
	ret 
endp Visualize

;------------------------------------------------------------------------;
;SetXandYProperly
;Args: nubmer to use with ax 
;Action: get number of node 
;Return: number with ax 
;------------------------------------------------------------------------;
proc SetXandYProperly 
	push dx 
	push bx 
	mov bx, 10
	xor dx, dx 
	div bx 
	mul bx 
	inc ax 
	pop bx 
	pop dx 
	ret
endp SetXandYProperly

;------------------------------------------------------------------------;
;CycleSpeed
;Args: ScanCode Of key
;Action: CycleSpeed
;Return: None
;------------------------------------------------------------------------;
KeyPressed_CycleSpeed equ [bp + 4]
proc CycleSpeed 
	push bp
	mov bp, sp 
	pusha
	
	mov ax, 72 ;key scan of up arrow 
	cmp KeyPressed_CycleSpeed, ax 
	jne IfDownArrow
	
	push 2
	call delay; it is with speed but not that noticable
	
	xor ax, ax 
	mov al, [Speed]
	cmp al, 5 
	je EndCycleSpeed
	inc ax
	mov [Speed], al
	jmp EndCycleSpeed
IfDownArrow:
	mov ax, 80; key scan of down arrow 
	cmp KeyPressed_CycleSpeed, ax 
	jne EndCycleSpeed
	push 2
	call delay ; it is with speed but not that noticable
	xor ax, ax 
	mov al, [Speed]
	cmp al, 1 
	je EndCycleSpeed
	dec ax
	mov [Speed], al
EndCycleSpeed:
	popa 
	pop bp 
	ret 2
endp CycleSpeed

start :
	mov ax, @data
	mov ds, ax
	
	
	; offset color 3, 4 if found   4 5 index of best node  6 7 numbers of nodes searched (found f value for it) x start point 8 9, y start point 10 11, x end point 12 13 , y end point 14 15      0 1 index of src 2 3 x 4 5 y 6 7 f value 
	
	; Graphic mode
	mov ax, 13h
	int 10h 
	
	call PaintScreen
	
	mov dx, offset picFilename
	call OpenFile
	
	mov bx, ax ; AX is the file handle from OpenFile
	mov dx, offset tmpHeader ; 54 bytes on memory
	call ReadBMPHeader
	
	mov dx, offset Palette
	call ReadBMPPalette
	
	mov si, dx
	call CopyBMPPalette

	mov dx, offset picture
	mov cx, 10 ; height
	mov ax, 320 ; width
	call CopyBMPToMemory
	
	call CloseFile
	
	push offset picture
	push 320 ; width
	push 10 ; height
	push 0 ; x
	push 0 ; y
	call DrawFromMemory

	mov ax, 123 
	call SetXandYProperly
	
	mov ax, 0h ; start mouse 
	int 033h
	
	mov ax, 01h;show mouse 
	int 033h
	
	waitfordata:
		mov ax, 03h
		int 033h
		
		push 4
		call delay 
		
		xor ax, ax 
		in al, 064h
		cmp al, 10b
		je waitfordata  ;idk why it doesnt work anyways
		in al, 60h
		cmp al, 02h
		jne IfSwitchDijkstra
		mov [Mode], 1
		jmp waitfordata
IfSwitchDijkstra:
		cmp al, 03h
		jne IfExit
		mov [Mode], 2
		jmp waitfordata
IfExit:
		cmp al, 01h
		jne IfSpeed 
		jmp exitgame 
IfSpeed:
		push ax
		call CycleSpeed


		push bx 
		cmp bx, 0h
		jne handleInput
		
		jmp waitfordata

		handleInput:
		
		mov ax, FoundPath
		cmp ax, 0 
		je NoNeedClear
		mov ax, 02h;remove mouse 
		int 033h
		
		call ClearArray
		call PaintScreen
		
		mov ax, 01h;remove mouse 
		int 033h
		
		jmp waitfordata
NoNeedClear:
		;make y click the edge of block
		mov ax, dx 
		mov bx, 10d 
		xor dx, dx 
		div bx 
		mul bx 
		mov dx, ax 
		inc dx 
		push dx 
		;make x click the edge of block
		shr cx, 1
		mov ax, cx 
		mov bx, 10d 
		xor dx, dx 
		div bx 
		mul bx 
		pop dx 
		mov cx, ax 
		inc cx
		
		;get color of block 
		mov ax, 02h;remove mouse 
		int 033h
		mov bh,0h
		mov ah,0Dh
		int 10h
		push ax 
		mov ax, 01h;place mouse 
		int 033h
		pop ax 
		
		pop bx 
		;check if click on endpoint, start point 
		cmp al, 59
		jne nextcolorcheckIfOnEndPoint
		jmp waitfordata
nextcolorcheckIfOnEndPoint:
		cmp al, 75
		jne notOnStartOrEnd
		jmp waitfordata
notOnStartOrEnd:
		cmp dx, 10 
		ja notControlPanel_click
		mov ax, cx 
		xor dx, dx 
		mov si, 50
		div si 
		;start checking on control panel 

		cmp ax, 0
		jne  notVizualize_clickcheck
		mov ax, 02h;remove mouse 
		int 033h
		call Visualize
		mov ax, 01h;show mouse 
		int 033h
		jmp waitfordata
		
		notVizualize_clickcheck:
		cmp ax, 1
		jne notWall_clickcheck
		mov color, 0h
		jmp waitfordata
		
		notWall_clickcheck:
		cmp ax, 3
		jne notStartPoint_clickcheck
		mov color, 59
		jmp waitfordata
		
		notStartPoint_clickcheck:
		cmp ax, 2 
		jne notEndPoint 
		mov color, 75
		jmp waitfordata
			
		notEndPoint:
		cmp ax, 4
		jne notClear 
		mov ax, 02h;remove mouse 
		int 033h
		call PaintScreen
		mov ax, 01h;show mouse 
		int 033h
		jmp waitfordata
		notClear:
		jmp waitfordata
		
		notControlPanel_click:

		cmp bx, 01h 
		je leftClickHandle
		
		cmp bx, 02h 
		jne checktypeOfclickOutOFrange
		jmp rightClickHandle
		
checktypeOfclickOutOFrange:
		leftClickHandle:
		mov ax, 02h;remove mouse 
		int 033h
		mov ax, color 
			
		push dx 
		push cx 
		cmp ax, 59
		je handleStartPoint ;check if need to handle start point
		
		cmp ax, 75
		je handleEndPoint ;check if need to handle end point
		
		jmp PaintBlockCicked
		
		handleStartPoint:
			pop cx 
			pop dx 
			push YendPoint
			push XendPoint
			push dx 
			push cx 
			call CheckIfNear
			cmp ax, 0
			jne StartNotNearEnd
			mov ax, 01h;show mouse 
			int 033h
			mov ax, color 
			jmp waitfordata
StartNotNearEnd:
			push dx 
			push cx
			mov dx, YstartPoint
			mov cx, XstartPoint
			mov color, 0Fh 
			push dx
			push cx 
			call PaintSquare
			pop cx 
			pop dx 
			mov YstartPoint, dx  
			mov XstartPoint, cx 
			mov color, 59
			push dx 
			push cx 
			jmp PaintBlockCicked
			
		handleEndPoint:
			pop cx 
			pop dx 
			push YstartPoint
			push XstartPoint
			push dx 
			push cx 
			call CheckIfNear
			cmp ax, 0
			jne EndNotNearStart
			mov ax, 01h;show mouse 
			int 033h
			mov ax, color 
			jmp waitfordata
EndNotNearStart:
			push dx 
			push cx 
			mov dx, YendPoint
			mov cx, XendPoint
			mov color, 0Fh 
			push dx
			push cx 
			call PaintSquare
			pop cx 
			pop dx 
			mov YendPoint, dx  
			mov XendPoint, cx 
			mov color, 75
			push dx 
			push cx 
			jmp PaintBlockCicked
		
		PaintBlockCicked:
			call PaintSquare
		
		mov ax, 01h
		int 033h
		jmp waitfordata
		
		rightClickHandle:
		mov ax, color 
		push ax 
		mov color, 0Fh 
		
		mov ax, 02h;remove mouse 
		int 033h
		
		push dx 
		push cx 
		call PaintSquare
		
		mov ax, 01h
		int 033h
		
		pop ax 
		mov color, ax 
		jmp waitfordata
		
	exitgame:

	mov ah,00h
	int 16h
	; Return to text mode
	mov ah, 0
	mov al, 2
	int 10h
exit :
	mov ax, 4c00h
	int 21h
END start