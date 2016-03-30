
package assembly Hamming is

pragma MEMORY_MODEL(1)

	jmp	Main

------------------------------------------------------------------------------
-- procedure Hamming_Encode

--	ENTER
--	Byte to encode on the stack.

--	EXIT
--	Ah contains the code bits.  The code bits are stored in the
-- lower 5 bits of the byte.  The upper bits are zero.  No other registers
-- changed. 
------------------------------------------------------------------------------

		CONSTSEG
Generator_Matrix	DB	0F8h, 0C7h, 0B6h, 06Dh, 01Bh

		CSEG

function Hamming_Encode(Data_Byte : IN Integer) RETURN Integer is

		-- Set up stack frame.
		push	bp
		mov	bp, sp
		push	es

		-- Get the data byte. (Have to skip over far address).
		mov	ax, [bp+6]

		-- Clear code bits to zero.
		xor	dl, dl

		-- Point at table of matrix columns and initialize loop.
		mov	bx, SEGMENT CONSTSEG
		mov	es, bx
		mov	bx, OFFSET Generator_Matrix
		mov	cx, 5

top_of_loop:
		push	ax

		-- "Multiply" each bit in message by each bit in a matrix column.
		and	al, es:[bx]

		-- "Add" resulting bits. Leave sum bit in carry flag.
		clc
		jpe	shift_it
		stc

		-- Shift result into least significant side of code bits.
shift_it:
		rcl	dl, 1

		-- Next loop pass.
		inc	bx
		pop	ax
		loop	top_of_loop

		-- Copy result into ah for return.
		mov	ah, dl

		-- Tear down stack frame and remove parameter on return.
		pop	es
		pop	bp
		ret FAR OFFSET 2

end Hamming_Encode;

Main:	-- Do nothing.

end Hamming;

