program bF_to_FORTRAN

integer :: i,j,dump
integer :: buf_pos, buf_len, ios
logical :: buffer_full
character(1) :: p,nothing
character(1000) :: buf
integer :: tape(30000)
tape=0
i=1
j=1
1 format(A1,$)

call ADD(8)
do while (tape(j)/=0)
call MVR(1)
call ADD(4)
do while (tape(j)/=0)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(1)
call MVL(41)
call SUB(1)
end do
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(1)
call MVR(1)
call ADD(2)
call MVR(5)
call ADD(2)
call MVR(1)
call ADD(3)
call MVR(2)
call ADD(1)
call MVR(3)
call ADD(2)
call MVR(2)
call ADD(1)
call MVR(1)
call MVR(1)
call ADD(2)
call MVR(2)
call MVR(1)
call ADD(1)
call MVR(2)
call MVR(1)
call ADD(3)
call MVR(1)
call ADD(2)
call MVR(1)
call ADD(3)
call MVR(1)
call MVR(3)
call ADD(2)
call MVR(2)
call ADD(1)
call MVR(1)
call ADD(1)
call MVR(2)
call MVR(1)
call ADD(1)
call MVR(2)
call MVR(1)
call ADD(1)
call MVR(3)
call MVL(43)
call SUB(1)
end do
call MVR(2)
call ADD(1)
call OUT
call MVR(1)
call SUB(1)
call OUT
call MVR(1)
call SUB(3)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(6)
call OUT
call MVR(1)
call SUB(1)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call ADD(2)
call OUT
call MVR(1)
call ADD(3)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call ADD(4)
call OUT
call MVR(1)
call ADD(3)
call OUT
call MVR(1)
call OUT
call MVR(1)
call OUT
call MVR(1)
call SUB(3)
call OUT
call MVR(1)
call ADD(5)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call ADD(6)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call SUB(1)
call OUT
call MVR(1)
call SUB(3)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(3)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call SUB(2)
call OUT
call MVR(1)
call ADD(1)
call OUT
call MVR(1)
call ADD(2)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(5)
call OUT
call MVR(1)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(5)
call OUT
call MVR(1)
call OUT
call MVR(1)
call OUT
call MVR(1)
call ADD(5)
call OUT
call MVR(1)
call ADD(1)
call OUT
call DUMPTAPE

contains
    subroutine ADD(COUNT)
        !This adds COUNT to the cell, overflowing when that occurs.
        integer :: count
        tape(j)=modulo(tape(j)+count,256)
    end subroutine ADD

    subroutine SUB(COUNT)
        !This subtracts COUNT from the cell, underflowing when that occurs.
        integer :: count
        tape(j)=modulo(tape(j)+(255*count),256)!This appears to work sooo :3
    end subroutine SUB

    subroutine MVR(COUNT)
        !This moves our pointer to the right.
        integer :: count
        j=j+count
    end subroutine MVR

    subroutine MVL(COUNT)
        !This moves our pointer to the left.
        integer :: count
        j=j-count
    end subroutine MVL

    subroutine OUT
        !This outputs the current cells ascii value.
        1 format(A1,$)
        write(*,1) achar(tape(j))
    end subroutine OUT

    subroutine INP !Stinky hack
        !This checks if we have anything in the buffer.
        if (.not. buffer_full) then !If we have read everything so far...
            read(*,'(A)',iostat=ios) buf !Take user input.
            if (ios /= 0) then !If there is nothing/an error...
                tape(j)=0 !Set this to zero
                buffer_full=.false. !This will not be true
                return !and leave.
            end if !Done with that part.
            buf_len=len_trim(buf) !Else, we get our input length
            buf_pos=1 !Set our buffer pointer to the beginning.
            buffer_full=.true. !Our buffer is now full.
        end if !Aight, let us get a character.
        !This gets our next character and checks for EOF
        if (buf_pos <= buf_len) then !If we are in bounds...
            tape(j)=iachar(buf(buf_pos:buf_pos)) !Set our cell to that input.
            buf_pos=buf_pos+1 !Proceed.
        else !If we have reached EOF...
            tape(j)=0 !Set it to 0x00
        buffer_full=.false. !Make sure that this is false
        end if
    end subroutine INP !Begone, foul subroutine! May your texts never torment me ever again!

    subroutine DUMPTAPE
        !This dumps all of the tapes contents.
        1 format(A1,$)
        open(newunit=dump, file='dump.hex', status='replace',action='write',position='append',iostat=ios)
        do m=1,size(tape)
            write(dump,1) achar(tape(m))
        end do
        close(dump)
        read(*,*) !nothing
    end subroutine DUMPTAPE
end program bF_to_FORTRAN
