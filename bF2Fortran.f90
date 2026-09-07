program bF2Fortran
    !Basic brainfuck to Fortran compiler
    !(technically a translator but whatever)
    !Made by LunaFox
    !This is a program that turns code written in brainfuck into code for Fortran.
    !This is optimizing. (This was hard. And hacky, but mostly hard)
    !As always, check the readme for more info.
    !Now, onto the show!
    !Set up the variables, arrays, etc.
    integer :: input_unit
    character(512) :: input_file, output_file
    integer :: file_unit !Hackity Hack Hack 
    integer :: ios !Hackity Hack Hack
    
    integer :: i,i2,j,j2,k
    character(:), allocatable :: prgm,l3 !This is our program.
    character(3000000) :: buffer !Init reading buffer
    integer :: tape(30000)!This is our cell array. It is currently set to 8 bit mode.
    character(1) :: l,p,r
    character(100) :: l2
    !integer :: p
    tape=0 !This is our 'RAM', in a sense. This sets it all to zero.
    i=1 !This is our program index, and our primary looping variable. It is 1.
    i2=1 !This is our secondary program index. Fortran arrays are 1 indexed.
    j=1 !This is our memory pointer.
    j2=1 !Is this even used? I don't know.
    k=0 !Here be dragons in this variable. It's behaviour is unknown. (pls help)
    l2="" !Hackity Hack Hack oooh Oh
    1 format(A1,$) !Not used here, just for reference.
    !This is a sample program. If this gets sent out, just know that that shouldn't happen.
    prgm="" !It's a fox.
    !Bad hack that works
    if (command_argument_count() < 1) then !If there is no input... 
        print *, "Usage: bF2Fortran <input.bf> [output.f90]" !Tell them that they are, in fact, an idiot.
        stop 1 !And quit. Just like I almost did on this 'fun' project. God I underestimated this.
    end if !Ok.
    call get_command_argument(1, input_file) !Don't know why this is placed here.
    call get_command_argument(2, output_file) !Don't know why this is placed here.
    open(newunit=input_unit, file=trim(input_file), status="old", & !Let us look at this file.
            action="read", iostat=ios)
    if (ios /= 0) then !If error...
        print *, "Error: could not open input file: ", trim(input_file) !Say this shtick
        stop 1 !And quit!
    end if !This isn't that hard!
    do !Noclue why works, but it works regardless so it stays. No touchie. 3:<
        read(input_unit, '(A)', iostat=ios) buffer !Read the file into the buffer.
        if (ios /= 0) exit !If it errors we leave
        prgm = prgm // trim(buffer) !Else we do what we did before :)
    end do !Alright, touchie is back on :3
    close(input_unit) !We are done with that.
    
    !This is for handling file output. It's actually quite simple!
    if (command_argument_count()<2) then !If the output argument is missing (It is safe to assume that the input would be there if we've gotten this far)
        open(newunit=file_unit, file="output.f90", status="replace",action="write", position="append", iostat=ios) !Open the default.
        if (ios /= 0) then !If error...
            print *, "Error: could not open output file: output.f90" !Say this shtick
            stop 1 !And quit!
        end if !This isn't that hard!
    else !If it isn't...
        open(newunit=file_unit, file=trim(output_file), status="replace",action="write", position="append", iostat=ios) !Open the new file.
        if (ios /= 0) then !If error...
            print *, "Error: could not open input file: ", trim(input_file) !Say this shtick
            stop 1 !And quit!
        end if !This isn't that hard!
    end if !Done with this, hooray!
    !open(newunit=file_unit, file="output.f90", status="replace",action="write", position="append", iostat=ios)
    !print the header of our program
    write(file_unit,'(A)') "program bF_to_FORTRAN"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "integer :: i,j"
    write(file_unit,'(A)') "integer :: buf_pos, buf_len, ios"
    write(file_unit,'(A)') "logical :: buffer_full"
    write(file_unit,'(A)') "character(1) :: p"
    write(file_unit,'(A)') "character(1000) :: buf"
    write(file_unit,'(A)') "integer :: tape(30000)"
    write(file_unit,'(A)') "tape=0"
    write(file_unit,'(A)') "i=1"
    write(file_unit,'(A)') "j=1"
    write(file_unit,'(A)') "1 format(A1,$)"
    write(file_unit,'(A)') ""

    !So now we get to the main part of this.
    do while (i<=len(prgm)) !This is the core of it all. The brain, if you will.
        l=prgm(i:i) !select case again.
        select case(l) !Now here's the checker *insert funny cat gif*
            case(">"); !If we have to move right...
                k=1
                !print '(A)',"j=j+1";
                i2=i !Set secondary index to our current index
                do while (.true.) !While in our loop...
                    r=prgm(i2:i2) !knock-off l
                    select case(r) !The other checker(board)
                        case(">"); k=k+1 !If it is another, Increment our count
                        case DEFAULT; exit !If not, we leave
                    end select !Alright. That wasn't bad.
                    i2=i2+1 !Increment index
                end do !Wow we are done!
                i=i2-1 !Minus 1
                !Stinky AI/Stack Overflow hack start
                !  1. Write the integer into a character variable (internal file)
                write(l2, '(I0)') k-1
                print *,l2
                !  2. Concatenate using //, cleaning up trailing and leading spaces
                l2 = "call MVR("//trim(l2)//")"
                print *,l2
                !Aight, no more AI.
                write(file_unit,'(A)') trim(l2) !Write to the file.
                i2=1 !Reset for the next one.
                k=1 !I have no idea what this does. Please do not touch any of these.
            !Next case.
            case("<"); !print '(A)',"j=j-1"; 
                k=1
                i2=i
                do while (.true.)
                    r=prgm(i2:i2)
                    select case(r)
                        case("<"); k=k+1
                        case DEFAULT; exit
                    end select
                    i2=i2+1
                end do
                i=i2-1
                !  1. Write the integer into a character variable (internal file)
                write(l2, '(I0)') k-1

                !  2. Concatenate using //, cleaning up trailing and leading spaces
                l2 = "call MVL("//trim(l2)//")"
                
                write(file_unit,'(A)') l2
                i2=1
                k=1
            !
            case("+"); !print '(A)',"tape(j) = tape(j)+1"; 
                k=1
                i2=i
                do while (.true.)
                    r=prgm(i2:i2)
                    select case(r)
                        case("+"); k=k+1
                        case DEFAULT; exit
                    end select
                    i2=i2+1
                end do
                i=i2-1
                !  1. Write the integer into a character variable (internal file)
                write(l2, '(I0)') k-1

                !  2. Concatenate using //, cleaning up trailing and leading spaces
                l2 = "call ADD("//trim(l2)//")"
                
                write(file_unit,'(A)') l2
                i2=1
                k=1        
            !
            case("-"); !print '(A)',"tape(j) = tape(j)-1"; 
                i2=i
                k=1
                do while (.true.)
                    r=prgm(i2:i2)
                    select case(r)
                        case("-"); k=k+1
                        case DEFAULT; exit
                    end select
                    i2=i2+1
                end do
                i=i2-1
                !  1. Write the integer into a character variable (internal file)
                write(l2, '(I0)') k-1

                !  2. Concatenate using //, cleaning up trailing and leading spaces
                l2 = "call SUB("//trim(l2)//")"
                
                write(file_unit,'(A)') l2
                i2=1
                k=1
            !Now we are done with the optimizing part.
            case("."); print '(A)',"write(*,1) achar(tape(j))"; write(file_unit,'(A)') "call OUT" !print *,"." !Write output
            case(","); print '(A)',"print *,'Program requests input: '; read(*,*) p; tape(j)=iachar(p)"; 
                write(file_unit,'(A)') "call INP" !print *,"," !Read input
            case("["); print '(A)',"do while (tape(j)/=0)"; write(file_unit,'(A)') "do while (tape(j)/=0)" !Start loop
            case("]"); print '(A)',"end do"; write(file_unit,'(A)') "end do" !End loop
            case DEFAULT; print *,"Ah, fiddlesticks! What now?" !Should probably comment this out
        end select !We are done with the bulk.
    i=i+1 !Next index.
    end do !Oh! We are done!
    print '(A)',"" !newline
    write(file_unit,'(A)') ""
    !Do our funcs
    write(file_unit,'(A)') "contains"
    write(file_unit,'(A)') "    subroutine ADD(COUNT)"!This adds COUNT to the cell.
    write(file_unit,'(A)') "        !This adds COUNT to the cell, overflowing when that occurs."
    write(file_unit,'(A)') "        integer :: count"
    write(file_unit,'(A)') "        tape(j)=modulo(tape(j)+count,256)"
    write(file_unit,'(A)') "    end subroutine ADD"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "    subroutine SUB(COUNT)"
    write(file_unit,'(A)') "        !This subtracts COUNT from the cell, underflowing when that occurs."
    write(file_unit,'(A)') "        integer :: count"
    write(file_unit,'(A)') "        tape(j)=modulo(tape(j)+(255*count),256)!This appears to work sooo :3"
    write(file_unit,'(A)') "    end subroutine SUB"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "    subroutine MVR(COUNT)"
    write(file_unit,'(A)') "        !This moves our pointer to the right."
    write(file_unit,'(A)') "        integer :: count"
    write(file_unit,'(A)') "        j=j+count"
    write(file_unit,'(A)') "    end subroutine MVR"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "    subroutine MVL(COUNT)"
    write(file_unit,'(A)') "        !This moves our pointer to the left."
    write(file_unit,'(A)') "        integer :: count"
    write(file_unit,'(A)') "        j=j-count"
    write(file_unit,'(A)') "    end subroutine MVL"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "    subroutine OUT"
    write(file_unit,'(A)') "        !This outputs the current cells ascii value."
    write(file_unit,'(A)') "        1 format(A1,$)"
    write(file_unit,'(A)') "        write(*,1) achar(tape(j))"
    write(file_unit,'(A)') "    end subroutine OUT"
    write(file_unit,'(A)') ""
    write(file_unit,'(A)') "    subroutine INP !Stinky hack"
    write(file_unit,'(A)') "        !This checks if we have anything in the buffer."
    write(file_unit,'(A)') "        if (.not. buffer_full) then !If we have read everything so far..."
    write(file_unit,'(A)') "            read(*,'(A)',iostat=ios) buf !Take user input."
    write(file_unit,'(A)') "            if (ios /= 0) then !If there is nothing/an error..."
    write(file_unit,'(A)') "                tape(j)=0 !Set this to zero"
    write(file_unit,'(A)') "                buffer_full=.false. !This will not be true"
    write(file_unit,'(A)') "                return !and leave."
    write(file_unit,'(A)') "            end if !Done with that part."
    write(file_unit,'(A)') "            buf_len=len_trim(buf) !Else, we get our input length"
    write(file_unit,'(A)') "            buf_pos=1 !Set our buffer pointer to the beginning."
    write(file_unit,'(A)') "            buffer_full=.true. !Our buffer is now full."
    write(file_unit,'(A)') "        end if !Aight, let us get a character."
    write(file_unit,'(A)') "        !This gets our next character and checks for EOF"
    write(file_unit,'(A)') "        if (buf_pos <= buf_len) then !If we are in bounds..."
    write(file_unit,'(A)') "            tape(j)=iachar(buf(buf_pos:buf_pos)) !Set our cell to that input."
    write(file_unit,'(A)') "            buf_pos=buf_pos+1 !Proceed."
    write(file_unit,'(A)') "        else !If we have reached EOF..."
    write(file_unit,'(A)') "            tape(j)=0 !Set it to 0x00"
    write(file_unit,'(A)') "        buffer_full=.false. !Make sure that this is false"
    write(file_unit,'(A)') "        end if"
    write(file_unit,'(A)') "    end subroutine INP !Begone, foul subroutine! May your texts never torment me ever again!"
    print '(A)',"end program bF_to_FORTRAN" !End of our outputted code.
    write(file_unit,'(A)') "end program bF_to_FORTRAN"
    close(file_unit) !And throw this file to the wayside. Good riddence.
end program bF2Fortran