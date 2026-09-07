program bF
    !bFORTRAN: A brainfuck interpreter in Fortran.
    !Made by LunaFox
    !This program is a fully fledged brainfuck interpreter.
    !Everything should work to specification. If anything doesn't work, please tell me!
    !All documentaion should be in the readMe. Good luck!
    !Now, onto the main show!
    !use, intrinsic :: iso_fortran_env !This will be useful. Actually not very useful, but oh well.
    !Set up the variables, arrays, etc. 
    integer :: input_unit
    character(512) :: input_file
    integer :: ios !Hackity Hack Hack
    integer :: i,i2,j,k,m,n,o,c,b
    INTEGER MSG,KBD
    character(:), allocatable :: prgm !This is our program.
    character(20000) :: buffer !Init reading buffer
    INTEGER, allocatable :: jmp1(:),jmp2(:), ind(:) !Here are our jump arrays.
    integer :: tape(30000)!This is our cell array. It is currently set to 8 bit mode.
    character(1000) :: buf !This is our input buffer.
    character(1) :: l,p
    integer :: buf_pos, buf_len
    logical :: buffer_full
    !integer :: p
    tape=0 !This is our 'RAM', in a sense. This sets it all to zero.
    i=1 !This is our program index, and our primary looping variable. It is 1.
    i2=1 !This is our secondary program index. Fortran arrays are 1 indexed.
    j=1 !This is our memory pointer.
    k=0 !This is unused.
    kbd=5 !This, too, is unused.
    msg=6 !What a surprise! This is also unused.
    c=1 !Used for jumping.
    1 FORMAT(A1,$) !This formats it so that the output is all on one line.

    !This is our program. Here is a sample that prints out the golden ratio!
    !(god there are a lot of lines)
    !Just be careful with your line lengths, lest you trigger line truncation! /j
    prgm=""
    !This is our reader. I had to set a placeholder.
    !print *,"Please input a program: "
    !read(*, '(A)', iostat=err) input_string
    !I call upon you, old sketchy hack from 2010! Please, make this work, and make it work well!
    
    !print *,"Please input a program: " !This is the input prompt.
    !buffer="" !This resets our buffer.
    !read(*,'(A)') buffer !We store the input in this buffer.
    !print *,buffer
    !prgm=trim(buffer) !We trim the buffer and put that in the program array.
    if (command_argument_count() < 1) then !If there is no input... 
        print *, "Usage: bFortran <input.bf>" !Tell them that they are, in fact, an idiot.
        stop 1 !And quit. Just like I almost did on this 'fun' project. God I underestimated this.
    end if !Ok.
    call get_command_argument(1, input_file) !Don't know why this is placed here.
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
    n=len(prgm) !allocate 1...
    allocate(ind(n), jmp1(n), jmp2(n)) !And allocate 2!
    ind=[0]
    jmp1=[0]
    !jmp1=[6,9] !The index of where to jump to after a '['
    !jmp2=[4,16] !The index of where to jump to after a ']'
    
    !Preprocess the whole damn program cause thats the oly way i coulf think of getting this piece of shit program working.
    !This first part tracks the indexes of '['s
    b=0 !Clear our nesting counter
    !print *,"Finding loop start indices..."
    do while (i<=len(prgm)) !Start the loop to find our '[' indexes
        l=prgm(i:i)!set our case selector
        select case(l) !The glimpse.
            case("[") !If this is a '['
                !print *,"In a loop" !print shit
                ind=[ind,i] !add the index to the array
            case("]") !This case is a leftover.
                !print *,"Out a loop"
            case DEFAULT !For anything else...
                !Do nothing.
        end select !ok
        i=i+1 !Go to next index
    end do !done with indexing the start of loops, lets get with the main part of this.
    !Reset our variables
    i=1
    i2=2
    b=0
    !print *,ind
    jmp2=ind !Copy an array.
    !print *,"Tracking loop pairs..."
    !This Actually tracks the pairs of brackets.
    do while (i2<=size(ind)) ! lets a go!
        i=ind(i2) !Start at the next index.
        do while (i<=len(prgm)) !Go through the program.
            l=prgm(i:i) !select case variable
            select case(l) !but here's the checker *insert funny cat gif*
                case("[") !If it is the start of a different loop...
                    !print *,"Went into a loop" 
                    b=b+1 !...Increment our nesting tracker.
                case("]") !If it is the end of a loop...
                    !print *,"Exited a loop"
                    b=b-1 !decrement our tracker.
                    if (b==0) then !If we are at the end of the loops...
                        !print *,"end found!"
                        jmp1=[jmp1,i] !Add this index to the end tracker.
                        exit !Leave this loop and go to the next index
                    end if !Well, it is kinda embaressing how long it took to
                           !figure this solution out.
            end select !At least it works.
            !print *,"next program thingie"
            i=i+1 !Next character.
        end do !Oh! we are done with that index!
        !print *,"Next starting point"
        i2=i2+1 !Go to the next starting point.
    end do !Well that was easy. whats next
    !Reset our variables.
    i=1
    i2=1
    b=0
    c=1
    !write(*,*) ind
    !print *,jmp1
    !Actually do the thing
    !print *,"Running program!"
    do while (i<=len(prgm)) !This is the core of it all. The brain, if you will.
        l=prgm(i:i) !select case again.
        select case(l) !The checker 2 *insert second funny cat gif*
            case(">"); j=j+1; !print *,">" !Move right
            case("<"); j=j-1; !print *,"<" !Move left
            case("+"); tape(j) = modulo(tape(j)+1,256); !print *,"+" !Increment
            case("-"); tape(j) = modulo(tape(j)-1,256); !print *,"-" !Decrement
            case("."); write(*,1) achar(tape(j)); !print *,"." !Write output
            case(",");
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
            case("[") !Loop start. Now this is the hard part.
                    if (tape(j)==0) then !When the tape is zero...
                        !Iterate on the prgm() until we find the correct ']'
                        do while (c<=size(jmp2)) !While we are still inbounds...
                            if (jmp2(c)==i) then !If our index equals the other
                                !print *,'Yay' 
                                i=jmp1(c) !Go to the correct index
                                exit !and leave.
                            else !If not?
                                !print *,"Aww"
                                c=c+1 !Check the next.
                            end if !Alright. cool.
                        end do !So i haveth a laser pointer
                        c=1 !Reset this.
                    end if !We are done with that.
            case("]") !Loop end. Now this is the other hard part.
                    if (tape(j)/=0) then !If the tape isn't zero...
                        !Iterate on the prgm() until we find a '['
                        do while (c<=size(jmp1)) !While we are still inbounds...
                            if (jmp1(c)==i) then !If our index equals the other
                                !print *,'Yay'
                                i=jmp2(c) !Go to the correct index
                                exit !and leave.
                            else !If not?
                                !print *,"Aww"
                                c=c+1 !Check the next.
                            end if !Well, that wasn't too bad
                        end do !It was still bad, but...
                        c=1 !Reset, Retreat!
                    end if !Done with that.
            case DEFAULT; !print *,"Ah, fiddlesticks! What now?"
        end select !We are done with the bulk.
    i=i+1 !Next index.
    !debug stuff, please ignore
    !write(*,*) tape
    !write(*,*) i
    end do
    !debug stuff. please ignore.
    !write(*,*) jmp1
    !write(*,*) jmp2
    !write(*,*) ind
    
end program bF