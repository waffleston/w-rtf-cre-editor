! w-rtf-cre-editor v0.0.34
! (c) 2017 Brendyn Sonntag
! Licensed under Apache 2.0, see LICENSE file.
! Maximum dictionary size: 300000 entries, each with a max length of 320 chars.

module universal
        ! This module has the core subroutines, independent of terminal REPL interface.

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition - "Statics"
        !---------------------------------------------------------------------------------------------------------------------------

        character (len=25) :: corev = "w-rtf-cre-editor v0.0.34"
        integer, parameter :: maxCLen = 320
        integer, parameter :: maxDSize = 300000

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition - "Globals"
        !---------------------------------------------------------------------------------------------------------------------------

        character (len=320) :: dictionaryfile ! The filename of the main dictionary.
        integer :: numberoflines ! Largest possible number of entries. (Disregarding deletions and metadata.)

        !---------------------------------------------------------------------------------------------------------------------------
        ! Subroutines
        !---------------------------------------------------------------------------------------------------------------------------

        contains

        subroutine finder(collection,word,res,errlog)
                ! Returns an array of all entries matching the search word.
                implicit none
                character(len=maxCLen), dimension(maxDSize), intent(in) :: collection
                character(len=maxCLen), intent(in) :: word
                character(len=maxCLen), dimension(maxDSize), intent(out) :: res
                character(len=maxCLen), intent(out) :: errlog
                
                integer :: l
                integer :: indexofword
                logical :: entrystat

                entrystat = .false.
                errlog = ""

                do l=1,numberoflines
        
                        indexofword = index(collection(l), trim(word))
                        if (indexofword > 0) then
                                res(l) = collection(l)
                               entrystat = .true.
                        else
                                res(l) = ""
                        end if
                end do

                if (entrystat .eqv. .false.) then
                        errlog = "No entries matched search criteria."
                end if

        end subroutine
        subroutine saver(dictsteno,dictentry,filenum)
                ! Writes the steno and translations to the provided file number.
                character(len=maxCLen), dimension(maxDSize), intent(in) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(in) :: dictentry
                integer, intent(in) :: filenum

                integer :: l
                integer :: indexofsteno
                integer :: indexoftrans
                logical :: stenoexists
                logical :: transexists

                character(len=maxCLen) :: ds
                character(len=maxCLen) :: de
                write (filenum,'(a)') "{\rtf1\ansi{\*\cxrev100}\cxdict{\*\cxsystem WRTFE}{\stylesheet{\s0 Normal;}}"
                ! TODO ^ We're clearing any metadata that might have been in the dictionary here.
                ! This is probably not advised practice, but supporting metadata isn't a high priority right now.

                do l=1,numberoflines
                        indexofsteno = len(trim(dictsteno(l)))
                        indexoftrans = len(trim(dictentry(l)))
                        stenoexists = (indexofsteno < maxCLen .and. indexofsteno > 0)
                        transexists = (indexoftrans < maxCLen .and. indexoftrans > 0)
                        ds = dictsteno(l)
                        de = dictentry(l)
                        if (stenoexists .and. transexists) then
                                write (filenum,'(a)') '{\*\cxs '//trim(ds)//'}'//trim(de)
                        end if
                end do

                write (filenum,'(a)') "}"
        end subroutine saver
        character(len=maxCLen) function steno_validation(steno)
                ! Validation of characters based on: STKPWHRAO*EUFRPBLGTSDZ #-/ 1234567890

                character(len=maxCLen),intent(in) :: steno

                character(len=20) :: swp
                
                integer :: k

                steno_validation = ""

                ! RTF specification -> 7-bit ASCII.
                ! Extended ASCII -> 8-bit.
                ! I'll just use EASCII to be safe. 256 characters is a terminal standard (CP437 and the like)
                ! TODO: does gfortran support unicode/utf-8?  This would be useful for international dictionaries.
                do k=1,255
                        if (index(steno,achar(k)) > 0) then
                                ! ASCII not allowed:
                                ! >90 (lowercase and misc symbols)
                                ! <48!32,35,42,45,47 (other symbols, but allow " ","#","*","-","/")
                                ! 58,59,60,61,62,63,64 (":",";","<","=",">","?","@")
                                ! 67,73,74,77,81,86,88,89 (C,I,J,M,N,Q,V,X,Y)
                                if (k > 90 &
                                        & .or. (k < 48 .and. (k /= 32 .and. k /= 35 .and. k /= 42 .and. k /= 45 .and. k /= 47))&
                                        & .or. k==58 .or. k==59 .or. k==60 .or. k==61 .or. k==62 .or. k==63 .or. k==64&
                                        & .or. k==67 .or. k==73 .or. k==74 .or. k==77 .or. k==81 .or. k==86 .or. k==88 .or. k==89&
                                        &) then
                                        write (swp,*) k
                                        steno_validation = trim(steno_validation)&
                                                &//"The character "//achar(k)//" ("//trim(adjustl(swp))//") at position: "
                                        swp = ""
                                        write (swp,*) index(steno,achar(k))
                                        steno_validation = trim(steno_validation)&
                                                &//trim(adjustl(swp))//" might not be valid in steno."//achar(10)
                                end if
                        end if
                        

                end do
                if (len(trim(steno_validation)) == 0) steno_validation = mech_validation(steno)
                return
        end function steno_validation
        character(len=maxCLen) function mech_validation(steno_)
                ! This function is a visual mess, and I should probably make a flowchart for it.
                character(len=maxCLen), intent(in) :: steno_
                character(len=maxCLen) :: steno
                character(len=maxCLen) :: swp
                character :: k_
                character :: m_

                character (len=maxCLen) :: a
                integer :: k

                steno = steno_


                ! Numbers are positionally the same as the letter they replace, so let's make them letters.
                do
                        a = steno
                        if (index(steno,'1') > 0) steno(index(steno,'1'):index(steno,'1')) = 'S'
                        if (index(steno,'2') > 0) steno(index(steno,'2'):index(steno,'2')) = 'T'
                        if (index(steno,'3') > 0) steno(index(steno,'3'):index(steno,'3')) = 'P'
                        if (index(steno,'4') > 0) steno(index(steno,'4'):index(steno,'4')) = 'H'
                        if (index(steno,'5') > 0) steno(index(steno,'5'):index(steno,'5')) = 'A'
                        if (index(steno,'6') > 0) steno(index(steno,'6'):index(steno,'6')) = 'F'
                        if (index(steno,'7') > 0) steno(index(steno,'7'):index(steno,'7')) = 'P'
                        if (index(steno,'8') > 0) steno(index(steno,'8'):index(steno,'8')) = 'L'
                        if (index(steno,'9') > 0) steno(index(steno,'9'):index(steno,'9')) = 'T'
                        if (index(steno,'0') > 0) steno(index(steno,'0'):index(steno,'0')) = 'O'
                        if (steno == a) exit
                end do

                swp = "Warning: That entry's steno might not be mechanically possible."
                mech_validation = ""
                !     STKPWHRAO*EUFRPBLGTSDZ
                !     *$ %  ^      ^%   $*
                ! [x] Cannot follow S, T:
                !     FBLG
                ! [x] Cannot follow K:
                !     ST FBLGDZ
                ! [x] Cannot follow P:
                !     STK F
                ! [x] Cannot follow W:
                !     STKP FBLGDZ
                ! [x] Cannot follow H:
                !     STKPW FBLGDZ
                ! [x] Cannot follow R:
                !     STKWH F
                ! [x] Cannot follow A, O, E, U, *:
                !     KWH -
                ! [x] Cannot follow F:
                !     KWH AO*EU -
                ! [x] Cannot follow B:
                !     KPWHRAO*EUF -
                ! [x] Cannot follow L:
                !     KPWHRAO*EUFB -
                ! [x] Cannot follow G:
                !     KPWHRAO*EUFBL -
                ! [x] Cannot follow D:
                !     STKPWHRAO*EUFBLG -
                ! [x] Cannot follow Z:
                !     Literally everything except EOL or /
                do k=1,(len(trim(steno)))
                        k_ = steno(k:k)
                        m_ = steno(k+1:k+1)
                        if (k_ == m_) then ! Letters should neve be back-to-back: always S-S, T-T, R-R, P-P.
                                mech_validation = swp
                                exit
                        end if

                        if (k_ == "K" .or. k_ == "P" .or. k_ == "W" .or. k_ == "H") then
                                if (m_ == "S" .or. m_ == "T" .or. m_ == "K" .or. m_ == "F") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "K" .or. k_ == "W" .or. k_ == "H") then
                                if (m_ == "B" .or. m_ == "L" .or. m_ == "G" .or. m_ == "D" .or. m_ == "Z") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "W" .or. k_ == "H" .or. k_ == "R") then
                                if ((k_ /= "R" .and. m_ == "P") .or. m_ == "W") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "R" .and. m_ == "H") then
                                mech_validation = swp
                                exit
                        end if

                        if (k_ == "R" .and. (m_ == "K" .or. m_ == "F")) then
                                mech_validation = swp
                                exit
                        end if

                        ! Second half (Vowels and such)

                        if (k_ == "A" .or. k_ == "O" .or. k_ == "E" .or. k_ == "U" .or. k_ == "*"&
                                &.or. k_ == "F" .or. k_ == "B" .or. k_ == "L" .or. k_ == "G" .or. k_ == "D") then
                                if (m_ == "-" .or. m_ == "K" .or. m_ == "W" .or. m_ == "H") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "F" .or. k_ == "B" .or. k_ == "L" .or. k_ == "G" .or. k_ == "D") then
                                if (m_ == "A" .or. m_ == "O" .or. m_ == "*" .or. m_ == "E" .or. m_ == "U") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if
                        
                        if (k_ == "S" .or. k_ == "T") then
                                if (m_ == "F" .or. m_ == "B" .or. m_ == "L" .or. m_ == "G") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "B" .or. k_ == "L" .or. k_ == "G" .or. k_ == "D") then
                                if (m_ == "P" .or. m_ == "R" .or. m_ == "F" .or. m_ == "B") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if
                        
                        if (k_ == "Z" .and. (m_ /= "/" .and. m_ /= achar(32))) then
                                mech_validation = swp
                                exit
                        end if
                        
                        if (k_ == "G" .or. k_ == "D") then
                                if (m_ == "L" .or. m_ == "G") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if

                        if (k_ == "D") then
                                if (m_ == "S" .or. m_ == "T") then
                                        mech_validation = swp
                                        exit
                                end if
                        end if
                        
                end do
                return
        end function mech_validation

end module universal
module terminal
        ! REPL IO & CLI access, subroutines should be terminal specific - TUI, GUI implementations in another module.
        use universal
        contains

        subroutine repl(dictsteno,dictentry,ploverfix)
                
                implicit none
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictentry
                logical, intent(inout) :: ploverfix

                character(len=maxCLen) :: command
                
                command = ""

                print *, "Welcome to the command REPL, type help for command list."
                print *, "All metadata will be lost/replaced."
                print *, "NO CHANGES WILL BE SAVED UNTIL YOU QUIT."
                

                do while (.true.)
                        command = ""
                        write(*,"(a)",advance='no') "wrtfe >> "
                        read (*,'(a)') command

                        if (index(command,"quit") == 1) then
                                exit
                        else if (index(command,"cancel") == 1) then
                                stop
                        else
                                call perform(dictsteno,dictentry,ploverfix,command)
                        end if
                end do
        end subroutine repl

        subroutine perform(dictsteno,dictentry,ploverfix,command)
                
                implicit none
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictentry
                logical, intent(inout) :: ploverfix

                character(len=maxCLen), intent(inout) :: command

                character(len=maxCLen) :: arguments
                character(len=maxCLen), dimension(maxDSize) :: swapspace

                integer :: dswap
                character(len=maxCLen) :: char_swp

                character(len=maxCLen) :: asteno
                character(len=maxCLen) :: atrans

                character(len=maxCLen) :: errlog
                integer :: iostaterr
                
                arguments = ""
                dswap = 0

                if (index(command,"finds") == 1) then
                        arguments = command(7:maxCLen)
                        !print *, command
                        call finder(dictsteno,arguments,swapspace,errlog)
                        if (numberoflines == 0) print*, "There are no entries in the dictionary."
                        if (len(trim(errlog)) > 0) print*, trim(errlog)
                        call printer(swapspace,dictentry)
                else if (index(command,"findt") == 1) then
                        arguments = command(7:maxCLen)
                        call finder(dictentry,arguments,swapspace,errlog)
                        if (numberoflines == 0) print*, "There are no entries in the dictionary."
                        if (len(trim(errlog)) > 0) print*, trim(errlog)
                        call printer(dictsteno,swapspace)
                else if (index(command,"del") == 1) then
                        arguments = command(5:maxCLen)
                        read(arguments,*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines) then
                                print *, "No valid entry number was provided."
                        else
                                print *, "Deleting entry ", dswap, ": ", trim(dictsteno(dswap)), " ", trim(dictentry(dswap))
                                dictsteno(dswap) = ""
                                dictentry(dswap) = ""
                        end if
                else if (index(command,"add") == 1) then
                        arguments = command(5:maxCLen)
                        asteno = arguments(1:index(arguments," ")-1)
                        atrans = arguments(index(arguments," ")+1:maxCLen)
                        numberoflines = numberoflines + 1
                        dictsteno(numberoflines) = asteno
                        dictentry(numberoflines) = atrans
                        char_swp = steno_validation(asteno)
                        
                        print *, "Added entry", numberoflines, ":", trim(dictsteno(numberoflines)), " ",&
                                &trim(dictentry(numberoflines))
                        print *, trim(char_swp) !Error log for validation.
                else if (index(command,"fixs") == 1) then
                        arguments = command(6:maxCLen)
                        asteno = arguments(index(arguments," ")+1:maxCLen)
                        read(arguments(1:index(arguments," ")-1),*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines) then
                                print *, "No valid entry number was provided."
                        else
                                print *, "Replacing Entry ", dswap, ": [", trim(dictsteno(dswap)), " ", trim(dictentry(dswap)),&
                                        &"] with [", trim(asteno), " ", trim(dictentry(dswap)), "]"
                                dictsteno(dswap) = asteno
                        end if
                else if (index(command,"fixt") == 1) then
                        arguments = command(6:maxCLen)
                        atrans = arguments(index(arguments," ")+1:maxCLen)
                        read(arguments(1:index(arguments," ")-1),*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines) then
                                print *, "No valid entry number was provided."
                        else
                                print *, "Replacing Entry ", dswap, ": [", trim(dictsteno(dswap)), " ", trim(dictentry(dswap)),&
                                        & "] with [", trim(dictsteno(dswap)), " ", trim(atrans), "]"
                                dictentry(dswap) = atrans
                        end if
                else if (index(command,"test") == 1) then
                        call find_duplicates(dictsteno,dictentry)
                else if (index(command,"to") == 1) then
                        dictionaryfile = trim(command(4:maxCLen))
                else if (index(command,"help") > 0) then
                        print *, "Command list:"
                        print *, "add [STROKES] [string] - Adds the entry (No spaces in steno)."
                        print *, "del [number] - Deletes the entry from both arrays."
                        print *, "finds [string] - Finds [string] in the steno array."
                        print *, "findt [string] - Finds [string] in the translation array."
                        print *, "fixs [number] [STROKES] - Replaces the given enrty's steno."
                        print *, "fixt [number] [string] - Replaces the given entry's translation."
                        print *, "test - Alerts you to any duplicate entries in the dictionary."
                        print *, "to [file path] - Changes the destination for exit/save."
                        print *, "plover - also saves a plover-specific (But RTF) dictionary (fixes \line)."
                        print *, "quit - Saves RTF/CRE file, then exits."
                        print *, "cancel = exits without saving."
                else if (index(command,"plover") > 0) then
                        ploverfix = .true.
                else
                        print *, "Command not found, try help."
                end if
        end subroutine perform

        subroutine printer(steno,trans)
                implicit none
                character(len=maxCLen), dimension(maxDSize), intent(in) :: steno
                character(len=maxCLen), dimension(maxDSize), intent(in) :: trans
                
                integer :: l
                integer :: indexofsteno
                integer :: indexoftrans
                logical :: stenoexists
                logical :: transexists
                do l=1,numberoflines
                        indexofsteno = len(trim(steno(l)))
                        indexoftrans = len(trim(trans(l)))
                        stenoexists = (indexofsteno < maxCLen .and. indexofsteno > 0)
                        transexists = (indexoftrans < maxCLen .and. indexoftrans > 0)
                        if (stenoexists .and. transexists) then
                                print *, l, trim(steno(l)), " -> ", trim(trans(l))
                        end if
                end do
        end subroutine

        subroutine singleprint(array)
                implicit none
                character(len=maxCLen), dimension(maxDSize), intent(in) :: array

                integer :: l

                do l=1,numberoflines
                        if (len(trim(array(l))) > 0 .and. len(trim(array(l))) < maxCLen) then
                                print *, l, trim(array(l))
                        end if
                end do
        end subroutine singleprint

        subroutine all_arguments(cmd,filename)
                character(len=maxCLen), intent(out) :: cmd
                character(len=maxCLen), intent(in) :: filename

                character(len=maxCLen) :: arguments
                character(len=maxCLen) :: two
                character(len=maxCLen) :: three
                character(len=maxCLen) :: four

                integer :: afterfile
                integer :: lenfile
                integer :: swapspace

                arguments = ""

                afterfile = 0
                lenfile = 0
                swapspace = 0

                call get_command(cmd)
                
                afterfile = index(cmd,trim(filename))
                lenfile = len(trim(filename))

                arguments = cmd(afterfile+lenfile+1:maxCLen)

                cmd = arguments
        end subroutine all_arguments
        subroutine find_duplicates(dictsteno, dicttrans)
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dicttrans

                ! Fill these with entires known to be duplicates, then crosscheck.
                character(len=maxCLen), dimension(maxDSize) :: nopesteno
                character(len=maxCLen), dimension(maxDSize) :: nopetrans
                
                integer :: dentry
                integer :: dtest

                integer :: nsteno
                integer :: ntrans

                character(len=maxCLen) :: csteno
                character(len=maxCLen) :: ctrans

                character(len=maxCLen) :: osteno
                character(len=maxCLen) :: otrans

                character(len=7) :: tempchar

                integer :: iostator
                integer :: dsteno
                integer :: dtrans

                !real :: start,finish
                !call cpu_time(start)

                nsteno = 0
                ntrans = 0

                dsteno = 0
                dtrans = 0

                osteno = ""
                otrans = ""

                

                do dentry=1,numberoflines-1
                        csteno = dictsteno(dentry)
                        ctrans = dicttrans(dentry)

                        ! I'm assuming that entries without strokes are invalid or incorrectly deleted.
                        if (len(trim(csteno)) > 0&
                                &.and. .not. indexarr(nopetrans,ctrans,dtrans)&
                                &.and. .not. indexarr(nopesteno,csteno,dsteno)) then

                                do dtest = dentry+1,numberoflines
                                        if (csteno == dictsteno(dtest) .and. ctrans ==&
                                                &dicttrans(dtest)) then

                                                print *, "Duplicate entry detected:"
                                                print *, dentry, " ", trim(csteno), " ", trim(ctrans)
                                                print *, dtest, " ", trim(dictsteno(dtest)), " ", trim(dicttrans(dtest))
                                                dictsteno(dtest) = ""
                                                dicttrans(dtest) = ""
                                                print *, "Second entry deleted."

                                        else if (csteno == dictsteno(dtest)) then
                                                write(tempchar, "(I0)") dtest

                                                osteno = trim(osteno)//trim(tempchar)//", "
                                                nsteno = nsteno + 1

                                        else if (ctrans == dicttrans(dtest)) then
                                                write(tempchar, "(I0)") dtest

                                                otrans = trim(otrans)//trim(tempchar)//", "
                                                ntrans = ntrans + 1

                                        end if
                                end do
                                if (nsteno > 0) then
                                        print *, "Entries ", trim(osteno), " and ", dentry
                                        print *, "Share steno ''", trim(csteno), "''"
                                        nsteno = 0
                                        dsteno = dsteno + 1
                                        nopesteno(dsteno) = csteno
                                        osteno = ""
                                end if
                                if (ntrans > 0) then
                                        print *, "Entries ", trim(otrans), " and ", dentry
                                        print *, "share translation ''", trim(ctrans), "''"
                                        ntrans = 0
                                        dtrans = dtrans + 1
                                        nopetrans(dtrans) = ctrans
                                        otrans = ""
                                end if
                        end if
                end do
                !call cpu_time(finish)
                !print '("Executed in ", f6.3, " seconds.")', finish-start
        end subroutine find_duplicates
        logical function indexarr(carray,cfind,length)
                character(len=maxCLen) :: cfind
                character(len=maxCLen), dimension(maxDSize) :: carray

                integer :: length
                indexarr = .false.

                do k=1,length
                        if (trim(carray(k)) == trim(cfind)) then
                                indexarr = .true.
                                exit
                        end if
                end do
                return
        end function indexarr
end module terminal

program waffleRTFEditor
        use universal
        use terminal
        implicit none

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition
        !---------------------------------------------------------------------------------------------------------------------------

        integer :: i, j, k ! loop var.

        character(len=maxCLen) :: arg
        logical :: dictpresent
        integer :: iostaterror
        
        character(len=maxCLen), dimension(maxDSize) :: dictionarycontent ! Fortran arrays start at 1.
        character(len=maxCLen), dimension(maxDSize) :: dictentry
        character(len=maxCLen), dimension(maxDSize) :: dictsteno

        logical :: isentry

        character (len=78) :: introduction ! 320 char list.
        character (len=maxCLen) :: headertext !Name of program

        character (len=1) :: esc ! escape character

        integer :: lengthofentry
        integer :: lengthofsteno

        logical :: ploverfix

        !---------------------------------------------------------------------------------------------------------------------------
        ! Initialization
        !---------------------------------------------------------------------------------------------------------------------------

        i = 0
        j = 0
        k = 0

        arg = ""
        dictpresent = .false.
        iostaterror = 0

        ! Does this fill the entire array with ""?
        dictionarycontent = ""
        dictentry = ""
        dictsteno = ""

        isentry = .false.

        introduction = ""
        headertext = "Waffle RTF/CRE Editor"

        esc = achar(27)

        lengthofentry = 0
        lengthofsteno = 0

        ploverfix = .false.

        !---------------------------------------------------------------------------------------------------------------------------
        ! Globals - from universal module
        !---------------------------------------------------------------------------------------------------------------------------

        dictionaryfile = ""
        numberoflines = 0

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Arguments and Validation
        !---------------------------------------------------------------------------------------------------------------------------

        ! Get command Arguments first.
        if (command_argument_count()==0) then
                print *, "No argument detected, try --help (-h, --h, -help)."
                stop
        end if

        do j = 1,1
                call get_command_argument(j, arg)

                select case(trim(arg))
                case ('-v', '--version', '-version', '--v')
                        print *, corev
                        stop
                case ('-h', '--help', '-help', '--h')
                        print *, "Usage: ./wrtfe filename.rtf [options]"
                        print *, "    Options not necessary, can be found by typing ''help'' in REPL."
                        print *, " -h Displays this help text."
                        print *, " -v Displays version number."
                        stop
                case default
                        dictionaryfile = trim(arg) ! Dictionary filename acquired, proceed.
                        if (index(dictionaryfile,".rtf") > 0) then
                                dictionaryfile = dictionaryfile(1:index(dictionaryfile,".rtf")+4)
                        else if (index(dictionaryfile,".RTF") > 0) then
                                dictionaryfile = dictionaryfile(1:index(dictionaryfile,".RTF")+4)
                        else
                                print *, trim(dictionaryfile)
                                print *, "[file extension] Your file does not appear to be an rtf/cre dictionary."
                                stop
                        end if
                end select

                inquire (file=dictionaryfile, exist=dictpresent)
                if (.not. dictpresent) then
                        print *, "File not found."
                        stop
                end if
                open(1, file=dictionaryfile, iostat = iostaterror)
                if (iostaterror /= 0) then
                        print *, "Fatal Error: ", iostaterror
                        stop
                end if
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Read dictionary and split into two arrays.
        !---------------------------------------------------------------------------------------------------------------------------

        do k=1,maxDSize
                ! Reads each line into an array, until an exception is thrown.
                ! I'd set it up to alert the user if it's not an EOF error, but I haven't been able to get a consistent value for
                ! EOF; IOSTAT 5001 is not the listed as EOF, but OPTION_CONFLICT.  Perhaps my implementation is wrong but it seems
                ! to work perfectly aside from the odd IOSTAT exit code.
                read(1,'(a)',iostat = iostaterror) dictionarycontent(k)
                if (iostaterror > 0) exit
        end do
        
        do k=1,maxDSize
                ! Splits dictionarycontent into two seperate arrays - but only if the values exist.
                isentry = (index(dictionarycontent(k),"{\*\cxs ") == 1)
                if (isentry .and. index(dictionarycontent(k),"}") > 6) then
                        lengthofentry = len(trim(dictionarycontent(k)))
                        lengthofsteno = index(dictionarycontent(k),"}")
                        dictsteno(k) = dictionarycontent(k)(9:lengthofsteno-1)
                        dictentry(k) = dictionarycontent(k)(lengthofsteno+1:lengthofentry)

                        ! We need to be aware of possible metadata and how this could affect the number of entries.
                        ! This is a better solution that the previous one, but is still restricted to only correcting "on open"
                        ! count issues.  REMEMBER that the repl ADDS 1 to numberoflines before adding an entry.
                        numberoflines = k
                end if
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Manipulation.
        !---------------------------------------------------------------------------------------------------------------------------
        
        call all_arguments(arg,dictionaryfile)
        ! Get all text after the filename.
        if (len(trim(arg)) > 0) then
                !print *, arg
                call perform(dictsteno,dictentry,ploverfix,arg)
        else
                ! REPL loop, exiting the subroutine means quit has been called.
                call repl(dictsteno,dictentry,ploverfix)
        end if

        ! Save the file:
        close (1)
        open(1, file=dictionaryfile, iostat = iostaterror, status="replace")

        call saver(dictsteno,dictentry,1)
        
        close (1)

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Other dictionary outputs
        !---------------------------------------------------------------------------------------------------------------------------
        ! Plover /line converter
        if (ploverfix) then
                open(2, file=trim(dictionaryfile)//".plover.rtf", iostat = iostaterror, status="replace")
                do k=1,numberoflines
                        if (index(dictentry(k),"\line") == 1) then
                                dictentry(k) = "{^"//achar(10)//"^}{-|}"
                        end if
                end do

                call saver(dictsteno,dictentry,2)

                close (2)
        end if

        stop
end program waffleRTFEditor
