! w-rtf-cre-editor v0.0.49
! (c) 2017 Brendyn Sonntag
! Licensed under Apache 2.0, see LICENSE file.
! Maximum dictionary size: 300000 entries, each with a max length of 320 chars.

module universal
        ! This module has the core subroutines, independent of terminal REPL interface.

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition - "Statics"
        !---------------------------------------------------------------------------------------------------------------------------

        character (len=38), parameter :: corev = "w-rtf-cre-editor v0.0.49 (2017-jul-02)"
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

        function finder(collection,word,errlog) RESULT(resultant)
                ! Normal operation returns an array of all entries containing the search word.
                ! `errlog` argument of "match" returns all entries with the search word as a stroke.  This is mainly to present
                ! entries that may exist based on a full stroke provided.
                implicit none

                character(len=maxCLen), dimension(maxDSize), intent(in) :: collection
                character(len=maxCLen), intent(in) :: word
                character(len=maxCLen), intent(out) :: errlog

                character(len=maxCLen), dimension(maxDSize) :: resultant
                
                integer :: l ! loop variable
                integer :: indexofword
                character :: endchar
                logical :: entryfound

                resultant = ""
                entryfound = .false.
                ! errlog = ""

                do l=1,numberoflines
        
                        indexofword = index(collection(l), trim(word))
                        if (indexofword > 0) then
                                resultant(l) = collection(l)
                                entryfound = .true.
                        end if
                end do

                if (entryfound .eqv. .false.) then
                        errlog = "No entries matched search criteria."
                else
                        errlog = ""
                end if
                return
        end function finder

        function match(dictsteno,dicttrans,stroke) RESULT(resultant)
                character(len=maxCLen), dimension(maxDSize), intent(in) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(in) :: dicttrans
                character(len=maxCLen), intent(in) :: stroke

                character(len=maxCLen) :: resultant
                character(len=maxCLen) :: swp ! Temporary storage of translation.
                integer :: numberof

                resultant = ""
                swp = ""
                numberof = 1

                do l=1,numberoflines
                        if (dictsteno(l) == stroke) then
                                ! just `stroke` so that we don't get any trailing characters.
                                swp = dicttrans(l)
                        else if (index(dictsteno(l),trim(stroke)//"/") == 1) then
                                ! `trim(stroke)` so that we get all chars after it.
                                numberof = numberof + 1
                        end if
                end do

                write(resultant,*) numberof-1
                resultant = trim(swp)//achar(10)//trim(resultant)//" possible successive entries"

                return
        end function match

        subroutine saver(dictsteno,dicttrans,filenumber,creheader)
                ! Writes the steno and translations to the provided file number.

                character(len=maxCLen), dimension(maxDSize), intent(in) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(in) :: dicttrans
                integer, intent(in) :: filenumber
                character(len=maxCLen*5), optional :: creheader

                integer :: l, m, n ! loop variables
                integer :: lengthofsteno
                integer :: lengthoftrans
                logical :: stenoexists
                logical :: transexists

                character(len=maxCLen) :: thissteno
                character(len=maxCLen) :: thistrans

                character(len=maxCLen) :: cresystem
                integer :: cresyscount

                if (present(creheader)) then
                        if (len(trim(creheader)) > 0) then
                                ! Time to isolate the system that created the RTF file, then replace it.
                                do m=(index(creheader,"{\*\cxsystem")+13),len(trim(creheader))
                                        cresystem = creheader(index(creheader,"{\*\cxsystem"):m)
                                        cresyscount = 1
                                        do n=13,len(trim(cresystem))
                                                if (cresystem(n:n) == "{") then
                                                        cresyscount = cresyscount + 1
                                                else if (cresystem(n:n) == "}") then
                                                        cresyscount = cresyscount - 1
                                                end if
                                        end do
                                        if (cresyscount == 0) then
                                                ! When we hit this we have the system tag, and should be able to replace the
                                                ! provider with our own.  If the system tag doesn't exist, this wont be hit.
                                                creheader = creheader(1:index(creheader,"{\*\cxsystem")+12)//"w-rtf-cre-editor"//&
                                                        &creheader(m:len(trim(creheader)))
                                                exit
                                        end if
                                end do
                                write (filenumber,'(a)') trim(creheader)
                        else
                                ! This is redundant of the below overwrite, but the program crashes if we test for length and it's
                                ! not defined.
                                write (filenumber,'(a)') "{\rtf1\ansi{\*\cxrev100}"//&
                                        &"\cxdict{\*\cxsystem w-rtf-cre-editor}{\stylesheet{\s0 Normal;}}"
                        end if
                else
                        write (filenumber,'(a)') "{\rtf1\ansi{\*\cxrev100}"//&
                                &"\cxdict{\*\cxsystem w-rtf-cre-editor}{\stylesheet{\s0 Normal;}}"
                end if

                do l=1,numberoflines
                        thissteno = dictsteno(l)
                        thistrans = dicttrans(l)
                        lengthofsteno = len(trim(thissteno))
                        lengthoftrans = len(trim(thistrans))
                        stenoexists = (lengthofsteno > 0)
                        transexists = (lengthoftrans > 0)
                        
                        if (stenoexists .and. transexists) then
                                write (filenumber,'(a)') '{\*\cxs '//trim(thissteno)//'}'//trim(thistrans)
                        end if
                end do

                write (filenumber,'(a)') "}"
        end subroutine saver
        
        character(len=maxCLen) function steno_validation(strokes)
                ! Verifies validity of the strokes by ensuring there are no "non-steno" characters.
                ! Validation of characters based on: STKPWHRAO*EUFRPBLGTSDZ #-/ 1234567890
                ! Returns error text.

                character(len=maxCLen),intent(in) :: strokes

                character(len=20) :: swp ! Holds k as character array.
                
                integer :: k ! loop var

                steno_validation = ""

                ! RTF specification -> 7-bit ASCII.
                ! Extended ASCII -> 8-bit.
                ! I'll just use EASCII to be safe. 256 characters is a terminal standard (CP437 and the like)
                ! TODO: does gfortran support unicode/utf-8?  This would be useful for international dictionaries.
                do k=1,256
                        if (index(strokes,achar(k)) > 0) then
                                ! ASCII not allowed:
                                ! >90 (lowercase and misc symbols)
                                ! <48!32,35,42,45,47 (other symbols, but allow " ","#","*","-","/")
                                ! 58,59,60,61,62,63,64 (":",";","<","=",">","?","@")
                                ! 67,73,74,77,81,86,88,89 (C,I,J,M,Q,V,X,Y)
                                ! 78 (N)
                                if (k > 90 &
                                        & .or. (k < 48 .and. (k /= 32 .and. k /= 35 .and. k /= 42 .and. k /= 45 .and. k /= 47))&
                                        & .or. k==58 .or. k==59 .or. k==60 .or. k==61 .or. k==62 .or. k==63 .or. k==64&
                                        & .or. k==67 .or. k==73 .or. k==74 .or. k==77 .or. k==81 .or. k==86 .or. k==88 .or. k==89&
                                        & .or. k==78&
                                        &) then
                                        write (swp,*) k
                                        steno_validation = trim(steno_validation)&
                                                &//"The character "//achar(k)//" ("//trim(adjustl(swp))//") at position: "
                                        swp = ""
                                        write (swp,*) index(strokes,achar(k))
                                        steno_validation = trim(steno_validation)&
                                                &//trim(adjustl(swp))//" might not be valid in steno."//achar(10)
                                end if
                        end if
                        

                end do
                if (len(trim(steno_validation)) == 0) steno_validation = mech_validation(strokes)
                return
        end function steno_validation
        
        character(len=maxCLen) function mech_validation(steno_)
                ! Verifies validity of the strokes by ensuring they're mechanically possible.
                ! This function is a visual mess, and I should probably make a flowchart for it.
                ! Returns error text.

                character(len=maxCLen), intent(in) :: steno_
                character(len=maxCLen) :: steno
                character(len=maxCLen) :: swp ! Error text holder.
                character :: k_ ! current char
                character :: m_ ! following char

                character (len=maxCLen) :: a ! loop char
                integer :: k ! loop var

                steno = steno_
                ! TODO steno should be strokes, but I'll let it slide this time.


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

                        ! Vowel order

                        if ((k_ == "O" .or. k_ == "*" .or. k_ == "E" .or. k_ == "U") .and. m_ == "A") then
                                mech_validation = swp
                                exit
                        end if

                        if ((k_ == "*" .or. k_ == "E" .or. k_ == "U") .and. m_ == "O") then
                                mech_validation = swp
                                exit
                        end if

                        if ((k_ == "E" .or. k_ == "U") .and. m_ == "*") then
                                mech_validation = swp
                                exit
                        end if

                        if (k_ == "U" .and. m_ == "E") then
                                mech_validation = swp
                                exit
                        end if

                        ! Second half

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
        use ansicolors

        !---------------------------------------------------------------------------------------------------------------------------
        ! Subroutines
        !---------------------------------------------------------------------------------------------------------------------------
        
        contains

        subroutine repl(dictsteno,dicttrans,ploverfix)
                implicit none

                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dicttrans
                logical, intent(inout) :: ploverfix

                character(len=maxCLen) :: command
                
                command = ""

                print *, "Welcome to the command REPL, type"//c_yellow//" help "//c_reset//"for command list."
                print *, "All metadata will be lost/replaced."
                print *, "NO CHANGES WILL BE SAVED UNTIL YOU QUIT."
                

                do while (.true.)
                        command = ""
                        write(*,"(a)",advance='no') "wrtfe >> "
                        read (*,'(a)') command

                        if (index(command,"save") == 1) then
                                exit
                        else if (index(command,"cancel") == 1) then
                                stop
                        else if (index(command,"quit") == 1) then
                                print *, "Would you like to save before exiting? (Y/n) + enter"
                                ! Fortran doesn't have a "get single char" function, so we'll make do with the general read().
                                read (*,'(a)') command
                                if (index(command,"Y") == 1) then
                                        exit
                                else if (index(command,"n") == 1) then
                                        stop
                                else
                                        print *, "Response did not meet criteria."
                                end if
                        else
                                call perform(dictsteno,dicttrans,ploverfix,command)
                        end if
                end do
        end subroutine repl

        subroutine perform(dictsteno,dicttrans,ploverfix,command)
                implicit none

                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dictsteno
                character(len=maxCLen), dimension(maxDSize), intent(inout) :: dicttrans
                logical, intent(inout) :: ploverfix

                character(len=maxCLen), intent(inout) :: command

                character(len=maxCLen) :: arguments
                character(len=maxCLen), dimension(maxDSize) :: swapspace ! multiple use variable: used in `finds`, `findt`.

                integer :: dswap ! multiple use variable: used in `del`, `fixs`, `fixt`.

                character(len=maxCLen) :: asteno ! added strokes
                character(len=maxCLen) :: atrans ! added translation

                character(len=maxCLen) :: errlog ! multiple use error text dump.
                integer :: iostaterr ! read commands require a variable to dump into for error handling.
                
                arguments = ""
                dswap = 0

                if (index(command,"finds") == 1) then
                        arguments = command(7:maxCLen)
                        !print *, command
                        if (arguments(1:1) == "'") arguments = arguments(2:maxCLen)
                        swapspace = finder(dictsteno,arguments,errlog)
                        if (numberoflines == 0) print*, "There are no entries in the dictionary."
                        if (len(trim(errlog)) > 12) print*, trim(errlog)
                        call printer(swapspace,dicttrans)
                else if (index(command,"findt") == 1) then
                        arguments = command(7:maxCLen)
                        swapspace = finder(dicttrans,arguments,errlog)
                        if (numberoflines == 0) print*, "There are no entries in the dictionary."
                        if (len(trim(errlog)) > 12) print*, trim(errlog)
                        call printer(dictsteno,swapspace)
                else if (index(command,"del") == 1) then
                        arguments = command(5:maxCLen)
                        read(arguments,*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines .or. dswap == 0) then
                                print *, c_red//"No valid entry number was provided."//c_reset
                        else
                                print *, c_yellow//"Deleted entry:"
                                print '(a,I12,a,a,a,a)', &
                                        &c_red, dswap, " "//c_cyan, trim(dictsteno(dswap)), &
                                        & c_red//" -> "//c_reset, trim(dicttrans(dswap))
                                dictsteno(dswap) = ""
                                dicttrans(dswap) = ""
                        end if
                        if (dswap == numberoflines) then
                                numberoflines = numberoflines - 1
                        end if
                else if (index(command,"add") == 1) then
                        arguments = command(5:maxCLen)
                        asteno = arguments(1:index(arguments," ")-1)
                        atrans = arguments(index(arguments," ")+1:maxCLen)
                        if (atrans /= "") then
                                numberoflines = numberoflines + 1
                                dictsteno(numberoflines) = asteno
                                dicttrans(numberoflines) = atrans
                                errlog = steno_validation(asteno)
                                print *, "Added entry:"
                                print '(a,I12,a,a,a,a)', &
                                        &c_green, numberoflines, " "//c_cyan, trim(dictsteno(numberoflines)), &
                                        & c_green//" -> "//c_reset, trim(dicttrans(numberoflines))
                        else
                                errlog = c_red//"Cancelled add: translation not provided."//c_reset
                        end if
                        
                        print *, c_yellow//trim(errlog)//c_reset !Error log for validation.
                else if (index(command,"fixs") == 1) then
                        arguments = command(6:maxCLen)
                        if (arguments(1:1) == "'") arguments = arguments(2:maxCLen)
                        asteno = arguments(index(arguments," ")+1:maxCLen)
                        read(arguments(1:index(arguments," ")-1),*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines .or. len(trim(arguments)) == 0) then
                                print *, c_red//"No valid entry number was provided."//c_reset
                        else
                                print *, c_yellow//"Replaced entry:"//c_reset
                                print '(a,I12,a,a,a,a)', &
                                        &c_red, dswap, " "//c_cyan, trim(dictsteno(dswap)), &
                                        & c_red//" -> "//c_reset, trim(dicttrans(dswap))
                                print '(a,I12,a,a,a,a)', &
                                        &c_green, dswap, " "//c_cyan, trim(asteno), &
                                        & c_green//" -> "//c_reset, trim(dicttrans(dswap))
                                !print *, "Replacing Entry ", dswap, ": [", trim(dictsteno(dswap)), " ", trim(dicttrans(dswap)),&
                                !        &"] with [", trim(asteno), " ", trim(dicttrans(dswap)), "]"
                                dictsteno(dswap) = asteno
                        end if
                else if (index(command,"fixt") == 1) then
                        arguments = command(6:maxCLen)
                        atrans = arguments(index(arguments," ")+1:maxCLen)
                        read(arguments(1:index(arguments," ")-1),*,iostat=iostaterr) dswap
                        if (iostaterr > 0 .or. dswap > numberoflines .or. len(trim(arguments)) == 0) then
                                print *, c_red//"No valid entry number was provided."//c_reset
                        else
                                print *, c_yellow//"Replaced entry:"//c_reset
                                print '(a,I12,a,a,a,a)', &
                                        &c_red, dswap, " "//c_cyan, trim(dictsteno(dswap)), &
                                        & c_red//" -> "//c_reset, trim(dicttrans(dswap))
                                print '(a,I12,a,a,a,a)', &
                                        &c_green, dswap, " "//c_cyan, trim(dictsteno(dswap)), &
                                        & c_green//" -> "//c_reset, trim(atrans)
                                !print *, "Replacing Entry ", dswap, ": [", trim(dictsteno(dswap)), " ", trim(dicttrans(dswap)),&
                                !        & "] with [", trim(dictsteno(dswap)), " ", trim(atrans), "]"
                                dicttrans(dswap) = atrans
                        end if
                else if (index(command,"match") == 1) then
                        arguments = command(7:maxCLen)
                        if (arguments(1:1) == "'") arguments = arguments(2:maxCLen)
                        print *, trim(match(dictsteno,dicttrans,arguments))
                else if (index(command,"test") == 1) then
                        call find_duplicates(dictsteno,dicttrans)
                else if (index(command,"count") == 1) then
                        print *, numberoflines
                else if (index(command,"to") == 1) then
                        dictionaryfile = trim(command(4:maxCLen))
                else if (index(command,"colors") == 1) then
                        if (len(c_reset) == 0) then
                                call usecolors(.true.)
                        else
                                call usecolors(.false.)
                        end if
                else if (index(command,"help") > 0) then
                        print *, "Command list:"
                        print *, c_yellow//"add"//c_cyan//" [STROKES] [string] "&
                                &//c_reset//"- Adds the entry (No spaces in steno)."
                        print *, c_yellow//"del"//c_cyan//" [number] "&
                                &//c_reset//"- Deletes the entry from both arrays."
                        print *, c_yellow//"finds"//c_cyan//" [string] "&
                                &//c_reset//"- Finds [string] in the steno array."
                        print *, c_yellow//"findt"//c_cyan//" [string] "&
                                &//c_reset//"- Finds [string] in the translation array."
                        print *, c_yellow//"fixs"//c_cyan//" [number] [STROKES] "&
                                &//c_reset//"- Replaces the given enrty's steno."
                        print *, c_yellow//"fixt"//c_cyan//" [number] [string] "&
                                &//c_reset//"- Replaces the given entry's translation."
                        print *, c_yellow//"match"//c_cyan//" [STROKES] "&
                                &//c_reset//"- Returns translation of direct match, and number of further translations."
                        print *, c_yellow//"test "&
                                &//c_reset//"- Alerts you to any duplicate entries in the dictionary."
                        print *, c_yellow//"count "&
                                &//c_reset//"- Prints the number of entries in the dictionary."
                        print *, c_yellow//"colors "&
                                &//c_reset//"- Toggles colors on/off in REPL interface."
                        print *, c_yellow//"to"//c_cyan//" [file path] "&
                                &//c_reset//"- Changes the destination for exit/save."
                        print *, c_yellow//"plover "&
                                &//c_reset//"- also saves a plover-specific (But RTF) dictionary (fixes \line)."
                        print *, c_yellow//"save "&
                                &//c_reset//"- Saves RTF/CRE file, then exits."
                        print *, c_yellow//"quit "&
                                &//c_reset//"- prompt for save or cancel"
                        print *, c_yellow//"cancel "&
                                &//c_reset//"- exits without saving."
                else if (index(command,"plover") > 0) then
                        ploverfix = .true.
                else
                        print *, "Command not found, try help."
                end if
        end subroutine perform

        subroutine printer(steno,trans)
                ! Prints out the contents of the two arrays side-by-side prefixed by row number.
                implicit none
                
                character(len=maxCLen), dimension(maxDSize), intent(in) :: steno
                character(len=maxCLen), dimension(maxDSize), intent(in) :: trans
                
                integer :: l
                integer :: lengthofsteno
                integer :: lengthoftrans
                logical :: stenoexists
                logical :: transexists

                do l=1,numberoflines
                        lengthofsteno = len(trim(steno(l)))
                        lengthoftrans = len(trim(trans(l)))
                        stenoexists = (lengthofsteno > 0)
                        transexists = (lengthoftrans > 0)
                        if (stenoexists .and. transexists) then
                                print '(a,I12,a,a,a,a)', &
                                        &c_yellow, l, c_cyan//" ", trim(steno(l)), c_yellow//" -> "//c_reset, trim(trans(l))
                        end if
                end do
        end subroutine

        subroutine all_arguments(cmd,filename)
                ! Removes the filename from `cmd`.

                character(len=maxCLen), intent(out) :: cmd
                character(len=maxCLen), intent(in) :: filename

                character(len=maxCLen) :: arguments

                integer :: afterfile
                integer :: lenfile

                arguments = ""

                afterfile = 0
                lenfile = 0

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
                
                integer :: dentry ! outer loop var
                integer :: dtest ! inner loop var

                integer :: nsteno ! number of duplicate strokes of core
                integer :: ntrans ! number of duplicate translations of core

                character(len=maxCLen) :: csteno ! core strokes
                character(len=maxCLen) :: ctrans ! core translation

                character(len=maxCLen) :: osteno ! list of duplicate strokes of core
                character(len=maxCLen) :: otrans ! list of duplicate translations of core

                character(len=7) :: tempchar

                integer :: iostator
                integer :: dsteno ! number of strokes already deemed duplicates.
                integer :: dtrans ! number of translations already deemed duplicates.

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

                        ! If strokes and translation exist, it's an entry.  Then make sure it's not a duplicate.
                        if (len(trim(csteno)) > 0 .and. len(trim(ctrans)) > 0) then

                                do dtest = dentry+1,numberoflines
                                        if (csteno == dictsteno(dtest) .and. ctrans ==&
                                                &dicttrans(dtest)) then

                                                print *, "Duplicate entry detected:"
                                                print '(a,I12,a,a,a,a)', &
                                                        &c_yellow, dentry, " "//c_cyan, trim(csteno), &
                                                        & c_yellow//" -> "//c_reset, trim(ctrans)
                                                print '(a,I12,a,a,a,a)', &
                                                        &c_red, dtest, " "//c_cyan, trim(dictsteno(dtest)), &
                                                        & c_red//" -> "//c_reset, trim(dicttrans(dtest))
                                                !print *, dentry, " ", trim(csteno), " ", trim(ctrans)
                                                !print *, dtest, " ", trim(dictsteno(dtest)), " ", trim(dicttrans(dtest))
                                                dictsteno(dtest) = ""
                                                dicttrans(dtest) = ""
                                                print *, c_yellow//"Second entry deleted."//c_reset

                                        else if (csteno == dictsteno(dtest)&
                                &.and. .not. indexarr(nopetrans,ctrans,dtrans)&
                                &.and. .not. indexarr(nopesteno,csteno,dsteno)) then
                                                write(tempchar, "(I0)") dtest

                                                osteno = trim(osteno)//trim(tempchar)//", "
                                                nsteno = nsteno + 1

                                        else if (ctrans == dicttrans(dtest)&
                                &.and. .not. indexarr(nopetrans,ctrans,dtrans)&
                                &.and. .not. indexarr(nopesteno,csteno,dsteno)) then
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

        subroutine printform()
        end subroutine printform

end module terminal

program waffleRTFEditor
        use universal
        use terminal
        use ansicolors
        implicit none

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition
        !---------------------------------------------------------------------------------------------------------------------------

        integer :: i, j, k, l ! loop var.

        character(len=maxCLen) :: arg
        logical :: dictpresent
        integer :: iostaterror
        
        character(len=maxCLen), dimension(maxDSize) :: dictionarycontent ! Fortran arrays start at 1.
        character(len=maxCLen), dimension(maxDSize) :: dictentry
        character(len=maxCLen), dimension(maxDSize) :: dictsteno

        logical :: isentry

        integer :: lengthofsteno

        logical :: ploverfix ! used as flag for whether or not to create a plover-specific copy.

        character(len=maxCLen*5) :: creheader ! Following the CRE spec, the header metadata.
        integer :: creheaderrows ! The number of rows taken up by header metadata.

        !---------------------------------------------------------------------------------------------------------------------------
        ! Initialization
        !---------------------------------------------------------------------------------------------------------------------------

        i = 0
        j = 0
        k = 0
        l = 0

        arg = ""
        dictpresent = .false.
        iostaterror = 0

        ! Array initialization takes a VERY long time...
        ! We'll just assume the indices that matter get initialized during execution.
        ! dictionarycontent = ""
        ! dictentry = ""
        ! dictsteno = ""

        isentry = .false.

        lengthofsteno = 0

        ploverfix = .false.

        creheader = ""
        creheaderrows = 0

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
                        ! Temporarily use dictionaryfile as the extension validator.
                        dictionaryfile = arg(len(trim(arg))-3:len(trim(arg)))
                        if ((dictionaryfile /= ".rtf" .and. dictionaryfile /= ".RTF") .or. len(trim(arg)) < 5) then
                                print *, "The provided file: "//trim(arg)
                                print *, "Your file does not appear to be an rtf/cre dictionary."
                                stop
                        end if 
                        ! It's good, let's proceed.
                        dictionaryfile = arg
                end select

                inquire (file=dictionaryfile, exist=dictpresent)
                if (.not. dictpresent) then
                        print *, "The provided file does not exist."
                        stop
                end if
                open(1, file=dictionaryfile, iostat = iostaterror)
                if (iostaterror /= 0) then
                        print *, "Error opening provided file: IOSTAT error ", iostaterror, "."
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

                ! Splits dictionarycontent into two seperate arrays - but only if the values exist.
                isentry = (index(dictionarycontent(k),"{\*\cxs ") == 1)
                if (isentry .and. index(dictionarycontent(k),"}") > 9) then
                        lengthofsteno = index(dictionarycontent(k),"}")
                        dictsteno(k-creheaderrows) = dictionarycontent(k)(9:lengthofsteno-1)
                        dictentry(k-creheaderrows) = dictionarycontent(k)(lengthofsteno+1:maxCLen)

                        ! We need to be aware of possible metadata and how this could affect the number of entries.
                        ! This is a better solution that the previous one, but is still restricted to only correcting "on open"
                        ! count issues.  REMEMBER that the repl ADDS 1 to numberoflines before adding an entry.
                        numberoflines = k-creheaderrows
                else if (numberoflines == 0) then
                        creheader = trim(creheader)//trim(dictionarycontent(k))//achar(10)
                        creheaderrows = creheaderrows + 1
                end if
        end do
        ! This removes the trailing achar(10).
        creheader = creheader(1:len(trim(creheader))-1)

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Manipulation.
        !---------------------------------------------------------------------------------------------------------------------------
        
        call all_arguments(arg,dictionaryfile)
        ! Get all text after the filename.
        if (len(trim(arg)) > 0) then
                ! print *, arg
                call usecolors(.false.)
                call perform(dictsteno,dictentry,ploverfix,arg)
        else
                ! REPL loop, exiting the subroutine means quit has been called.
                call platformcolors()
                call repl(dictsteno,dictentry,ploverfix)
        end if

        ! Save the file:
        close (1)
        open(1, file=dictionaryfile, iostat = iostaterror, status="replace")

        call saver(dictsteno,dictentry,1,creheader)
        
        close (1)

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Other dictionary outputs
        !---------------------------------------------------------------------------------------------------------------------------
        ! Plover /line converter
        if (ploverfix) then
                open(2, file=dictionaryfile(1:len(trim(dictionaryfile))-4)//".plover.rtf", iostat = iostaterror, status="replace")
                do k=1,numberoflines
                        if (index(dictentry(k),"\line") == 1) then
                                ! Interesting note here:
                                ! Plover glitches up unless this CR is 10.  Normally, GFORTRAN detects the operating system and
                                ! outputs the appropriate CRs - windows uses 13&10.  Since this is a manual CR it does not agree
                                ! with the automatic CRs and this can cause some weird issues when opened in some windows text
                                ! editors.
                                ! TODO Instead of outputing non-standard RTF, let's try outputing a JSON dictionary for plover
                                ! instead.
                                dictentry(k) = "{^"//achar(10)//"^}{-|}"
                        end if
                end do

                call saver(dictsteno,dictentry,2)

                close (2)
        end if

        stop
end program waffleRTFEditor
