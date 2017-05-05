! Maximum dictionary size: 300000 entries, each with a max length of 320 chars.
module universal
        contains

        subroutine finder(collection,word,res)
                implicit none
                character(len=320), dimension(300000), intent(in) :: collection
                character(len=320), intent(in) :: word
                character(len=320), dimension(300000), intent(out) :: res
                
                integer :: l
                integer :: indexofword
                do l=1,300000
        
                        indexofword = index(collection(l), trim(word))
                        if (indexofword > 0) then
                                res(l) = collection(l)
                        else
                                res(l) = ""
                        end if
                end do
        end subroutine
        subroutine saver(dictsteno,dictentry)
                character(len=320), dimension(300000), intent(in) :: dictsteno
                character(len=320), dimension(300000), intent(in) :: dictentry

                integer :: l
                integer :: indexofsteno
                integer :: indexoftrans
                logical :: stenoexists
                logical :: transexists

                character(len=320) :: ds
                character(len=320) :: de
                write (1,'(a)') "{\rtf1\ansi{\*\cxrev100}\cxdict{\*\cxsystem WRTFE}{\stylesheet{\s0 Normal;}}"

                do l=1,300000
                        indexofsteno = len(trim(dictsteno(l)))
                        indexoftrans = len(trim(dictentry(l)))
                        stenoexists = (indexofsteno < 320 .and. indexofsteno > 0)
                        transexists = (indexoftrans < 320 .and. indexoftrans > 0)
                        ds = dictsteno(l)
                        de = dictentry(l)
                        if (stenoexists .and. transexists) then
                                write (1,'(a)') '{\*\cxs '//trim(ds)//'}'//trim(de)
                        end if
                end do

                write (1,'(a)') "}"
        end subroutine saver
end module universal
module terminal
        ! terminal-based REPL IO
        use universal
        contains

        subroutine repl(dictsteno,dictentry,numberoflines,filename)
                
                implicit none
                character(len=320), dimension(300000), intent(inout) :: dictsteno
                character(len=320), dimension(300000), intent(inout) :: dictentry
                integer, intent(inout) :: numberoflines
                character(len=32), intent(in) :: filename

                character(len=320) :: command
                character(len=320) :: arguments
                character(len=320), dimension(300000) :: swapspace

                integer :: dswap

                character(len=320) :: asteno
                character(len=320) :: atrans
                
                command = ""
                arguments = ""
                dswap = 0

                print *, "Welcome to the command REPL, type help for command list."
                print *, "All metadata will be lost/replaced."
                print *, "NO CHANGES WILL BE SAVED UNTIL YOU QUIT."

                do while (.true.)
                        command = ""
                        arguments = ""
                        read (*,'(a)') command
                        if (index(command,"finds") == 1) then
                                arguments = command(7:320)
                                !print *, command
                                call finder(dictsteno,arguments,swapspace)
                                call printer(swapspace,dictentry)
                        else if (index(command,"findt") == 1) then
                                arguments = command(7:320)
                                call finder(dictentry,arguments,swapspace)
                                call printer(dictsteno,swapspace)
                        else if (index(command,"del") == 1) then
                                arguments = command(5:320)
                                read(arguments,*) dswap
                                print *, "Deleting entry ", dswap, ": ", trim(dictsteno(dswap)), " ", trim(dictentry(dswap))
                                dictsteno(dswap) = ""
                                dictentry(dswap) = ""
                        else if (index(command,"add") == 1) then
                                arguments = command(5:320)
                                asteno = arguments(1:index(arguments," ")-1)
                                atrans = arguments(index(arguments," ")+1:320)
                                numberoflines = numberoflines + 1
                                dictsteno(numberoflines) = asteno
                                dictentry(numberoflines) = atrans
                                print *, "Added entry", numberoflines, ":", trim(dictsteno(numberoflines)), " ",&
                                &trim(dictentry(numberoflines))
                        else if (index(command,"quit") > 0) then
                                exit
                        else if (index(command,"help") > 0) then
                                print *, "Command list:"
                                print *, "finds [string] - Finds [string] in the steno array."
                                print *, "findt [string] - Finds [string] in the translation array."
                                print *, "del [NUMBER] - Deletes the entry from both arrays."
                                print *, "add [STENO] [String] - Adds the entry (No spaces in steno)."
                                print *, "quit - Saves RTF/CRE file, then exits."
                        else
                                print *, "Command not found, try help."
                        end if
                end do
        end subroutine repl

        subroutine printer(steno,trans)
                implicit none
                character(len=320), dimension(300000), intent(in) :: steno
                character(len=320), dimension(300000), intent(in) :: trans
                
                integer :: l
                integer :: indexofsteno
                integer :: indexoftrans
                logical :: stenoexists
                logical :: transexists
                do l=1,300000
                        indexofsteno = len(trim(steno(l)))
                        indexoftrans = len(trim(trans(l)))
                        stenoexists = (indexofsteno < 320 .and. indexofsteno > 0)
                        transexists = (indexoftrans < 320 .and. indexoftrans > 0)
                        if (stenoexists .and. transexists) then
                                print *, l, trim(steno(l)), " -> ", trim(trans(l))
                        end if
                end do
        end subroutine

        subroutine singleprint(array)
                implicit none
                character(len=320), dimension(300000), intent(in) :: array

                integer :: l

                do l=1,300000
                        if (len(trim(array(l))) > 0 .and. len(trim(array(l))) < 320) then
                                print *, l, trim(array(l))
                        end if
                end do
        end subroutine singleprint

end module terminal

program waffleRTFEditor
        use universal
        use terminal
        implicit none

        !---------------------------------------------------------------------------------------------------------------------------
        ! Definition
        !---------------------------------------------------------------------------------------------------------------------------

        integer :: i, j, k ! loop var.

        character (len=32) :: dictionaryfile ! The filename of the dictionary.
        character(len=32) :: arg
        logical :: dictpresent
        integer :: iostaterror
        
        character(len=320), dimension(300000) :: dictionarycontent ! Fortran arrays start at 1.
        character(len=320), dimension(300000) :: dictentry
        character(len=320), dimension(300000) :: dictsteno

        logical :: isentry

        integer :: numberoflines

        character (len=78) :: introduction ! 320 char list.
        character (len=320) :: headertext !Name of program

        character (len=1) :: esc ! escape character
        character(len=320) :: tuiTopLeft

        integer :: lengthofentry
        integer :: lengthofsteno

        !---------------------------------------------------------------------------------------------------------------------------
        ! Initialization
        !---------------------------------------------------------------------------------------------------------------------------

        i = 0
        j = 0
        k = 0

        dictionaryfile = ""
        arg = ""
        dictpresent = .false.
        iostaterror = 0

        ! dictionarycontent cannot be initialized effectively.
        ! dictentry
        ! dictsteno

        isentry = .false.

        numberoflines = 0

        introduction = ""
        headertext = "Waffle RTF/CRE Editor"

        esc = achar(27)
        tuiTopLeft = esc//"[320A"//esc//"[320D"

        lengthofentry = 0
        lengthofsteno = 0

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Arguments and Validation
        !---------------------------------------------------------------------------------------------------------------------------

        ! Get command Arguments first.
        if (command_argument_count()==0) then
                print *, "No argument detected, try --help (-h, --h, -help)."
                stop
        end if

        do j = 1, command_argument_count()
                call get_command_argument(j, arg)

                select case(trim(arg))
                case ('-v', '--version', '-version', '--v')
                        print *, "Version 1.0"
                        stop
                case ('-h', '--help', '-help', '--h')
                        print *, "Usage: ./TUI [filename.rtf]"
                        stop
                case default
                        dictionaryfile = trim(arg) ! Dictionary filename acquired, proceed.
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

        do k=1,300000
                read(1,'(a)',iostat = iostaterror) dictionarycontent(k)
                ! print *, trim(dictionarycontent(k)), " - SUBSTR ", index(dictionarycontent(k),"}")
                ! We need to be aware that not all entries in the array are usefull, and some have garbage in them.
                if (iostaterror > 0) then
                        numberoflines = k
                        exit
                end if
        end do
        
        ! 2 to avoid line one, which has random RTF metadata.
        do k=1,300000
                isentry = (index(dictionarycontent(k),"{\*\cxs ") == 1)
                if (isentry .and. index(dictionarycontent(k),"}") > 6) then
                        lengthofentry = len(trim(dictionarycontent(k)))
                        lengthofsteno = index(dictionarycontent(k),"}")
                        dictsteno(k) = dictionarycontent(k)(9:lengthofsteno-1)
                        dictentry(k) = dictionarycontent(k)(lengthofsteno+1:lengthofentry)
                end if
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Manipulation.
        !---------------------------------------------------------------------------------------------------------------------------
        
        call repl(dictsteno,dictentry,numberoflines, dictionaryfile)

        ! Time to save the file:
        ! call printer(dictionarycontent,dictionarycontent)
        ! call singleprint(dictionarycontent)
        close (1)
        open(1, file=dictionaryfile, iostat = iostaterror, status="replace")
        call saver(dictsteno,dictentry)
        close (1)
        stop
        
        !---------------------------------------------------------------------------------------------------------------------------
        ! MISC TUI stuff - Ignored.
        !---------------------------------------------------------------------------------------------------------------------------

        !print *, esc//"[2J", tuiTopLeft ! Blank screen, move cur to top, move cur to left
       ! print *, esc//"[1A", esc//"[30;42m", esc//"[2D", headertext ,esc//"[0m" ! Move cur up one, Set to black on green, move cur 2
        ! to left, ..., reset styles.
        !print *, ""
!        do while (i < 320) 
!                eighty = trim(eighty) // "h"
!                i = i + 1
!        end do
        !print *, introduction
end program waffleRTFEditor
