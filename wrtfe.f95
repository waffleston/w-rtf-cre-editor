module universal
        contains

        subroutine finder(collection,word,res)
                implicit none
                character(len=80), dimension(1000), intent(in) :: collection
                character(len=80), intent(in) :: word
                character(len=80), dimension(1000), intent(out) :: res
                
                integer :: l
                integer :: indexofword
                do l=1,1000
        
                        indexofword = index(collection(l), trim(word))
                        if (indexofword > 0) then
                                res(l) = collection(l)
                        else
                                res(l) = ""
                        end if
                end do
        end subroutine
end module universal
module terminal
        ! terminal-based REPL IO
        use universal
        contains

        subroutine repl(dictsteno,dictentry)
                
                implicit none
                character(len=80), dimension(1000), intent(inout) :: dictsteno
                character(len=80), dimension(1000), intent(inout) :: dictentry

                character(len=80) :: command
                character(len=80) :: arguments
                character(len=80), dimension(1000) :: swapspace
                
                command = ""
                arguments = ""

                print *, "Welcome to the command REPL, type help for command list."
                print *, "NO CHANGES WILL BE SAVED UNTIL YOU QUIT."

                do while (.true.)
                        command = ""
                        arguments = ""
                        read (*,'(a)') command
                        if (index(command,"finds") > 0 .and. index(command,"finds") < 2) then
                                arguments = command(7:80)
                                !print *, command
                                call finder(dictsteno,arguments,swapspace)
                                call printer(swapspace,dictentry)
                        else if (index(command,"findt") > 0 .and. index(command,"findt") < 2) then
                                arguments = command(7:80)
                                call finder(dictentry,arguments,swapspace)
                                call printer(dictsteno,swapspace)
                        else if (index(command,"quit") > 0) then
                                exit
                        else if (index(command,"help") > 0) then
                                print *, "Command list:"
                                print *, "finds [string] - Finds [string] in the steno array."
                                print *, "findt [string] - Finds [string] in the translation array."
                                print *, "quit - Saves RTF/CRE file, then exits."
                        else
                                print *, "Command not found, try help."
                        end if
                end do
        end subroutine repl

        subroutine printer(steno,trans)
                implicit none
                character(len=80), dimension(1000), intent(in) :: steno
                character(len=80), dimension(1000), intent(in) :: trans
                
                integer :: l
                integer :: indexofsteno
                integer :: indexoftrans
                logical :: stenoexists
                logical :: transexists
                do l=1,1000
                        indexofsteno = len(trim(steno(l)))
                        indexoftrans = len(trim(trans(l)))
                        stenoexists = (indexofsteno < 80 .and. indexofsteno > 0)
                        transexists = (indexoftrans < 80 .and. indexoftrans > 0)
                        if (stenoexists .and. transexists) then
                                print *, l, trim(steno(l)), " -> ", trim(trans(l))
                        end if
                end do
        end subroutine

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
        
        character(len=80), dimension(1000) :: dictionarycontent ! Fortran arrays start at 1.
        character(len=80), dimension(1000) :: dictentry
        character(len=80), dimension(1000) :: dictsteno

        integer :: numberoflines

        character (len=78) :: introduction ! 80 char list.
        character (len=80) :: headertext !Name of program

        character (len=1) :: esc ! escape character
        character(len=80) :: tuiTopLeft

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

        numberoflines = 0

        introduction = ""
        headertext = "Waffle RTF/CRE Editor"

        esc = achar(27)
        tuiTopLeft = esc//"[80A"//esc//"[80D"

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

        do k=1,1000
                read(1,'(a)',iostat = iostaterror) dictionarycontent(k)
                ! print *, trim(dictionarycontent(k)), " - SUBSTR ", index(dictionarycontent(k),"}")
                ! We need to be aware that not all entries in the array are usefull, and some have garbage in them.
                if (iostaterror > 0) then
                        numberoflines = k
                        exit
                end if
        end do
        
        ! 2 to avoid line one, which has random RTF metadata.
        do k=2,1000
                if (index(dictionarycontent(k),"}") > 3) then
                        lengthofentry = len(trim(dictionarycontent(k)))
                        lengthofsteno = index(dictionarycontent(k),"}")
                        dictsteno(k) = dictionarycontent(k)(9:lengthofsteno-1)
                        dictentry(k) = dictionarycontent(k)(lengthofsteno+1:lengthofentry)
                end if
        end do

        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - Manipulation.
        !---------------------------------------------------------------------------------------------------------------------------
        
        call repl(dictsteno,dictentry)

        ! Time to save the file:
        
        stop
        
        !---------------------------------------------------------------------------------------------------------------------------
        ! Execution - IGNORED
        !---------------------------------------------------------------------------------------------------------------------------

        print *, esc//"[2J", tuiTopLeft ! Blank screen, move cur to top, move cur to left
        print *, esc//"[1A", esc//"[30;42m", esc//"[2D", headertext ,esc//"[0m" ! Move cur up one, Set to black on green, move cur 2
        ! to left, ..., reset styles.
        print *, ""
!        do while (i < 80) 
!                eighty = trim(eighty) // "h"
!                i = i + 1
!        end do
        print *, introduction


        close(1)
end program waffleRTFEditor
