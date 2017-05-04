module useful

        contains

subroutine finder(collection,word,res)! result(res)
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

        call printer(res)
end subroutine

subroutine printer(collection)
        implicit none
        character(len=80), dimension(1000), intent(in) :: collection
        integer :: l
        integer :: indexofword
        do l=1,1000

                indexofword = len(trim(collection(l)))
                if (indexofword < 80 .and. indexofword > 0) then
                        print *, collection(l), indexofword
                end if
        end do
end subroutine

end module useful

function add(a,b) result(c)
        integer :: a
        integer :: b
        integer :: c

        c = a + b
end function

program waffleRTFEditor
        use useful
        implicit none

        !---------------------------------------------------------------------------------------------------------------------------
        !! Interfaces
        

        !interface
        !        subroutine finder(collection, word, res)
        !                character(len=80), dimension(1000), intent(in) :: collection
        !                character(len=80), intent(in) :: word
        !                character(len=80), dimension(1000), intent(out) :: res
        !        end subroutine finder
        !end interface


        !---------------------------------------------------------------------------------------------------------------------------
        !! Definition

        integer :: i, j, k ! loop var.

        character (len=32) :: dictionaryfile ! The filename of the dictionary.
        character(len=32) :: arg
        logical :: dictpresent
        integer :: iostaterror
        
        character(len=80), dimension(1000) :: dictionarycontent ! Fortran arrays start at 1.
        character(len=80), dimension(1000) :: dictentry
        character(len=80), dimension(1000) :: dictsteno
        
        character(len=80), dimension(1000) :: finderresults ! Related interface one.
        character(len=80) :: findersearchterm

        integer :: numberoflines

        character (len=78) :: introduction ! 80 char list.
        character (len=80) :: headertext !Name of program

        character (len=1) :: esc ! escape character
        character(len=80) :: tuiTopLeft

        integer :: lengthofentry
        integer :: lengthofsteno

        !---------------------------------------------------------------------------------------------------------------------------
        !! Initialization

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

        ! finderresults
        findersearchterm = ""

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
        print *, "Provide search term"
        do while (.true.)
                findersearchterm = ""
                read(*,*) findersearchterm
                if (trim(findersearchterm) ==  "QUIT") then
                        exit
                end if

                call finder(dictsteno,findersearchterm, finderresults)
                !print *, finderresults(1)
                
        end do

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
