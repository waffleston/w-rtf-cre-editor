program waffleRTFEditor
        implicit none
        
        ! Definitions
        integer :: i, j, k ! loop var.

        character (len=32) :: dictionaryfile ! The filename of the dictionary.
        character(len=32) :: arg
        logical :: dictpresent
        integer :: iostaterror
        
        !Fortran arrays start at 1, surprisingly.
        character(len=80), dimension(1000) :: dictionarycontent
        ! The dictionary entry is 80 chars long, and there are 1000 entries.

        integer :: numberoflines

        !Get command Arguments first.
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
        do k=1,1000
                read(1,'(a)',iostat = iostaterror) dictionarycontent(k)
                print *, trim(dictionarycontent(k)), " - SUBSTR ", index(dictionarycontent(k),"}")
                ! We need to be aware that not all entries in the array are usefull, and some have garbage in them.
                if (iostaterror > 0) then
                        print *, "Reached EOF!", k
                        numberoflines = k
                        exit
                end if
        end do
        
        do k=1,numberoflines
                if (index(dictionarycontent(k),"}") > 5) then

                        print *, dictionarycontent(k)
                end if
        end do


        close(1)
end program waffleRTFEditor
